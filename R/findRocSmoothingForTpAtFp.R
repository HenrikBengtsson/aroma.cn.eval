###########################################################################/**
# @RdocDefault findRocSmoothingForTpAtFp
#
# @title "Find the amount of smoothing needed to obtain a minimum true-positive (TP) rate at a give false-positive (FP) rate"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{truth}{A @numeric @vector of length N.}
#   \item{data}{A @numeric @vector of length N.}
#   \item{fpRate}{A @double in [0,1] specifying the target FP rate.}
#   \item{minTpRate}{A @double in [0,1] specifying the minimum TP rate.}
#   \item{nstepsR}{An @integer ...}
#   \item{accTp}{A @double specifying the accuracy ...}
#   \item{accR}{A @double specifying the accuracy ...}
#   \item{...}{Additional arguments passed to @see "findRocTpAtFp".}
#   \item{verbose}{See @see "R.utils::Verbose".}
#   \item{.checkArgs}{If @TRUE, arguments are validated, otherwise not.}
# }
#
# \value{
#   Returns a positive @double scalar.
# }
#
# @author
#
# \seealso{
#   @see "findRocTpAtFp".
#   @see "scanRocTpAtFp".
# }
#
# @keyword internal
# @keyword utilities
#*/###########################################################################
setMethodS3("findRocSmoothingForTpAtFp", "default", function(truth, data, fpRate=0.05, minTpRate=0.95, nstepsR=2, accTp=0.001, accR=0.01, ..., verbose=FALSE, .checkArgs=TRUE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (.checkArgs) {
    # Argument 'truth':
    if (!is.matrix(truth))
      throw("Argument 'truth' is not a matrix.");

    # Argument 'data':
    if (!is.matrix(data))
      throw("Argument 'data' is not a matrix.");

    # Argument 'verbose':
    verbose <- Arguments$getVerbose(verbose);
    if (verbose) {
      pushState(verbose);
      on.exit(popState(verbose));
    }
  }


  verbose && enter(verbose, "Searching for the amount of smoothing needed to achieve TP rate at given FP rate");

  # Remember orignal (truth, data) tuple
  truth0 <- truth;
  data0 <- data;

  # Number of data rows
  n <- nrow(truth);

  hTp <- matrix(0, nrow=1, ncol=3);

  h <- c(1,5,10);
  lastR <- Inf;
  iter <- 1;
  while (diff(range(h)) > accR) {
    verbose && enter(verbose, sprintf("Iteration #%d", iter));
    verbose && printf(verbose, "minTpRate: %.4f\n", minTpRate);
    verbose && printf(verbose, "accTpRate: %.4f\n", accTp);
    verbose && printf(verbose, "Levels: %s\n", paste(h, collapse=","));

    # Create block-averaging map for smoothing (truth, data) tuple
    idxs <- getBlockAverageMap(n=n, h=h[2]);
    hApprox2 <- attr(idxs, "hApprox");

    # Smooth (truth, data)
    truth <- colAvgsPerRowSet(truth, S=idxs);
    data <- colAvgsPerRowSet(data, S=idxs);

    # Find TP rate at given FP rate for smoothed (truth, data)
    fit <- findRocTpAtFp(truth, data, fpRate=fpRate, ...,
                              verbose=less(verbose), .checkArgs=FALSE);

    # The identified TP rate
    tpRate2 <- fit$tpRate;

    verbose && printf(verbose, "tpRate @ %.4f (~%.4f): %.4f\n", h[2], hApprox2, tpRate2);

    if (!h[2] %in% hTp[,1]) {
      t <- c(h[2], hApprox2, tpRate2);
      hTp <- rbind(hTp, t);
      o <- order(hTp[,1]);
      hTp <- hTp[o,];
    }

    if (tpRate2 > minTpRate) {
      # All done?
      if (abs(tpRate2-minTpRate) < accTp)
        break;
      hUse <- h[1];
    } else {
      hUse <- h[3];
    }

    idxs <- getBlockAverageMap(n=n, h=hUse);
    hApprox <- attr(idxs, "hApprox");
    data <- colAvgsPerRowSet(data0, S=idxs);
    truth <- colAvgsPerRowSet(truth0, S=idxs);

    fit <- findRocTpAtFp(truth, data, fpRate=fpRate, ..., verbose=less(verbose), .checkArgs=FALSE);
    tpRate <- fit$tpRate;
    verbose && printf(verbose, "tpRate @ %.4f (~%.4f): %.4f\n", hUse, hApprox, tpRate);

    if (!hUse %in% hTp[,1]) {
      t <- c(hUse, hApprox, tpRate);
      hTp <- rbind(hTp, t);
      o <- order(hTp[,1]);
      hTp <- hTp[o,];
    }

    if (tpRate > minTpRate) {
      # All done?
      if (abs(tpRate-minTpRate) < accTp)
        break;
    }

    if (tpRate2 > minTpRate) {
      h <- c(h[1], h[2]);
      # Approximate estimate; need adjustment 10% of boundary?
      if (tpRate > minTpRate)
        h[1] <- h[1] - 0.05*diff(h);
    } else {
      h <- c(h[2], h[3]);
      # Approximate estimate; need adjustment 10% of boundary?
      if (tpRate < minTpRate)
        h[2] <- h[2] + 0.05*diff(h);
    }
    h <- c(h[1], (h[1]+h[2])/2, h[2]);

    print(hTp);

    iter <- iter + 1;
    verbose && exit(verbose);
  } # while(...)

  verbose && exit(verbose);

  hTp;
}) # findRocSmoothingForTpAtFp()




############################################################################
# HISTORY:
# 2013-12-12
# o DOCUMENTATION: Added help for findRocSmoothingForTpAtFp().
# 2007-08-20
# o Added file caching to fitRoc2().
# 2007-08-19
# o Renamed argument 'call' to 'toCall' in fitRoc().
# 2007-04-15
# o Added scanTpAtFp().
# 2007-04-14
# o Added interpolation in findTpAtFp().
# o Removed gc() from fitRoc().
# o Added findTpAtFp() to locate TP rate for given FP rate.
# 2007-03-2x
# o Added fitRoc().
# o Created.
############################################################################
