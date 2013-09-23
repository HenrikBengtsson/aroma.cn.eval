setMethodS3("scanRocTpAtFp", "default", function(truth, data, fpRate, ..., W=NULL, hs=seq(from=1, to=10, by=0.1), fit=NULL, shifts=0, verbose=FALSE, .checkArgs=TRUE) {
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

    # Argument 'fpRate':
    fpRate <- Arguments$getDouble(fpRate, range=c(0,1));

    # Argument 'hs':
    hs <- Arguments$getDoubles(hs, range=c(1,Inf));
    hs <- unique(hs);
    hs <- sort(hs);

    # Argument 'verbose':
    verbose <- Arguments$getVerbose(verbose);
    if (verbose) {
      pushState(verbose);
      on.exit(popState(verbose));
    }
  }

  verbose && enter(verbose, "Finding TP rates for different amount of smoothing");
  verbose && cat(verbose, "FP rate: ", fpRate);

  dim <- dim(data);
  verbose && cat(verbose, "Data dimensions: ", paste(dim, collapse="x"));
  if (!identical(dim, dim(truth)))
    throw("Internal error: ");

  nbrOfNAs <- sum(is.na(data));
  verbose && printf(verbose, "Number of missing values: %d\n", nbrOfNAs);

  # Calculate the fraction (0 <= c <= 1) of non-missing values
  fractionOKs <- (1 - nbrOfNAs / length(data));
  verbose && printf(verbose, "Fraction of OK observations: %.4f%%\n",
                                                       100*fractionOKs);

  # Record original (truth, data)
  data0 <- data;
  truth0 <- truth;

  if (is.null(fit)) {
    colnames <- c("h", "hApprox", "tpRateEst", "callRate");
    fit <- matrix(NA, nrow=length(hs), ncol=length(colnames));
    fit[,1] <- hs;
    colnames(fit) <- colnames;
  } else {
    hs <- hs[!(hs %in% fit[,1])];
    # Expand fit for these new ones
    t <- matrix(NA, nrow=length(hs), ncol=ncol(fit));
    t[,1] <- hs;
    fit <- rbind(fit, t);
    rm(t);
    o <- order(fit[,1]);
    fit <- fit[o,];
    rm(o);
  }

  # Skip already existing ones.
  for (kk in seq(along=hs)) {
    h <- hs[kk];

    verbose && enter(verbose, sprintf("Iteration #%d of %d", kk, length(hs)));
    verbose && printf(verbose, "h: %.4f\n", h);

    if (h == 1) {
      verbose && enter(verbose, "No smoothing of (truth, data)");
      # No smoothing
      hApprox <- h;
      data <- data0;
      truth <- truth0;
      verbose && exit(verbose);
    } else {
      verbose && enter(verbose, "Smoothing (truth, data)");
      data <- truth <- c();
      for (shift in shifts) {
        idxs <- getBlockAverageMap(n=dim[1], h=h, s=shift);
        if (is.null(W)) {
          dataAvg <- colAvgsPerRowSet(data0, S=idxs);
        } else {
          dataAvg <- colAvgsPerRowSet(data0, W=W, S=idxs,
                                      FUN=rowWeightedMeans.matrix, tFUN=TRUE);
        }
        data <- rbind(data, dataAvg);
        truth <- rbind(truth, colAvgsPerRowSet(truth0, S=idxs));
      } # for (shift ...)
      hApprox <- attr(idxs, "hApprox");
      verbose && exit(verbose);
    }

    verbose && enter(verbose, "Finding TP rate at FP rate for smoothed (truth, data)");
    # Find TP rate at given FP rate
    fitT <- findRocTpAtFp(truth, data, fpRate=fpRate, ...,
                                  verbose=less(verbose), .checkArgs=FALSE);
    verbose && exit(verbose);

    tpRateEst <- fitT$tpRateEst;
    callRate <- fitT$callRate;
    verbose && cat(verbose, "Estimated TP rate: ", tpRateEst);
    verbose && cat(verbose, "Call rate: ", callRate);

    rr <- which(h == fit[,1]);

    fit[rr,] <- c(h, hApprox, tpRateEst, callRate);
    verbose && print(verbose, fit);

    verbose && exit(verbose);
  } # while(...)

  attr(fit, "fractionOKs") <- fractionOKs;

  verbose && exit(verbose);

  fit;
}) # scanRocTpAtFp()

############################################################################
# HISTORY:
# 2013-09-23
# o SPEEDUP/CLEANUP: normalizeTumorBoost() now uses which() instead of
#   whichVector() of 'R.utils'.  Before R (< 2.11.0), which() used to be
#   10x slower than whichVector(), but now it's 3x faster.
# 2009-02-01
# o Renamed from scanTpAtFpLite() to scanRdocTpAtFp().
# 2008-07-25
# o Updated.
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
