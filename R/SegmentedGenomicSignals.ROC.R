###########################################################################/**
# @set "class=SegmentedGenomicSignalsInterface"
# @RdocMethod fitRoc
#
# @title "Estimates the ROC for calling the state of genomic loci"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{states}{}
#   \item{recall}{}
#   \item{...}{Additional arguments passed to @see "fitRoc".}
# }
#
# \value{
#  Returns what @see "fitRoc" returns.
# }
#
# @examples "../incl/SegmentedCopyNumbers.ROC.Rex"
#
# @author
#
# \seealso{
#   @see "aroma.core::SegmentedGenomicSignalsInterface"
# }
#*/###########################################################################
setMethodS3("fitRoc", "SegmentedGenomicSignalsInterface", function(this, states=NULL, recall=states[1], ...) {
  # Extract by state?
  if (!is.null(states)) {
    this <- extractSubsetByState(this, states=states);
  }

  # Extract data
  data <- as.data.frame(this, translate=FALSE);
  rm(this);

  # Recall?
  if (!is.null(recall)) {
    data$state <- as.integer(is.element(data$state, recall));
  }

  fitRoc(truth=data$state, data=data$y, ...);
})



###########################################################################/**
# @set "class=SegmentedGenomicSignalsInterface"
# @RdocMethod findRocTpAtFp
#
# @title "Estimates the true-positive rate at a given false-positive rate for calling the state of genomic loci"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{states}{}
#   \item{recall}{}
#   \item{...}{Additional arguments passed to @see "findRocTpAtFp".}
# }
#
# \value{
#  Returns what @see "findRocTpAtFp" returns.
# }
#
# @author
#
# \seealso{
#   @see "aroma.core::SegmentedGenomicSignalsInterface"
# }
#*/###########################################################################
setMethodS3("findRocTpAtFp", "SegmentedGenomicSignalsInterface", function(this, states=NULL, recall=states[1], ...) {
  # Extract by state?
  if (!is.null(states)) {
    this <- extractSubsetByState(this, states=states);
  }

  # Extract data
  data <- as.data.frame(this, translate=FALSE);
  rm(this);

  # Recall?
  if (!is.null(recall)) {
    data$state <- as.integer(is.element(data$state, recall));
  }

  findRocTpAtFp(truth=data$state, data=data$y, ...);
})



###########################################################################/**
# @set "class=SegmentedGenomicSignalsInterface"
# @RdocMethod findRocSmoothingForTpAtFp
#
# @title "Finds the abount of binned smoothing required to achieve a true-positive at a given false-positive rate"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{states}{}
#   \item{recall}{}
#   \item{fpRate}{}
# }
#
# \value{
#  Returns a @list.
# }
#
# @author
#
# \seealso{
#   @see "aroma.core::SegmentedGenomicSignalsInterface"
# }
#*/###########################################################################
setMethodS3("findRocSmoothingForTpAtFp", "SegmentedGenomicSignalsInterface", function(this, minTpRate=0.95, fpRate=0.05, states=NULL, recall=states[1], nstepsR=2, accTp=0.001, accR=0.01, ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'minTpRate':
  minTpRate <- Arguments$getDouble(minTpRate, range=c(0,1));

  # Argument 'fpRate':
  fpRate <- Arguments$getDouble(fpRate, range=c(0,1));

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  verbose && enter(verbose, "Searching for the amount of smoothing needed to achieve TP rate at given FP rate");

  verbose && enter(verbose, "Extracting genomic signals with states of interest");
  gs <- extractSubsetByState(this, states=states, recall=recall);
  verbose && print(verbose, gs);
  verbose && exit(verbose);

  binWidthRange <- c(1, 20);
  binWidth <- mean(binWidthRange);
  lastR <- Inf;  
  iter <- 1;
  status <- "UNKNOWN";
  status <- "TOOSMALL";
  while (TRUE) {
    verbose && enter(verbose, sprintf("Iteration #%d", iter));
    verbose && printf(verbose, "status: %s\n", status);
    verbose && printf(verbose, "minTpRate: %.4f\n", minTpRate);
    verbose && printf(verbose, "accTpRate: %.4f\n", accTp);
    verbose && printf(verbose, "binWidthRange: [%g,%g]\n", binWidthRange[1], binWidthRange[2]);
    verbose && printf(verbose, "binWidth: %g\n", binWidth);

    verbose && enter(verbose, "Smoothing");
    verbose && printf(verbose, "binWidth: %g\n", binWidth);
    gsS <- binnedSmoothingByState(gs, by=binWidth);
    gsS <- extractSubsetByState(gsS, states=states);
    verbose && print(verbose, gsS);
    verbose && exit(verbose);

    verbose && enter(verbose, "findRocTpAtFp()");
    # Find TP rate at given FP rate for smoothed (truth, data)
    fit <- findRocTpAtFp(gsS, fpRate=fpRate, recall=recall, ..., verbose=less(verbose));
    verbose && str(verbose, fit);
    verbose && exit(verbose);

    # The identified TP rate
    tpRateEst <- fit$tpRateEst;
    fpRateEst <- fit$fpRateEst;
    verbose && printf(verbose, "tpRate @ fpRate=%g & w=%g: %g\n", fpRateEst, binWidth, tpRateEst);

    binWidthLast <- binWidth;
    if (status == "TOOSMALL") {
      if (tpRateEst > minTpRate) {
        # All done?
        if (abs(tpRateEst-minTpRate) < accTp)
          break;
        status <- "TOOLARGE";
        binWidthRange[2] <- binWidthLast;
      } else {
        binWidthRange[1] <- binWidthLast;
      }
    } else if (status == "TOOLARGE") {
      if (tpRateEst < minTpRate) {
        # All done?
        if (abs(tpRateEst-minTpRate) < accTp)
          break;
        status <- "TOOSMALL";
        binWidthRange[1] <- binWidthLast;
      } else {
        binWidthRange[2] <- binWidthLast;
      }
    }
    binWidth <- binWidthRange[1] + 1/2*diff(binWidthRange);

    if (diff(binWidthRange) <= accR) {
      verbose && cat(verbose, "binWidthRange small enough: ", diff(binWidthRange), " <= ", accR);
      break;
    }

    iter <- iter + 1;
    verbose && exit(verbose);
  } # while(...)

  verbose && exit(verbose);

  list(tpRateEst=tpRateEst, fpRateEst=fpRateEst, binWidthRange=binWidthRange, binWidth=binWidth);
})


############################################################################
# HISTORY:
# 2009-06-10
# o Created ROC functions for SegmentedGenomicSignalsInterface from former
#   SegmentedCopyNumbers.ROC.R.
# 2009-02-08
# o Added findRocSmoothingForTpAtFp(). TODO:
#   - Recording all intermediate estimates.
#   - Interpolation of final results.
#   - Better return structure.
#   - Testing.
#   - Memoization.
# o Added Rdoc comments with examples.
# 2009-02-07
# o Created.
############################################################################
