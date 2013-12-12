###########################################################################/**
# @RdocDefault findRocTpAtFp
#
# @title "Find the ROC true-positive (TP) rate for a given false-positive (FP) rate"
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
#   \item{acc}{A @double specifying the accuracy.}
#   \item{recall}{(Optional) Unless \code{truth} is given as binary
#     \eqn{\{0,1\}} values, it can be reclassified as such.}
#   \item{hasNAs}{If @TRUE (@FALSE), ROC is calculated as if there are
#     (no) missing values. [Not used!]}
#   \item{isOrdered}{If @FALSE, data is ordered, otherwise not.}
#   \item{...}{Not used.}
#   \item{.checkArgs}{If @TRUE, arguments are validated, otherwise not.}
#   \item{verbose}{See @see "R.utils::Verbose".}
# }
#
# \value{
#   Returns a @list with elements:
#   \item{tpRateEst,tpRateEstRange}{The estimated TP rate with lower and upper limits.}
#   \item{fpRateEst,fpRateEstRange}{The estimated FP rate with lower and upper limits. This should be close to the request target FP rate.}
#   \item{fpRate}{The target FP rate (equals the corresponding argument).}
#   \item{callRate}{Fraction of data points called.  If less than one, for
#     instance data points with missing values may have been excluded.}
#   \item{orderedIdxs}{The indices of the ordered data points corresponding to
#     the lower and upper limits.}
#   \item{w}{The weights of the lower and upper limits.}
# }
#
# @examples "../incl/findRocTpAtFp.Rex"
#
# @author
#
# \seealso{
#   @see "findRocSmoothingForTpAtFp".
#   @see "scanRocTpAtFp".
# }
#
# @keyword internal
# @keyword utilities
#*/###########################################################################
setMethodS3("findRocTpAtFp", "default", function(truth, data, fpRate, acc=1e-3, recall=NULL, hasNAs=TRUE, isOrdered=FALSE, ..., .checkArgs=TRUE, verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (.checkArgs) {
    # Argument 'verbose':
    verbose <- Arguments$getVerbose(verbose);
    if (verbose) {
      pushState(verbose);
      on.exit(popState(verbose));
    }

    # Argument 'data':
    if (length(data) != length(truth)) {
      throw("Argument 'data' and 'truth' are of different lengths: ",
                                 length(data), " != ", length(truth));
    }

    # Argument 'fpRate':
    fpRate <- Arguments$getDouble(fpRate, range=c(0,1));
  }



  verbose && enter(verbose, "Finding ROC TP rate at given FP rate");

  truth <- as.vector(truth);
  data <- as.vector(data);

  # Remove missing values?
  if (hasNAs) {
    verbose && enter(verbose, "Removing NAs");
    ok <- (is.finite(truth) & is.finite(data));
    callRate <- sum(ok)/length(ok);
    truth <- truth[ok];
    data <- data[ok];
    rm(ok);
    verbose && exit(verbose);
  } else {
    callRate <- 1;
  }

  if (!isOrdered) {
    verbose && enter(verbose, "Ordering data");
    o <- order(data);
    truth <- truth[o];
    data <- data[o];
    rm(o);
    verbose && exit(verbose);
  }

  # Turn 'truth' into (0,1) variable by re-calling?
  if (!is.null(recall)) {
    verbose && enter(verbose, "Recalling truth to (0,1)");
    verbose && cat(verbose, "Calling value: ", recall);
    truth <- (truth == recall);
    truth <- as.integer(truth);
    verbose && exit(verbose);
  }

  nbrOfDataPoints <- length(truth);
  totalTpCount <- as.integer(sum(truth));
  totalFpCount <- nbrOfDataPoints - totalTpCount;
  verbose && printf(verbose, "Total (FP,TP): (%d,%d)\n",
                                         totalFpCount, totalTpCount);

  fpCount <- fpRate * totalFpCount;
##  fpCount <- fpRate * nbrOfDataPoints; ## ?!? /HB 2008-09-12

  C <- as.integer(1) - truth;
  verbose && str(verbose, C);
  C <- cumsum(C);
  verbose && cat(verbose, "Cumulative count of FPs:");
  verbose && str(verbose, C);

  verbose && cat(verbose, "Position of FP count equal or smaller than target:");
  idx <- match(floor(fpCount), C);
  verbose && str(verbose, idx);

  fpCountEstRange <- c(C[idx], C[idx]+1);
  fpRateEstRange <- fpCountEstRange/totalFpCount;
  verbose && cat(verbose, "FP rate interval:");
  verbose && print(verbose, fpRateEstRange);

  d <- abs(fpRateEstRange - fpRate);
  w <- rev(d) / sum(d);
  fpRateEst <- w[1]*fpRateEstRange[1]+w[2]*fpRateEstRange[2];
  verbose && cat(verbose, "Estimated FP rate:");
  verbose && print(verbose, fpRateEst);

  tpCountEstRange <- c(idx, idx+1) - fpCountEstRange;
  tpRateEstRange <- tpCountEstRange/totalTpCount;
  verbose && cat(verbose, "TP rate interval:");
  verbose && print(verbose, tpRateEstRange);

  tpRateEst <- w[1]*tpRateEstRange[1]+w[2]*tpRateEstRange[2];
  verbose && cat(verbose, "Estimated TP rate:");
  verbose && print(verbose, tpRateEst);

  verbose && exit(verbose);

  list(tpRateEst=tpRateEst, tpRateEstRange=tpRateEstRange,
       fpRateEst=fpRateEst, fpRateEstRange=fpRateEstRange, fpRate=fpRate,
       callRate=callRate,
       orderedIdxs=as.integer(c(idx, idx+1)), w=w);
}) # findRocTpAtFp()


############################################################################
# HISTORY:
# 2009-02-01
# o Renamed from findTpAtFpLite() to findRocTpAtFp().
# 2008-07-25
# o Added 'callRate' to output.
# 2008-07-24
# o Created.
############################################################################
