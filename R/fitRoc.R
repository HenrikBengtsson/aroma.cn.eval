###########################################################################/**
# @RdocDefault fitRoc
#
# @title "Calculates the Receiver Operating Characteristic (ROC)"
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
#   \item{recall}{(Optional) Unless \code{truth} is given as binary
#     \eqn{\{0,1\}} values, it can be reclassified as such.}
#   \item{idxs}{}
#   \item{ncuts}{}
#   \item{hasNAs}{If @TRUE (@FALSE), ROC is calculated as if there are
#     (no) missing values. [Not used!]}
#   \item{isOrdered}{If @FALSE, data is ordered, otherwise not.}
#   \item{...}{Not used.}
#   \item{verbose}{See @see "R.utils::Verbose".}
# }
#
# \value{
#   Returns a @list with elements:
#    \item{roc}{A Kx2 @double @matrix where each row contains an estimate
#      of (FP,TP) rates at a given cut.}
#    \item{cuts}{A @double @vector of length K of the correspond cuts used.}
# }
#
# @examples "../incl/fitRoc.Rex"
#
# @author
#
# @keyword internal
# @keyword utilities
#*/###########################################################################
setMethodS3("fitRoc", "default", function(truth, data, recall=NULL, idxs=NULL, ncuts=1200, hasNAs=TRUE, isOrdered=FALSE, as=c("list", "RocCurve"), ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

  # Argument 'idxs':
  if (!is.null(idxs)) {
    idxs <- Arguments$getIndices(idxs);
  }

  # Argument 'as':
  as <- match.arg(as);


  verbose && enter(verbose, "Fitting ROC");

  truth <- as.vector(truth);  # Calling/counting 1's
  data <- as.vector(data);    # in [-Inf, Inf]

  # Remove missing values?
  if (hasNAs) {
    if (FALSE) {
      verbose && enter(verbose, "Treating NAs as FPs");
      bad <- is.na(data);
      data[bad] <- Inf;
      verbose && exit(verbose);
    } else {
      verbose && enter(verbose, "Removing NAs");
      ok <- (is.finite(truth) & is.finite(data));
      truth <- truth[ok];
      data <- data[ok];
      ok <- NULL; # Not needed anymore
      verbose && exit(verbose);
    }
  }

  if (!isOrdered) {
    verbose && enter(verbose, "Ordering data");
    # (If perfect separation, all 0's and 1's are at seperate ends)
    o <- order(data);
    truth <- truth[o];
    data <- data[o];
    o <- NULL; # Not needed anymore
    verbose && exit(verbose);
  }

  if (is.null(idxs)) {
    verbose && enter(verbose, "Identifying indices for cuts");
    n <- length(data);
    idxs <- seq(from=1, to=n, length.out=ncuts+1L);
    verbose && exit(verbose);
  }

  cuts <- data[idxs];
  data <- NULL; # Not needed anymore

  # Turn 'truth' into (0,1) variable by re-calling?
  if (!is.null(recall)) {
    verbose && enter(verbose, "Recalling truth to (0,1)");
    verbose && cat(verbose, "Calling value: ", recall);
    truth <- (truth == recall);
    truth <- as.integer(truth);
    verbose && exit(verbose);
  }

  ns <- c(P=sum(truth), N=NA_integer_);
  ns[2L] <- length(truth) - ns["P"];

  verbose && enter(verbose, "Counting TPs and FPs in blocks");
  ncounts <- length(idxs)-1L;
  counts <- matrix(0L, nrow=ncounts, ncol=2);
  for (kk in 1:ncounts) {
    # Identify block of data points to count
    ii <- idxs[kk]:(idxs[kk+1L]);

    # Count the number 1's
    counts[kk,2] <- sum(truth[ii]);

    # Count the number 0's
    counts[kk,1] <- length(ii) - counts[kk,2];
  }
  truth <- NULL; # Not needed anymore

  # Add up cumulatively
  counts <- apply(counts, MARGIN=2, FUN=cumsum);

  # Calculate FP and TP rates
  counts[,1] <- counts[,1] / counts[ncounts,1];
  counts[,2] <- counts[,2] / counts[ncounts,2];
  colnames(counts) <- c("fpRate", "tpRate");

  verbose && exit(verbose);


  # Return structure
  if (as == "list") {
    res <- list(roc=counts, cuts=cuts, counts=ns);
  } else if (as == "RocCurve") {
    res <- RocCurve(roc=counts, cuts=cuts, counts=ns);
  }

  verbose && exit(verbose);

  res;
}) # fitRoc()



############################################################################
# HISTORY:
# 2014-03-24
# o Now fitRoc() also returns the number of positives and negatives.
# o Added argument 'as' to fitRoc().
# 2009-01-29
# o Added Rdoc comments.
# o Renamed from fitRocLite() to fitRoc().
# 2007-08-24
# o Added findTpAtFpLite() which is quite fast.
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
