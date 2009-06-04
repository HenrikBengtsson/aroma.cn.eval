###########################################################################/**
# @RdocDefault getBlockAverageMap
#
# @title "Gets a index map for averaging blocks of data"
#
# \description{
#  @get "title".
# }
# 
# @synopsis
#
# \arguments{
#   \item{n}{An @integer specifying the total number of data points
#    to average over.}
#   \item{h}{An @integer (or @double) specifying the (average) number 
#    of data points to average over in each block.}
#   \item{s}{An (optional) positive @integer specifying amount of shift.}
#   \item{...}{Not used.}
# }
#
# \value{
#   Returns an @integer vector of indices.
#   The @double attribute \code{hApprox} specifies the on average number of
#   data points in each block.
# }
#
# @examples "../incl/getBlockAverageMap.Rex"
#
# @author
#
# \seealso{
#   @see "blockAvg.matrix".
# }
#
# @keyword internal
# @keyword utilities
#*/###########################################################################
setMethodS3("getBlockAverageMap", "default", function(n, h=1, s=0, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'n':
  n <- Arguments$getInteger(n, range=c(1,Inf));

  # Argument 'h':
  h <- Arguments$getDouble(h, range=c(1,1000));

  # Argument 's':
  s <- Arguments$getInteger(s, range=c(0,ceiling(h)));


  # Is h an integer?
  if (h == as.integer(h)) {
    idxs <- matrix(seq_len(n), nrow=h);
    # Drop "looped" indices
    if (n %% h != 0) {
      idxs <- idxs[,-ncol(idxs)];
    }
  } else {
    K <- ceiling(n/h);
    idxs <- matrix(TRUE, nrow=ceiling(h), ncol=K);
    steps <- (h %% 1) * ceiling(K);
    incl <- seq(from=1, to=K, length=steps);
    incl <- round(incl);
    idxs[ceiling(h), -incl] <- FALSE;

    # Shift?
    if (s > 0) {
      lastRow <- idxs[ceiling(h),];
      tail <- seq(from=length(lastRow)-s+1, to=length(lastRow));
      lastRow <- c(lastRow[tail], lastRow[-tail]);
      idxs[ceiling(h),] <- lastRow;
    }
    values <- seq_len(n);
    values <- rep(values, length.out=sum(idxs));
    idxs[idxs] <- values;
    idxs[idxs == 0] <- NA;
  }

  # Skip last column in case looping
  if (n %% h != 0)
    idxs <- idxs[,-ncol(idxs)];

  # The effective 'h'
  hApprox <- sum(!is.na(idxs))/ncol(idxs);
  attr(idxs, "hApprox") <- hApprox;

  idxs;
}) # getBlockAverageMap()


##############################################################################
# HISTORY:
# 2009-02-01
# o BUG FIX: In more recent versions of R, idxs[idxs] <- seq_len(n) did not
#   work because if sum(idxs) != n the values were not looped over 
#   automatically.
# o Added Rdoc comments and an example.
# o Extracted from blockAvg.R.
# 2008-01-11
# o Extracted from CRMA-Fig8,res,filtered.R.
# 2007-11-16
# o Added argument 'W' to blockAvg().
############################################################################## 
