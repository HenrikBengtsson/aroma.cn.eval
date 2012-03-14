setMethodS3("unique", "list", function(x, ..., fromLast=FALSE, FUN=all.equal) {
  # Argument 'FUN':
  stopifnot(is.function(FUN));

  # Argument 'fromLast':
  fromLast <- as.logical(fromLast);
  if (fromLast) {
    stop("unique(..., fromLast=TRUE) is not implemented for lists.");
  }

  # Nothing todo?
  if (length(x) <= 1) {
    return(x);
  }

  xPrev <- NULL;
  while(!identical(x, xPrev)) {
    xPrev <- x;
    n <- length(x);
    for (ii in seq(length=n-1L)) {
      for (jj in (ii+1L):n) {
        # Drop, iff equal
        if (FUN(x[[jj]], x[[ii]])) {
          x[[jj]] <- NULL;
          break;
        }
      } # for (jj ...)
    } # for (ii ...)
  } # while()

  rm(xPrev);

  x;
}) # unique()


##############################################################################
# HISTORY:
# 2012-02-22
# o Created.
##############################################################################
