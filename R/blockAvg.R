###########################################################################/**
# @set "class=matrix"
# @RdocMethod blockAvg
#
# @title "Average matrix"
#
# \description{
#  @get "title".
# }
# 
# @synopsis
#
# \arguments{
#   \item{X}{A @numeric NxM @matrix.}
#   \item{idxs}{An @integer KxJ @matrix specifying an block indices map.}
#   \item{FUN}{The @function used to average over each block.}
#   \item{W}{An optional @numeric NxM @matrix of weights.}
#   \item{...}{Additional arguments passed to then \code{FUN} @function.}
# }
#
# \value{
#   Returns a @numeric @matrix with colnames.
# }
#
# @examples "../incl/blockAvg.Rex"
#
# @author
#
# \seealso{
#   @see "getBlockAverageMap".
# }
#
# @keyword internal
# @keyword utilities
#*/###########################################################################
setMethodS3("blockAvg", "matrix", function(X, idxs, FUN=rowMeans, W=NULL, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'idxs':
  if (!is.matrix(idxs)) {
    throw("Argument 'idxs' is not a matrix: ", class(idxs)[1]);
  }
  if (!is.numeric(idxs)) {
    throw("Argument 'idxs' is not numeric: ", mode(idxs));
  }

  # Argument 'FUN':
  if (!is.function(FUN)) {
    throw("Argument 'FUN' is not a function: ", mode(idxs));
  }

  # Argument 'W':
  if (!is.null(W)) {
    if (!is.matrix(W)) {
      throw("Argument 'W' is not a matrix: ", class(W)[1]);
    }
    if (any(dim(W) != dim(X))) {
      throw("Argument 'W' does not have the same dimension as 'X': ", paste(dim(W), collapse="x"), " != ", paste(dim(X), collapse="x"));
    }
    if (!is.numeric(W)) {
      throw("Argument 'W' is not numeric: ", mode(W));
    }
  }
  

  # Check if missing values have to be excluded while averaging
  na.rm <- (anyMissing(X) || anyMissing(idxs));

  # Record names of dimension
  dimnames <- dimnames(X);
  dimnames(X) <- NULL;

  # Transpose input data and optional weights
  X <- t(X);
  if (!is.null(W)) {
    W <- t(W);
  }

  # Average in blocks of columns
  Z <- apply(idxs, MARGIN=2, FUN=function(jj) {
    # Extract block of columns from X
    jj <- jj[is.finite(jj)];
    Zjj <- X[,jj,drop=FALSE];

    # Average by weights
    if (!is.null(W)) {
      Wjj <- W[,jj,drop=FALSE];
      Zjj <- FUN(Zjj, W=Wjj, ..., na.rm=na.rm);
    } else {
      Zjj <- FUN(Zjj, ..., na.rm=na.rm);
    }
    Zjj;        
  });

  # Transpose back
  Z <- t(Z);
  colnames(Z) <- dimnames[[2]];

  Z;
}) # blockAvg()


##############################################################################
# HISTORY:
# 2008-01-11
# o Extracted from CRMA-Fig8,res,filtered.R.
# 2007-11-16
# o Added argument 'W' to blockAvg().
############################################################################## 
