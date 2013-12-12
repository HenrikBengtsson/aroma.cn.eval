###########################################################################/**
# @set "class=matrix"
# @RdocMethod blockAvg
#
# @title "Average matrix"
#
# \description{
#  @get "title".
#
#  \emph{This method is deprecated. Use @see "matrixStats::colAvgsPerRowSet"
#  instead.}
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
  colAvgsPerRowSet(X, W=W, S=idxs, FUN=FUN, tFUN=TRUE, ...);
}, deprecated=TRUE) # blockAvg()


##############################################################################
# HISTORY:
# 2011-11-29
# o DEPRECATED: blockAvg() has been replaced by colAvgsPerRowSet() in
#   the matrixStats package.
# 2008-01-11
# o Extracted from CRMA-Fig8,res,filtered.R.
# 2007-11-16
# o Added argument 'W' to blockAvg().
##############################################################################
