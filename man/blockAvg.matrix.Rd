%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  blockAvg.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 \name{blockAvg.matrix}
\alias{blockAvg.matrix}
\alias{blockAvg.matrix}


 \title{Average matrix}

 \description{
  Average matrix.
 }

 \usage{\method{blockAvg}{matrix}(X, idxs, FUN=rowMeans, W=NULL, ...)}

 \arguments{
   \item{X}{A \code{\link[base]{numeric}} NxM \code{\link[base]{matrix}}.}
   \item{idxs}{An \code{\link[base]{integer}} KxJ \code{\link[base]{matrix}} specifying an block indices map.}
   \item{FUN}{The \code{\link[base]{function}} used to average over each block.}
   \item{W}{An optional \code{\link[base]{numeric}} NxM \code{\link[base]{matrix}} of weights.}
   \item{...}{Additional arguments passed to then \code{FUN} \code{\link[base]{function}}.}
 }

 \value{
   Returns a \code{\link[base]{numeric}} \code{\link[base]{matrix}} with colnames.
 }

 \examples{
X <- matrix(rnorm(5*20), ncol=5)

for (h in c(1,2,3,4)) {
  print(h)
  map <- getBlockAverageMap(nrow(X), h=h)
  print(map)
  Xh <- blockAvg(X, idxs=map)
  print(Xh)
}


h <- 4.5
print(h)
map <- getBlockAverageMap(nrow(X), h=h)
print(map)
Xh <- blockAvg(X, idxs=map)
print(Xh)
}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link{getBlockAverageMap}}().
 }



\keyword{methods}
\keyword{internal}
\keyword{utilities}
