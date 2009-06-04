%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  getBlockAverageMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getBlockAverageMap}
\alias{getBlockAverageMap.default}
\alias{getBlockAverageMap}

 \title{Gets a index map for averaging blocks of data}

 \description{
  Gets a index map for averaging blocks of data.
 }

 \usage{\method{getBlockAverageMap}{default}(n, h=1, s=0, ...)}

 \arguments{
   \item{n}{An \code{\link[base]{integer}} specifying the total number of data points
    to average over.}
   \item{h}{An \code{\link[base]{integer}} (or \code{\link[base]{double}}) specifying the (average) number
    of data points to average over in each block.}
   \item{s}{An (optional) positive \code{\link[base]{integer}} specifying amount of shift.}
   \item{...}{Not used.}
 }

 \value{
   Returns an \code{\link[base]{integer}} vector of indices.
   The \code{\link[base]{double}} attribute \code{hApprox} specifies the on average number of
   data points in each block.
 }

 \examples{
# Blocks of two and two
map <- getBlockAverageMap(n=20, h=2)
print(map)
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
## [1,]    1    3    5    7    9   11   13   15   17    19
## [2,]    2    4    6    8   10   12   14   16   18    20
## attr(,"hApprox")
## [1] 2

# Blocks of three and three
map <- getBlockAverageMap(n=21, h=3)
print(map)
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
## [1,]    1    4    7   10   13   16   19
## [2,]    2    5    8   11   14   17   20
## [3,]    3    6    9   12   15   18   21
## attr(,"hApprox")
## [1] 3

# Blocks of three and three (dropping non-complete blocks)
map <- getBlockAverageMap(n=20, h=3)
print(map)
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    4    7   10   13
## [2,]    2    5    8   11   14
## [3,]    3    6    9   12   15
## attr(,"hApprox")
## [1] 3


# Blocks with on average 2.5 data points
map <- getBlockAverageMap(n=20, h=2.5)
print(map)
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## [1,]    1    4    6    9   11   13   16   18
## [2,]    2    5    7   10   12   14   17   19
## [3,]    3   NA    8   NA   NA   15   NA   20
## attr(,"hApprox")
## [1] 2.5

# Blocks with on average 2.5 data points (shifted one position)
map <- getBlockAverageMap(n=20, h=2.5, s=1)
print(map)
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## [1,]    1    4    7    9   12   14   16   19
## [2,]    2    5    8   10   13   15   17   20
## [3,]    3    6   NA   11   NA   NA   18   NA
## attr(,"hApprox")
## [1] 2.5

# Blocks with on average 2.5 data points (shifted two positions)
map <- getBlockAverageMap(n=20, h=2.5, s=2)
print(map)
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## [1,]    1    3    6    9   11   14   16   18
## [2,]    2    4    7   10   12   15   17   19
## [3,]   NA    5    8   NA   13   NA   NA   20
## attr(,"hApprox")
## [1] 2.5

# Blocks with on average ~2.4 data points (becomes ~2.364)
map <- getBlockAverageMap(n=28, h=2.4)
str(map)
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11]
## [1,]    1    4    6    8   11   13   16   18   20    23    25
## [2,]    2    5    7    9   12   14   17   19   21    24    26
## [3,]    3   NA   NA   10   NA   15   NA   NA   22    NA    NA
## attr(,"hApprox")
## [1] 2.363636
}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link{blockAvg.matrix}}().
 }



\keyword{internal}
\keyword{utilities}
