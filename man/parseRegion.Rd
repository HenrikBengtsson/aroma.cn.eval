%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  parseRegion.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{parseRegion}
\alias{parseRegion}


\title{Parses a ROC change-point region string}

\description{
 Parses a ROC change-point region string in the format
 '<sample>:Chr<chr>@<start>-<stop>,cp=<pos>+/-<width>,s=<state0>/<state1>',
 where <sample> is a sample name, <chr> is an index, <start>, <stop>
 and <pos> (<width>) are genomic locations (lengths) (in units of Mb),
 and <state0> and <state1> are integers specifying the genomic state of
 the two segments flanking the change point at <pos>.
}

\usage{
parseRegion(region, xScale=1e+06, ...)
}

\arguments{
  \item{region}{A \code{\link[base]{character}} string.}
  \item{xScale}{A positive \code{\link[base]{numeric}} specifying the unit length.}
  \item{...}{Not used.}
}

\value{
  Returns a named \code{\link[base]{list}}.
}

\examples{
reg <- parseRegion("TCGA-23-1027:Chr2@108-140,cp=124+/-0.5,s=0/1")
str(reg)

stateFcn <- makeTruth(reg)
print(reg)

data <- data.frame(chromosome=2, x=seq(from=122e6, to=126e6, by=0.2e6))
data$state <- stateFcn(data)
print(data)
}

\seealso{
 \code{\link{makeTruth}}().
}

\author{Henrik Bengtsson}



\keyword{internal}
\keyword{utilities}
