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
parseRegion(region, ...)
}

\arguments{
  \item{region}{A \code{\link[base]{character}} string.}
  \item{...}{Not used.}
}

\value{
  Returns a named \code{\link[base]{list}}.
}

\examples{
reg <- parseRegion("TCGA-23-1027:Chr2@108-140,cp=124+/-0.5,s=0/1")
str(reg)
}

\author{Henrik Bengtsson}



\keyword{internal}
\keyword{utilities}
