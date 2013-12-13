%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  makeTruth.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{makeTruth}
\alias{makeTruth}


\title{Creates a state function for a given ROC change-point region}

\description{
 Creates a state function for a given ROC change-point region
}

\usage{
makeTruth(region, ..., verbose=FALSE)
}

\arguments{
  \item{region}{A \code{\link[base]{character}} string.}
  \item{...}{Not used.}
  \item{verbose}{See \code{\link[R.utils]{Verbose}}.}
}

\value{
 Returns a \code{\link[base]{function}} that takes a \code{\link[base]{data.frame}} of (chromosome, x) loci
 and returns the genomic states for those loci.
}

\seealso{
 \code{\link{parseRegion}}().
}

\author{Henrik Bengtsson}



\keyword{internal}
\keyword{utilities}
