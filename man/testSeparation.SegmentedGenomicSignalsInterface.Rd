%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  SegmentedGenomicSignalsInterface.testSeparation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{testSeparation.SegmentedGenomicSignalsInterface}
\alias{testSeparation.SegmentedGenomicSignalsInterface}
\alias{SegmentedGenomicSignalsInterface.testSeparation}
\alias{testSeparation,SegmentedGenomicSignalsInterface-method}

\title{Tests statistically the separation between two states}

\description{
 Tests statistically the separation between two states.
}

\usage{
\method{testSeparation}{SegmentedGenomicSignalsInterface}(this, test=c("t.test", "ks.test"), stateIdxs=1:2, ...)
}

\arguments{
  \item{test}{A \code{\link[base]{character}} string specifying the statistical test to use.}
  \item{stateIdxs}{An \code{\link[base]{integer}} \code{\link[base]{vector}} specifying the indicies of the
    two states to be used.}
  \item{...}{Not used.}
}

\value{
 Returns what test function returns.
}

\author{Henrik Bengtsson, Pierre Neuvial}

\seealso{
  \code{\link[aroma.core]{SegmentedGenomicSignalsInterface}}
}
\keyword{internal}
\keyword{methods}
