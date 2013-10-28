%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  SegmentedGenomicSignals.ROC.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{findRocSmoothingForTpAtFp.SegmentedGenomicSignalsInterface}
\alias{findRocSmoothingForTpAtFp.SegmentedGenomicSignalsInterface}
\alias{SegmentedGenomicSignalsInterface.findRocSmoothingForTpAtFp}
\alias{findRocSmoothingForTpAtFp,SegmentedGenomicSignalsInterface-method}

\title{Finds the abount of binned smoothing required to achieve a true-positive at a given false-positive rate}

\description{
 Finds the abount of binned smoothing required to achieve a true-positive at a given false-positive rate.
}

\usage{
\method{findRocSmoothingForTpAtFp}{SegmentedGenomicSignalsInterface}(this, minTpRate=0.95, fpRate=0.05, states=NULL,
  recall=states[1], nstepsR=2, accTp=0.001, accR=0.01, ..., verbose=FALSE)
}

\arguments{
  \item{states}{}
  \item{recall}{}
  \item{fpRate}{}
}

\value{
 Returns a \code{\link[base]{list}}.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[aroma.core]{SegmentedGenomicSignalsInterface}}
}
\keyword{internal}
\keyword{methods}
