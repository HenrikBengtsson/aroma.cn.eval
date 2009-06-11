%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  SegmentedGenomicSignals.ROC.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{findRocTpAtFp.SegmentedGenomicSignalsInterface}
\alias{findRocTpAtFp.SegmentedGenomicSignalsInterface}
\alias{SegmentedGenomicSignalsInterface.findRocTpAtFp}
\alias{findRocTpAtFp.SegmentedGenomicSignalsInterface}
\alias{findRocTpAtFp,SegmentedGenomicSignalsInterface-method}


\title{Estimates the true-positive rate at a given false-positive rate for calling the state of genomic loci}

\description{
 Estimates the true-positive rate at a given false-positive rate for calling the state of genomic loci.
}

\usage{\method{findRocTpAtFp}{SegmentedGenomicSignalsInterface}(this, states=NULL, recall=states[1], ...)}

\arguments{
  \item{states}{}
  \item{recall}{}
  \item{...}{Additional arguments passed to \code{\link{findRocTpAtFp}}().}
}

\value{
 Returns what \code{\link{findRocTpAtFp}}() returns.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[aroma.core]{SegmentedGenomicSignalsInterface}}
}
\keyword{internal}
\keyword{methods}
