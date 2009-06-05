%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  SegmentedCopyNumbers.ROC.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{findRocTpAtFp.SegmentedCopyNumbers}
\alias{findRocTpAtFp.SegmentedCopyNumbers}
\alias{SegmentedCopyNumbers.findRocTpAtFp}
\alias{findRocTpAtFp.SegmentedCopyNumbers}
\alias{findRocTpAtFp,SegmentedCopyNumbers-method}


\title{Estimates the true-positive rate at a given false-positive rate for calling copy numbers of state}

\description{
 Estimates the true-positive rate at a given false-positive rate for calling copy numbers of state.
}

\usage{\method{findRocTpAtFp}{SegmentedCopyNumbers}(this, states=NULL, recall=states[1], ...)}

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
  \code{\link[aroma.core]{SegmentedCopyNumbers}}
}
\keyword{internal}
\keyword{methods}
