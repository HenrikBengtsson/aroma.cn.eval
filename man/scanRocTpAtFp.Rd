%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  scanRocTpAtFp.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{scanRocTpAtFp}
\alias{scanRocTpAtFp.default}
\alias{scanRocTpAtFp}

\title{Find the ROC true-positive (TP) rate for a given false-positive (FP) rate}

\description{
 Find the ROC true-positive (TP) rate for a given false-positive (FP) rate.
}

\usage{
\method{scanRocTpAtFp}{default}(truth, data, fpRate, ..., W=NULL, hs=seq(from = 1, to = 10, by = 0.1),
  fit=NULL, shifts=0, verbose=FALSE, .checkArgs=TRUE)
}

\arguments{
  \item{truth}{An NxM \code{\link[base]{numeric}} \code{\link[base]{matrix}} of length N.}
  \item{data}{A NxM \code{\link[base]{numeric}} \code{\link[base]{matrix}} of length N.}
  \item{fpRate}{A \code{\link[base]{double}} in [0,1] specifying the target FP rate.}
  \item{...}{Additional arguments passed to \code{\link{findRocTpAtFp}}().}
  \item{W}{An optional NxM \code{\link[base]{numeric}} \code{\link[base]{matrix}} specifying weights in [0,1]
   specific to each data points in \code{data}.}
  \item{hs}{A \code{\link[base]{double}} \code{\link[base]{vector}} with elements equal or greater than one.}
  \item{fit}{...}
  \item{shifts}{A \code{\link[base]{double}} \code{\link[base]{vector}} ...}
  \item{verbose}{See \code{\link[R.utils]{Verbose}}.}
  \item{.checkArgs}{If \code{\link[base:logical]{TRUE}}, arguments are validated, otherwise not.}
}

\value{
  Returns a ...
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link{findRocTpAtFp}}().
  \code{\link{findRocSmoothingForTpAtFp}}().
}



\keyword{internal}
\keyword{utilities}
