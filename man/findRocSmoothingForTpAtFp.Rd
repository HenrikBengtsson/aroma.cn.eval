%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  findRocSmoothingForTpAtFp.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{findRocSmoothingForTpAtFp}
\alias{findRocSmoothingForTpAtFp.default}
\alias{findRocSmoothingForTpAtFp}

\title{Find the amount of smoothing needed to obtain a minimum true-positive (TP) rate at a give false-positive (FP) rate}

\description{
 Find the amount of smoothing needed to obtain a minimum true-positive (TP) rate at a give false-positive (FP) rate.
}

\usage{
\method{findRocSmoothingForTpAtFp}{default}(truth, data, fpRate=0.05, minTpRate=0.95, nstepsR=2,
  accTp=0.001, accR=0.01, ..., verbose=FALSE, .checkArgs=TRUE)
}

\arguments{
  \item{truth}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} of length N.}
  \item{data}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} of length N.}
  \item{fpRate}{A \code{\link[base]{double}} in [0,1] specifying the target FP rate.}
  \item{minTpRate}{A \code{\link[base]{double}} in [0,1] specifying the minimum TP rate.}
  \item{nstepsR}{An \code{\link[base]{integer}} ...}
  \item{accTp}{A \code{\link[base]{double}} specifying the accuracy ...}
  \item{accR}{A \code{\link[base]{double}} specifying the accuracy ...}
  \item{...}{Additional arguments passed to \code{\link{findRocTpAtFp}}().}
  \item{verbose}{See \code{\link[R.utils]{Verbose}}.}
  \item{.checkArgs}{If \code{\link[base:logical]{TRUE}}, arguments are validated, otherwise not.}
}

\value{
  Returns a positive \code{\link[base]{double}} scalar.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link{findRocTpAtFp}}().
  \code{\link{scanRocTpAtFp}}().
}



\keyword{methods}
\keyword{internal}
\keyword{utilities}
