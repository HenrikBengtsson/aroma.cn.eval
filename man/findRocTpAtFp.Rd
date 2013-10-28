%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  findRocTpAtFp.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{findRocTpAtFp}
\alias{findRocTpAtFp.default}
\alias{findRocTpAtFp}

 \title{Find the ROC true-positive (TP) rate for a given false-positive (FP) rate}

 \description{
  Find the ROC true-positive (TP) rate for a given false-positive (FP) rate.
 }

 \usage{
\method{findRocTpAtFp}{default}(truth, data, fpRate, acc=0.001, recall=NULL, hasNAs=TRUE, isOrdered=FALSE,
  ..., .checkArgs=TRUE, verbose=FALSE)
}

 \arguments{
   \item{truth}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} of length N.}
   \item{data}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} of length N.}
   \item{fpRate}{A \code{\link[base]{double}} in [0,1] specifying the target FP rate.}
   \item{acc}{A \code{\link[base]{double}} specifying the accuracy.}
   \item{recall}{(Optional) Unless \code{truth} is given as binary
     \eqn{\{0,1\}} values, it can be reclassified as such.}
   \item{hasNAs}{If \code{\link[base:logical]{TRUE}} (\code{\link[base:logical]{FALSE}}), ROC is calculated as if there are
     (no) missing values. [Not used!]}
   \item{isOrdered}{If \code{\link[base:logical]{FALSE}}, data is ordered, otherwise not.}
   \item{...}{Not used.}
   \item{.checkArgs}{If \code{\link[base:logical]{TRUE}}, arguments are validated, otherwise not.}
   \item{verbose}{See \code{\link[R.utils]{Verbose}}.}
 }

 \value{
   Returns a \code{\link[base]{list}} with elements:
   \item{tpRateEst,tpRateEstRange}{The estimated TP rate with lower and upper limits.}
   \item{fpRateEst,fpRateEstRange}{The estimated FP rate with lower and upper limits. This should be close to the request target FP rate.}
   \item{fpRate}{The target FP rate (equals the corresponding argument).}
   \item{callRate}{Fraction of data points called.  If less than one, for
     instance data points with missing values may have been excluded.}
   \item{orderedIdxs}{The indices of the ordered data points corresponding to
     the lower and upper limits.}
   \item{w}{The weights of the lower and upper limits.}
 }

 \examples{
n <- 1000
x <- rnorm(n=n, mean=0, sd=1)
y <- rnorm(n=n, mean=2, sd=1)

T <- integer(2*n)
T[1:n] <- 1L

D <- c(x,y)

fit <- fitRoc(T,D)
par(xaxs="i", yaxs="i")
xlim <- c(0, 0.5)
plot(fit$roc, type="l", lwd=3, xlim=xlim, ylim=c(1-xlim[2], 1))

adj <- c(-0.05,-0.2)
for (fpRate in c(0.2, 0.4)) {
  fit <- findRocTpAtFp(T,D, fpRate=fpRate)
  fp <- fit$fpRateEst
  tp <- fit$tpRateEst
  lines(x=c(0,fp,fp), y=c(tp,tp,xlim[2]))
  text(x=fp, y=xlim[2], sprintf("FP rate: \%.3f", fp), srt=90, adj=adj)
  text(x=0, y=tp, sprintf("TP rate: \%.3f", tp), adj=adj)
}
}

 \author{Henrik Bengtsson}



\keyword{methods}
\keyword{internal}
\keyword{utilities}
