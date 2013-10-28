%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  fitRoc.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{fitRoc}
\alias{fitRoc.default}
\alias{fitRoc}

 \title{Calculates the Receiver Operating Characteristic (ROC)}

 \description{
  Calculates the Receiver Operating Characteristic (ROC).
 }

 \usage{
\method{fitRoc}{default}(truth, data, recall=NULL, idxs=NULL, ncuts=1200, hasNAs=TRUE, isOrdered=FALSE,
  ..., verbose=FALSE)
}

 \arguments{
   \item{truth}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} of length N.}
   \item{data}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} of length N.}
   \item{recall}{(Optional) Unless \code{truth} is given as binary
     \eqn{\{0,1\}} values, it can be reclassified as such.}
   \item{idxs}{}
   \item{ncuts}{}
   \item{hasNAs}{If \code{\link[base:logical]{TRUE}} (\code{\link[base:logical]{FALSE}}), ROC is calculated as if there are
     (no) missing values. [Not used!]}
   \item{isOrdered}{If \code{\link[base:logical]{FALSE}}, data is ordered, otherwise not.}
   \item{...}{Not used.}
   \item{verbose}{See \code{\link[R.utils]{Verbose}}.}
 }

 \value{
   Returns a \code{\link[base]{list}} with elements:
    \item{roc}{A Kx2 \code{\link[base]{double}} \code{\link[base]{matrix}} where each row contains an estimate
      of (FP,TP) rates at a given cut.}
    \item{cuts}{A \code{\link[base]{double}} \code{\link[base]{vector}} of length K of the correspond cuts used.}
 }

 \examples{
n <- 1000
x <- rnorm(n=n, mean=0, sd=1)
y1 <- rnorm(n=n, mean=2, sd=1)
y2 <- rnorm(n=n, mean=3, sd=1)

T <- integer(2*n)
T[1:n] <- 1L

D1 <- c(x,y1)
D2 <- c(x,y2)

fit1 <- fitRoc(T,D1)
plot(fit1$roc, type="l", lwd=2, xlim=c(0,1), ylim=c(0,1))

fit2 <- fitRoc(T,D2)
lines(fit2$roc, type="l", lwd=2, col="red")
}

 \author{Henrik Bengtsson}



\keyword{internal}
\keyword{utilities}
