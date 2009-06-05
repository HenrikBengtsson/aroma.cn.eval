%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  SegmentedCopyNumbers.ROC.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{fitRoc.SegmentedCopyNumbers}
\alias{fitRoc.SegmentedCopyNumbers}
\alias{SegmentedCopyNumbers.fitRoc}
\alias{fitRoc.SegmentedCopyNumbers}
\alias{fitRoc,SegmentedCopyNumbers-method}


\title{Estimates the ROC for calling copy numbers of state}

\description{
 Estimates the ROC for calling copy numbers of state.
}

\usage{\method{fitRoc}{SegmentedCopyNumbers}(this, states=NULL, recall=states[1], ...)}

\arguments{
  \item{states}{}
  \item{recall}{}
  \item{...}{Additional arguments passed to \code{\link{fitRoc}}().}
}

\value{
 Returns what \code{\link{fitRoc}}() returns.
}

\examples{
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Simulating copy-number data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# True CN states
stateFcn <- function(x, ...) {
  states <- integer(length(x))
  states[200 <=x & x <= 500] <- -1L
  states[600 <=x & x <= 900] <- +1L
  states
}

# Number of loci
J <- 1000

y <- rnorm(J, sd=1/2)
x <- 1:length(y)
for (state in c(-1,+1)) {
  idxs <- (stateFcn(x) == state)
  y[idxs] <- y[idxs] + 0.5*state
}


cnR <- SegmentedCopyNumbers(y, x, states=stateFcn)
print(cnR)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Focus on gains
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
states <- c(0,1)
cn <- extractSubsetByState(cnR, states=states)
print(cn)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Smooth data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Amount of smoothing
binWidths <- c(1.5, 2, 4, 8, 12)
print(binWidths)
cnList <- list(raw=cn)
for (kk in seq(along=binWidths)) {
  binWidth <- binWidths[kk]
  cnS <- binnedSmoothingByState(cn, by=binWidth)
  cnS <- extractSubsetByState(cnS, states=states)
  key <- sprintf("w=\%g", binWidth)
  cnList[[key]] <- cnS
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
labels <- sapply(seq(along=cnList), FUN=function(kk) {
  sprintf("\%s [n=\%d]", names(cnList)[kk], nbrOfLoci(cnList[[kk]]))
})

layout(seq(along=cnList))
par(mar=c(3,4,1,1)+0.1)
ylim <- c(-1,1)*2
for (kk in seq(along=cnList)) {
  cn <- cnList[[kk]]
  plot(cn, ylim=ylim, col=kk)
  x <- xSeq(cn, by=1)
  x <- x[getStates(cn, x=x) == 1]
  points(x,rep(ylim[1],length(x)), pch=15, col="#999999")
  stext(side=3, pos=0, labels[kk])
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ROC
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fits <- lapply(cnList, FUN=fitRoc, recall=states[1])

layout(1)
par(mar=c(4,4,2,1)+0.1)
for (kk in seq(along=fits)) {
  fit <- fits[[kk]]
  roc <- fit$roc
  if (kk == 1) {
    plot(roc, type="l", lwd=4, col=kk)
    abline(a=0, b=1, lty=3)
    legend("bottomright", col=seq(along=fits), lwd=4, labels, bty="n")
    title("Calling the CN gain")
  } else {
    lines(fit$roc, lwd=4, col=kk)
  }
}

fpRates <- seq(from=0, to=1, by=0.03)
fits <- lapply(cnList, FUN=function(cn) {
  res <- NULL
  for (fpRate in fpRates) {
    fit <- findRocTpAtFp(cn, recall=states[1], fpRate=fpRate)
    fptp <- c(fpRate=fit$fpRateEst, tpRate=fit$tpRateEst)
    res <- rbind(res, fptp)
  }
  res
})
for (kk in seq(along=fits)) {
  fit <- fits[[kk]]
  points(fit, pch=21, lwd=2, col="black", bg=kk)
}
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[aroma.core]{SegmentedCopyNumbers}}
}
\keyword{internal}
\keyword{methods}
