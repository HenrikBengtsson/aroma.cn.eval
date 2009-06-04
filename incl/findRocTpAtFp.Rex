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
  text(x=fp, y=xlim[2], sprintf("FP rate: %.3f", fp), srt=90, adj=adj)
  text(x=0, y=tp, sprintf("TP rate: %.3f", tp), adj=adj)
}
