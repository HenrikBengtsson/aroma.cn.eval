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
