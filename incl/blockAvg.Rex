X <- matrix(rnorm(5*20), ncol=5)

for (h in c(1,2,3,4)) {
  print(h)
  map <- getBlockAverageMap(nrow(X), h=h)
  print(map)
  Xh <- blockAvg(X, idxs=map)
  print(Xh)
  Xh2 <- colAvgsPerRowSet(X, S=map)
  stopifnot(identical(Xh2, Xh))
}


h <- 4.5
print(h)
map <- getBlockAverageMap(nrow(X), h=h)
print(map)
Xh <- blockAvg(X, idxs=map)
print(Xh)
Xh2 <- colAvgsPerRowSet(X, S=map)
stopifnot(identical(Xh2, Xh))
