## test anova using same method as implementation
# X parameter is a data frame.

cm <- function(X) {
  size <- dim(X)[1]*dim(X)[2]
  total <- sum(X)^2
  cm1 <- (1/size)*total
  return (cm1)
}

totalSS <- function(X) {
  data <- X^2
  totalSS1 <- sum(data) - cm(X)
  return (totalSS1)
}

sst <- function(X) {
  cols <- c(1:dim(X)[2])
  data <- lapply(cols, 
                 function(i) {
                   C <- X[,i]
                   ni <- length(C)
                   total <- sum( C )^2 / ni
                   return (total)
                 })
  total <- sum(unlist(data))
  sst1 <- total - cm(X)
  return (sst1)
}

sse <- function(X) {
  sse1 <- totalSS(X) - sst(X)
  return (sse1)
}

mst <- function(X) {
  k <- dim(X)[2]
  mst1 <- sst(X) / (k-1)
  return (mst1)
}

mse <- function(X) {
  n <- dim(X)[1]*dim(X)[2]
  k <- dim(X)[2]
  mse1 <- sse(X) / (n-k)
  return (mse1)
}

fstat <- function(X) {
  mst1 <- mst(X)
  mse1 <- mse(X)
  return (mst1 / mse1)
}

testanova <- function(X) {
  totalSS1 <- totalSS(X)
  sst1 <- sst(X)
  sse1 <- sse(X)
  mst1 <- mst(X)
  mse1 <- mse(X)
  f1 <- fstat(X)
  print(paste("Total SS", totalSS1))
  print(paste("SST", sst1))
  print(paste("SSE", sse1))
  print(paste("MST", mst1))
  print(paste("MSE", mse1))
  print(paste("F-stat", f1))
  n <- dim(X)[1]*dim(X)[2]
  k <- dim(X)[2]
  F <- qf(0.05, k-1, n-k, lower.tail=FALSE)
  print(paste("Critical Value: ", F))
  print(paste("P-Value: ", df(f1, k-1, n-k)))
  print(paste("Area: ", pf(F,k-1,n-k, lower.tail=FALSE)) )
  print(paste("F > F_alpha ", f1 > F))
}

X <- t(data.frame(c(65.0, 75.0, 59.0, 94.0),
c(87.0, 69.0, 78.0, 89.0),
c(73.0, 83.0, 67.0, 80.0),
c(79.0, 81.0, 62.0, 88.0),
c(81.0, 72.0, 83.0, 0.0),
c(69.0, 79.0, 76.0, 0.0),
c(0.0, 90.0, 0.0, 0.0)))

testanova(X)

# example alpha value is 0.05, for df1/10
# F_0.05 is about 4.96
qf(0.05, 1, 10)
#
# the probability integral also demonstrates the equivalent for the qf 
# 0.05 = p(CriticalValue, 1, 10)
pf(4.96, 1, 10, lower.tail=FALSE)

