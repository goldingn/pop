# functions for deterministic analysis of dynamics
r0 <- function (x) {
  stopifnot(is.dynamic(x))
  # get the recursion matrix
  mat <- as.matrix(x, type= 'R')
  r0 <- Re(eigen(mat)$values[1])
  return (r0)
}

lambda <- function (x) {
  stopifnot(is.dynamic(x))
  mat <- as.matrix(x, type= 'A')
  r0 <- Re(eigen(mat)$values[1])
  return (r0)
}
