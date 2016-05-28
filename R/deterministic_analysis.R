# functions for deterministic analysis of dynamics

r0 <- function (x) {
  stopifnot(is.dynamic(x))
  r0 <- Re(eigen(x$matrix)$values[1])
  return (r0)
}
