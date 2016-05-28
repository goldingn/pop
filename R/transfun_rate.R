# rate transition function

r <- function (x) {
  # label a value as a rate
  stopifnot(is.numeric(x))
  stopifnot(x > 0)
  f <- function() x
  f <- as.rate(f)
  return (f)
}


is.rate <- function (x) inherits(x, 'rate')
as.rate <- function (x) {
  class(x) <- c('rate', 'transfun', class(x))
  return (x)
}
