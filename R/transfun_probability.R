# probability transition function

p <- function (x) {
  # label a value as a probability
  stopifnot(is.numeric(x))
  stopifnot(x > 0 & x < 1)
  f <- function() x
  f <- as.probability(f)
  return (f)
}

is.probability <- function (x) inherits(x, 'probability')
as.probability <- function (x) {
  class(x) <- c('probability', 'transfun', class(x))
  return (x)
}
