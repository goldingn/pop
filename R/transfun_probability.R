# probability transition function

#' @title probability transfun
#' @name probability
#' @rdname probability
#' @description Create a transfun object representing a probability of
#'  transition between states. Typically used inside a call to
#'  \code{\link{transition}}
#' @param value a numeric between 0 and 1 representing a probability
#' @details \code{p()} is a shorthand for \code{probability()}.
#' @export
#' @examples
#' # these are equivalent
#' prob <- probability(0.2)
#' prob <- p(0.2)
#'
probability <- function (value) {
  # label a value as a probability
  stopifnot(is.numeric(value))
  stopifnot(value > 0 & value < 1)
  param = list(p = value)
  f <- function (landscape) param$p
  f <- as.probability(f)
  return (f)
}

#' @rdname probability
#' @name p
#' @export
p <- probability

#' @rdname probability
#' @param x an object to be tested as a probability transfun object
#' @export
#' @examples
#' is.probability(prob)
is.probability <- function (x) inherits(x, 'probability')

# unexported
as.probability <- function (x) {
  if (!is.transfun(x)) {
    class(x) <- c('transfun', class(x))
  }
  if (!is.probability(x)) {
    class(x) <- c('probability', class(x))
  }
  return (x)
}


