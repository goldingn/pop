# rate transition function

#' @title rate transfun
#' @name rate
#' @rdname rate
#' @description Create a transfun object representing a rate of transition
#'   between states - e.g. an expected number of offspring generated into one
#'   state from another. Typically used inside a call to
#'   \code{\link{transition}}
#' @param value a numeric greater than 0 representing a rate
#' @details \code{r()} is a shorthand for \code{rate()}.
#' @export
#' @examples
#' # these are equivalent
#' rate <- rate(0.2)
#' rate <- r(0.2)
#'
rate <- function (value) {
  # label a value as a rate
  stopifnot(is.numeric(value))
  stopifnot(value > 0)
  stopifnot(is.finite(value))
  param = list(r = value)
  f <- function (landscape) param$r
  f <- as.rate(f)
  return (f)
}

#' @rdname rate
#' @name r
#' @export
r <- rate

#' @rdname rate
#' @param x an object to be tested as a rate transfun object
#' @export
#' @examples
#' is.rate(rate)
is.rate <- function (x) inherits(x, 'rate')

as.rate <- function (x) {
  if (!is.transfun(x)) {
    class(x) <- c('transfun', class(x))
  }
  if (!is.rate(x)) {
    class(x) <- c('rate', class(x))
  }
  return (x)
}
