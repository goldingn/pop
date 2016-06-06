# dispersal transition function

#' @title dispersal transfun
#' @name dispersal
#' @rdname dispersal
#' @description Create a transfun object representing a relative probability of
#'   dispersal between patches. Typically used inside a call to
#'   \code{\link{transition}}
#' @param value the (positive) exponential rate of decay of dispersal
#'   probabilities. Large values imply shorter range dispersal.
#' @details \code{d()} is a shorthand for \code{dispersal()}. The
#'   \code{transfun} object returned, when applplied to a \code{landscape}
#'   object, produces a square symmetric matrix, with zero diagonal and
#'   off-diagonals giving the relative between patch dispersal probability.
#' @export
#' @examples
#' # these are equivalent
#' disp <- dispersal(3)
#' disp <- d(3)
#'
dispersal <- function (value) {
  # label a value as a probability
  stopifnot(is.numeric(value))
  stopifnot(value >= 0)
  param = list(l = value)
  f <- function (landscape) {
    ans <- exp(param$l * -distance(landscape))
    return (ans)
  }
  f <- as.dispersal(f)
  return (f)
}

#' @rdname dispersal
#' @name d
#' @export
d <- dispersal

#' @rdname dispersal
#' @param x an object to be tested as a dispersal transfun object
#' @export
#' @examples
#' is.dispersal(disp)
is.dispersal <- function (x) inherits(x, 'dispersal')

# unexported
as.dispersal <- function (x) {
  if (!is.transfun(x)) {
    class(x) <- c('transfun', class(x))
  }
  if (!is.dispersal(x)) {
    class(x) <- c('dispersal', class(x))
  }
  return (x)
}


