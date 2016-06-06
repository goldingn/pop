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
#'   \code{transfun} object returned, when applied to a \code{landscape} object,
#'   produces a square symmetric matrix, with zero diagonal and off-diagonals
#'   giving the relative between patch dispersal probability. This implies that
#'   \emph{all} individuals in the state will \emph{try} to disperse. The
#'   fraction remaining in the patch depends on \code{value}. To have only some
#'   fraction try to disperse, a dispersal transfun can be multiplied by a
#'   probability transfun indicating the probability of attempting dispersal.
#'
#'   The relative dispersal probability is given by \code{exp(-d * value)},
#'   where \code{d} is the Euclidean distance between the origin and
#'   destination patch.
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
    if (nrow(ans) > 1) {
      ans <- sweep(ans, 1, rowSums(ans), '/')
    }
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


