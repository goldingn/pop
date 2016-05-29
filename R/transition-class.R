#' @title transition objects
#' @name transition
#' @rdname transition
#' @param formula a two-sided formula identifying the states between which the
#'   transition occurs
#' @param transfun a \code{\link{transfun}} object quantifying the transition.
#' @description creates a \code{transition} object, encoding a transition
#'   between two states. E.g. the probability of a seed germinating, or of an
#'   individual surviving in each time step
#' @details \code{tr} is just a shorthand for \code{transition}
#' @export
#' @examples
#' # 50/50 chance of a larva emerging from an egg
#' hatching <- tr(lava ~ egg, p(0.5))
#'
#' # three eggs laid per adult per time step
#' fecundity <- tr(egg ~ adult, r(3))
#'
#' # 0.1 probability of a larva pupating into an adult
#' pupa <- tr(adult ~ larva, p(0.1))
#'
transition <- function (formula, transfun) {
  # given a formula describing a particular transition,
  # parse into an object & store the value

  stopifnot(inherits(formula, 'formula'))
  stopifnot(is.transfun(transfun))

  to <- as.character(formula[[2]])
  from <- as.character(formula[[3]])

  object <- list(to = to,
                 from = from,
                 transfun = transfun)

  class(object) <- 'transition'
  return (object)

}

#' @rdname transition
#' @export
#' @name tr
tr <- transition

#' @rdname transition
#' @export
is.transition <- function (x) inherits(x, 'transition')

as.transition <- function (x) {
  class(x) <- c(class(x), 'transition')
  return (x)
}

#' @rdname transition
#' @param x an object to print or test as a transition object
#' @param \dots further arguments passed to or from other methods.
#' @export
#' @examples
#' # print method
#' print(pupa)
print.transition <- function (x, ...) {
  text <- sprintf('transition:\t%s -> %s with %s\n',
                  x$from,
                  x$to,
                  capture.output(print(x$transfun)))
  cat(text)
}
