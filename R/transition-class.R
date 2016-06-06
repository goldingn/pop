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
#' hatching <- tr(larva ~ egg, p(0.5))
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

  # stop any cross-state dispersals
  if (to != from & contains(transfun, 'dispersal')) {
    stop ('dispersals can only occur between patches, within the same state')
  }

  object <- list(to = to,
                 from = from,
                 transfun = transfun)

  object <- as.transition(object)
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
  if (!is.transition(x)) {
    class(x) <- c('transition', class(x))
  }
  return (x)
}

#' @rdname transition
#' @param x an object to print or test as a transition object
#' @param \dots further arguments passed to or from other methods.
#' @export
#' @examples
#' # print method
#' print(pupa)
#'
print.transition <- function (x, ...) {
  if (containsUserTransfun(x$transfun)) {
    text <- sprintf('transition:\t%s -> %s with user-defined transfun\n',
                    x$from,
                    x$to)

  } else {
    landscape <- as.landscape(NULL)
    text <- sprintf('transition:\t%s -> %s with expectation %s\n',
                    x$from,
                    x$to,
                    x$transfun(landscape))
  }
  cat(text)
}

#' @rdname transition
#' @param y a transition object to be multiplied with another with the same
#'   pathway
#' @details multiplication of transition objects with the same pathway results
#'   in a transition object whose \code{transfun} object is a compound of the
#'   two transfuns in the transitions. See \code{\link{transfun}} for more
#'   details of compound transfuns.
#' @export
#' @examples
#' # make a compound transition to include a probability of laying eggs
#' prob_laying <- tr(egg ~ adult, p(0.6))
#' (recruitment <- prob_laying * fecundity)
#'
`*.transition` <- function (x, y) {
  # given two transition objects on the same pathway, combine their transfuns
  # into a compound transfun
  stopifnot(x$from == y$from & x$to == y$to)
  stopifnot(is.transition(x))
  stopifnot(is.transition(y))
  x$transfun <- x$transfun * y$transfun
  return (x)
}

#' @rdname transition
#' @export
#' @examples
#' # extract the transfun parameters
#' (param_pupa <- parameters(pupa))
#' (param_recruitment <- parameters(recruitment))
#'
parameters.transition <- function (x) {
  parameters(x$transfun)
}

#' @rdname transition
#' @export
#' @param value a named list of parameters matching those currently defined for \code{x}
#' @examples
#' # update the parameters of these transfuns
#' param_pupa$p <- 0.6
#' parameters(pupa) <- param_pupa
#' parameters(pupa)
#'
#' param_recruitment$r <- 15
#' parameters(recruitment) <- param_recruitment
#' parameters(recruitment)
`parameters<-.transition` <- function (x, value) {
  parameters(x$transfun) <- value
  return (x)
}
