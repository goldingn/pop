# things related to the transfun class

transfunClasses <- function () {
  # list all available classes of transfun
  c('probability', 'rate', 'compound')
}

transfunType <- function (x) {
  # get the type of the transfun object
  stopifnot(is.transfun(x))
  classes <- class(x)
  matches <- na.omit(match(transfunClasses(), classes))
  if (length(matches) == 0) {
    stop ('this transfun object does not correspond to any known transfun types')
  } else if (length(matches) > 1) {
    stop ('this transfun object correspond to multiple transfun types')
  } else {
    type <- classes[matches]
  }
  return (type)
}

#' @title transfun objects
#' @name transfun
#' @rdname transfun
#' @param x a transfun object to print or an object to test as a transfun object
#' @description utility functions for the \code{transfun} class. \code{transfun}
#'   objects are created by functions such as \code{\link{probability}}.
#' @export
#' @examples
#' prob <- p(0.3)
#' is.transfun(prob)
#'
is.transfun <- function (x) inherits(x, 'transfun')

#' @rdname transfun
#' @param \dots further arguments passed to or from other methods.
#' @export
#' @examples
#' prob
print.transfun <- function(x, ...) {
  text <- sprintf('%s transfun with expectation %s\n',
                  transfunType(x),
                  expected(x))
  cat(text)
}

as.compound <- function (x) {
  # define a compoud transfun class
  class(x) <- c('compound', 'transfun', class(x))
  return (x)
}

#' @rdname transfun
#' @param y a transfun object to be multiplied with another with the same
#'   pathway
#' @details multiplication of transfun objects with the same pathway results in
#'   a compound transfun object (also of class \code{transfun}). When used in a
#'   stochastic model, the two stochastic transitions are evaluated one after
#'   another. When analysed deterministically, the expectation of the compound
#'   transition function is taken as the product of the expectations of the two
#'   basis transfuns.
#' @export
#' @examples
#' (compound <- prob * r(4.3))
#'
`*.transfun` <- function (x, y) {
  # given two transfun objects, combine them into a compound transfun
  stopifnot(is.transfun(x))
  stopifnot(is.transfun(y))
  z <- function () list(x, y)
  z <- as.compound(z)
  return (z)
}

# add expectation function to grab expectations from transfuns for as.matrix
expected <- function (transfun) {
  # get transfun type, if it's a compound, call expectation recursively
  type <- transfunType(transfun)
  if (type == 'compound') {
    # expand and get sub-expectations
    components <- transfun()
    expect <- expected(components[[1]]) * expected(components[[2]])
  } else {
    expect <- transfun()
  }
  return (expect)
}

