# things related to the transfun class

transfunClasses <- function () {
  # list all available classes of transfun
  c('probability', 'rate')
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
  text <- sprintf('%s %s\n',
                  transfunType(x),
                  x())
  cat(text)
}
