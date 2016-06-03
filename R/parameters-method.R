# define S3 generic to get/set parameters from objects

#' @title get and set parameters
#' @rdname parameters
#' @description this documents the S3 generic functions \code{parameters} to
#'   extract or assign parameter values from objects in the \code{pop} package.
#'   Methods of this function are defined for various object classes, including
#'   \code{transfun}, \code{transition} and \code{dynamic} objects.
#' @param x an object from which to extract parameters, or in which to set them
#' @param value an object to assign as the parameters of \code{x}
#' @details each class-specific method will return parameters in a slightly
#'   different structure, and will require \code{value} to be provided in a
#'   different format (though the structures returned and required will
#'   generally be the same for all classes. See the helpfile for each class for
#'   the specific details and examples.
#' @export
parameters <- function (x) {
  UseMethod('parameters', x)
}

#' @rdname parameters
#' @export
`parameters<-` <- function (x, value) {
  UseMethod('parameters<-', x)
}
#
#
#
# parameter.transfun <- function (x) {
#   parameters(x)
# }
#
# parameter.transition <- function (x) {
#   parameter(x$transfun)
# }
#
# parameter.dynamic <- function (x) {
#   lapply(x, parameter)
# }
#
#
#
#
# `parameter<-.transfun` <- function (x, value) {
#   parameters(x) <- value
#   x
# }
#
# `parameter<-.transition` <- function (x, value) {
#   parameters(x) <- value
#   x
# }
#
# `parameter<-.dynamic` <- function (x, value) {
#   parameters(x) <- value
#   x
# }
#
# parameter(stasis_egg$transfun)
# parameter(stasis_egg)
# str(parameter(all), 2)
#
# tr <- stasis_egg$transfun
#
# parameters(tr)
# parameters(tr) <- list(p = 0.1)
# class(tr)
#
#
# parameter(tr) <- list(p = 0.2)
# parameters(tr)
#
#
# parameter(stasis_egg$transfun)
# a <- b <- 'blah'
# class(a) <- 'foo'
# parameter()
