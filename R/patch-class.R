# dots approach doesn't seem to work well. Instead pass a patch object *patch*
# to the transfun (except dispfuns)


#' @title patch objects
#' @name patch
#' @rdname patch
#' @description \code{patch} objects represent patches in a metapopulation,
#'   storing information (such as area, population and environmental features)
#'   that may impact on the dynamic transitions occurring in that patch
#' @param patch_data an object to turn into a \code{patch} object. Currently
#'   this can either be a list or NULL (see \code{details}), though more
#'   approaches will be added in the future
#' @return an object of class \code{patch}, with elements \code{area},
#'   \code{population}, \code{features} providing information about a habitat
#'   patch
#' @export
#' @details \code{patch_data} can be a list containing the following elements:
#'   \code{population}, a named numeric vector giving the number of individuals
#'   of each stage *within the patch*; \code{area}; a single numeric value
#'   giving the area of the patch in square kilometres; and \code{features}, a
#'   named numeric vector containing miscellaneous features of the habitat
#'   patch, such and measures of patch quality or environmental variables.
#'   Alternatively, \code{patch_data = NULL}, will set up a 'default' patch with
#'   \code{area = 1} and blank \code{population} and \code{features} elements.
#'   This is what's used in analysing a \code{dynamic} object without
#'   metapopulation structure.
#' @examples
#' # create a default patch
#' patch <- patch(NULL)
#'
#' # create a more interesting patch
#' patch <- patch(list(area = 10,
#'                     population = c(adult = 10, larva = 3, egg = 20),
#'                     features = c(temperature = 10)))
patch <- function (patch_data) {
  switch(class(patch_data),
         NULL = patchDefault(),
         list = list2patch(patch_data))
}

#' @rdname patch
#' @export
is.patch <- function (x) inherits(x, 'patch')

#' @rdname patch
#' @param x an object to print or test as a patch object
#' @param \dots further arguments passed to or from other methods.
#' @export
#' @examples
#' # print method
#' print(patch)
#'
print.patch <- function(x, ...) {
  text <- sprintf('patch with area of %s square km\n',
                  round(x$area, 2))
  cat(text)
}

# accessor functions for patches
#' @rdname patch
#' @export
#' @param patch an object of class \code{patch}
#' @details the accessor functions \code{area}, \code{population} and
#'   \code{features} either return or set the elements of the same name in a
#'   \code{patch} object
#' @examples
#' # get and set the area
#' area(patch)
#' area(patch) <- 2
#' area(patch)
#'
area <- function (patch) {
  stopifnot(is.patch(patch))
  return(patch$area)
}

#' @rdname patch
#' @export
`area<-` <- function (patch, value) {
  stopifnot(is.patch(patch))
  areaCheck(value)
  patch$area <- value
  return(patch)
}

#' @rdname patch
#' @export
#' @examples
#'# get and set the population
#' population(patch)
#' population(patch) <- population(patch) * 2
#' population(patch)
#'
population <- function (patch) {
  stopifnot(is.patch(patch))
  return(patch$population)
}

#' @rdname patch
#' @export
`population<-` <- function (patch, value) {
  stopifnot(is.patch(patch))
  populationCheck(value)
  patch$population <- value
  return(patch)
}

#' @rdname patch
#' @export
#' @examples
#'# get and set the features
#' features(patch)
#' features(patch) <- c(features(patch), rainfall = 100)
#' features(patch)
#'
features <- function (patch) {
  stopifnot(is.patch(patch))
  return (patch$features)
}

#' @rdname patch
#' @export
`features<-` <- function (patch, value) {
  stopifnot(is.patch(patch))
  featuresCheck(value)
  patch$features <- value
  return(patch)
}

areaCheck <- function (area) {
  stopifnot(length(area) == 1)
  stopifnot(is.numeric(area))
  stopifnot(is.finite(area))
  stopifnot(area > 0)
}

populationCheck <- function (population) {
  stopifnot(is.numeric(population))
  stopifnot(!is.null(names(population)))
  stopifnot(all(is.finite(population)))
  stopifnot(all(population > 0))
}

featuresCheck <- function (features) {
  stopifnot(is.numeric(features))
  stopifnot(!is.null(names(features)))
  stopifnot(all(is.finite(features)))
}

list2patch <- function (list) {

  # check the elements
  stopifnot(length(list) == 3)
  stopifnot(sort(names(list)) == c('area', 'features', 'population'))

  # check components
  areaCheck(list$area)
  populationCheck(list$population)
  featuresCheck(list$features)

  # reset order
  patch <- list(area = list$area,
                population = list$population,
                features = list$features)

  # set class & return
  patch <- as.patch(patch)
  return (patch)

}

as.patch <- function (x) {
  if (!is.patch(x)) {
    class(x) <- c('patch', class(x))
  }
  return (x)
}

patchDefault <- function () {
  patch <- list(area = 1,
                population = c(NULL = 0),
                variables = c(NULL = 0))
  patch <- as.patch(patch)
  return (patch)
}
