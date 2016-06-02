#' @title landscape objects
#' @rdname landscape
#' @name landscape
#' @description \code{landscape} objects represent sets of patches forming a
#'   metapopulation, storing information (such as area, population and
#'   environmental features) that may impact on the dynamic transitions
#'   occurring in each component patch. \code{dynamic} objects all have a
#'   \code{landscape} object (by default a single-patch landscape) as a an
#'   attribute which can be accessed and set via the function \code{landscape}.
#'   \code{as.landscape} is used to create landscape objects, and the functions
#'   \code{population}, \code{area} and \code{features} access and set each of
#'   the trhee key elements of a landscape.
#' @param dynamic an object of class \code{dynamic}
#' @param value an object of class \code{landscape} (for
#'   \code{landscape(dynamic) <- value}) or the value to assign to the
#'   \code{area}, \code{population}, or \code{features} elements of a
#'   \code{landscape} object

#' @export
#' @details The accessor function \code{landscape} either returns or sets the
#'   landscape structure of the dynamic, encoded as a \code{\link{landscape}}
#'   object
landscape <- function (dynamic) {
  stopifnot(is.dynamic(dynamic))
  value <- attr(dynamic, 'landscape')
  return (value)
}

#' @rdname landscape
#' @export
`landscape<-` <- function (dynamic, value) {
  stopifnot(is.dynamic(dynamic))
  stopifnot(is.landscape(value))
  attr(dynamic, 'landscape') <- value
  return (dynamic)
}

#' @rdname landscape
#' @name as.landscape
#' @param patches an object to turn into a \code{landscape} object. Currently
#'   this can either be a dynamic, a list or \code{NULL} (see \code{details}),
#'   though more approaches will be added in the future
#' @return an object of class \code{landscape}, with elements \code{area},
#'   \code{population}, \code{features} providing information about all
#'   component habitat patches
#' @export
#' @details \code{patches} can be a list containing the following elements:
#'   \code{population}, a dataframe giving the number of individuals of each
#'   stage (columns) within each patch (rows); \code{area}, a numeric vector
#'   giving the areas of the patches in square kilometres; and \code{features},
#'   a dataframe containing miscellaneous features (columns) of the patches
#'   (rows), such as measures of patch quality or environmental variables.
#'   Alternatively, \code{patches = NULL}, will set up a 'default' one-patch
#'   landscape with \code{area = 1} and blank \code{population} and
#'   \code{features} elements. The other option is to pass a \code{dynamic}
#'   object as \code{patches}, in which case the set up will be the same as for
#'   \code{patches = NULL} except that \code{population} will be a one-row
#'   dataframe of 0s, with columns corresponding to the states in the dynamic.
#'   This is what's used when analysing a \code{dynamic} object without
#'   user-specified metapopulation structure.
#' @examples
#' # create a default landscape
#' landscape <- as.landscape(NULL)
#'
#' # create a marginally more interesting one-patch landscape
#' landscape <- as.landscape(list(area = 10,
#'                     population = data.frame(adult = 10, larva = 3, egg = 20),
#'                     features = data.frame(temperature = 10)))
as.landscape <- function (patches) {
  switch(class(patches)[1],
         NULL = landscapeDefault(),
         dynamic = dynamicLandscapeDefault(patches),
         list = list2landscape(patches))
}

#' @rdname landscape
#' @export
is.landscape <- function (x) inherits(x, 'landscape')

#' @rdname landscape
#' @param x an object to print or test as a landscape object
#' @param \dots further arguments passed to or from other methods.
#' @export
#' @examples
#' # print method
#' print(landscape)
#'
print.landscape <- function(x, ...) {
  text <- sprintf('landscape with %s patches\n',
                  length(x$area))
  cat(text)
}

#' @rdname landscape
#' @export
#' @param landscape an object of class \code{landscape}
#' @details the accessor functions \code{area}, \code{population} and
#'   \code{features} either return or set the elements of the same name in a
#'   \code{landscape} object
#' @examples
#' # get and set the area
#' area(landscape)
#' area(landscape) <- 2
#' area(landscape)
#'
area <- function (landscape) {
  stopifnot(is.landscape(landscape))
  return(landscape$area)
}

#' @rdname landscape
#' @export
`area<-` <- function (landscape, value) {
  stopifnot(is.landscape(landscape))
  areaCheck(value, landscape)
  landscape$area <- value
  return(landscape)
}

#'@rdname landscape
#'@param states an optional character vector naming the states for which the
#'  populations are required
#'@export
#' @examples
#'# get and set the population
#' population(landscape)
#' population(landscape) <- population(landscape) * 2
#' population(landscape)
#'
population <- function (landscape, states = NULL) {
  stopifnot(is.landscape(landscape))

  # extract the population
  pop <- landscape$population

  # if which speecified, get those elements
  if (!is.null(states)) {
    stopifnot(all(states %in% names(landscape$population)))
    pop <- pop[, states]
  }

  # return the requested populations
  return (pop)
}

#' @rdname landscape
#' @export
`population<-` <- function (landscape, value) {
  stopifnot(is.landscape(landscape))
  populationCheck(value, landscape)
  landscape$population <- value
  return (landscape)
}

#' @rdname landscape
#' @export
#' @examples
#'# get and set the features
#' features(landscape)
#' features(landscape) <- cbind(features(landscape), rainfall = 100)
#' features(landscape)
#'
features <- function (landscape) {
  stopifnot(is.landscape(landscape))
  return (landscape$features)
}

#' @rdname landscape
#' @export
`features<-` <- function (landscape, value) {
  stopifnot(is.landscape(landscape))
  featuresCheck(value, landscape)
  landscape$features <- value
  return (landscape)
}

areaCheck <- function (area, landscape = NULL) {
  stopifnot(is.numeric(area))
  stopifnot(all(is.finite(area)))
  stopifnot(all(area > 0))
  if (!is.null(landscape)) {
    stopifnot(length(area) == length(area(landscape)))
  }
}

populationCheck <- function (population, landscape = NULL) {
  stopifnot(is.data.frame(population))
  stopifnot(all(!is.null(names(population))))
  stopifnot(all(sapply(population, is.finite)))
  stopifnot(all(sapply(population, function(x) all(x >= 0))))
  if (!is.null(landscape)) {
    stopifnot(all(dim(population) == dim(population(landscape))))
  }
}

featuresCheck <- function (features, landscape = NULL) {
  stopifnot(is.data.frame(features))
  stopifnot(all(!is.null(names(features))))
  if (!is.null(landscape)) {
    stopifnot(nrow(features) == nrow(features(landscape)))
  }
}

list2landscape <- function (list) {

  # check the elements
  stopifnot(length(list) == 3)
  stopifnot(sort(names(list)) == c('area', 'features', 'population'))

  # check components
  areaCheck(list$area)
  populationCheck(list$population)
  featuresCheck(list$features)

  # reset order
  landscape <- list(area = list$area,
                population = list$population,
                features = list$features)

  # set class & return
  class(landscape) <- c('landscape', class(landscape))
  return (landscape)

}

# default standalone landscape
landscapeDefault <- function () {
  landscape <- list(area = 1,
                population = data.frame()[1, ],
                features = data.frame()[1, ])
  class(landscape) <- c('landscape', class(landscape))
  return (landscape)
}

# default landscape for a dynamic
dynamicLandscapeDefault <- function (dynamic) {
  population <- as.list(rep(0, length(dynamic$states)))
  names(population) <- dynamic$states
  population <- as.data.frame(population)
  landscape <- list(area = 1,
                population = population,
                features = data.frame()[1, ])
  class(landscape) <- c('landscape', class(landscape))
  return (landscape)
}
