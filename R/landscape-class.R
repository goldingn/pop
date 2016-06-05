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
#'   \code{population}, \code{area}, \code{distance} and \code{features}
#'   access and set each of the elements of a landscape.
#' @param dynamic an object of class \code{dynamic}
#' @param value an object of class \code{landscape} (for
#'   \code{landscape(dynamic) <- value}) or the value to assign to the
#'   \code{distance}, \code{area}, \code{population}, or \code{features}
#'   elements of a \code{landscape} object

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
#' @return an object of class \code{landscape}, essentially a dataframe
#'   containing the coordinates, area, population and features (as columns) for
#'   each patch (rows)
#' @export
#' @details \code{patches} can be a list containing the following elements:
#'   \code{population}, a dataframe giving the number of individuals of each
#'   stage (columns) within each patch (rows); \code{area}, a one-column
#'   dataframe giving the areas of the patches in square kilometres;
#'   \code{coordinates}, a dataframe giving the coordinates of the habitat
#'   patches; and \code{features}, a dataframe containing miscellaneous features
#'   (columns) of the patches (rows), such as measures of patch quality or
#'   environmental variables. Alternatively, \code{patches = NULL}, will set up
#'   a 'default' one-patch landscape with \code{area = data.frame(area =1)},
#'   \code{coordinates = data.frame(x = 0, y = 0)} and blank \code{population}
#'   and \code{features} elements. The other option is to pass a \code{dynamic}
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
#' landscape <- as.landscape(list(coordinates = data.frame(x = c(10, 11),
#'                                                         y = c(11, 12)),
#'                                area = data.frame(area = 10),
#'                                population = data.frame(adult = 10,
#'                                                        larva = 3,
#'                                                        egg = 20),
#'                                features = data.frame(temperature = 10)))
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
                  nrow(x))
  cat(text)
}

#' @rdname landscape
#' @export
#' @param landscape an object of class \code{landscape}
#' @details the accessor functions \code{distance}, \code{area},
#'   \code{population} and \code{features} either return or set corresponding
#'   sub-dataframes of the \code{landscape} object
#' @examples
#' # get and set the area
#' area(landscape)
#' area(landscape) <- area(landscape) * 2
#' area(landscape)
#'
area <- function (landscape) {
  stopifnot(is.landscape(landscape))
  ans <- landscape[, attr(landscape, 'area'), drop = FALSE]
  ans <- squashLandscape(ans)
  return (ans)
}

#' @rdname landscape
#' @export
`area<-` <- function (landscape, value) {
  areaCheck(value)
  stopifnot(is.landscape(landscape))
  landscape[, attr(landscape, 'area')] <- value
  landscape
}

#' @rdname landscape
#' @export
#' @examples
#'# get and set the population
#' population(landscape)
#' population(landscape) <- population(landscape) * 2
#' population(landscape)
#'
population <- function (landscape) {
  stopifnot(is.landscape(landscape))
  ans <- landscape[, attr(landscape, 'population'), drop = FALSE]
  ans <- squashLandscape(ans)
  return (ans)
}

#' @rdname landscape
#' @export
`population<-` <- function (landscape, value) {
  stopifnot(is.landscape(landscape))
  populationCheck(value)
  stopifnot(all.equal(names(population(landscape)), names(value)))
  landscape[, attr(landscape, 'population')] <- value
  landscape
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
  ans <- landscape[, attr(landscape, 'features'), drop = FALSE]
  ans <- squashLandscape(ans)
  return (ans)
}

#' @rdname landscape
#' @export
`features<-` <- function (landscape, value) {
  stopifnot(is.landscape(landscape))
  stopifnot(is.data.frame(value))

  # for features, just overwrite whatever's there - including column numbers
  feature_cols <- attr(landscape, 'features')

  if (is.null(feature_cols) | length(feature_cols) == 0) {
    # if null (currently no features), add them
    attr(landscape, 'features') <- ncol(landscape) + 1:ncol(value)
  } else {
    # if not null (currently some features), overwrite them
    attrib <- attributes(landscape)
    attrib$names <- attrib$names[-feature_cols]
    # attrib$names
    landscape <- landscape[, -feature_cols]
    attributes(landscape) <- attrib
    attr(landscape, 'features') <- ncol(landscape) + seq_len(ncol(value))
  }
  landscape[, attr(landscape, 'features')] <- value
  landscape
}

#' @rdname landscape
#' @export
#' @examples
#'# get and set the distance matrix
#' distance(landscape)
#' distance(landscape) <- sqrt(distance(landscape))
#' distance(landscape)
#'
distance <- function (landscape) {
  stopifnot(is.landscape(landscape))
  ans <- attr(landscape, 'distance')
  return (ans)
}

#' @rdname landscape
#' @export
`distance<-` <- function (landscape, value) {
  stopifnot(is.landscape(landscape))
  distanceCheck(value, landscape)
  attr(landscape, 'distance') <- value
  return (landscape)
}

#' @rdname landscape
#' @param i index specifying the patches to include in the subset
#'   \code{landscape} object
#' @export
#' @examples
#' # landscapes can be subsetted to get sub-landscapes of patches with double
#' # braces
#' landscape
#' landscape[[1]]
#' landscape[[1:2]]
#'
`[[.landscape` <- function (x, i) {
  attrib <- attributes(x)
  attrib$row.names <- attrib$row.names[i]
  d <- attrib$distance[i, i, drop = FALSE]
  rownames(d) <- colnames(d) <- seq_along(i)
  attrib$distance <- d
  x <- squashLandscape(x)
  x <- x[i, ]
  attributes(x) <- attrib
  return (x)
}

coordinates <- function (landscape) {
  stopifnot(is.landscape(landscape))
  ans <- landscape[, attr(landscape, 'coordinates'), drop = FALSE]
  ans <- squashLandscape(ans)
  return (ans)
}

areaCheck <- function (area) {
  stopifnot(ncol(area) == 1)
  stopifnot(is.numeric(area[, 1]))
  stopifnot(all(is.finite(area[, 1])))
  stopifnot(all(area[, 1] > 0))
}

populationCheck <- function (population) {
  stopifnot(all(sapply(population, is.finite)))
  stopifnot(all(sapply(population, function(x) all(x >= 0))))
}

distanceCheck <- function (distance, landscape) {
  stopifnot(is.matrix(distance))
  stopifnot(nrow(distance) == ncol(distance))
  stopifnot(nrow(distance) == nrow(landscape))
  stopifnot(all(is.finite(distance)))
  stopifnot(all(distance >= 0))
  stopifnot(all(diag(distance) == 0))
}

list2landscape <- function (list) {

  # check the elements
  stopifnot(length(list) == 4)
  stopifnot(sort(names(list)) == c('area', 'coordinates', 'features', 'population'))
  stopifnot(all(sapply(list, is.data.frame)))

  # check components
  areaCheck(list$area)
  populationCheck(list$population)

  # reset order and tidy up row names
  suppressWarnings(landscape <- data.frame(list$coordinates,
                                           area = list$area,
                                           list$population,
                                           list$features))
  rownames(landscape) <- 1:nrow(landscape)

  # work out column numbers
  ncoord <- ncol(list$coordinates)
  narea <- 1
  npop <- ncol(list$population)
  nfeat <- ncol(list$features)

  attr(landscape, 'coordinates') <- seq_len(ncoord)
  attr(landscape, 'area') <- narea + ncoord
  attr(landscape, 'population') <- seq_len(npop) + narea + ncoord
  attr(landscape, 'features') <- seq_len(nfeat) + npop + narea + ncoord

  # set class
  class(landscape) <- c('landscape', class(landscape))

  # add distance matrix
  coord <- coordinates(landscape)
  distance <- as.matrix(dist(coord))
  distance(landscape) <- distance

  # set class & return
  return (landscape)

}

# default standalone landscape
landscapeDefault <- function () {
  landscape_list <- list(coordinates = data.frame(x = 0, y = 0),
                         area = data.frame(area = 1),
                         population = data.frame()[1, ],
                         features = data.frame()[1, ])
  landscape <- list2landscape(landscape_list)
  return (landscape)
}

# default landscape for a dynamic
dynamicLandscapeDefault <- function (dynamic) {
  population <- as.list(rep(0, length(states(dynamic))))
  names(population) <- states(dynamic)
  population <- as.data.frame(population)
  landscape_list <- list(coordinates = data.frame(x = 0, y = 0),
                         area = data.frame(area = 1),
                         population = population,
                         features = data.frame()[1, ])
  landscape <- list2landscape(landscape_list)
  return (landscape)
}

squashLandscape <- function (x) {
  # if an object is a landscape, remove the landscape class (to make it a
  # dataframe again)
  if (is.landscape(x)) {
    classes <- class(x)
    classes <- classes[-which(classes == 'landscape')]
    class(x) <- classes
  }
  return (x)
}
