# dynamic class functions

#' @title dynamic objects
#' @name dynamic
#' @rdname dynamic
#' @param \dots one or more transition objects making up the dynamic (for the
#'   \code{dynamic} function) or additional arguments (for \code{plot} and
#'   \code{print})
#' @description creates a \code{dynamic} object, comprising multiple
#'   \code{transition} objects to define a dynamical system. \code{dynamic}
#'   objects are the core of \code{pop}, since they can be created and updated
#'   using various methods (MPMs, IPMs etc.), combined (by addition of two
#'   \code{dynamic} objects to make another) and and analysed in various ways
#'   (deterministically to obtain demographic parameters, simulated to evaluate
#'   population viability etc.)
#' @export
#' @examples
#' # define transitions for a simple three-stage system (with implicit
#' # mortality):
#' stasis_egg <- tr(egg ~ egg, p(0.4))
#' stasis_larva <- tr(larva ~ larva, p(0.3))
#' stasis_adult <- tr(adult ~ adult, p(0.8))
#' hatching <- tr(larva ~ egg, p(0.5))
#' fecundity <- tr(egg ~ adult, r(3))
#' pupation <- tr(adult ~ larva, p(0.2))
#'
#' #combine these into separate,component dynamics
#' stasis <- dynamic(stasis_egg,
#'                   stasis_larva,
#'                   stasis_adult)
#' growth <- dynamic(hatching,
#'                   pupation)
#' reproduction <- dynamic(fecundity)
#'
dynamic <- function (...) {
  # given a bunch of transition functions, build an object representing a
  # dynamical system

  transitions <- list(...)
  states <- getStates(transitions)
  object <- list(transitions = transitions,
                 states = states)

  # set class and return
  class(object) <- 'dynamic'
  return (object)
}

#' @rdname dynamic
#' @export
#' @param y a dynamic object to be added to another
#' @examples
#' # combine these into one dynamic
#' all <- stasis + growth + reproduction
`+.dynamic` <- function (x, y) add.dynamic(x, y)

#' @rdname dynamic
#' @export
is.dynamic <- function (x) inherits(x, 'dynamic')

as.dynamic <- function (x) {
  class(x) <- c(class(x), 'dynamic')
  return (x)
}

#' @rdname dynamic
#' @param x a dynamic object to print, plot, convert to a transition matrix, or
#'   an object to test as a dynamic object,
#' @export
#' @import igraph
#' @examples
#' # plot these
#' plot(stasis)
#' plot(growth)
#' plot(all)
#'
plot.dynamic <- function (x, ...) {
  # plot a dynamic using igraph

  # extract the transition matrix & create an igraph graph object
  mat <- t(as.matrix(x))
  g <- graph.adjacency(mat, weighted = TRUE)

  # vertex plotting details
  V(g)$color <- grey(0.9)
  V(g)$label.color <- grey(0.4)
  V(g)$label.family <- 'sans'
  V(g)$size <- 50
  V(g)$frame.color <- NA

  # edge plotting details
  E(g)$color <- grey(0.5)
  E(g)$curved <- curve_multiple(g, 0.1)
  E(g)$arrow.size <- 0.5
  E(g)$label <- get.edge.attribute(g, 'weight')
  E(g)$loop.angle <- 4
  E(g)$label.color <- grey(0.4)

  plot(g)

  # return the igraph object
  return (invisible(g))

}

# ~~~~~~~
# dynamic composition functions

add.dynamic <- function (dynamic1, dynamic2) {
  transitions <- c(dynamic1$transitions, dynamic2$transitions)
  dynamic <- do.call(dynamic, transitions)
  return (dynamic)
}

#' @rdname dynamic
#' @export
#' @examples
#' # print method
#' print(all)
print.dynamic <- function (x, ...) {
  text <- sprintf('dynamic:\ttransitions between: %s\n',
                  paste(x$states, collapse = ', '))
  cat(text)
}

#' @rdname dynamic
#' @export
#' @examples
#' # convert to a transition matrix
#' as.matrix(all)
as.matrix.dynamic <- function (x, ...) {
  # given a vector of states and list of transitions,
  # build a transition matrix
  states <- x$states
  transitions <- x$transitions

  # set up empty matrix
  n_states <- length(states)
  mat <- matrix(0, n_states, n_states)
  rownames(mat) <- colnames(mat) <- states

  # add in the transitions we know about
  for (t in transitions) {
    mat[t$to, t$from] <- mat[t$to, t$from] + expected(t$transfun)
  }

  # set class and return
  class(mat) <- c(class(mat), 'transition_matrix')
  return (mat)

}
