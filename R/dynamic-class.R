

dynamic <- function (...) {
  # given a bunch of transition functions, build an object representing a
  # dynamical system

  transitions <- list(...)
  states <- getStates(transitions)
  matrix <- getMatrix(states, transitions)
  object <- list(transitions = transitions,
                 matrix = matrix,
                 states = states)

  # set class and return
  class(object) <- 'dynamic'
  return (object)
}

#' @importFrom igraph graph.adjacency
#' @importFrom igraph plot.igraph
plot.dynamic <- function (x, ...) {
  # plot a dynamic using igraph

  # create an igraph graph object
  g <- graph.adjacency(t(x$matrix), weighted = TRUE)

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

}

# ~~~~~~~
# dynamic composition functions

add.dynamic <- function (dynamic1, dynamic2) {
  transitions <- c(dynamic1$transitions, dynamic2$transitions)
  dynamic <- do.call(dynamic, transitions)
  return (dynamic)
}

`+.dynamic` <- function (x, y) add.dynamic(x, y)



is.dynamic <- function (x) inherits(x, 'dynamic')
as.dynamic <- function (x) {
  class(x) <- c(class(x), 'dynamic')
  return (x)
}



print.dynamic <- function (x, ...) {
  text <- sprintf('dynamic:\ttransitions between: %s\n',
                  paste(x$states, collapse = ', '))
  cat(text)
}

