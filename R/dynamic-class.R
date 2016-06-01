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
  object <- as.dynamic(object)
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
  if (!is.dynamic(x)) {
    class(x) <- c('dynamic', class(x))
  }
  return (x)
}

#' @rdname dynamic
#' @param x a dynamic object to print, plot, convert to a transition matrix, or
#'   an object to test as a dynamic object (for \code{is.dynamic}),
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
  textmat <- t(textMatrix(x))
  linkmat <- textmat != ''
  g <- graph.adjacency(linkmat, weighted = TRUE)

  # extract edge labels
  labels <- textmat[get.edges(g, seq_len(sum(linkmat)))]

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
  E(g)$label <- labels
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
#' @param which which type of matrix to build: the overall population growth
#'   matrix (\code{'A'}), the probabilistic progression matrix (\code{'P'}), the
#'   fecundity matrix (\code{'F'}) or the intrinsic reproduction matrix
#'   (\code{'R'})
#' @param patch an optional \code{patch} object providing details on the patch
#'   in which the dynamic is being evaluated. If the default of \code{patch =
#'   NULL} is specified, a 'default' patch is used.
#' @export
#' @details If \code{patch = NULL}, a default patch is created using
#'   \code{patch(NULL)}, and used to construct the matrix (see
#'   \code{\link{patch}} for details of this default). This default may well not
#'   contain sufficient information to evaluate the matrix (since the required
#'   features, or populations of life stages in the dynamic may not be present).
#'   If the dynamic contains a user-defined transition function, and
#'   \code{patch} is left at it's default, a message is issued reminding the
#'   user of this (they may also get an error shortly after)
#' @importFrom MASS ginv
#' @examples
#' # convert to a transition matrix
#' as.matrix(all)
as.matrix.dynamic <- function (x, which = c('A', 'P', 'F', 'R'), patch = NULL, ...) {

  user_defined <- sapply(x$transitions,
                         function (z) containsUserTransfun(z$transfun))

  # if a default patch is required, get one
  if (is.null(patch)) {
    patch <- patch(NULL)

    # if there's also a user-defined transition, let them know it might break
    if(any(user_defined)) {
      message ('This dynamic contains transitions with user-defined transfun objects.
               To construct the matrix, you may need to provide an appropriate patch object.
               See ?as.transfun for details')
    }

  }



  # build the overall, reproduction (R), progression (P) of fecundity (F) matrix
  which <- match.arg(which)

  mat <- switch(which,
                `A` = getA(x, patch),
                `P` = getP(x, patch),
                `F` = getF(x, patch),
                `R` = getR(x, patch))

  # set class and return
  class(mat) <- c(class(mat), 'transition_matrix')
  return (mat)

}

getA <- function (x, patch) {

  # get the full population projection matrix from a dynamic
  # set up empty matrix
  mat <- diag(length(x$states))
  rownames(mat) <- colnames(mat) <- x$states

  # apply the transitions
  for (t in x$transitions) {

    # get the expectation
    expectation <- expected(t$transfun, patch)

    # if it's a rate (or compound containing a rate)
    if (containsRate(t$transfun)) {

      if (t$to == t$from) {

        # if it's the diagonal (clonal reproduction), multiply by the expectation and add
        mat[t$to, t$from] <- mat[t$to, t$from] * (1 + expectation)

      } else {

        # if it's the off-diagonal, get the diagonal probability
        diag_prob <- mat[t$from, t$from]

        # multiply by probability (proportion surviving)
        mat[t$to, t$from] <- mat[t$to, t$from] + diag_prob * expectation

      }

    } else {
      # if it's not a rate (nor compound containing a rate)

      if (t$to == t$from) {
        # if it's the diagonal, multiply by the expectation
        mat[t$to, t$from] <- mat[t$to, t$from] * expectation
      } else {
        # if it's the off-diagonal, get the diagonal probability
        diag_prob <- mat[t$from, t$from]

        # multiply by probability and not probability
        mat[t$to, t$from] <- mat[t$to, t$from] + diag_prob * expectation

        # reduce the diagonal by the reciprocal
        mat[t$from, t$from] <- diag_prob * (1 - expectation)
      }

    }

  }

  return (mat)

}

getR <- function (x, patch) {
  # get the reproduction matrix from a dynamic;
  # combine P & F accounting for clonal
  # reproduction (rates on diagonal)
  P <- getP(x, patch)
  eye <- diag(nrow(P))
  mat <- getF(x, patch) %*% ginv(eye - P)
  return (mat)
}

getF <- function (x, patch) {

  # set up empty matrix
  mat <- diag(length(x$states)) * 0
  rownames(mat) <- colnames(mat) <- x$states

  # apply the transitions
  for (t in x$transitions) {

    # if it's a rate (or compound containing a rate)
    if (containsRate(t$transfun)) {

      # get the expectation and add it in
      expectation <- expected(t$transfun, patch)
      mat[t$to, t$from] <-  expectation

    }

  }

  return (mat)

}

getP <- function (x, patch) {

  # set up empty matrix
  mat <- diag(length(x$states))
  rownames(mat) <- colnames(mat) <- x$states

  # apply the transitions
  for (t in x$transitions) {

    # if it's not a rate (nor compound containing a rate)
    if (!containsRate(t$transfun)) {

      # get the expectation
      expectation <- expected(t$transfun, patch)

      if (t$to == t$from) {
        # if it's the diagonal, multiply by the expectation
        mat[t$to, t$from] <- mat[t$to, t$from] * expectation
      } else {
        # if it's the off-diagonal, get the diagonal probability
        diag_prob <- mat[t$from, t$from]

        # multiply by probability and not probability
        mat[t$to, t$from] <- mat[t$to, t$from] + diag_prob * expectation

        # reduce the diagonal by the reciprocal
        mat[t$from, t$from] <- diag_prob * (1 - expectation)
      }

    }

  }

  return (mat)

}

containsRate <- function (transfun) {
  # check whether a transition contains a rate transition (rather than pure
  # probability)
  type <- transfunType(transfun)
  if (type == 'compound') {
    # if it's a compound, call recursively to look for any
    components <- transfun()
    ans <- containsRate(components[[1]]) | containsRate(components[[2]])
  } else if (type == 'rate') {
    ans <- TRUE
  } else {
    ans <- FALSE
  }
  return (ans)
}

# create a matrix contining text reporting the transition
textMatrix <- function (x) {
  mat <- matrix('', length(x$states), length(x$states))
  rownames(mat) <- colnames(mat) <- x$states
  for (t in x$transitions) {
    mat[t$to, t$from] <- transfun2text(t$transfun)
  }
  return (mat)
}

transfun2text <- function (transfun) {
  # create a short text representation of a transfun, for use in plotting
  type <- transfunType(transfun)
  if (type == 'compound') {
    components <- transfun()
    text <- paste0(transfun2text(components[[1]]),
                   ' * ',
                   transfun2text(components[[2]]))
  } else {

    # make a nice simple text representation
    prefix <- switch(type,
                     probability = 'p',
                     rate = 'r')

    # don't try to find the expectation if it's user-defined
    expect <- ifelse(containsUserTransfun(transfun),
                     '?',
                     round(expected(transfun), 2))

    text <- sprintf('%s(%s)',
                    prefix,
                    expect)

  }
  return (text)
}

