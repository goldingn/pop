# dynamic class functions

#' @title dynamic objects
#' @name dynamic
#' @rdname dynamic
#' @param \dots for \code{dynamic()}: one or more \code{transition} (or other
#'   \code{dynamic}) objects making up the dynamic. For \code{plot()} and
#'   \code{print()}: further arguments passed to or from other methods
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
#' # combine these into separate dynamics
#' stasis <- dynamic(stasis_egg,
#'                   stasis_larva,
#'                   stasis_adult)
#' growth <- dynamic(hatching,
#'                   pupation)
#' reproduction <- dynamic(fecundity)
#'
#' # combine these into one dynamic (the same as listing all the transitions
#' # separately)
#' all <- dynamic(stasis, growth, reproduction)
#'
dynamic <- function (...) {
  # given a bunch of transition functions, build an object representing a
  # dynamical system

  # capture objects
  object <- captureDots(...)

  # unpack any dynamics into their component transitions, keeping names etc
  object <- unpackDynamics(object)

  # check they're transitions
  stopifnot(all(sapply(object, is.transition)))

  # set class, add default landscape and return
  object <- as.dynamic(object)
  landscape(object) <- as.landscape(object)
  return (object)
}

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

#' @rdname dynamic
#' @name states
#' @export
#' @examples
#' # get component states
#' states(all)
#'
states <- function (x) {
  getStates(x)
}

#' @rdname dynamic
#' @export
#' @examples
#' # print method
#' print(all)
#'
print.dynamic <- function (x, ...) {
  text <- sprintf('dynamic:\ttransitions between: %s\n',
                  paste(states(x), collapse = ', '))
  cat(text)
}

#' @rdname dynamic
#' @param which which type of matrix to build: the overall population growth
#'   matrix (\code{'A'}), the probabilistic progression matrix (\code{'P'}), the
#'   fecundity matrix (\code{'F'}) or the intrinsic reproduction matrix
#'   (\code{'R'})
#' @export
#' @importFrom MASS ginv
#' @examples
#' # convert to a transition matrix
#' as.matrix(all)
as.matrix.dynamic <- function (x, which = c('A', 'P', 'F', 'R'), ...) {

  # build the overall, reproduction (R), progression (P) of fecundity (F) matrix
  which <- match.arg(which)

  # find the numbers of patches and states
  n_patches <- nrow(landscape(x))
  n_states <-  length(states(x))
  n_cells <- n_patches * n_states

  # split the dynamic into demographic and dispersal components
  is_disp <- sapply(x, function (x) contains(x$transfun, 'dispersal'))
  x_disp <- subDynamic(x, which(is_disp))
  x_demog <- subDynamic(x, which(!is_disp))

  if (n_patches > 1) {

    # if there are multiple patches, set up metamatrix
    mat <- matrix(0, n_cells, n_cells)

    # loop through them patches getting the demographic components
    for (patch in seq_len(n_patches)) {
      # remove dispersals
      sub_dynamic <- x_demog
      landscape(sub_dynamic) <- landscape(x_disp)[[patch]]
      sub_mat <- as.matrix(sub_dynamic, which = which, ...)
      idx <- (patch - 1) * n_states + seq_len(n_states)
      mat[idx, idx] <- sub_mat
    }

    # for all except fecundity
    if (which != 'F') {

      # get the dispersal probabilities and add to matrix in correct places
      for (trans in x_disp) {

        # find indices in meta matrix
        from_state <- match(trans$from, states(x))
        to_state <- match(trans$to, states(x))
        from_idx <- from_state + (seq_len(n_patches) - 1) * n_states
        to_idx <- to_state + (seq_len(n_patches) - 1) * n_states
        cells <- as.matrix(expand.grid(to_idx, from_idx))

        # get expectation of dispersal
        disp <- trans$transfun(landscape(x))

        # make it sum to 1, row-wise
        disp <- sweep(disp, 1, rowSums(disp), '/')

        # apply diagonal transitions (some fraction staying in state)
        disp <- sweep(disp, 1, diag(mat)[from_idx], '*')

        # insert into meta matrix
        mat[cells] <- as.numeric(disp)

      }

    }

  } else {

    # if only one patch, just get the demographic component
    mat <- switch(which,
                  `A` = getA(x_demog),
                  `P` = getP(x_demog),
                  `F` = getF(x_demog),
                  `R` = getR(x_demog))

  }

  # set class and return
  class(mat) <- c(class(mat), 'transition_matrix')
  return (mat)

}

getA <- function (x) {

  # get the full population projection matrix from a dynamic
  # set up empty matrix
  mat <- diag(length(states(x)))
  rownames(mat) <- colnames(mat) <- states(x)
  landscape <- landscape(x)

  # apply the transitions
  for (t in x) {

    # get the expectation
    expectation <- t$transfun(landscape)

    # if it's a rate (or compound containing a rate)
    if (contains(t$transfun, 'rate')) {

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

getR <- function (x) {
  # get the reproduction matrix from a dynamic;
  # combine P & F accounting for clonal
  # reproduction (rates on diagonal)
  P <- getP(x)
  eye <- diag(nrow(P))
  mat <- getF(x) %*% ginv(eye - P)
  return (mat)
}

getF <- function (x) {

  # set up empty matrix
  mat <- diag(length(states(x))) * 0
  rownames(mat) <- colnames(mat) <- states(x)
  landscape <- landscape(x)

  # apply the transitions
  for (t in x) {

    # if it's a rate (or compound containing a rate)
    if (contains(t$transfun, 'rate')) {

      # get the expectation and add it in
      expectation <- t$transfun(landscape)
      mat[t$to, t$from] <-  expectation

    }

  }

  return (mat)

}

getP <- function (x) {

  # set up empty matrix
  mat <- diag(length(states(x)))
  rownames(mat) <- colnames(mat) <- states(x)
  landscape <- landscape(x)

  # apply the transitions
  for (t in x) {

    # if it's not a rate (nor compound containing a rate)
    if (!contains(t$transfun, 'rate')) {

      # get the expectation
      expectation <- t$transfun(landscape)

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

contains <- function (transfun, which = c('probability', 'rate', 'dispersal')) {
  # check whether a transition contains a rate transition (rather than pure
  # probability)
  which <- match.arg(which)
  type <- transfunType(transfun)
  if (type == 'compound') {
    # if it's a compound, call recursively to look for any
    tf_x <- environment(transfun)$x
    tf_y <- environment(transfun)$y
    ans <- contains(tf_x, which) | contains(tf_y, which)
  } else if (type == which) {
    ans <- TRUE
  } else {
    ans <- FALSE
  }
  return (ans)
}

# create a matrix contining text reporting the transition
textMatrix <- function (x) {
  states <- states(x)
  mat <- matrix('', length(states), length(states))
  rownames(mat) <- colnames(mat) <- states
  for (t in x) {
    mat[t$to, t$from] <- transfun2text(t$transfun)
  }
  return (mat)
}

transfun2text <- function (transfun) {
  # create a short text representation of a transfun, for use in plotting
  type <- transfunType(transfun)
  if (type == 'compound') {
    tf_x <- environment(transfun)$x
    tf_y <- environment(transfun)$y
    text <- paste0(transfun2text(tf_x),
                   ' * ',
                   transfun2text(tf_y))
  } else {

    # make a nice simple text representation
    prefix <- switch(type,
                     probability = 'p',
                     rate = 'r',
                     dispersal = 'd')

    # don't try to find the expectation if it's user-defined
    landscape <- as.landscape(NULL)
    expect <- ifelse(containsUserTransfun(transfun),
                     '?',
                     round(transfun(landscape), 2))

    text <- sprintf('%s(%s)',
                    prefix,
                    expect)

  }
  return (text)
}

#' @rdname dynamic
#' @export
#' @examples
#' # extract the parameters
#' (param_stasis <- parameters(stasis))
#' (param_all <- parameters(all))
#'
parameters.dynamic <- function (x) {
  lapply(x, parameters)
}

#' @rdname dynamic
#' @export
#' @param value a nested named list of parameters within each transition
#'   matching those currently defined for \code{x}
#' @examples
#' # update the parameters of these transfuns
#' param_stasis$stasis_egg$p <- 0.6
#' parameters(stasis) <- param_stasis
#' parameters(stasis)
#'
#' param_all$fecundity$r <- 15
#' parameters(all) <- param_all
#' parameters(all)
`parameters<-.dynamic` <- function (x, value) {
  for (i in 1:length(x)) {
    parameters(x[[i]]) <- value[[i]]
  }
  return (x)
}

unpackDynamics <- function (object) {
  # given a named list of (hopefully) transition and dynamic objects, expand out
  # all the component transitions of the dynamics, in order, into a named list
  # of dynamics. This is harder than it should be, but is the tidiest way I
  # found to keep the transition names without prepending the dynamic name to it

  # look for dynamics
  dynamics <- sapply(object, is.dynamic)

  # if it's just one dynamic, return it as is
  if (length(dynamics) == 1 && dynamics) {

    object <- object[[1]]

  } else if (any(dynamics)) {

    # grab the first one
    elem <- which(dynamics)[1]

    if (elem == 1) {
      # if it's the first element (can't be only element)
      object <- c(object[[elem]], object[-elem])
    } else if (elem == length(object)) {
      # if it's the last element (can't be only element)
      object <- c(object[-elem], object[[elem]])
    } else {
      # it must be in the middle
      object <- c(object[1:(elem - 1)], object[[elem]], object[(elem + 1):length(object)])
    }

    # one down, now recursively look for more
    object <- unpackDynamics(object)

  }

  # return
  return (object)

}


subDynamic <- function (x, i) {
  attrib <- attributes(x)
  attrib$names <- attrib$names[i]
  x <- x[i]
  attributes(x) <- attrib
  return (x)
}
