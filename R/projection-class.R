# projection class

#' @title Deterministic projection
#' @name projection
#' @rdname projection
#' @description Project a population dynamic model in discrete time, recording
#'   the number of individuals in each state at each time point.
#' @param dynamic a population dynamic model of class \code{\link{dynamic}}
#' @param population a dataframe or named vector of positive integers, giving
#'   the number of individuals in each state of \code{dynamic}. If a dataframe,
#'   it should have only one row (as in the examples below), or as many rows as
#'   patches in the metapopulation if a multi-patch landscape has been defined
#'   for \code{dynamic} (using \code{\link{landscape}}). If a multi-patch
#'   landscape has been defined for \code{dynamic}, but \code{population} has
#'   only one row or is a vector, this population will be duplicated for all
#'   patches in the landscape.
##' @param timesteps a positive integer giving the number of time steps
#'   (iterations) over which to simulate the model
#' @return an object of class \code{pop_projection}
#' @export
#' @examples
#' # set up a three-stage model
#' stasis_egg <- tr(egg ~ egg, p(0.6))
#' stasis_larva <- tr(larva ~ larva, p(0.4))
#' stasis_adult <- tr(adult ~ adult, p(0.9))
#' hatching <- tr(larva ~ egg, p(0.35))
#' fecundity <- tr(egg ~ adult, r(20))
#' pupation <- tr(adult ~ larva, p(0.2))
#'
#' pd <- dynamic(stasis_egg,
#'               stasis_larva,
#'               stasis_adult,
#'               hatching,
#'               pupation,
#'               fecundity)
#'
#' population <- data.frame(egg = 1200, larva = 250, adult = 50)
#'
#' # simulate for 50 timesteps, 30 times
#' proj <- projection(dynamic = pd,
#'                    population = population,
#'                    timesteps = 50)
#'
projection <- function (dynamic, population, timesteps = 1) {
  # given a dynamic and starting population, project the population for some
  # timesteps

  # coerce the population to the correct format
  population <- expandPopulation(population, dynamic)

  # update the dynamic's landscape population with the requested starting population
  population(landscape(dynamic)) <- population

  # get the number of patches
  n_patches <- nrow(landscape(dynamic))

  # set up results matrix
  result <- matrix(0,
                   nrow = timesteps + 1,
                   ncol = length(states(dynamic)) * n_patches)
  rownames(result) <- 0:timesteps
  colnames(result) <- popvecNames(population)

  # add population to first row
  popvec <- pop2vec(population)
  result[1, ] <- popvec

  # loop through timesteps projecting according to the landscape state
  for(i in seq_len(timesteps)) {

    # get the time-dependent transition matrix
    A <- as.matrix(dynamic)

    # project to the next timestep
    popvec <- (A %*% popvec)[, 1]

    # update the landscape population
    population(landscape(dynamic)) <- vec2pop(popvec, population)

    # store the result
    result[i + 1, ] <- popvec

  }

  # return simulations ina pop_simulation object
  result <- list(dynamic = dynamic,
                 projection = result)
  result <- as.pop_projection(result)
  return (result)

}

#' @rdname projection
#' @param x a \code{pop_projection} object, or an object to be tested as one
#' @export
#' @examples
#' is.pop_projection(proj)
#'
is.pop_projection <- function (x) {
  inherits(x, 'pop_projection')
}

#' @rdname projection
#' @param \dots further arguments passed to or from other methods.
#' @param states character vector naming the states in the \code{dynamic} object
#'   used to run the projection that should be plotted. By default all of them
#'   are plotted.
#' @param patches vector of positive integers identifying the patches for which
#'   to plot the projections. By default only projections for the first patch
#'   are plotted.
#' @export
#' @examples
#' par(mfrow = c(3, 1))
#' plot(proj)
plot.pop_projection <- function (x, states = NULL, patches = 1, ...) {

  # get states if they aren't specified
  if (is.null(states)) states <- states(x$dynamic)

  # check they're sane
  n_states <- length(states(x$dynamic))
  n_patches <- nrow(landscape(x$dynamic))
  stopifnot(states %in% states(x$dynamic))
  stopifnot(all(patches %in% seq_len(n_patches)))

  # plot them one at a time
  for (patch in patches) {
    for (state in states) {

      if (n_patches == 1) {
        title <- state
      } else {
        title <- sprintf('%s in patch %i',
                         state,
                         patch)
      }

      # get column index & column
      idx <- (patch - 1) * n_states + match(state, states(x$dynamic))
      state_population <- x$projection[, idx]

      # get y axis range
      ylim = range(state_population, na.rm = TRUE)

      # get x axis
      xaxs <- as.numeric(rownames(x$projection))

      # set up an empty plot
      plot(state_population ~ xaxs,
           type = 'n',
           ylim = ylim,
           ylab = 'population',
           xlab = 'time',
           main = title)

      lines(state_population ~ xaxs,
            lwd = 2,
            col = grey(0.4))

    }
  }
  # name and return result
  return (invisible(x$projection))

}

as.pop_projection <- function (x) {
  if (!is.pop_projection(x)) {
    class(x) <- c('pop_projection', class(x))
  }
  return (x)
}

# functions to flatten and unflatten population
pop2vec <- function (population) {
  # convert a population dataframe into a vector for deterministic analysis
  ans <- as.vector(t(as.matrix(population)))
  return (ans)
}

popvecNames <- function (population) {
  # get appropriate names for flattened version of population dataframe
  states <- colnames(population)
  patches <- as.character(seq_len(nrow(population)))
  if (length(patches) == 1) {
    # if only one patch, don't pollute the names
    names <- states
  } else {
    names <- apply(expand.grid(states, patches),
                   1,
                   paste,
                   sep = '',
                   collapse = '_patch_')
  }
  return (names)
}

vec2pop <- function (vector, population) {
  n_state <- ncol(population)
  n_patch <- nrow(population)
  population[] <- matrix(vector,
                         nrow = n_patch,
                         ncol = n_state,
                         byrow = TRUE)
  return (population)
}
