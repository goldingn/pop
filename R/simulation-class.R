# simulation class

#' @title Stochastic Simulation
#' @name simulation
#' @rdname simulation
#' @description Simulate a population dynamic model in discrete time, recording
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
#' @param timesteps a positive integer giving the number of time steps
#'   (iterations) over which to simulate the model
#' @param replicates a positive integer giving the number of independent time
#'   series to simulate
#' @param ncores an optional positive integer giving the number of cpu cores to
#'   use when running simulations. By default (when \code{ncores = NULL}) all
#'   cores are used (or as many as \code{parallel::detectCores} can find). This
#'   argument is ignored is \code{replicates = 1}
#' @return an object of class \code{simulation}
#' @details The order of the dynamics in the simulation is defined by the order
#'   in which the transitions were passed to \code{dynamic}. I.e. if the stasis
#'   probability of a life stage (e.g. fraction surviving and remaining in the
#'   stage) was specified before the reproduction rate, then only those staying
#'   in the state will reproduce. Conversely, if reproduction was given first,
#'   individuals will reproduce before the stasis probability is applied.
#' @export
#' @import parallel
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
#' sim <- simulation(dynamic = pd,
#'                   population = population,
#'                   timesteps = 50,
#'                   replicates = 30,
#'                   ncores = 1)
#'
simulation <- function (dynamic, population, timesteps = 1, replicates = 1, ncores = NULL) {
  # given a dynamic and starting population, simulate the population for some
  # timesteps, and replicate a number of times, optionally in parallel

  # coerce the population to the correct format
  population <- expandPopulation(population, dynamic)

  # update the dynamic's landscape population with the requested starting population
  population(landscape(dynamic)) <- population

  # if the number of cores is not defined, assume they want all of them
  if (is.null(ncores)) {
    ncores <- parallel::detectCores()
  }

  # run the replicate simulations, in parallel or sequence
  if (ncores > 1 & replicates > 1) {
    # parallel case

    # set up the cluster
    on.exit(parallel::stopCluster(cl))
    cl <- parallel::makeCluster(ncores)

    # export pop
    parallel::clusterEvalQ(cl = cl,
                           library(pop))

    # run simulations in parallel
    sims <- parallel::parLapply(cl = cl,
                                seq_len(replicates),
                                fun = popSimulate,
                                dynamic = dynamic,
                                population = population,
                                timesteps = timesteps)

  } else {
    # sequence case

    # otherwise just lapply
    sims <- lapply(seq_len(replicates),
                   FUN = popSimulate,
                   dynamic = dynamic,
                   population = population,
                   timesteps = timesteps)

  }

  # return simulations ina pop_simulation object
  result <- list(dynamic = dynamic,
                 simulations = sims)
  result <- as.simulation(result)
  return (result)

}

#' @rdname simulation
#' @param x a \code{simulation} object, or an object to be tested as a \code{simulation}
#' @export
#' @examples
#' is.simulation(sim)
is.simulation <- function (x) {
  inherits(x, 'simulation')
}

#' @rdname simulation
#' @param \dots further arguments passed to or from other methods.
#' @param states a character vector naming the states in the \code{dynamic}
#'   object used to run the simulation that should be plotted. By default all of
#'   them are.
#' @param patches vector of positive integers identifying the patches for which
#'   to plot the simulations. By default only projections for the first patch
#'   are plotted.
#' @export
#' @examples
#' par(mfrow = c(3, 1))
#' plot(sim)
plot.simulation <- function (x, states = NULL, patches = 1, ...) {
  # plot a pop simulation
  # state gives the name of the state to plot (by default all of them, in separate plots)

  # get states if they aren't specified
  if (is.null(states)) states <- states(x$dynamic)

  # check they're sane
  n_states <- length(states(x$dynamic))
  n_patches <- nrow(landscape(x$dynamic))
  stopifnot(states %in% states(x$dynamic))
  stopifnot(all(patches %in% seq_len(n_patches)))

  # object to store the results in
  result <- list()

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

      # extract the simulations for this state
      sims <- lapply(x$simulations,
                     function(x) x[, idx])

      # if there are replicates, get CIs and medians of counts for each timepoint
      if (length(sims) > 1) {
        sims_mat <- do.call(cbind, sims)
        quants <- t(apply(sims_mat, 1, quantile, c(0.025, 0.5, 0.975)))
      } else {
        quants <- cbind(rep(NA, length(sims[[1]])),
                        sims[[1]],
                        rep(NA, length(sims[[1]])))
      }

      colnames(quants) <- c('lower_95_CI',
                            'median',
                            'upper_95_CI')

      rownames(quants) <- names(sims[[1]])

      # get y axis range
      ylim = range(quants, na.rm = TRUE)

      # get x axis
      xaxs <- as.numeric(names(sims[[1]]))

      # set up an empty plot
      plot(sims[[1]] ~ xaxs,
           type = 'n',
           ylim = ylim,
           ylab = 'population',
           xlab = 'time',
           main = title)

      # draw the 95% CI polygon (if available) and median line
      polygon(x = c(xaxs, rev(xaxs)),
              y = c(quants[, 1], rev(quants[, 3])),
              col = grey(0.9),
              border = NA)

      lines(quants[, 2] ~ xaxs,
            lwd = 2,
            col = grey(0.4))

      result[[title]] <- quants

    }
  }

  # name and return result
  names(result) <- states
  return (invisible(result))

}

update <- function (population, dynamic) {
  # stochastically update the population based on a dynamic.

  # get new population object to fill
  new_population <- population * 0

  # get landscape object
  landscape <- landscape(dynamic)

  # loop through transitions
  for (trans in dynamic) {

    # get the old and new N
    N <- population[, trans$from]
    N_new <- stoch(trans$transfun, N = N, landscape = landscape)

    if (trans$to == trans$from) {
      # if it's to the same state...

      if (contains(trans$transfun, 'rate')) {

        # if it's a rate (recruitment) add to new population
        new_population[, trans$to] <- new_population[, trans$to] + N_new

      } else {

        # if it's a (survival) probability (not recruitment), or dispersal to
        # same state, replace *old* population with the new one
        population[, trans$to] <- N_new

      }

    } else {
      # if it's to another state (can't be a dispersal)

      if (contains(trans$transfun, 'rate')) {

        # if it was a recruitment event add to the state in the new population
        new_population[, trans$to] <- new_population[, trans$to] + N_new

      } else {

        # if it *wasn't* a recruitment event, update in the new population
        new_population[, trans$to] <- new_population[, trans$to] + N_new

        # and remove the same number from the old population
        population[, trans$from] <- population[, trans$from] - N_new

      }

    }

  }

  # add surviving members of the old population to the new one & return
  new_population <- new_population + population
  return (new_population)

}

# stochastic updates for probabilities and rates
stoch_prob <- function (expectation, N) {
  rbinom(n = length(N), size = N, prob = expectation)
}
stoch_rate <- function (expectation, N) {
  rpois(n = length(N), lambda = N * expectation)
}
stoch_disp <- function (expectation, N) {
  # random multinomial draws on a square matrix
  disp <- N * 0
  for (i in seq_along(N)) {
    disp <- disp + rmultinom(1, N[i], expectation[i, ])
  }
  return (disp[, 1])
}

stoch <- function (transfun, N, landscape) {
  # given a parameter value, a number of individuals in the *from* state,
  # stochastically generate the number of individuals in the *to* state

  # get type
  type <- transfunType(transfun)

  # if it's a dispersal
  if (contains(transfun, 'dispersal')) {

    # randomly them move across the landscape
    N <- stoch_disp(transfun(landscape), N)

  } else if (type == 'compound') {

    # if it's a (non-dispersal) compound transfun, call stoch recursively on each component
    tf_x <- environment(transfun)$x
    tf_y <- environment(transfun)$y
    N <- stoch(tf_x, N, landscape)
    N <- stoch(tf_y, N, landscape)

  } else {

    # otherwise execute the basis transition on its own
    N <- switch (type,
                 probability = stoch_prob(transfun(landscape), N),
                 rate = stoch_rate(transfun(landscape), N))

  }

  return (N)

}

popSimulate <- function (iter, dynamic, population, timesteps) {
  # internal function to run simulations. First element is a dummy for use
  # with (par)lapply.

  # get the numbers of states and patches
  n_states <- length(states(dynamic))
  n_patches <- nrow(landscape(dynamic))

  # set up results matrix
  res <- matrix(0,
                nrow = timesteps + 1,
                ncol = n_states * n_patches)
  rownames(res) <- 0:timesteps
  colnames(res) <- popvecNames(population)

  # add population to first row
  popvec <- pop2vec(population)
  res[1, ] <- popvec


  for (time in seq_len(timesteps)) {

    # sample the population
    population <- update(population, dynamic)

    # update it in the landscape information
    population(landscape(dynamic)) <- population

    # store the result & call it quits if they're all gone
    res[time + 1, ] <- pop2vec(population)
    if (all(population == 0)) break()

  }

  return (res)

}

as.simulation <- function (x) {
  if (!is.simulation(x)) {
    class(x) <- c('simulation', class(x))
  }
  return (x)
}
