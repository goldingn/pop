# simulation class

#' @title Stochastic Simulation
#' @name simulation
#' @rdname simulation
#' @description Simulate a population dynamic model in discrete time, recording
#'   the number of individuals in each state at each time point.
#' @param dynamic a population dynamic model of class \code{\link{dynamic}}
#' @param population a named vector of positive integers, giving the number of
#'   individuals in each state of \code{dynamic}
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
#' population <- c(egg = 1200, larva = 250, adult = 50)
#' names(population) <- pd$states
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

  # check the population vector makes sense
  stopifnot(length(population) == length(dynamic$states))
  stopifnot(sort(names(population)) == sort(dynamic$states))

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
#' @export
#' @examples
#' par(mfrow = c(3, 1))
#' plot(sim)
plot.simulation <- function (x, states = NULL, ...) {
  # plot a pop simulation
  # state gives the name of the state to plot (by default all of them, in separate plots)

  # get states if they aren't specified
  if (is.null(states)) states <- x$dynamic$states

  # check they're sane
  stopifnot(states %in% x$dynamic$states)

  # object to store the results in
  result <- list()

  # plot them one at a time
  for (state in states) {

    # extract the simulations for this state
    sims <- lapply(x$simulations,
                   function(x) x[, state])

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
    ylim = range(sims[[1]], na.rm = TRUE)

    # get x axis
    xaxs <- as.numeric(names(sims[[1]]))

    # set up an empty plot
    plot(sims[[1]] ~ xaxs,
         type = 'n',
         ylim = ylim,
         ylab = 'population',
         xlab = 'time',
         main = state)

    # draw the 95% CI polygon (if available) and median line
    polygon(x = c(xaxs, rev(xaxs)),
            y = c(quants[, 1], rev(quants[, 3])),
            col = grey(0.9),
            border = NA)

    lines(quants[, 2] ~ xaxs,
          lwd = 2,
          col = grey(0.4))

    result[[state]] <- quants

  }

  # name and return result
  names(result) <- states
  return (invisible(result))

}

update <- function (population, transition, ...) {
  # stochastically update the population based on a transition the dots argument
  # may be used to specify environmental or other determinants of the transition
  # function
  N <- population[transition$from]
  population[transition$to] <- stoch(transition$transfun,
                                     N = N)
  return (population)
}

# stochastic updates for probabilities and rates
stoch_prob <- function (parameters, N) {
  rbinom(n = 1, size = N, prob = parameters[1])
}
stoch_rate <- function (parameters, N) {
  rpois(n = 1, lambda = N * parameters[1])
}

stoch <- function (transfun, N) {
  # given a parameter value, a number of individuals in the *from* state,
  # stochastically generate the number of individuals in the *to* state

  # get type
  type <- transfunType(transfun)

  if (type == 'compound') {

    # if it's a compound transfun, call stoch recursively on each component
    components <- transfun()
    N <- stoch(components[[1]], N)
    N <- stoch(components[[2]], N)

  } else {

    # otherwise execute the transition
    N <- switch(type,
                probability = stoch_prob(expected(transfun), N),
                rate = stoch_rate(expected(transfun), N))

  }

  return (N)

}

popSimulate <- function (iter, dynamic, population, timesteps) {
  # internal function to run simulations. First element is a dummy for use
  # with (par)lapply.

  # set up matrix to store results
  res <- matrix(0,
                nrow = timesteps + 1,
                ncol = length(population))
  res[1, ] <- population
  rownames(res) <- 0:timesteps
  colnames(res) <- names(population)

  for (time in seq_len(timesteps)) {
    for (trans in dynamic$transitions) {

      # update the population stochastically
      population <- update(population, trans)

    }

    # store the result & call it quits if they're all gone
    res[time + 1, ] <- population
    if (all(population == 0)) break()

  }

  return (res)

}

as.simulation <- function (x) {
  class(x) <- c('simulation', class(x))
  return (x)
}
