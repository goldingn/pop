# core internal functions

getStates <- function (transitions) {
  # given a list of transitions, extract all of the mentioned states
  all_states <- lapply(transitions,
                       function (x) c(x$to, x$from))
  states <- unique(unlist(all_states))
  return (states)
}

expandPopulation <- function (population, dynamic) {

  # convert the population to a dataframe if required
  if (!is.data.frame(population)) {

    # check the population vector makes sense
    stopifnot(is.numeric(population))
    stopifnot(length(population) == length(states(dynamic)))
    stopifnot(all(sort(names(population)) == sort(states(dynamic))))

    # make it into a dataframe
    names <- names(population)
    population <- as.data.frame(as.list(population))
    names(population) <- names

  }

  # if it's a one-row dataframe, replicate for the number of habitat patches
  if (nrow(population) == 1) {
    stopifnot(ncol(population) == length(states(dynamic)))
    stopifnot(all(sort(names(population)) == sort(states(dynamic))))
    n_patches <- nrow(population(landscape(dynamic)))
    population <- population[rep(1, n_patches), ]
  }

  return (population)

}


captureDots <- function (...) {
  # capture arguments passed as dots, and grab their names even if not directly
  # named
  ans <- list(...)
  dots <- substitute(list(...))[-1]
  names(ans) <- sapply(dots, deparse)
  ans
}
