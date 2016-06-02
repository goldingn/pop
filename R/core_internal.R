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
    stopifnot(length(population) == length(dynamic$states))

    # make it into a dataframe
    names <- names(population)
    population <- as.data.frame(as.list(population))
    names(population) <- names

  }

  # if it's a one-row dataframe, replicate for the number of habitat patches
  if (nrow(population) == 1) {
    n_patches <- nrow(population(landscape(dynamic)))
    population <- population[rep(1, n_patches), ]
  }

  return (population)

}
