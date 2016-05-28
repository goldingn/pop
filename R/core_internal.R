# core internal functions

getStates <- function (transitions) {
  # given a list of transitions, extract all of the mentioned states
  all_states <- lapply(transitions,
                       function (x) c(x$to, x$from))
  states <- unique(unlist(all_states))
  return (states)
}

getMatrix <- function (states, transitions) {
  # given a vector of states and list of transitions,
  # build a transition matrix

  # set up empty matrix
  n_states <- length(states)
  mat <- matrix(0, n_states, n_states)
  rownames(mat) <- colnames(mat) <- states

  # add in the transitions we know about
  for (t in transitions) {
    mat[t$to, t$from] <- t$transfun()
  }

  # set class and return
  class(mat) <- c(class(mat), 'transition_matrix')
  return (mat)

}
