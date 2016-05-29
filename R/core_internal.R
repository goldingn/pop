# core internal functions

getStates <- function (transitions) {
  # given a list of transitions, extract all of the mentioned states
  all_states <- lapply(transitions,
                       function (x) c(x$to, x$from))
  states <- unique(unlist(all_states))
  return (states)
}

