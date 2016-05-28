tr <- function (formula, transfun) {
  # given a formula describing a particular transition,
  # parse into an object & store the value

  stopifnot(inherits(formula, 'formula'))
  stopifnot(is.transfun(transfun))

  to <- as.character(formula[[2]])
  from <- as.character(formula[[3]])

  object <- list(to = to,
                 from = from,
                 transfun = transfun)

  class(object) <- 'transition'
  return (object)

}

is.transition <- function (x) inherits(x, 'transition')
as.transition <- function (x) {
  class(x) <- c(class(x), 'transition')
  return (x)
}


print.transition <- function (x, ...) {
  text <- sprintf('transition:\t%s -> %s with %s\n',
                  x$from,
                  x$to,
                  capture.output(print(x$transfun)))
  cat(text)
}
