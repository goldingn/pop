# things related to the transfun class

transfunClasses <- function () {
  # list all available classes of transfun
  c('probability', 'rate')
}

transfunType <- function (x) {
  # get the type of the transfun object
  stopifnot(is.transfun(x))
  classes <- class(x)
  matches <- na.omit(match(transfunClasses(), classes))
  if (length(matches) == 0) {
    stop ('this transfun object does not correspond to any known transfun types')
  } else if (length(matches) > 1) {
    stop ('this transfun object correspond to multiple transfun types')
  } else {
    type <- classes[matches]
  }
  return (type)
}

is.transfun <- function (x) inherits(x, 'transfun')

print.transfun <- function(x, ...) {
  text <- sprintf('%s %s\n',
                  transfunType(x),
                  x())
  cat(text)
}
