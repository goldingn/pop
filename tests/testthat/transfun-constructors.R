context('transfun-constructors')

test_that('transfun constructors work', {
  
  # ~~~~~~~~~~
  # probability
  
  # alias works
  expect_equal(p, probability)
  
  # invalid values
  expect_error(p(0))
  expect_error(p(1))
  expect_error(p(1.1))
  expect_error(p(-0.1))
  expect_error(p(NA))
  expect_error(p(NULL))
  expect_error(p(-Inf))
  expect_error(p(Inf))
  
  # valid values
  expect_error(p(.Machine$double.eps), NA)
  expect_error(p(1 - .Machine$double.eps), NA)
  
  prob <- p(0.5)
  
  # check they have the right class
  expect_s3_class(prob, c('transfun', 'probability', 'function'))
  
  # check is.probability works
  expect_true(is.probability(prob))
  expect_false(is.probability(list()))
  expect_false(is.probability(NA))
  expect_false(is.probability(NULL))
  expect_false(is.probability(r(3)))

  
  # ~~~~~~~~~~
  # rate
  
  # alias works
  expect_equal(r, rate)
  
  # invalid values
  expect_error(r(0))
  expect_error(r(-0.1))
  expect_error(r(-Inf))
  expect_error(r(Inf))
  expect_error(r(NA))
  expect_error(r(NULL))
  
  # valid values
  expect_error(r(.Machine$double.eps), NA)
  
  ra <- r(3)
  
  # check they have the right class
  expect_s3_class(ra, c('transfun', 'rate', 'function'))
  
  # check is.probability works
  expect_true(is.rate(ra))
  expect_false(is.rate(list()))
  expect_false(is.rate(NA))
  expect_false(is.rate(NULL))
  expect_false(is.rate(p(0.5)))
  
})