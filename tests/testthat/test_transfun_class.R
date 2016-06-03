context('transfun-class')

test_that('transfun classes work', {

  # two types of transfun
  prob <- p(0.5)
  rate <- r(3)

  # a user-specified transfun
  ddfun <- function (landscape) {
    adult_density <- population(landscape, 'adults') / area(landscape)
    param$p * exp(-adult_density / param$area)
  }
  dd <- as.transfun(ddfun,
                    param = list(p = 0.9,
                                 area = 1000),
                    type = 'probability')

  # compound transfuns
  compound <- prob * rate
  compound_user <- prob * dd


  # check they have the right class
  expect_s3_class(prob, 'transfun')
  expect_s3_class(rate, 'transfun')
  expect_s3_class(dd, 'transfun')
  expect_s3_class(compound, 'transfun')
  expect_s3_class(compound_user, 'transfun')


  # check is.transfun works on transfuns
  expect_true(is.transfun(prob))
  expect_true(is.transfun(rate))
  expect_true(is.transfun(dd))
  expect_true(is.transfun(compound))
  expect_true(is.transfun(compound_user))

  # check is.transfun works on non-transfuns
  expect_false(is.transfun(list()))
  expect_false(is.transfun(NA))
  expect_false(is.transfun(NULL))

  # check print.transfun works on boring transfuns
  expect_equal(capture.output(print(prob)),
               'probability transfun with expectation 0.5')
  expect_equal(capture.output(print(rate)),
               'rate transfun with expectation 3')
  expect_equal(capture.output(print(dd)),
               'user-specified probability transfun')
  expect_equal(capture.output(print(compound)),
               'compound transfun with expectation 1.5')
  expect_equal(capture.output(print(compound_user)),
               'user-specified compound transfun')

  # screw with some transfuns and expect an error
  bad_prob2 <- bad_prob <- prob
  class(bad_prob) <- c('flooflah', 'transfun', 'function')
  class(bad_prob2) <- c('probability', 'rate', 'transfun', 'function')

  # they're still transfuns, but the internal checks should error
  expect_true(is.transfun(bad_prob))
  expect_true(is.transfun(bad_prob2))
  expect_error(pop:::transfunType(bad_prob))
  expect_error(pop:::transfunType(bad_prob2))

})
