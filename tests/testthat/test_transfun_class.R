context('transfun-class')

test_that('transfun classes work', {
  
  # two types of transfun
  prob <- p(0.5)
  rate <- r(3)
  
  # check they have the right class
  expect_s3_class(prob, 'transfun')
  expect_s3_class(rate, 'transfun')
  
  # check is.transfun works on transfuns
  expect_true(is.transfun(prob))
  expect_true(is.transfun(rate))

  # check is.transfun works on non-transfuns
  expect_false(is.transfun(list()))
  expect_false(is.transfun(NA))
  expect_false(is.transfun(NULL))
  
  # check print.transfun works
  expect_equal(capture.output(print(prob)),
               'probability 0.5')
  expect_equal(capture.output(print(rate)),
               'rate 3')
  
})