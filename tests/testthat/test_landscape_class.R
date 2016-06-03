context('landscape-class')

test_that('landscape classes work', {

  # create a dummy dynamic to get at its landscape
  # set up a simple model and initial population
  # generate four types of dynamic
  stasis_egg <- tr(eggs ~ eggs, p(0.4))
  stasis_larva <- tr(larvae ~ larvae, p(0.3))
  stasis_adult <- tr(adults ~ adults, p(0.8))
  hatching <- tr(larvae ~ eggs, p(0.5))
  fecundity <- tr(eggs ~ adults, p(0.2) * r(3))
  pupation <- tr(adults ~ larvae, p(0.2))
  clonal <- tr(larvae ~ larvae, r(1.4))

  all <- dynamic(stasis_egg,
                 stasis_larva,
                 stasis_adult,
                 hatching,
                 pupation,
                 clonal,
                 fecundity)

  ls_all <- landscape(all)
  ls_null <- as.landscape(NULL)
  ls_list <- as.landscape(list(area = c(10, 15, 12),
                               population = data.frame(eggs = 1,
                                                       larvae = 3,
                                                       adults = 12),
                               features = data.frame()[1, ]))

  # class we're expecting
  expect_s3_class(ls_all, 'landscape')
  expect_s3_class(ls_null, 'landscape')
  expect_s3_class(ls_list, 'landscape')

  # is.landscape
  expect_true(is.landscape(ls_all))
  expect_true(is.landscape(ls_null))
  expect_true(is.landscape(ls_list))
  expect_false(is.landscape(NA))
  expect_false(is.landscape(list()))
  expect_false(is.landscape(NULL))

  # expected print method output
  expect_equal(capture.output(print(ls_all)),
               'landscape with 1 patches')
  expect_equal(capture.output(print(ls_null)),
               'landscape with 1 patches')
  expect_equal(capture.output(print(ls_list)),
               'landscape with 3 patches')

  # getting and setting areas
  expect_equal(area(ls_all), 1)
  area(ls_all) <- 3
  expect_equal(area(ls_all), 3)

  expect_equal(area(ls_null), 1)
  area(ls_null) <- 3
  expect_equal(area(ls_null), 3)

  expect_equal(area(ls_list), c(10, 15, 12))
  area(ls_list) <- 3:1
  expect_equal(area(ls_list), 3:1)

  # wrong length
  expect_error(area(ls_list) <- 3)

  # getting and setting populations
  expect_equal(population(ls_all),
               data.frame(eggs = 0, larvae = 0, adults = 0))
  population(ls_all) <- population(ls_all) + 3
  expect_equal(population(ls_all),
               data.frame(eggs = 3, larvae = 3, adults = 3))
  expect_equal(population(ls_all, 'eggs'),
               3)

  # wrong lengths
  expect_error(population(ls_all) <- 3)
  expect_error(population(ls_all, 'bees'))


  # getting and setting features
  expect_equal(features(ls_all),
               data.frame()[1, ])
  features(ls_all) <- data.frame(temp = 10, rainfall = 11)
  expect_equal(features(ls_all),
               data.frame(temp = 10, rainfall = 11))

  # wrong dimension
  expect_error(features(ls_all) <- 3)

})
