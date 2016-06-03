context('dynamic-class')

test_that('dynamic classes work', {

  # generate four types of dynamic
  stasis_egg <- tr(egg ~ egg, p(0.4))
  stasis_larva <- tr(larva ~ larva, p(0.3))
  stasis_adult <- tr(adult ~ adult, p(0.8))
  hatching <- tr(larva ~ egg, p(0.5))
  fecundity <- tr(egg ~ adult, r(3))
  pupation <- tr(adult ~ larva, p(0.2))
  stasis <- dynamic(stasis_egg,
                    stasis_larva,
                    stasis_adult)
  growth <- dynamic(hatching,
                    pupation)
  reproduction <- dynamic(fecundity)
  all1 <- dynamic(stasis_egg,
                  stasis_larva,
                  stasis_adult,
                  hatching,
                  pupation,
                  fecundity)

  # make sure adding two dyanmics is the same as compiling their transitions in
  # one go
  all2 <- dynamic(stasis, growth, reproduction)
  expect_equal(all2, all1)

  # check 3 trs, then two dynamics
  all3 <- dynamic(stasis_egg,
                  stasis_larva,
                  stasis_adult,
                  growth,
                  reproduction)
  expect_equal(all3, all1)

  # check dynamic sandwich
  all4 <- dynamic(stasis_egg,
                  stasis_larva,
                  stasis_adult,
                  growth,
                  fecundity)
  expect_equal(all4, all1)

  # check they have the right class
  expect_s3_class(stasis, 'dynamic')
  expect_s3_class(growth, 'dynamic')
  expect_s3_class(reproduction, 'dynamic')
  expect_s3_class(all1, 'dynamic')
  expect_s3_class(all2, 'dynamic')

  # check is.dynamic works on dynamics
  expect_true(is.dynamic(stasis))
  expect_true(is.dynamic(growth))
  expect_true(is.dynamic(reproduction))
  expect_true(is.dynamic(all1))
  expect_true(is.dynamic(all2))

  # check is.dynamic works on non-dynamics
  expect_false(is.dynamic(list()))
  expect_false(is.dynamic(NA))
  expect_false(is.dynamic(NULL))

  # check is.dynamic works on transitions
  expect_false(is.dynamic(stasis_egg))
  expect_false(is.dynamic(fecundity))

  # check as.dynamic works
  obj1 <- pop:::as.dynamic(list())
  obj2 <- pop:::as.dynamic(NA)
  obj3 <- pop:::as.dynamic(Inf)
  expect_s3_class(obj1, 'dynamic')
  expect_s3_class(obj2, 'dynamic')
  expect_s3_class(obj3, 'dynamic')

  # check print.dynamic works
  expect_equal(capture.output(print(all1)),
               'dynamic:	transitions between: egg, larva, adult')
  expect_equal(capture.output(print(reproduction)),
               'dynamic:	transitions between: egg, adult')

  # check as.matrix
  mat_stasis <- as.matrix(stasis)
  mat_growth <- as.matrix(growth)
  mat_reproduction <- as.matrix(reproduction)
  mat_all1 <- as.matrix(all1)
  mat_all2 <- as.matrix(all2)

  # check classes
  expect_s3_class(mat_stasis, c('matrix', 'transition_matrix'))
  expect_s3_class(mat_growth, c('matrix', 'transition_matrix'))
  expect_s3_class(mat_reproduction, c('matrix', 'transition_matrix'))
  expect_s3_class(mat_all1, c('matrix', 'transition_matrix'))
  expect_s3_class(mat_all2, c('matrix', 'transition_matrix'))

  # check dimensions are correct
  expect_equal(dim(mat_stasis), c(3, 3))
  expect_equal(dim(mat_growth), c(3, 3))
  expect_equal(dim(mat_reproduction), c(2, 2))
  expect_equal(dim(mat_all1), c(3, 3))
  expect_equal(dim(mat_all2), c(3, 3))

  # check all1 and all2 are still the same even as matrices
  expect_equal(mat_all1, mat_all2)

  # check that plot returns an igraph object
  plot_stasis <- plot(stasis)
  plot_growth <- plot(growth)
  plot_reproduction <- plot(reproduction)
  plot_all1 <- plot(all1)
  plot_all2 <- plot(all2)
  expect_s3_class(plot_stasis, 'igraph')
  expect_s3_class(plot_growth, 'igraph')
  expect_s3_class(plot_reproduction, 'igraph')
  expect_s3_class(plot_all1, 'igraph')
  expect_s3_class(plot_all2, 'igraph')

})
