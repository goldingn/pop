context('stochastic-analysis')

test_that('stochastic analyses work', {

  skip_on_cran()

  # set up a simple model and initial population
  stasis_egg <- tr(eggs ~ eggs, p(0.4))
  stasis_larva <- tr(larvae ~ larvae, p(0.3))
  stasis_adult <- tr(adults ~ adults, p(0.8))
  hatching <- tr(larvae ~ eggs, p(0.5))
  pupation <- tr(adults ~ larvae, p(0.2))
  fecundity <- tr(eggs ~ adults, r(20))
  all <- dynamic(stasis_egg,
                 stasis_larva,
                 stasis_adult,
                 hatching,
                 pupation,
                 fecundity)
  population <- data.frame(eggs = 1000,
                           larvae = 200,
                           adults = 50)


  # set the RNG seed and simulate 30 times for 50 generations each
  set.seed(1)

  # run with 1 core
  sim <- simulation(dynamic = all,
                    population = population,
                    timesteps = 50,
                    replicates = 30,
                    ncores = 1)

  # check it has the right class and structure
  expect_s3_class(sim, 'simulation')
  expect_s3_class(sim$dynamic, 'dynamic')
  expect_true(is.list(sim$simulations))

  # 30 replicates of 51 snapshots
  expect_equal(length(sim$simulations), 30)
  maxt <- sapply(sim$simulations, nrow)
  expect_true(all(maxt == 51))

  # check there are no NAs in there
  NAs <- sapply(sim$simulations, anyNA)
  expect_false(any(NAs))

  # each should have the right number of states
  cols <- sapply(sim$simulations, ncol)
  expect_true(all(cols == length(states(all))))

  # check we get errors for dodgy inputs

  # for a transition instead of a dynamic
  expect_error(simulation(dynamic = all[[1]],
                          population = population,
                          timesteps = 50,
                          replicates = 30))

  # for the wrong size of population
  expect_error(simulation(dynamic = all,
                          population = population[1:2],
                          timesteps = 50,
                          replicates = 30))

  # for the wrong population names
  population2 <- population
  names(population2) <- paste0(names(population2), '_blaaaargh')
  expect_error(simulation(dynamic = all,
                          population = population2,
                          timesteps = 50,
                          replicates = 30))

  # nagative populations
  expect_error(simulation(dynamic = all,
                          population = population * -1,
                          timesteps = 50,
                          replicates = 30))

  # non-finite populations
  expect_error(simulation(dynamic = all,
                          population = population * Inf,
                          timesteps = 50,
                          replicates = 30))

  # non-finite populations
  expect_error(simulation(dynamic = all,
                          population = population * NA,
                          timesteps = 50,
                          replicates = 30))

  # negative timesteps
  expect_error(simulation(dynamic = all,
                          population = population,
                          timesteps = -1,
                          replicates = 30))

  # negative replicates
  expect_error(simulation(dynamic = all,
                          population = population,
                          timesteps = 50,
                          replicates = -1))

  # check is.simulation
  expect_true(is.simulation(sim))
  expect_false(is.simulation(sim$dynamic))

  # check as.simulation
  expect_true(is.simulation(pop:::as.simulation(NA)))
  expect_false(is.simulation(pop:::as.dynamic(NA)))

  # check simulation plotting

  # check output structure for one state
  plot_out_egg <- plot(sim, state = 'eggs')
  # list
  expect_true(is.list(plot_out_egg))
  # first element as 3 columns and right no. rows
  expect_equal(dim(plot_out_egg[[1]]), c(51,3))
  # no NAs
  expect_true(!any(is.na(plot_out_egg[[1]])))

  # check output structure for all state
  plot_out_all <- plot(sim)
  # list
  expect_true(is.list(plot_out_all))
  # first element as 3 columns and right no. rows
  expect_equal(dim(plot_out_all[[1]]), c(51,3))
  # no NAs
  expect_true(!any(is.na(plot_out_all[[1]])))

  # error on bad states
  expect_error(plot(sim, state = 'bee'))
  expect_error(plot(sim, state = NA))

})
