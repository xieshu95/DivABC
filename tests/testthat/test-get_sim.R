test_that("test get_sim output is correct for DAISIE", {

  param_space <- load_param_space(
    param_space_name = "DAISIE_ABC_DI")
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  parameters <- param_space[1,][1:4]

  sim <- get_DAISIE_sim_DI(
    parameters = parameters,
    replicates = 1)
  expect_length(sim, 1)
  expect_length(sim[[1]][[1]], 19)
  expect_equal(sim[[1]][[1]][[1]]$island_age, 5)
  expect_equal(sim[[1]][[1]][[1]]$not_present, 982)
  expect_equal(nrow(sim[[1]][[1]][[1]]$stt_all), 62)
  expect_equal(ncol(sim[[1]][[1]][[1]]$stt_all), 5)
  expect_equal(sim[[1]][[1]][[2]]$branching_times,
               c(5.00, 4.43300848826766,1.67845258616101))
  expect_equal(sim[[1]][[1]][[2]]$stac, 2)
  expect_equal(sim[[1]][[1]][[2]]$missing_species, 0)
})

