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

  sim <- get_DAISIE_sim(
    parameters = parameters,
    K = Inf,
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

test_that("test run_novel_sim output is correct for oceanic_sea_level", {
  skip_if(Sys.getenv("CI") == "", message = "Run only on CI")

  param_space <- load_param_space(
    param_space_name = "oceanic_sea_level_cs")
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  sim_pars <- extract_param_set(
    param_space_name = "oceanic_sea_level_cs",
    param_space = param_space,
    param_set = 1)
  novel_sim <- run_novel_sim(
    param_space_name = "oceanic_sea_level_cs",
    sim_pars = sim_pars)
  expect_length(novel_sim, 1)
  expect_length(novel_sim[[1]], 38)
  expect_equal(novel_sim[[1]][[1]]$island_age, 2.55)
  expect_equal(novel_sim[[1]][[1]]$not_present, 963)
  expect_equal(nrow(novel_sim[[1]][[1]]$stt_all), 115)
  expect_equal(ncol(novel_sim[[1]][[1]]$stt_all), 5)
  expect_equal(novel_sim[[1]][[2]]$branching_times,
               c(2.55, 0.78784699357233))
  expect_equal(novel_sim[[1]][[2]]$stac, 4)
  expect_equal(novel_sim[[1]][[2]]$missing_species, 0)
})

