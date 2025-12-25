test_that("test calc_loglik_prior output is correct with DAISIE model", {

  datalist <- load_obs_sim(param_space_name = "DAISIE_ABC_DI")[[1]][[1]][[1]]
  params <- c(0.4,0.1,0.003,0.1)
  loglik <- calc_log_lik_DAISIE_DI(params = params, datalist = datalist)

  expect_length(loglik, 1)
  expect_equal(loglik, -111.641793456707)
})


test_that("test calc_loglik_prior output is correct with SecSSE model", {

  datalist <- load_obs_sim(param_space_name = "bisse_ABC_test")[[1]][[1]]
  params <- c(0.3,0.3,0.05,0.05,0.05,0.05)
  loglik <- calc_log_lik_bisse(params = params, datalist = datalist)

  expect_length(loglik, 1)
  expect_equal(loglik, -141.5576266553215)
})
