test_that("test calc_loglik_prior output is correct with DAISIE model", {

  datalist <- load_obs_sim(param_space_name = "DAISIE_ABC_DI")[[1]][[1]][[1]]
  params <- c(0.4,0.1,0.003,0.1)
  loglike <- calc_log_lik_DAISIE(params = params, datalist = datalist)

  expect_length(loglike, 1)
  expect_equal(loglike, -111.6417957812863)
})


test_that("test calc_loglik_prior output is correct with SecSSE model", {

  datalist <- load_obs_sim(param_space_name = "secsse_ABC_test")[[1]][[1]]
  params <- c(0.3,0.3,0.05,0.05,0.05,0.05)
  loglike <- calc_log_lik_secsse(params = params, datalist = datalist)

  expect_length(loglike, 1)
  expect_equal(loglike, -141.5576266553215)
})
