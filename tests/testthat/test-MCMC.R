test_that("test MCMC output is correct", {

  param_space <- load_param_space(
    param_space_name = "DAISIE_ABC_DI")
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  obs_sim_pars <- param_space[1,]
  obs_sim <- load_obs_sim(param_space_name = "DAISIE_ABC_DI")[[1]]
  initparsopt <- as.numeric(obs_sim_pars[c(1,2,3,4)])
  for(n in 1:4){
    initparsopt[n]<-exp(log(initparsopt[n]) +
                          stats::rnorm(1, 0, 0.01))+ 0.000001
  }

  mcmc <- MCMC(datalist = obs_sim[[1]][[1]],
               log_lik_function = calc_log_lik_DAISIE,
               log_prior_function = calc_log_prior_DAISIE,
               parameters = as.numeric(initparsopt),
               iterations = 100,
               burnin = 10,
               thinning = 1, #200
               sigma = 0.2,
               idparsopt = 1:4)
  expect_length(mcmc, 606)
  expect_length(mcmc[1,], 6)
  expect_length(mcmc[,1], 101)
  expect_equal(mcmc[1,], c(3.634345219618640e-01, 4.166803457684400e-07, 3.697587390778852e-03, 8.772137813769644e-02, -1.128673973092209e+02, -1.953706189199297e+01))
})


