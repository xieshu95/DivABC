test_that("test ABC_SMC output is correct", {

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
  ss_set = 3
  init_epsilon <- c(100,20,20)
  idparsopt = 1:4
  ABC <- ABC_SMC (
    obs_data = obs_sim,
    sim_function <- get_DAISIE_sim,
    calc_ss_function <- calc_ss_diff_daisie,
    init_epsilon_values = init_epsilon,
    prior_generating_function <- prior_gen,
    prior_density_function <- prior_dens,
    number_of_particles = 5,
    sigma = 0.2,
    stop_rate = 0.01,
    num_iterations = 3,
    idparsopt = as.numeric(idparsopt),
    pars = as.numeric(obs_sim_pars[1:4]),
    ss_set = 3
  )
  expect_length(ABC, 6)
  expect_length(ABC$sim_list, 5)
  expect_length(ABC$ABC, 3)
  expect_equal(ABC$n_iter, 3)
  expect_length(ABC$ss_diff_list, 3)
})


test_that("test ABC_SMC_secsse output is correct", {

  param_space <- load_param_space(
    param_space_name = "secsse_ABC_test")
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  obs_sim_pars <- param_space[1,]
  obs_sim <- load_obs_sim(param_space_name = "secsse_ABC_test")[[1]]
  ss_set = 1
  init_epsilon <- c(1,1,1)
  idparsopt = 1:6
  ABC <- ABC_SMC_secsse (
    obs_data = obs_sim,
    sim_function <- get_secsse_sim,
    calc_ss_function <- calc_ss_diff_secsse,
    prior_generating_function <- prior_gen_secsse,
    prior_density_function <- prior_dens_secsse,
    init_epsilon_values = init_epsilon,
    number_of_particles = 5,
    sigma = 0.2,
    stop_rate = 0.01,
    num_iterations = 3,
    idparsopt = as.numeric(idparsopt),
    fixpars = as.numeric(obs_sim_pars[1:6]),
    ss_set = ss_set
  )
  expect_length(ABC, 8)
  expect_length(ABC$sim_list, 5)
  expect_length(ABC$ABC, 3)
  expect_equal(ABC$n_iter, 3)
  expect_length(ABC$ss_diff_list, 3)
})
