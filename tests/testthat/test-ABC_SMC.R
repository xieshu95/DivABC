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
  init_epsilon <- c(500,100,100)
  idparsopt = 1:4
  ABC <- ABC_SMC (
    obs_data = obs_sim,
    sim_function <- get_DAISIE_sim_DI,
    calc_ss_function <- calc_ss_diff_daisie,
    init_epsilon_values = init_epsilon,
    prior_generating_function <- prior_gen_DI,
    prior_density_function <- prior_dens_DI,
    number_of_particles = 5,
    sigma = 0.2,
    stop_rate = 0.01,
    num_iterations = 2,
    idparsopt = as.numeric(idparsopt),
    pars = as.numeric(obs_sim_pars[1:4]),
    ss_set = 3
  )
  expect_length(ABC, 6)
  expect_length(ABC$sim_list, 5)
  expect_length(ABC$ABC, 2)
  expect_equal(ABC$n_iter, 2)
  expect_length(ABC$ss_diff_list, 2)
})


test_that("test ABC_SMC_bisse output is correct", {

  param_space <- load_param_space(
    param_space_name = "bisse_ABC_test")
  param_space_name = "bisse_ABC_test"
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  obs_sim_pars <- param_space[1,]
  obs_sim <- load_obs_sim(param_space_name = "bisse_ABC_test")[[1]]
  ss_set = 1
  init_epsilon <- c(1,1,1)
  idparsopt = 1:6
  ABC <- ABC_SMC_bisse (
    obs_data = obs_sim,
    sim_function <- get_bisse_sim,
    calc_ss_function <- calc_ss_diff_bisse,
    prior_generating_function <- prior_gen_bisse,
    prior_density_function <- prior_dens_bisse,
    init_epsilon_values = init_epsilon,
    number_of_particles = 5,
    sigma = 0.2,
    stop_rate = 0.01,
    num_iterations = 2,
    idparsopt = as.numeric(idparsopt),
    pars = as.numeric(obs_sim_pars[1:6]),
    ss_set = ss_set
  )

  expect_length(ABC, 6)
  expect_length(ABC$sim_list, 5)
  expect_length(ABC$ABC, 2)
  expect_equal(ABC$n_iter, 2)
  expect_length(ABC$ss_diff_list, 2)
})
