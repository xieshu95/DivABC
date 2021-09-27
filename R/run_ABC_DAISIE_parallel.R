#' Run ABC in parallel
#'
#' @author Tianjian Qin, Shu Xie
#' @return
#' @export

run_ABC_DAISIE_parallel <- function(param_space_name,
                           idparsopt,
                           save_output = TRUE,
                           strategy = future::multisession,
                           workers = NULL){

  obs_sim_pars <- load_param_space(param_space_name = param_space_name)
  rep <- as.numeric(param_space[param_set,1])
  set.seed(rep)

  message("Param space name: ", param_space_name)
  message("Running param set: ", param_set)
  message("seed: ", rep)

  check_create_folders(
    param_space_name = param_space_name,
    save_output = save_output
  )

  if (is.null(workers)) {
    stop("Number of workers not set")
  }
  message(paste0(
    "Running multisession simulation with ",
    workers,
    " workers"
  ))
  future::plan(strategy, workers = workers)
  future_opts <- furrr::furrr_options(seed = TRUE)
  furrr::future_map(
    .x = obs_sim_pars,
    .f = TraisieABC::run_ABC_DAISIE_wrapper,
    .options = future_opts,
    idparsopt = idparsopt
  )

  if (save_output == TRUE) {
    save_output(
      output = abc,
      param_space_name = param_space_name,
      param_set = param_set
    )
  } else {
    return(abc)
  }
}

run_ABC_DAISIE_wrapper <- function(obs_sim_pars, idparsopt) {
  obs_sim <- get_DAISIE_sim(
    parameters = c(
      obs_sim_pars$lac,
      obs_sim_pars$mu,
      obs_sim_pars$gam,
      obs_sim_pars$laa
    ),
    K = as.numeric(obs_sim_pars$K),
    replicates = 50
  )
  init_epsilon <- calc_epsilon_init(obs_sim)

  abc <- ABC_SMC_DAISIE (
    obs_data = obs_sim,
    sim_function = get_DAISIE_sim,
    init_epsilon_values = init_epsilon,
    prior_generating_function = prior_gen,
    prior_density_function = prior_dens,
    number_of_particles = 5,
    sigma = 0.2,
    stop_rate = 0.0005,
    replicates = 1,
    ## simulation replicates for each parameter set
    num_iterations = 1,
    K = as.numeric(obs_sim_pars$K),
    idparsopt = as.numeric(idparsopt),
    fixpars = as.numeric(obs_sim_pars[2:5])
  )
}