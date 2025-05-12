#' Run mcmc
#'
#' @return
#' @export

run_MCMC_geosse <- function(param_space_name,
                           param_set,
                           idparsopt,
                           save_output = TRUE){

  param_space <- load_param_space(param_space_name = param_space_name)
  seed <- param_set
  set.seed(param_set)

  message("Param space name: ", param_space_name)
  message("Running param set: ", param_set)
  message("seed: ", seed)

  check_create_folders(
    param_space_name = param_space_name,
    save_output = save_output
  )

  obs_sim_pars <- param_space[param_set,]
  obs_sim <- load_obs_sim(param_space_name = param_space_name)[[param_set]]
  initparsopt <- rep(0.5,7)
  seed_mcmc <-as.integer(Sys.time()) %% 1000000L * param_set
  set.seed(seed_mcmc)
  message("seed_mcmc: ", seed_mcmc)

  mcmc <- MCMC(datalist = obs_sim[[1]],
               log_lik_function = calc_log_lik_geosse,
               log_prior_function = calc_log_prior_geosse,
               logform = FALSE,
               parameters = as.numeric(initparsopt),
               iterations = 1000000,
               burnin = 100000,
               thinning = 100,
               sigma = 0.01,
               idparsopt = idparsopt)

  if (save_output == TRUE) {
    save_output(
      output = mcmc,
      param_space_name = param_space_name,
      param_set = param_set,
      ss_set = 1
    )
  } else {
    return(mcmc)
  }
}



