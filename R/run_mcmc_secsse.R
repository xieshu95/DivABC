#' Run mcmc
#'
#' @author Shu Xie
#' @return
#' @export

run_MCMC_secsse <- function(scenario,
                            param_set,
                            idparsopt,
                            save_output = TRUE){

  param_space <- load_scenario(scenario = scenario)
  seed <- param_set
  set.seed(param_set)

  message("Param space name: ", scenario)
  message("Running param set: ", param_set)
  message("seed: ", seed)

  check_create_folders(
    scenario = scenario,
    save_output = save_output
  )

  obs_sim_pars <- param_space[param_set,]
  obs_sim <- load_obs_sim(param_space_name = param_space_name)[[param_set]]
  initparsopt <- obs_sim_pars
  seed_mcmc <-as.integer(Sys.time()) %% 1000000L * param_set
  set.seed(seed_mcmc)
  message("seed_mcmc: ", seed_mcmc)
  for(n in 1:6){
    initparsopt[n]<-exp(log(initparsopt[n]) +
                          stats::rnorm(1, 0, 0.0001))+ 0.00001
  }

  mcmc <- MCMC(datalist = obs_sim[[1]],
                      log_lik_function = calc_log_lik_secsse,
                      log_prior_function = calc_log_prior_secsse,
                      parameters = as.numeric(initparsopt),
                      iterations = 1000000, ##1000,000
                      burnin = 100000, #100,000
                      thinning = 100, #100
                      sigma = 0.5,
                      idparsopt = idparsopt)

  if (save_output == TRUE) {
    save_output(
      output = mcmc,
      scenario = scenario,
      param_set = param_set,
      ss_set = 1
    )
  } else {
    return(mcmc)
  }
}

