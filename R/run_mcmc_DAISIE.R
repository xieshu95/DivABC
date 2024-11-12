#' Run mcmc
#'
#' @author Shu Xie
#' @return
#' @export

run_MCMC_DAISIE <- function(param_space_name,
                            param_set,
                            idparsopt,
                            save_output = TRUE){

  param_space <- load_param_space(param_space_name = param_space_name)
  seed <- param_set
  set.seed(seed)

  message("Param space name: ", param_space_name)
  message("Running param set: ", param_set)
  message("seed: ", seed)

  check_create_folders(
    param_space_name = param_space_name,
    save_output = save_output
  )

  obs_sim_pars <- param_space[param_set,]
  obs_sim <- load_obs_sim(param_space_name = param_space_name)[[param_set]]

  initparsopt <- as.numeric(obs_sim_pars[c(1,2,3,4)])
  seed_mcmc <-as.integer(Sys.time()) %% 1000000L * sample(1:10,1)
  set.seed(seed_mcmc)
  message("seed_mcmc: ", seed_mcmc)
  for(n in 1:4){
    initparsopt[n]<-exp(log(initparsopt[n]) +
                          stats::rnorm(1, 0, 0.01))+ 0.000001
  }
  message("initial pars:", initparsopt)
  mcmc <- MCMC(datalist = obs_sim[[1]][[1]],
               log_lik_function = calc_log_lik_DAISIE_DD,
               log_prior_function = calc_log_prior_DAISIE_DD,
               parameters = as.numeric(initparsopt),
               iterations = 1000000,
               burnin = 100000,
               thinning = 200, #200
               sigma = 0.2,
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
