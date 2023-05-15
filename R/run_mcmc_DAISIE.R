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

  # obs_sim_pars_init <- obs_sim_pars + 0.0001
  initparsopt <- as.numeric(obs_sim_pars[c(1,2,3,4)]) + 0.000001
  seed_mcmc <-as.integer(Sys.time()) %% 1000000L * sample(1:10,1)
  set.seed(seed_mcmc)
  message("seed_mcmc: ", seed_mcmc)
  # for(n in 1:4){
  #   initparsopt[n]<-exp(log(initparsopt[n]) +
  #                         stats::rnorm(1, 0, 0.01))
  # }
  message("initial pars:", initparsopt)
  mcmc <- MCMC(datalist = obs_sim[[1]][[1]],
               log_lik_function = calc_log_lik_DAISIE,
               log_prior_function = calc_log_prior_DAISIE,
               parameters = as.numeric(initparsopt),
               iterations = 500000, ##500000
               burnin = 100000,   # 100000
               thinning = 100, #100
               sigma = 0.05,
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



### function to decide the initial values for the parameters
### should test if idparopt only contains 4 rates in MCMC
#' Run mcmc
#'
#' @author Shu Xie
#' @return
#' @export
# idparsopt <- c(1,3,4)
# obs_sim_pars1 <- obs_sim_pars
mcmc_init <- function(idparsopt, obs_sim_pars){
  for (m in idparsopt){
    if(m == 3){
      obs_sim_pars[m + 1] <- 0.05
    } else {
      obs_sim_pars[m + 1] <- 2
    }
  }
  return(obs_sim_pars)
}

# a <- mcmc_init(idparsopt = idparsopt,obs_sim_pars = obs_sim_pars1)
# obs_sim_pars
# a
