#' Run mcmc
#'
#' @author Shu Xie
#' @return
#' @export

run_MCMC_secsse <- function(param_space_name,
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
  # obs_sim_pars_init <- obs_sim_pars + 0.0001
  # test <- readr::parse_number(param_space_name)
  # load(paste0("/home/p286026/TraisieABC/scripts/loglik_test/whole_df_MLE",test,".RData"))
  startingpoint <- DDD::bd_ML(brts = ape::branching.times(obs_sim[[1]]$phy))

  initparsopt <- c(startingpoint$lambda0,startingpoint$lambda0,
                   startingpoint$mu0,startingpoint$mu0,
                   0.1,0.1)
  seed_mcmc <-as.integer(Sys.time()) %% 1000000L * param_set
  set.seed(seed_mcmc)
  message("seed_mcmc: ", seed_mcmc)
  for(n in 1:6){
    initparsopt[n]<-exp(log(initparsopt[n]) +
                          stats::rnorm(1, 0, 0.0001))+ 0.00001
  }
  # initparsopt <- as.numeric(whole_df_MLE[param_set,7:12])
  options(warn=-1)
  mcmc <- MCMC(datalist = obs_sim[[1]],
                      log_lik_function = calc_log_lik_secsse,
                      log_prior_function = calc_log_prior_secsse,
                      parameters = as.numeric(initparsopt),
                      iterations = 100, ##1000,000
                      burnin = 10, #100,000
                      thinning = 1, #100
                      sigma = 0.5,
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


# a <- mcmc_init(idparsopt = idparsopt,obs_sim_pars = obs_sim_pars1)
# obs_sim_pars
# a
