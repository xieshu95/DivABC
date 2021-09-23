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
  rep <- as.numeric(param_space[param_set,1])
  set.seed(rep)

  message("Param space name: ", param_space_name)
  message("Running param set: ", param_set)
  message("seed: ", rep)

  check_create_folders(
    param_space_name = param_space_name,
    save_output = save_output
  )

  obs_sim_pars <- param_space[param_set,]
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = as.numeric(obs_sim_pars$K),
                            replicates = 1)

  mcmc <- MCMC_DAISIE(obs_sim[[1]][[1]], calc_loglik,
                      parameters = as.numeric(c(obs_sim_pars$lac,
                                                obs_sim_pars$mu,
                                                obs_sim_pars$gam,
                                                obs_sim_pars$laa)),
                      iterations = 200,
                      burnin = 10,
                      thinning = 1,
                      sigma = 1,
                      idparsopt = idparsopt)

  if (save_output == TRUE) {
    save_output(
      output = mcmc,
      param_space_name = param_space_name,
      param_set = param_set
    )
  } else {
    return(mcmc)
  }
}
