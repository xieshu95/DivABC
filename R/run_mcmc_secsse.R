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
  set.seed(42)

  message("Param space name: ", param_space_name)
  message("Running param set: ", param_set)
  message("seed: ", seed)

  check_create_folders(
    param_space_name = param_space_name,
    save_output = save_output
  )

  obs_sim_pars <- param_space[param_set,]
  obs_sim <- get_secsse_sim(parameters = as.numeric(obs_sim_pars),
                                       K = Inf,
                                       replicates = 1)

  obs_sim_pars_init <- c(2,2,2,2,2,2)
  mcmc <- MCMC_secsse(datalist = obs_sim[[1]],
                      likelihood_function=calc_log_pp_secsse,
                      parameters = as.numeric(c(obs_sim_pars_init)),
                      iterations = 100, ##100000
                      burnin = 10,
                      thinning = 1,
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


# a <- mcmc_init(idparsopt = idparsopt,obs_sim_pars = obs_sim_pars1)
# obs_sim_pars
# a