#' Run mcmc
#'
#' @author Shu Xie
#' @return
#' @export

run_mcmc_DAISIE <- function(param_space_name,
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

  prior_dens <- function(x,idparsopt) {
    if(1 %in% idparsopt){
      dens_lac <- stats::dunif(x[1],0,0.2)
    } else {
      dens_lac <- 1
    }
    if(2 %in% idparsopt){
      dens_mu <- stats::dunif(x[2],0,1)
    } else {
      dens_mu <- 1
    }
    if(3 %in% idparsopt){
      dens_gam <- stats::dunif(x[3],0,0.02)
    } else {
      dens_gam <- 1
    }
    if(4 %in% idparsopt){
      dens_laa <- stats::dunif(x[4],0,1)
    } else {
      dens_laa <- 1
    }
    return(dens_lac * dens_mu * dens_gam * dens_laa)
  }

  mcmc <- mcmc_nltt(obs_sim[[1]][[1]], calc_loglik,
                    parameters = c(obs_sim_pars$lac,
                                   obs_sim_pars$mu,
                                   obs_sim_pars$gam,
                                   obs_sim_pars$laa),
                    logtransforms = c(TRUE,TRUE,TRUE,TRUE),
                    iterations = 100,
                    burnin = 10,
                    thinning = 1,
                    sigma = 1,
                    idparsopt = c(1,3))

  if (save_output == TRUE) {
    save_output(
      output = mcmc,
      param_space_name = param_space_name,
      param_set = param_set,
      rep = rep
    )
  } else {
    return(mcmc)
  }
}
