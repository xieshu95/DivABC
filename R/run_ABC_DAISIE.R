#' Run ABC
#'
#' @author Shu Xie
#' @return
#' @export

run_ABC_DAISIE <- function(param_space_name,
                           param_set,
                           idparsopt,
                           save_output = TRUE){

  param_space <- load_param_space(param_space_name = param_space_name)
  rep <- as.numeric(param_space[param_set,1])
  set.seed(rep)

  message(Sys.time())
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
                            replicates = 5)
  init_epsilon <- calc_epsilon_init(obs_sim)

  prior_gen <- function(idparsopt){
    if(1 %in% idparsopt){
      lac <- stats::runif(1,0,0.2)
    } else {
      lac <- 0.2
    }
    if(2 %in% idparsopt){
      mu <- stats::runif(1,0,1)
    } else {
      mu <- 0.2
    }
    if(3 %in% idparsopt){
      gam <- stats::runif(1,0,0.02)
    } else {
      gam <- 0.01
    }
    if(4 %in% idparsopt){
      laa <- stats::runif(1,0,1)
    } else {
      laa <- 0.2
    }
    return(c(lac,mu,gam,laa))
  }

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

  abc <- ABC_SMC_DAISIE (
    obs_data = obs_sim,
    sim_function = get_DAISIE_sim,
    init_epsilon_values = init_epsilon,
    prior_generating_function = prior_gen,
    prior_density_function = prior_dens,
    number_of_particles = 2000,
    sigma = 0.5,
    stop_rate = 0.002,
    replicates = 1,  ## simulation replicates for each parameter set
    num_iterations = 10,
    K = as.numeric(obs_sim_pars$K),
    idparsopt = idparsopt
  )
  if (save_output == TRUE) {
    save_output(
      output = abc,
      param_space_name = param_space_name,
      param_set = param_set,
      rep = rep
    )
  } else {
    return(abc)
  }
}

