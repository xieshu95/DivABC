#' Run ABC
#' @param param_space_name A string with the parameter space to run. Can
#'  be \code{"DAISIE_ABC"}, \code{"DAISIE_MCMC"},\code{"TraiSIE_ABC"}
#' @param param_set A numeric of a specific line of parameter set in parameter space
#' @param idparsopt A vector of positions of the parameters that be optimized.
#' @param sim_model \code{"DAISIE"}, \code{"TraiSIE"}
#' @param save_output A boolean to determine whether to save or return output.
#' @author Shu Xie
#' @return
#' @export

run_ABC <- function(param_space_name,
                    param_set,
                    idparsopt,
                    sim_model = "DAISIE",
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
  if (sim_model == "DAISIE") {
    obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                             obs_sim_pars$mu,
                                             obs_sim_pars$gam,
                                             obs_sim_pars$laa),
                              K = as.numeric(obs_sim_pars$K),
                              replicates = 30)
    sim_function <- get_DAISIE_sim
    prior_generating_function <- prior_gen
    prior_density_function <- prior_dens
  } else if (sim_model == "TraiSIE") {
    obs_sim <- get_TraiSIE_sim(parameters = as.numeric(c(obs_sim_pars$lac,
                                                         obs_sim_pars$mu,
                                                         obs_sim_pars$gam,
                                                         obs_sim_pars$laa,
                                                         obs_sim_pars$lac2,
                                                         obs_sim_pars$mu2,
                                                         obs_sim_pars$gam2,
                                                         obs_sim_pars$laa2,
                                                         obs_sim_pars$trans,
                                                         obs_sim_pars$trans2)),
                               K = as.numeric(obs_sim_pars$K),
                               replicates = 30)
    sim_function <- get_TraiSIE_sim
    prior_generating_function <- prior_gen_trait
    prior_density_function <- prior_dens_trait
  }

  init_epsilon <- calc_epsilon_init(obs_sim)

  abc <- ABC_SMC_DAISIE (
    obs_data = obs_sim,
    sim_function = sim_function,
    init_epsilon_values = init_epsilon,
    prior_generating_function = prior_generating_function,
    prior_density_function = prior_density_function,
    number_of_particles = 5, #2000
    sigma = 1,
    stop_rate = 0.001,
    replicates = 1,  ## simulation replicates for each parameter set
    num_iterations = 2,
    K = as.numeric(obs_sim_pars$K),
    idparsopt = as.numeric(idparsopt),
    fixpars = as.numeric(obs_sim_pars[2:5])
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

