#' Run ABC_TRAISIE
#'
#' @author Shu Xie
#' @return
#' @export

run_ABC_TraiSIE <- function(param_space_name,
                            param_set,
                            idparsopt,
                            save_output = TRUE){

  param_space <- read.csv2(file = 'data/trait_DAISIE_single_change.csv')
  seed <- param_set
  set.seed(seed)

  obs_sim_pars <- param_space[param_set,]
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
  init_epsilon <- calc_epsilon_init(obs_sim)

  abc <- ABC_SMC_DAISIE (
    obs_data = obs_sim,
    sim_function = get_TraiSIE_sim,
    init_epsilon_values = init_epsilon,
    prior_generating_function = prior_gen_trait,
    prior_density_function = prior_dens_trait,
    number_of_particles = 5,
    sigma = 1,
    stop_rate = 0.001,
    replicates = 1,  ## simulation replicates for each parameter set
    num_iterations = 2,
    K = as.numeric(obs_sim_pars$K),
    idparsopt = as.numeric(idparsopt),
    fixpars = as.numeric(obs_sim_pars[c(2:5,7:12)]) ### !!change the positio of K in pars space
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

