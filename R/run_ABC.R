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
                    save_output = TRUE,
                    ss_set = 1){

  # param_space <- readr::read_csv2("data/secsse_ABC.csv")
  param_space <- load_param_space(param_space_name = param_space_name)
  # param_space <- read.csv2(file = 'data/DAISIE_ABC.csv')
  seed <- param_set ##as.integer(Sys.time()) %% 1000000L * param_set
  set.seed(param_set)

  message("Param space name: ", param_space_name)
  message("Running param set: ", param_set)
  message("seed: ", seed)

  check_create_folders(
    param_space_name = param_space_name,
    save_output = save_output
  )
  message("sim_model: ", sim_model)
  obs_sim_pars <- param_space[param_set,]
  obs_sim <- load_obs_sim(sim_model)[[param_set]]
  if (sim_model == "DAISIE") {
    # obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
    #                                          obs_sim_pars$mu,
    #                                          obs_sim_pars$gam,
    #                                          obs_sim_pars$laa),
    #                           K = as.numeric(obs_sim_pars$K),
    #                           replicates = 1)  ## replicates = 30
    sim_function <- get_DAISIE_sim
    calc_ss_function <- calc_ss_diff_daisie
    prior_generating_function <- prior_gen
    prior_density_function <- prior_dens
    fixpars = as.numeric(obs_sim_pars[1:4])

    # init_epsilon_all <- c(150,150,50,50,10,10)
    if(ss_set == 0){
      init_epsilon <- c(150,50,50,10,10,100,50,50)
    } else if (ss_set == 1){
      init_epsilon <- c(150,50,50,10)
    } else if (ss_set == 2){
      init_epsilon <- c(100,50,50,10)
    }
  } else if (sim_model == "TraiSIE") {
    # obs_sim <- get_TraiSIE_sim(parameters = as.numeric(c(obs_sim_pars$lac,
    #                                                      obs_sim_pars$mu,
    #                                                      obs_sim_pars$gam,
    #                                                      obs_sim_pars$laa,
    #                                                      obs_sim_pars$lac2,
    #                                                      obs_sim_pars$mu2,
    #                                                      obs_sim_pars$gam2,
    #                                                      obs_sim_pars$laa2,
    #                                                      obs_sim_pars$trans,
    #                                                      obs_sim_pars$trans2)),
    #                            K = as.numeric(obs_sim_pars$K),
    #                            replicates = 1) ## replicates = 30
    sim_function <- get_TraiSIE_sim
    calc_ss_function <- calc_ss_diff_traisie
    prior_generating_function <- prior_gen_trait
    prior_density_function <- prior_dens_trait
    fixpars = as.numeric(obs_sim_pars[c(1:10)])

    init_epsilon_all <- c(100,20,5,5,10,10,10)
    if(ss_set == 0 || ss_set > 10){
      init_epsilon <- init_epsilon_all
    } else {
      init_epsilon <- init_epsilon_all[-ss_set]
    }
  } else if (sim_model == "secsse") {
    # obs_sim <- get_secsse_sim_create_obs(
    #   parameters = as.numeric(obs_sim_pars),
    #   K = Inf,
    #   replicates = 1) ## replicates = 30
    sim_function <- get_secsse_sim
    calc_ss_function <- calc_ss_diff_secsse
    prior_generating_function <- prior_gen_secsse
    prior_density_function <- prior_dens_secsse
    fixpars = as.numeric(obs_sim_pars[1:6])
    # init_epsilon <- calc_epsilon_init_secsse(sim = obs_sim)
    init_epsilon_all <- c(50,50,10,10,2,100,100,1,1,1)
    if(ss_set == 0){
      init_epsilon <- init_epsilon_all
    } else if(ss_set > 10){
      init_epsilon <- init_epsilon_all
    } else {
      init_epsilon <- init_epsilon_all[-ss_set]
    }
    obs_sim_pars$K <- Inf
  }

  seed_abc <-as.integer(Sys.time()) %% 1000000L * param_set
  set.seed(seed_abc)
  message("seed-abc: ", seed_abc)
  abc <- ABC_SMC (
    obs_data = obs_sim,
    sim_function = sim_function,
    calc_ss_function = calc_ss_function,
    init_epsilon_values = init_epsilon,
    prior_generating_function = prior_generating_function,
    prior_density_function = prior_density_function,
    number_of_particles = 400, #1000
    sigma = 0.2,
    stop_rate = 0.01,
    replicates = 1,  ## simulation replicates for each parameter set
    num_iterations = 10, #10
    K = as.numeric(obs_sim_pars$K),
    idparsopt = as.numeric(idparsopt),
    fixpars = fixpars,
    ss_set = ss_set
  )
  if (save_output == TRUE) {
    save_output(
      output = abc,
      param_space_name = param_space_name,
      param_set = param_set,
      ss_set = ss_set
    )
  } else {
    return(abc)
  }
}

