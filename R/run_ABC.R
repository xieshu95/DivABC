#' Run ABC
#' @param scenario A string with the parameter space to run. Can
#'  be \code{"DAISIE_ABC"}, \code{"DAISIE_MCMC"},\code{"TraiSIE_ABC"}
#' @param param_set A numeric of a specific line of parameter set in parameter space
#' @param idparsopt A vector of positions of the parameters that be optimized.
#' @param sim_model \code{"DAISIE"}, \code{"TraiSIE"}
#' @param save_output A boolean to determine whether to save or return output.
#' @author Shu Xie
#' @return
#' @export

run_ABC <- function(scenario,
                    param_set,
                    idparsopt,
                    sim_model = "DAISIE",
                    save_output = TRUE,
                    ss_set = 1){

  # param_space <- readr::read_csv2("data/secsse_ABC.csv")
  param_space <- load_scenario(scenario = scenario)
  # param_space <- read.csv2(file = 'data/DAISIE_ABC.csv')
  seed <- as.integer(Sys.time()) %% 1000000L * param_set
  set.seed(param_set)

  message("Param space name: ", scenario)
  message("Running param set: ", param_set)
  message("seed: ", seed)

  check_create_folders(
    scenario = scenario,
    save_output = save_output
  )
  message("sim_model: ", sim_model)
  obs_sim_pars <- param_space[param_set,]
  obs_sim <- load_obs_sim(scenario = scenario)[[param_set]]

  if (sim_model == "DAISIE") {
    sim_function <- get_DAISIE_sim
    calc_ss_function <- calc_ss_diff_daisie
    prior_generating_function <- prior_gen
    prior_density_function <- prior_dens
    fixpars = as.numeric(obs_sim_pars[1:4])

    # init_epsilon_all <- c(150,150,50,50,10,10)
    if(ss_set == 0){ # all
      init_epsilon <- c(150,50,50,20,20,150,50,50)
    } else if (ss_set == 1){  # phylogenetic-- nltt+sd
      init_epsilon <- c(150,30,30,20,20)
    } else if (ss_set == 2){  # tip
      init_epsilon <- c(150,50,50)
    } else if (ss_set == 3){  #nltt
      init_epsilon <- c(100,20,20)
    }

  } else if (sim_model == "TraiSIE") {
    sim_function <- get_TraiSIE_sim
    calc_ss_function <- calc_ss_diff_traisie
    prior_generating_function <- prior_gen_trait
    prior_density_function <- prior_dens_trait
    fixpars = as.numeric(obs_sim_pars[c(1:10)])

    init_epsilon_all <- c(100,20,5,5,10,10,10)
    if(ss_set == 0){
      init_epsilon <- init_epsilon_all
    } else {
      init_epsilon <- init_epsilon_all[-ss_set]
    }
  } else if (sim_model == "secsse") {
    sim_function <- get_secsse_sim
    calc_ss_function <- calc_ss_diff_secsse
    prior_generating_function <- prior_gen_secsse
    prior_density_function <- prior_dens_secsse
    fixpars = as.numeric(obs_sim_pars[1:6])
    # init_epsilon <- calc_epsilon_init_secsse(sim = obs_sim)
    # init_epsilon_all <- c(50,50,10,10,100,100,1,1,1)
    if(ss_set == 0){
      init_epsilon <- c(0.4,2,2)
    } else if (ss_set == 1){
      init_epsilon <- c(0.4,500,500)
    } else if (ss_set == 2){  # D+NLTT
      init_epsilon <- c(0.4) #0.4,0.4
    } else if (ss_set == 3){  # D+NLTT
      init_epsilon <- c(0.4)
    }
  }

  abc <- ABC_SMC(
    obs_data = obs_sim,
    sim_function = sim_function,
    calc_ss_function = calc_ss_function,
    init_epsilon_values = init_epsilon,
    prior_generating_function = prior_generating_function,
    prior_density_function = prior_density_function,
    number_of_particles = 500,
    sigma = 0.2,
    stop_rate = 0.002,
    replicates = 1,
    num_iterations = 20,
    K = as.numeric(obs_sim_pars$K),
    idparsopt = as.numeric(idparsopt),
    fixpars = fixpars,
    ss_set = ss_set
  )
  if (save_output == TRUE) {
    save_output(
      output = abc,
      scenario = scenario,
      param_set = param_set,
      ss_set = ss_set
    )
  } else {
    return(abc)
  }
}

