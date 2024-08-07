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
  seed <- as.integer(Sys.time()) %% 1000000L * param_set
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
  obs_sim <- load_obs_sim(param_space_name = param_space_name)[[param_set]]

  if (sim_model == "DAISIE") {
    if(ss_set == 0){ # all
      init_epsilon <- c(150,50,50,20,20,150,50,50)
    } else if (ss_set == 1){  # phylogenetic-- nltt+sd
      init_epsilon <- c(150,30,30,20,20)
    } else if (ss_set == 2){  # tip
      init_epsilon <- c(150,50,50)
    } else if (ss_set == 3){  #nltt
      init_epsilon <- c(100,20,20)
    }

    abc <- ABC_SMC (
      obs_data = obs_sim,
      sim_function <- get_DAISIE_sim,
      calc_ss_function <- calc_ss_diff_daisie,
      init_epsilon_values = init_epsilon,
      prior_generating_function <- prior_gen,
      prior_density_function <- prior_dens,
      number_of_particles = 500,
      sigma = 0.2,
      stop_rate = 0.001,
      num_iterations = 20,
      idparsopt = as.numeric(idparsopt),
      pars = as.numeric(obs_sim_pars[1:4]),
      ss_set = ss_set
    )

  } else if (sim_model == "TraiSIE") {

    init_epsilon_all <- c(100,20,5,5,10,10,10)
    if(ss_set == 0){
      init_epsilon <- init_epsilon_all
    } else {
      init_epsilon <- init_epsilon_all[-ss_set]
    }
    abc <- ABC_SMC (
      obs_data = obs_sim,
      sim_function <- get_TraiSIE_sim,
      calc_ss_function <- calc_ss_diff_traisie,
      prior_generating_function <- prior_gen_trait,
      prior_density_function <- prior_dens_trait,
      init_epsilon_values = init_epsilon,
      number_of_particles = 500,
      sigma = 0.1,
      stop_rate = 0.002,
      num_iterations = 15,
      idparsopt = as.numeric(idparsopt),
      fixpars = as.numeric(obs_sim_pars[c(1:10)]),
      ss_set = ss_set
    )

  } else if (sim_model == "secsse") {
    if(ss_set == 0){ # nltt + nltt1 + nltt2 + D
      init_epsilon <- c(1,1,1,1)
    } else if (ss_set == 1){ # nltt + nltt1 + nltt2
      init_epsilon <- c(1,1,1)
    } else if (ss_set == 2){  # nltt + D
      init_epsilon <- c(1,1) #0.4,0.4
    } else if (ss_set == 3){  # D
      init_epsilon <- c(1)
    } else if (ss_set == 4){  # mpd1 + mpd2 + D
      init_epsilon <- c(5,5,1)
    } else if (ss_set == 5){  # mntd1 + mntd2 + D
      init_epsilon <- c(5,5,1)
    } else if (ss_set == 6){  # colless1 + colless2 + D
      init_epsilon <- c(100,100,1)
    } else if (ss_set == 7){  # mpd1 + mpd2 + nltt
      init_epsilon <- c(5,5,1)
    }
    abc <- ABC_SMC_secsse (
      obs_data = obs_sim,
      sim_function <- get_secsse_sim,
      calc_ss_function <- calc_ss_diff_secsse,
      prior_generating_function <- prior_gen_secsse,
      prior_density_function <- prior_dens_secsse,
      init_epsilon_values = init_epsilon,
      number_of_particles = 500,
      sigma = 0.2,
      stop_rate = 0.001,
      num_iterations = 20,
      idparsopt = as.numeric(idparsopt),
      fixpars = as.numeric(obs_sim_pars[1:6]),
      ss_set = ss_set
    )
  }

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