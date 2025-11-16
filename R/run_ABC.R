#' Run ABC
#' @param param_space_name A string with the parameter space to run. Can
#'  be \code{"DAISIE_ABC"}, \code{"DAISIE_MCMC"},\code{"TraiSIE_ABC"}
#' @param param_set A numeric of a specific line of parameter set in parameter space
#' @param idparsopt A vector of positions of the parameters that be optimized.
#' @param sim_model \code{"DAISIE"}, \code{"TraiSIE"}
#' @param save_output A boolean to determine whether to save or return output.
#' @param ss_set A numeric of which summary statistic set to be used.
#' @author Shu Xie
#' @return
#' @export

run_ABC <- function(param_space_name,
                    param_set,
                    idparsopt,
                    sim_model,
                    save_output = TRUE,
                    ss_set = 1){

  # param_space <- readr::read_csv2("data/bisse_ABC.csv")
  param_space <- load_param_space(param_space_name = param_space_name)
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

  if (sim_model == "DAISIE_DI") {
    if(ss_set == 0){ # all
      init_epsilon <- c(800,200,200,200,200,800,200,200)
    } else if (ss_set == 1){  # phylogenetic-- nltt+sd
      init_epsilon <- c(800,200,200,200,200)
    } else if (ss_set == 2){  # tip
      init_epsilon <- c(800,200,200)
    } else if (ss_set == 3){  #nltt
      init_epsilon <- c(800,200,200)
    }

    abc <- ABC_SMC (
      obs_data = obs_sim,
      sim_function <- get_DAISIE_sim_DI,
      calc_ss_function <- calc_ss_diff_daisie,
      init_epsilon_values = init_epsilon,
      prior_generating_function <- prior_gen_DI,
      prior_density_function <- prior_dens_DI,
      number_of_particles = 500,
      sigma = 0.2,
      stop_rate = 0.0005,
      num_iterations = 20,
      idparsopt = as.numeric(idparsopt),
      pars = as.numeric(obs_sim_pars[1:4]),
      ss_set = ss_set
    )

  } else if (sim_model == "DAISIE_DD") {

    if(ss_set == 0){ # all
      init_epsilon <- c(800,200,200,200,200,800,200,200)
    } else if (ss_set == 1){  # phylogenetic-- nltt+sd
      init_epsilon <- c(800,200,200,200,200)
    } else if (ss_set == 2){  # tip
      init_epsilon <- c(800,200,200)
    } else if (ss_set == 3){  #nltt
      init_epsilon <- c(800,200,200)
    }

    abc <- ABC_SMC (
      obs_data = obs_sim,
      sim_function <- get_DAISIE_sim_DD,
      calc_ss_function <- calc_ss_diff_daisie,
      init_epsilon_values = init_epsilon,
      prior_generating_function <- prior_gen_DD,
      prior_density_function <- prior_dens_DD,
      number_of_particles = 500,
      sigma = 0.2,
      stop_rate = 0.002,
      num_iterations = 20,
      idparsopt = as.numeric(idparsopt),
      pars = as.numeric(obs_sim_pars[1:5]),
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
      pars = as.numeric(obs_sim_pars[c(1:10)]),
      ss_set = ss_set
    )

  } else if (sim_model == "bisse") {
    if(ss_set == 0){ # nltt + nltt1 + nltt2 + D
      init_epsilon <- c(1,1,1,1)
    } else if (ss_set == 1){ # nltt + nltt1 + nltt2
      init_epsilon <- c(1,1,1)
    } else if (ss_set == 2){  # nltt + D
      init_epsilon <- c(1,1)
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
    } else if (ss_set == 8){  # nltt + nltt1 + nltt2 + D + num1 + num2
      init_epsilon <- c(1,1,1,1,200,200)
    } else if (ss_set == 9){  # nltt
      init_epsilon <- c(1)
    } else if (ss_set == 10){  # mntd1 + mntd2 + nltt
      init_epsilon <- c(5,5,1)
    } else if (ss_set == 11){  # colless1 + colless2 + nltt
      init_epsilon <- c(100,100,1)
    } else if (ss_set == 12){  # tip-ratio + nltt
      init_epsilon <- c(1,1)
    } else if (ss_set == 13){  # nltt + nltt1 + nltt2 + Delta
      init_epsilon <- c(1,1,1,2)
    } else if (ss_set == 14){  # nltt + nltt1 + nltt2 + M
      init_epsilon <- c(1,1,1,0.5)
    } else if (ss_set == 15){  # nltt + nltt1 + nltt2 + MNTD1 + MNTD2 + D
      init_epsilon <- c(1,1,1,5,5,1)
    } else if (ss_set == 16){  # nltt + MNTD1 + MNTD2 + D
      init_epsilon <- c(1,5,5,1)
    }
    abc <- ABC_SMC_bisse (
      obs_data = obs_sim,
      sim_function <- get_bisse_sim,
      calc_ss_function <- calc_ss_diff_bisse,
      prior_generating_function <- prior_gen_bisse,
      prior_density_function <- prior_dens_bisse,
      init_epsilon_values = init_epsilon,
      number_of_particles = 5,
      sigma = 0.2,
      stop_rate = 0.001,
      num_iterations = 2,
      idparsopt = as.numeric(idparsopt),
      pars = as.numeric(obs_sim_pars[1:6]),
      ss_set = ss_set
    )
  } else if (sim_model == "musse") {
    if(ss_set == 0){ # nltt + nltt1 + nltt2 + nltt3 + D12+D23+D13
      init_epsilon <- c(1,1,1,1,1,1,1)
    } else if (ss_set == 1){ # nltt + nltt1 + nltt2 + nltt3 + M
      init_epsilon <- c(1,1,1,1,1)
    } else if (ss_set == 2){ # nltt + nltt1 + nltt2 + nltt3 + D1-23 + D2-13+D3-12
      init_epsilon <- c(1,1,1,1,1,1,1)
    } else if (ss_set == 3){ # nltt + nltt1 + nltt2 + nltt3 + D1-23 + D2-13+D3-12
      init_epsilon <- c(1,1,1,1,1,1,1,1,1,1,1)
    }

    abc <- ABC_SMC_musse (
      obs_data = obs_sim,
      sim_function <- get_musse_sim,
      calc_ss_function <- calc_ss_diff_musse,
      prior_generating_function <- prior_gen_musse,
      prior_density_function <- prior_dens_musse,
      init_epsilon_values = init_epsilon,
      number_of_particles = 300,
      sigma = 0.2,
      stop_rate = 0.001,
      num_iterations = 20,
      idparsopt = as.numeric(idparsopt),
      pars = as.numeric(obs_sim_pars[1:7]),
      ss_set = ss_set
    )
  } else if (sim_model == "geosse") {
    if(ss_set == 0){ # nltt + nltt1 + nltt2 + nltt3 + D12+D23+D13
      init_epsilon <- c(1,1,1,1,1,1,1)
    } else if (ss_set == 1){ # nltt + nltt1 + nltt2 + nltt3 + M
      init_epsilon <- c(1,1,1,1,1)
    } else if (ss_set == 2){ # nltt + nltt1 + nltt2 + nltt3 + D1-23 + D2-13+ D3-12
      init_epsilon <- c(1,1,1,1,1,1,1)
    } else if (ss_set == 3){ # nltt + nltt1 + nltt2 + nltt3 + D12 + D23 + D13 + M
      init_epsilon <- c(1,1,1,1,1,1,1,1)
    } else if (ss_set == 4){ # nltt + nltt1 + nltt2 + nltt3 + D1-23 + D2-13 + D3-12 + M
      init_epsilon <- c(1,1,1,1,1,1,1,1)
    } else if (ss_set == 5){ # nltt + nltt1 + nltt2 + nltt3 + D1-23 + D2-13 + D3-12
      init_epsilon <- c(1,1,1,1,1,1,1,1,1,1,1)
    }

    abc <- ABC_SMC_geosse (
      obs_data = obs_sim,
      sim_function <- get_geosse_sim,
      calc_ss_function <- calc_ss_diff_geosse,
      prior_generating_function <- prior_gen_geosse,
      prior_density_function <- prior_dens_geosse,
      init_epsilon_values = init_epsilon,
      number_of_particles = 300,
      sigma = 0.2,
      stop_rate = 0.001,
      num_iterations = 20,
      idparsopt = as.numeric(idparsopt),
      pars = as.numeric(obs_sim_pars[1:7]),
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
