#' Simulation function to create DAISIE simulations
#'
#' @param parameters A vector for CES rates.
#' @param K Carrying capacity, Inf for diverdity-independent models.
#' @param replicates The number of replicates(islands) for DAISIE simulation.
#'
#' @return A list contains simulated islands
#' @export


get_DAISIE_sim_DI <- function(parameters,
                              replicates = 1){
  sim <- list()
  for (j in seq_len(replicates)) {
    sim[[j]] <- DAISIE::DAISIE_sim_cr(
      time = 5,
      M = 1000,
      pars = as.numeric(c(parameters[1],parameters[2],Inf,parameters[3],parameters[4])),
      replicates = 1,
      nonoceanic_pars = c(0, 0),
      sample_freq  = Inf,
      plot_sims = FALSE,
      verbose = FALSE,
      cond = 1
    )
  }
  return(sim)
}

get_DAISIE_sim_DD <- function(parameters,
                              replicates = 1){
  sim <- list()
  for (j in seq_len(replicates)) {
    save <- 0
    while(save < 1){
      skip <- FALSE
      tryCatch(sim[[j]] <- DAISIE::DAISIE_sim_cr(
        time = 5,
        M = 1000,
        pars = as.numeric(c(parameters[1],parameters[2],parameters[5],parameters[3],parameters[4])),
        replicates = 1,
        nonoceanic_pars = c(0, 0),
        sample_freq  = Inf,
        plot_sims = FALSE,
        verbose = FALSE,
        cond = 1
      ), error=function(e) {
        # print("Error: undefined columns selected")
        skip <<- TRUE
      })
      if(skip == FALSE){
        save = 1
      }
    }
  }
  return(sim)
}

#' Simulation fucntion to create simulated data as observed data in ABC.
#'
#' @param parameters A vector for CES rates.
#' @param K Carrying capacity, Inf for diverdity-independent models.
#' @param replicates The number of replicates(islands) for TraiSIE simulation.
#'
#' @return A list contains simulated islands
#' @export
get_TraiSIE_sim <- function(parameters, replicates = 1){
  sim <- list()
  for (j in seq_len(replicates)) {
    sim[[j]] <- DAISIE::DAISIE_sim_trait_dep( ##TRAISIERCPP
      time = 4,
      M = 500,
      pars = c(parameters[1],parameters[2],Inf,parameters[3],parameters[4]),
      replicates = 1,
      sample_freq  = Inf,
      plot_sims = FALSE,
      cond = 1,
      verbose = FALSE,
      trait_pars = DAISIE::create_trait_pars(clado_rate2 = parameters[5],
                                             ext_rate2 = parameters[6],
                                             immig_rate2 = parameters[7],
                                             ana_rate2 = parameters[8],
                                             trans_rate = parameters[9],
                                             trans_rate2 = parameters[10],
                                             M2 = 500)
    )
  }
  return(sim)
}

#' Simulation fucntion to create BiSSE simualtions as observed data
#'
#' @param parameters A vector for CES rates.
#' @param replicates The number of replicates(islands) for secsse simulation.
#'
#' @return A list contains simulated islands
#' @author Shu Xie
#' @export

get_bisse_sim_create_obs <- function(parameters, pool_init_states, replicates = 1){
  states <- c("1", "2")
  spec_matrix <- c("1", "1", "1", 1)
  spec_matrix <- rbind(spec_matrix, c("2", "2", "2", 2))
  lambda_list <- secsse::create_lambda_list(state_names = states,
                                            num_concealed_states = 2,
                                            transition_matrix = spec_matrix,
                                            model = "ETD")
  mu_vector <- secsse::create_mu_vector(state_names = states,
                                        num_concealed_states = 2,
                                        model = "ETD",
                                        lambda_list = lambda_list)

  shift_matrix <- c("1", "2", 5)
  shift_matrix <- rbind(shift_matrix, c("2", "1", 6))

  q_matrix <- secsse::create_q_matrix(state_names = states,
                                      num_concealed_states = 2,
                                      shift_matrix = shift_matrix,
                                      diff.conceal = TRUE)
  pars <- c(parameters,0,0)
  lambdas <- secsse::fill_in(lambda_list, pars)
  mus <- secsse::fill_in(mu_vector, pars)
  q <- secsse::fill_in(q_matrix, pars)
  sim <- list()
  for (j in seq_len(replicates)) {
    save <- 0
    while(save < 1){
      sim[[j]] <- secsse::secsse_sim(
        lambdas = lambdas,
        mus = mus,
        qs = q,
        crown_age = 10,
        num_concealed_states = 2,
        pool_init_states = pool_init_states,
        conditioning = "obs_states")

      if(length(sim[[j]]$obs_traits) > 2 && ## at least 2 species
         length(sim[[j]]$obs_traits) < 1000 &&
         length(unique(sim[[j]]$obs_traits)) == 2 &&
         sum(sim[[j]]$obs_traits == 1) > 1 &&
         sum(sim[[j]]$obs_traits == 2) > 1){
        save = 1
      }
    }

  }
  return(sim)
}


#' Simulation function to create simulations in ABC.
#'
#' @param parameters A vector for CES rates.
#' @param replicates The number of replicates(islands) for bisse simulation.
#'
#' @return A list contains simulated islands
#' @export

get_bisse_sim <- function(parameters, pool_init_states, replicates = 1){
  states <- c("1", "2")
  spec_matrix <- c("1", "1", "1", 1)
  spec_matrix <- rbind(spec_matrix, c("2", "2", "2", 2))
  lambda_list <- secsse::create_lambda_list(state_names = states,
                                            num_concealed_states = 2,
                                            transition_matrix = spec_matrix,
                                            model = "ETD")
  mu_vector <- secsse::create_mu_vector(state_names = states,
                                        num_concealed_states = 2,
                                        model = "ETD",
                                        lambda_list = lambda_list)

  shift_matrix <- c("1", "2", 5)
  shift_matrix <- rbind(shift_matrix, c("2", "1", 6))

  q_matrix <- secsse::create_q_matrix(state_names = states,
                                      num_concealed_states = 2,
                                      shift_matrix = shift_matrix,
                                      diff.conceal = TRUE)
  pars <- c(parameters,0,0)
  lambdas <- secsse::fill_in(lambda_list, pars)
  mus <- secsse::fill_in(mu_vector, pars)
  q <- secsse::fill_in(q_matrix, pars)

  sim <- list()
  for (j in seq_len(replicates)) {
    save <- 0
    while(save < 1){
      skip <- FALSE
      tryCatch(sim[[j]] <- secsse::secsse_sim(
        lambdas = lambdas,
        mus = mus,
        qs = q,
        crown_age = 10,
        num_concealed_states = 2,
        pool_init_states = pool_init_states,
        max_spec = 1000,
        min_spec = 2,
        conditioning = "obs_states",
        start_at_crown = FALSE
      ), error=function(e) {
        # print("Error: undefined columns selected")
        skip <<- TRUE
      })
      if(skip == FALSE){
        save = 1
      }
    }
  }
  return(sim)
}




#' Simulation function to create MuSSE simulations as observed data
#'
#' @param parameters A vector for CES rates.
#' @param replicates The number of replicates(islands) for bisse simulation.
#'
#' @return A list contains simulated islands
#' @export

get_musse_sim_create_obs <- function(parameters, pool_init_states, replicates = 1){

  focal_matrix <-
    secsse::create_default_lambda_transition_matrix(state_names = c("1", "2", "3"),
                                                    model = "ETD")
  # and the ETD model:
  lambda_list_ETD <- secsse::create_lambda_list(state_names = c("1", "2", "3"),
                                                num_concealed_states = 3,
                                                transition_matrix = focal_matrix,
                                                model = "ETD")
  # and now the mu vector
  mus_ETD <- secsse::create_mu_vector(state_names = c("1", "2", "3"),
                                      num_concealed_states = 3,
                                      model = "ETD",
                                      lambda_list = lambda_list_ETD)

  t_ETD <- secsse::create_default_shift_matrix(state_names = c("1", "2", "3"),
                                               num_concealed_states = 3,
                                               mu_vector = mus_ETD)
  q_ETD <- secsse::create_q_matrix(state_names = c("1", "2", "3"),
                                   num_concealed_states = 3,
                                   shift_matrix = t_ETD,
                                   diff.conceal = TRUE)

  pars <- c(parameters,0,0,0,0,0,0)
  lambdas <- secsse::fill_in(lambda_list_ETD, pars)
  mus <- secsse::fill_in(mus_ETD, pars)
  q <- secsse::fill_in(q_ETD, pars)
  sim <- list()
  for (j in seq_len(replicates)) {
    save <- 0
    while(save < 1){
      sim[[j]] <- secsse::secsse_sim(
        lambdas = lambdas,
        mus = mus,
        qs = q,
        crown_age = 10,
        num_concealed_states = 3,
        pool_init_states = pool_init_states,
        conditioning = "obs_states")

      if(length(sim[[j]]$obs_traits) > 50 && ## at least 2 species
         length(sim[[j]]$obs_traits) < 1000 &&
         length(unique(sim[[j]]$obs_traits)) == 3 &&
         sum(sim[[j]]$obs_traits == 1) > 10 &&
         sum(sim[[j]]$obs_traits == 2) > 10 &&
         sum(sim[[j]]$obs_traits == 3) > 10){
        save = 1
      }
    }

  }
  return(sim)
}


#' Simulation function to create simulations in ABC.
#'
#' @param parameters A vector for CES rates.
#' @param replicates The number of replicates(islands) for musse simulation.
#'
#' @return A list contains simulated islands
#' @export

get_musse_sim <- function(parameters, pool_init_states, replicates = 1){
  focal_matrix <-
    secsse::create_default_lambda_transition_matrix(state_names = c("1", "2", "3"),
                                                    model = "ETD")
  # and the ETD model:
  lambda_list_ETD <- secsse::create_lambda_list(state_names = c("1", "2", "3"),
                                                num_concealed_states = 3,
                                                transition_matrix = focal_matrix,
                                                model = "ETD")
  # and now the mu vector
  mus_ETD <- secsse::create_mu_vector(state_names = c("1", "2", "3"),
                                      num_concealed_states = 3,
                                      model = "ETD",
                                      lambda_list = lambda_list_ETD)

  t_ETD <- secsse::create_default_shift_matrix(state_names = c("1", "2", "3"),
                                               num_concealed_states = 3,
                                               mu_vector = mus_ETD)
  q_ETD <- secsse::create_q_matrix(state_names = c("1", "2", "3"),
                                   num_concealed_states = 3,
                                   shift_matrix = t_ETD,
                                   diff.conceal = TRUE)

  pars <- c(parameters,0,0,0,0,0,0)
  lambdas <- secsse::fill_in(lambda_list_ETD, pars)
  mus <- secsse::fill_in(mus_ETD, pars)
  q <- secsse::fill_in(q_ETD, pars)
  sim <- list()
  for (j in seq_len(replicates)) {
    save <- 0
    while(save < 1){
      skip <- FALSE
      tryCatch(sim[[j]] <- secsse::secsse_sim(
        lambdas = lambdas,
        mus = mus,
        qs = q,
        crown_age = 10,
        num_concealed_states = 3,
        pool_init_states = pool_init_states,
        # max_spec = 1000,
        # min_spec = 10,
        conditioning = "obs_states"
      ), error=function(e) {
        # print("Error: undefined columns selected")
        skip <<- TRUE
      })
      if(skip == FALSE){
        save = 1
      }
    }
  }
  return(sim)
}


#' Simulation function to create GeoSSE simulations as observed data
#'
#' @param parameters A vector for CES rates.
#' @param replicates The number of replicates(islands) for geosse simulation.
#'
#' @return A list contains simulated islands
#' @export
get_geosse_sim_create_obs <- function(parameters,
                                      replicates = 1){
  sim <- list()
  save <- 0
  while(save < 1){
    sim[[1]] <- diversitree:::tree.geosse(
      pars = parameters,
      max.t = 10,
      x0 = 0
    )
    if(length(sim) > 0) {
      if(length(sim[[1]]$tip.state) > 10 && ## at least 2 species
         length(sim[[1]]$tip.state) < 1000 &&
         length(unique(sim[[1]]$tip.state)) == 3 &&
         sum(sim[[1]]$tip.state == 1) > 5 &&
         sum(sim[[1]]$tip.state == 2) > 5 &&
         sum(sim[[1]]$tip.state == 0) > 5){
        save = 1
      }
    }
  }
  return(sim)
}


#' Simulation function to create simulations in ABC.
#'
#' @param parameters A vector for CES rates.
#' @param replicates The number of replicates(islands) for geosse simulation.
#'
#' @return A list contains simulated islands
#' @export

get_geosse_sim <- function(parameters, replicates = 1){
  sim <- list()
  sim[[1]] <- diversitree:::tree.geosse(
    pars = parameters,
    max.t = 10,
    x0 = 0
  )
  return(sim)
}