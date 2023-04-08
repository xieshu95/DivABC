#' Simulation fucntion to create simulated data as observed data in ABC.
#'
#' @param parameters A vector for CES rates.
#' @param K Carrying capacity, Inf for diverdity-independent models.
#' @param replicates The number of replicates(islands) for DAISIE simulation.
#'
#' @return A list contains simulated islands
#' @author Shu Xie
#' @export


get_DAISIE_sim <- function(parameters, K, replicates){
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


#' Simulation fucntion to create simulated data as observed data in ABC.
#'
#' @param parameters A vector for CES rates.
#' @param K Carrying capacity, Inf for diverdity-independent models.
#' @param replicates The number of replicates(islands) for TraiSIE simulation.
#'
#' @return A list contains simulated islands
#' @author Shu Xie
#' @export
get_TraiSIE_sim <- function(parameters, K, replicates){
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

#' Simulation fucntion to create simulated data as observed data in ABC.
#'
#' @param parameters A vector for CES rates.
#' @param replicates The number of replicates(islands) for secsse simulation.
#'
#' @return A list contains simulated islands
#' @author Shu Xie
#' @export

get_secsse_sim_create_obs <- function(parameters, K, replicates){
  idparlist <- secsse::cla_id_paramPos(traits = c(1,2),
                                       num_concealed_states = 2)
  idparlist$lambdas[1,] <- rep(c(parameters[1], parameters[2]),2)
  idparlist$mus[1:4]<- rep(c(parameters[3], parameters[4]),2)
  masterBlock <- matrix(c(parameters[6], parameters[5]),
                        ncol=2,nrow=2,byrow=TRUE)
  diag(masterBlock) <- NA
  q <-secsse::q_doubletrans(c(1,2),masterBlock,diff.conceal=F)
  q[1,3]<- q[2,4] <- q[3,1] <- q[4,2] <- 0

  lambdas <- secsse::prepare_full_lambdas(c(1,2),2,idparlist$lambdas)
  states <- names(idparlist$mus)
  initialState<- sample(states,1)
  speciesTraits <- c(initialState,initialState)

  sim <- list()
  suppressWarnings(
    for (j in seq_len(replicates)) {
      save <- 0
      while(save < 1){
        sim[[j]] <- secsse_sim(timeSimul = 18,
                                     states = states,
                                     lambdas = lambdas,
                                     mus = idparlist$mus,
                                     qs = q,
                                     speciesTraits = speciesTraits,
                                     maxSpec = 400)  ## maximum 600 species
        if(length(sim[[j]]$examTraits) > 20 && ## at least 50 species
           length(sim[[j]]$examTraits) < 400 &&
           length(unique(sim[[j]]$examTraits)) == 2){
          save = 1
        }
      }

    }
  )
  return(sim)
}

#' Simulation fucntion to create simulated data as observed data in ABC.
#'
#' @param parameters A vector for CES rates.
#' @param replicates The number of replicates(islands) for secsse simulation.
#'
#' @return A list contains simulated islands
#' @author Shu Xie
#' @export

get_secsse_sim <- function(parameters, K, replicates){
  idparlist <- secsse::cla_id_paramPos(traits = c(1,2),
                                       num_concealed_states = 2)
  idparlist$lambdas[1,] <- rep(c(parameters[1], parameters[2]),2)
  idparlist$mus[1:4]<- rep(c(parameters[3], parameters[4]),2)
  masterBlock <- matrix(c(parameters[6], parameters[5]),
                        ncol=2,nrow=2,byrow=TRUE)
  diag(masterBlock) <- NA
  q <-secsse::q_doubletrans(c(1,2),masterBlock,diff.conceal=F)
  q[1,3]<- q[2,4] <- q[3,1] <- q[4,2] <- 0

  lambdas <- secsse::prepare_full_lambdas(c(1,2),2,idparlist$lambdas)
  states <- names(idparlist$mus)
  initialState<- sample(states,1)
  speciesTraits <- c(initialState,initialState)

  sim <- list()
  suppressWarnings(
    suppressMessages(
      for (j in seq_len(replicates)) {
        sim[[j]] <- secsse_sim(timeSimul = 18,
                                       states = states,
                                       lambdas = lambdas,
                                       mus = idparlist$mus,
                                       qs = q,
                                       speciesTraits = speciesTraits,
                                       maxSpec = 400)
      }
    )
  )
  return(sim)
}

# dimnames(q)[1:2]<-list(states)
# idparlist$Q <- q
# idparlist
