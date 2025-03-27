#' Calculates the posterior probability
#'
#' @return a numeric represents the posterior probability
#' @export

# DI model
calc_log_lik_DAISIE_DI <- function(params, datalist) {
  log_lik <- DAISIE::DAISIE_loglik_all(
    pars1 = as.numeric(c(params[1],params[2],Inf,params[3],params[4])),
    pars2 = c(100, 0, 1, 0),
    datalist = datalist,
    methode = "lsodes"
  )
  return(log_lik)
}

# DD model
calc_log_lik_DAISIE_DD <- function(params, datalist) {
  log_lik <- DAISIE::DAISIE_loglik_all(
    pars1 = as.numeric(c(params[1],params[2],params[5],params[3],params[4])),
    pars2 = c(100, 11, 1, 1),
    datalist = datalist,
    methode = "lsodes"
  )
  return(log_lik)
}


#' Calculates the log prior density
#'
#' @return a numeric represents the log prior density
#' @export
calc_log_prior_DAISIE_DI <- function(params,idparsopt) {
  log_prior <- sum(log(params)) + log(prior_dens_DI(params, idparsopt))
  return(log_prior)
}

calc_log_prior_DAISIE_DD <- function(params,idparsopt) {
  log_prior <- sum(log(params)) + log(prior_dens_DD(params, idparsopt))
  return(log_prior)
}
#' Calculates the log likelihood of secsse model
#'
#' @return a numeric represents the log likelihood
#' @export

calc_log_lik_bisse <- function(params, datalist) {
  pars <- secsse::id_paramPos(traits = datalist$obs_traits,num_concealed_states = 2)
  pars[[1]][] <- c(params[1],params[2],params[1],params[2])
  pars[[2]][] <- c(params[3],params[4],params[3],params[4])
  masterBlock <- matrix(c(params[6],params[5]),
                        ncol=2,nrow=2,byrow=TRUE)
  diag(masterBlock) <- NA
  q <-secsse::q_doubletrans(c(1,2),masterBlock,diff.conceal=F)
  q[1,3]<- q[2,4] <- q[3,1] <- q[4,2] <- 0
  pars[[3]][] <- q
  # skip <- FALSE
  # options(warn = -1)
  suppressWarnings(
    suppressMessages(
      log_lik <- secsse::secsse_loglik(
        parameter = pars,
        phy = datalist$phy,
        traits = datalist$obs_traits,
        num_concealed_states = 2,
        sampling_fraction = c(1,1),
        cond = "proper_cond")
    )
  )

  return(log_lik)
}

#' Calculates the log prior density
#'
#' @return a numeric represents the log prior density
#' @export
calc_log_prior_bisse <- function(params,idparsopt) {
  log_prior <- sum(log(params))+ log(prior_dens_bisse(params, idparsopt))
  return(log_prior)
}

#' Calculates the log likelihood of secsse model
#'
#' @return a numeric represents the log likelihood
#' @export

calc_log_lik_musse <- function(params, datalist) {
  pars <- secsse::id_paramPos(traits = datalist$obs_traits,num_concealed_states = 3)
  pars[[1]][] <- c(params[1],params[2],params[3],params[1],params[2],params[3],params[1],params[2],params[3])
  pars[[2]][] <- c(params[4],params[5],params[6],params[4],params[5],params[6],params[4],params[5],params[6])

  t_ETD <- secsse::create_default_shift_matrix(state_names = c("1", "2", "3"),
                                               num_concealed_states = 3,
                                               mu_vector = c(4,5,6,4,5,6,4,5,6))
  q_ETD <- secsse::create_q_matrix(state_names = c("1", "2", "3"),
                                   num_concealed_states = 3,
                                   shift_matrix = t_ETD,
                                   diff.conceal = TRUE)

  params <- c(params,0,0,0,0,0,0)
  q <- secsse::fill_in(q_ETD, params)
  pars[[3]][] <- q
  # skip <- FALSE
  # options(warn = -1)
  suppressWarnings(
    suppressMessages(
      log_lik <- secsse::secsse_loglik(
        parameter = pars,
        phy = datalist$phy,
        traits = datalist$obs_traits,
        num_concealed_states = 3,
        sampling_fraction = c(1,1,1),
        cond = "proper_cond")
    )
  )

  return(log_lik)
}

#' Calculates the log prior density
#'
#' @return a numeric represents the log prior density
#' @export
calc_log_prior_musse <- function(params,idparsopt) {
  log_prior <- sum(log(params))+ log(prior_dens_musse(params, idparsopt))
  return(log_prior)
}


