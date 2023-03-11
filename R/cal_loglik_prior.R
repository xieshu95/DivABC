#' Calculates the posterior probability
#'
#' @return a numeric represents the posterior probability
#' @export

calc_log_lik_DAISIE <- function(params, datalist,idparsopt) {
  log_lik <- DAISIE::DAISIE_loglik_all(
    pars1 = as.numeric(c(params[1],params[2],20,params[3],params[4])),
    pars2 = c(100, 11, 1, 0),
    datalist = datalist,
    methode = "lsodes"
  )
  return(log_lik)
}


#' Calculates the log prior density
#'
#' @return a numeric represents the log prior density
#' @export
calc_log_prior_DAISIE <- function(params) {
  log_prior <- sum(params)
  return(log_prior)
}

#' Calculates the log likelihood of secsse model
#'
#' @return a numeric represents the log likelihood
#' @export

calc_log_lik_secsse <- function(params, datalist,idparsopt) {
  pars <- secsse::id_paramPos(traits = datalist$examTraits,num_concealed_states = 2)
  pars[[1]][] <- c(params[1],params[2],params[1],params[2])
  pars[[2]][] <- c(params[3],params[4],params[3],params[4])
  masterBlock <- matrix(c(params[5],params[6]),
                        ncol=2,nrow=2,byrow=TRUE)
  diag(masterBlock) <- NA
  q <-secsse::q_doubletrans(c(1,2),masterBlock,diff.conceal=F)
  q[1,3]<- q[2,4] <- q[3,1] <- q[4,2] <- 0
  pars[[3]][] <- q
  # log_lik <- secsse::secsse_loglik(
  #   parameter = pars,
  #   phy = datalist$phy,
  #   traits = datalist$examTraits,
  #   num_concealed_states = 2,
  #   sampling_fraction = c(1,1),
  #   cond = "proper_cond"
  # )
  skip <- FALSE
  tryCatch(log_lik <- secsse::secsse_loglik(
    parameter = pars,
    phy = datalist$phy,
    traits = datalist$examTraits,
    num_concealed_states = 2,
    sampling_fraction = c(1,1),
    cond = "proper_cond"
  ), error=function(e) {
    print("Optimization has not converged. Try again with different initial values.")
    skip <<- TRUE
  })
  if(skip == TRUE){
    log_lik <- -Inf
  }
  return(log_lik)
}

#' Calculates the log prior density
#'
#' @return a numeric represents the log prior density
#' @export
calc_log_prior_secsse <- function(params) {
  log_prior <- sum(params)
  return(log_prior)
}
