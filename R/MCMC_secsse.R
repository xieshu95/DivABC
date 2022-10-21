#'mcmc
#'
#' @author Shu Xie
#' @return
#' @export
MCMC_secsse <- function(datalist,
                        likelihood_function,
                        parameters,
                        iterations,
                        burnin = round(iterations / 3),
                        thinning = 1,
                        sigma = 1,
                        idparsopt)
{
  # create a list for the samples & reserve memory for the chain
  chain <- array(dim = c(floor(iterations / thinning) + 1,
                         length(parameters)))

  for (j in seq_along(parameters)) {
    if (parameters[j] < 0) {
      #Just checking
      stop("mcmc_nltt: ",
           "initial parameter values have to be above zero\n",
           "but one was ", parameters[j], " instead")
    }
  }
  # pre-compute current posterior probability
  pp <- likelihood_function(parameters, datalist, idparsopt)

  cat("\nGenerating Chain\n")
  cat("0--------25--------50--------75--------100\n")
  cat("*")
  utils::flush.console()
  print_frequency <- 20

  for (i in seq_len(burnin + iterations)) {
    #propose new values
    parameters_old <- parameters
    for (m in idparsopt) {
      if (parameters[m] == 0) {
        stop("Cannot propose new value for a parameter with value 0.0.")
      }
      parameters[m] <- exp(stats::rnorm(1, log(parameters[m]), sigma))
    }
    # calculate the Hastings ratio
    hr            <- 0
    new_pp        <- likelihood_function(parameters, datalist,idparsopt)

    #accept or reject
    if (is.finite(new_pp) &&
        is.finite(hr) &&
        new_pp - pp + hr > log(stats::runif(1, 0, 1))) {
      pp <- new_pp
    } else {
      parameters <- parameters_old
    }

    # sample the parameter
    if (i >= burnin) {
      if ((i) %% ((iterations - burnin) / print_frequency) == 0) {
        cat("**")
        utils::flush.console()
      }
      if ((i - burnin) %% thinning == 0) {
        chain[(i - burnin) / thinning + 1, ] <- parameters
      }
    }
  }
  cat("\nFinished MCMC.\n")
  #return a mcmc object, used by coda to plot
  return(coda::as.mcmc(chain))
}


#' Calculates the posterior probability
#'
#' @return a numeric represents the posterior probability
#' @export

calc_log_pp_secsse <- function(params, datalist,idparsopt) {
  pars <- secsse::id_paramPos(traits = datalist$examTraits,num_concealed_states = 2)
  pars[[1]][] <- c(params[1],params[2],params[1],params[2])
  pars[[2]][] <- c(params[3],params[4],params[3],params[4])
  masterBlock <- matrix(c(params[5],params[6]),
                        ncol=2,nrow=2,byrow=TRUE)
  diag(masterBlock) <- NA
  q <-secsse::q_doubletrans(c(1,2),masterBlock,diff.conceal=F)
  q[1,3]<- q[2,4] <- q[3,1] <- q[4,2] <- 0
  pars[[3]][] <- q
  log_lik <- secsse::secsse_loglik(
    parameter = pars,
    phy = datalist$phy,
    traits = datalist$examTraits,
    num_concealed_states = 2,
    sampling_fraction = c(1,1)
  )
  log_prior  <- log(prior_dens_secsse(params, idparsopt)) # nolint
  return(log_lik + log_prior)
}
