#'mcmc
#'
#' @author Shu Xie
#' @return
#' @export
MCMC_DAISIE <- function(datalist,
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

calc_log_pp <- function(params, datalist,idparsopt) {
  log_lik <- DAISIE::DAISIE_loglik_all(
    pars1 = as.numeric(c(params[1],params[2],20,params[3],params[4])),
    pars2 = c(100, 11, 1, 0),
    datalist = datalist,
    methode = "lsodes"
  )
  log_prior  <- log(prior_dens(params, idparsopt)) # nolint
  return(log_lik + log_prior)
}

