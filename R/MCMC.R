#' mcmc
#' @references Janzen, T., Höhna, S. and Etienne, R.S. (2015), Approximate
#' Bayesian Computation of diversification rates from molecular phylogenies:
#' introducing a new efficient summary statistic, the nLTT. Methods Ecol Evol,
#' 6: 566-575. https://doi.org/10.1111/2041-210X.12350
#' @return
#' @export
MCMC <- function(datalist,
                          log_lik_function,
                          log_prior_function,
                          logform = FALSE, # logform = TRUE -> log_prior_function = calc_log_prior_bisse_logtrans
                          parameters,
                          iterations,
                          burnin,
                          thinning = 1,
                          sigma = 1,
                          idparsopt)
{
  # create a list for the samples & reserve memory for the chain
  chain <- array(dim = c(floor(iterations / thinning) + 1,
                         length(parameters) + 2))

  for (j in seq_along(parameters)) {
    if (parameters[j] < 0) {
      #Just checking
      stop("mcmc_nltt: ",
           "initial parameter values have to be above zero\n",
           "but one was ", parameters[j], " instead")
    }
  }
  # pre-compute current posterior probability
  log_lik <- log_lik_function(parameters, datalist)
  log_prior <- log_prior_function(parameters, idparsopt)

  cat("\nGenerating Chain\n")
  cat("0--------25--------50--------75--------100\n")
  cat("*")
  utils::flush.console()
  print_frequency <- 20

  for (i in seq_len(burnin + iterations)) {
    #propose new values
    parameters_old <- parameters
    if(logform == TRUE){
      parameters[idparsopt] <- exp(stats::rnorm(length(idparsopt),
                                              log(parameters[idparsopt]),
                                              sigma))
    } else if (logform == FALSE) {
      parameters[idparsopt] <- stats::rnorm(length(idparsopt),
                                            parameters[idparsopt],
                                                sigma)
    }

    if (all(parameters > 0)) {
      # calculate the Hastings ratio
      hr            <- 0
      new_log_lik <- log_lik_function(parameters, datalist)
      new_log_prior <- log_prior_function(parameters, idparsopt)

      #accept or reject
      if (is.finite(new_log_lik) &&
          is.finite(new_log_prior) &&
          is.finite(hr) &&
          new_log_lik - log_lik + new_log_prior - log_prior + hr > log(stats::runif(1, 0, 1))) {
        log_lik <- new_log_lik
        log_prior <- new_log_prior
      } else {
        parameters <- parameters_old
      }
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
        chain[(i - burnin) / thinning + 1, ] <- c(parameters,log_lik,log_prior)
      }
    }
  }
  cat("\nFinished MCMC.\n")
  #return a mcmc object, used by coda to plot
  return(coda::as.mcmc(chain))
}
