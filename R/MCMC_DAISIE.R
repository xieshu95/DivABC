#'mcmc
#'
#' @author Shu Xie
#' @return
#' @export
MCMC_DAISIE <- function(datalist,
                        likelihood_function,
                        parameters,
                        logtransforms = c(TRUE,TRUE,TRUE,TRUE),
                        iterations,
                        burnin = round(iterations / 3),
                        thinning = 1,
                        sigma = 1,
                        idparsopt = 1:4)
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
    for (j in idparsopt) {
      if (logtransforms[j] == TRUE) {
        if (parameters[j] == 0) {
          stop("Cannot propose new value for a parameter with value 0.0.")
        }

        eta           <- log(parameters[j])
        new_eta       <- eta + stats::rnorm(1, 0, sigma)
        new_val       <- exp(new_eta)
        # calculate the Hastings ratio
        hr            <- log(new_val / parameters[j])
        parameters[j] <- new_val
        new_pp        <- likelihood_function(parameters, datalist,idparsopt)

        #accept or reject
        if (is.finite(new_pp) &&
            is.finite(hr) &&
            new_pp - pp + hr > log(stats::runif(1, 0, 1))) {
          pp <- new_pp
        } else {
          parameters[j] <- exp(eta)
        }
      } else {

        eta           <- parameters[j]
        new_val       <- eta + stats::rnorm(1, 0, sigma)
        #calculate the Hastings ratio
        hr            <- 0.0
        parameters[j] <- new_val

        if (parameters[j] >= 0 & parameters[1] > 0) {
          new_pp        <- likelihood_function(parameters, datalist)

          #accept or reject
          if (is.finite(new_pp) &&
              is.finite(hr) &&
              new_pp - pp + hr > log(stats::runif(1, 0, 1))) {
            pp <- new_pp
          } else {
            parameters[j] <- eta
          }
        } else {
          parameters[j] <- eta
        }
      }
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


#' Calculates the log likelihood
#'
#' @return a numeric represents the log likelihood
#' @export

calc_loglik <- function(params, datalist,idparsopt) {
  lnl <- DAISIE::DAISIE_loglik_all(
    pars1 = c(params[1],params[2],Inf,params[3],params[4]),
    pars2 = c(100, 0, 0, 0),
    datalist = datalist,
    methode = "lsodes"
  )
  prior1  <- log(prior_dens(params, idparsopt)) # nolint
  return(lnl + prior1)
}

#' prior density function
#'
#' @return a numeric represents the log likelihood
#' @export
prior_dens <- function(x,idparsopt) {
  if(1 %in% idparsopt){
    dens_lac <- stats::dunif(x[1],0,1)
  } else {
    dens_lac <- 1
  }
  if(2 %in% idparsopt){
    dens_mu <- stats::dunif(x[2],0,1)
  } else {
    dens_mu <- 1
  }
  if(3 %in% idparsopt){
    dens_gam <- stats::dunif(x[3],0,0.05)
  } else {
    dens_gam <- 1
  }
  if(4 %in% idparsopt){
    dens_laa <- stats::dunif(x[4],0,1)
  } else {
    dens_laa <- 1
  }
  return(dens_lac * dens_mu * dens_gam * dens_laa)
}

