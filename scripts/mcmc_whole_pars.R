## estimation for whole 4 parameters(without K), DI for all estimations
prior_dens <- function(x) {
  return(dunif(x[1],0,2) * dunif(x[2],0,2) * dunif(x[3],0,0.1) * dunif(x[4],0,2))
}
set.seed(1)
obs_sim <- DAISIE::DAISIE_sim_constant_rate(
  time = 5,
  M = 1000,
  pars = c(0.5,0.3,40,0.02,0.5),
  replicates = 1,
  sample_freq  = Inf,
  plot_sims = FALSE,
  verbose = FALSE,
  cond = 0
)
obs <- obs_sim[[1]]
MLE_DI <- DAISIE::DAISIE_ML(
  datalist = obs,
  initparsopt = c(1.5, 1.5, 40, 0.1, 1),
  idparsopt = 1:5,
  parsfix = NULL,
  idparsfix = NULL,
  ddmodel = 0,
  cond = 0,
  eqmodel = 0,
  x_E = 0.95,
  x_I = 0.98,
  tol = c(1e-04, 1e-05, 1e-07),
  maxiter = 1000 * round((1.25) ^ 5),  ## ^length(idparsopt)
  methode = "lsodes",
  optimmethod = "subplex"
)
# Maximum likelihood parameter estimates: lambda_c: 0.666639, mu: 0.518875, K: 18.857638,
# gamma: 0.026190, lambda_a: 0.224103
# Maximum loglikelihood: -599.054381
MLE_DD <- DAISIE::DAISIE_ML(
  datalist = obs,
  initparsopt = c(1.5, 1.5, 40, 0.1, 0.5),
  idparsopt = 1:5,
  parsfix = NULL,
  idparsfix = NULL,
  ddmodel = 11,
  cond = 0,
  eqmodel = 0,
  x_E = 0.95,
  x_I = 0.98,
  tol = c(1e-04, 1e-05, 1e-07),
  maxiter = 1000 * round((1.25) ^ 5),  ## ^length(idparsopt)
  methode = "lsodes",
  optimmethod = "subplex"
)
# Maximum likelihood parameter estimates: lambda_c: 0.582056, mu: 0.445841, K: 20.381302,
# gamma: 0.021737, lambda_a: 0.352959
# Maximum loglikelihood: -475.158531


# datalist = obs
# likelihood_function = ll_b
# parameters = c(0.5)
# logtransforms = c(TRUE)
# iterations = 100
# burnin = 10
# thinning = 1
# sigma = 1
ll_b <- function(params, datalist) {
  lnl <- DAISIE::DAISIE_loglik_all(
    pars1 = c(params[1],params[2],40,params[3],params[4]),
    pars2 = c(100, 11, 0, 0),
    datalist = datalist,
    methode = "lsodes"
  )
  prior  <- log(prior_dens(params)) # nolint
  return(lnl + prior)
}

mcmc_nltt <- function( # nolint indeed a complex function
  datalist, likelihood_function,
  parameters, logtransforms, iterations,
  burnin = round(iterations / 3), thinning = 1, sigma = 1
) {

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
  pp <- likelihood_function(parameters, datalist)

  cat("\nGenerating Chain\n")
  cat("0--------25--------50--------75--------100\n")
  cat("*")
  utils::flush.console()
  print_frequency <- 20

  for (i in seq_len(burnin + iterations)) {
    #propose new values
    for (j in seq_along(parameters)) {
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
        new_pp        <- likelihood_function(parameters, datalist)

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
          new_pp        <- likelihood_function(parameters, phy)

          #accept or reject
          #The Markov chain then moves towards x∗ with acceptance probability
          #A(x,x∗)=min{1,[p(x)q(x∗|x)]^(−1)*p(x∗)q(x|x∗)}, otherwise it remains at x.
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
t1 <- Sys.time()
b <- mcmc_nltt(datalist = obs,
               likelihood_function = ll_b,
               parameters = c(1.5, 1.5, 1, 2),
               logtransforms = c(TRUE,TRUE,TRUE,TRUE),
               iterations = 1000,
               burnin = 100,
               thinning = 1,
               sigma = 1)
t2 <- Sys.time()
b_mcmc <- coda::as.mcmc(b)
plot(b_mcmc)
save(b,file = "G:/R/Traisie-ABC/results/mcmc_4params_dd_1000.RData")
mean(b[,1])
mean(b[,2])
mean(b[,3])
mean(b[,4])

load("G:/R/Traisie-ABC/results/mcmc_allpars_new_5000.RData")