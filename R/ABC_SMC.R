#' Using ABC approach on DAISIE to estimate CES rates.
#'
#' @param
#'
#' @return
#' @author
#' @export


ABC_SMC <- function( # nolint indeed a complex function
  obs_data,
  sim_function,
  init_epsilon_values,
  prior_generating_function,
  prior_density_function,
  number_of_particles = 1000,
  sigma = 0.05,
  stop_rate = 1e-3,
  replicates = 1,  ## simulation replicates for each parameter set
  num_iterations,
  K,
  idparsopt,
  fixpars
) {
  #just to get the number of parameters to be estimated.
  parameters <- prior_generating_function(fixpars,idparsopt)

  # # compute the observed statistics (no need)
  # obs_statistics <- c()
  # for (i in seq_along(statistics)) {
  #   obs_statistics[i] <- statistics[[i]](datalist)
  # }
  #

  #generate a matrix with epsilon values
  #we assume that the SMC algorithm converges within 50 iterations
  epsilon <- matrix(nrow = 50, ncol = length(init_epsilon_values))
  for (j in seq_along(init_epsilon_values)) {
    if (init_epsilon_values[j] < 0) {
      stop("abc_smc_nltt: ",
           "epsilon values have to be positive,",
           "but were instead: ", init_epsilon_values[j])
    }

    for (i in seq_len(50)) {
      epsilon[i, j] <- init_epsilon_values[j] * exp(-0.5 * (i - 1))
    }
  }

  #store weights
  new_weights <- c()
  new_params <- list(c(seq_along(parameters)))
  previous_weights <- c()
  previous_params  <- list(c(seq_along(parameters)))
  indices <- 1:number_of_particles
  n_iter <- 0


  #convergence is expected within 50 iterations
  #usually convergence occurs within 20 iterations
  for (i in 1:num_iterations) {
    n_iter <- n_iter + 1
    cat("\nGenerating Particles for iteration\t", i, "\n")
    cat("0--------25--------50--------75--------100\n")
    cat("*")
    utils::flush.console()

    print_frequency <- 20
    tried <- 0
    number_accepted <- 0

    #replace all vectors
    if (i > 1) {
      #normalize the weights and store them as previous weights.
      previous_weights <- new_weights / sum(new_weights)
      new_weights <- c() #remove all currently stored weights
      previous_params <- new_params #store found params
      new_params <- list(c(seq_along(parameters))) #clear new params
    }

    stoprate_reached <- FALSE

    while (number_accepted < number_of_particles) {
      #in this initial step, generate parameters from the prior
      if (i == 1) {
        parameters <- prior_generating_function(fixpars,idparsopt)
      } else {
        #if not in the initial step, generate parameters
        #from the weighted previous distribution:
        index <- sample(x = indices, size = 1,
                        replace = TRUE, prob = previous_weights)

        for (p_index in seq_along(parameters)) {
          parameters[p_index] <- previous_params[[index]][p_index]
        }

        #only perturb one parameter, to avoid extremely
        #low acceptance rates due to simultaneous perturbation
        if(length(idparsopt) == 1){
          to_change <- as.numeric(idparsopt)
        } else {
          to_change <- sample(idparsopt, 1)
        }

        # perturb the parameter a little bit,
        #on log scale, so parameter doesn't go < 0
        eta <- log(parameters[to_change]) + stats::rnorm(1, 0, sigma)
        parameters[to_change] <- exp(eta)
      }

      #reject if outside the prior
      if (prior_density_function(parameters,idparsopt) > 0) {
        #simulate a new tree, given the proposed parameters
        new_sim <- sim_function(parameters = parameters,
                                K = K,
                                replicates = replicates)
        accept <- TRUE

        #calculate the summary statistics for the simulated tree
        df_stats <- calc_ss_diff (sim1 = obs_data[[1]],
                                  sim2 = new_sim[[1]])

        # #check if the summary statistics are sufficiently
        # #close to the observed summary statistics

        # median_df <- base::lapply(df_stats,median)
        for (k in seq_along(df_stats)) {
          if (as.numeric(df_stats[k]) > epsilon[i, k]) {
            accept <- FALSE
            #the first step always accepts
            if (i == 1) accept <- TRUE
            break
          }
        }

        if (accept) {
          number_accepted <- number_accepted + 1
          new_params[[number_accepted]] <- parameters
          accepted_weight <- 1
          #calculate the weight
          if (i > 1) {
            accepted_weight <- calc_weight(previous_weights,
                                           previous_params,
                                           parameters,
                                           sigma,
                                           prior_density_function,
                                           idparsopt)
          }
          new_weights[number_accepted] <- accepted_weight

          if ((number_accepted) %%
              (number_of_particles / print_frequency) == 0) {
            cat("**")
            utils::flush.console()
          }
        }
      }

      #convergence if the acceptance rate gets too low
      tried <- tried + 1
      if (tried > (1 / stop_rate) & n_iter > 3) {
        if ((number_accepted / tried) < stop_rate) {
          stoprate_reached <- TRUE
          break
        }
      }
    }

    if (stoprate_reached) {
      break
    }
  }
  message("tried times: ", tried)

  ABC <- c()
  for (k in seq_along(previous_params)) {
    add <- c()
    for (m in seq_along(parameters)) {
      add <- c(add, previous_params[[k]][m])
    }
    ABC <- rbind(ABC, add)
  }
  output <- list(ABC = ABC,
                 n_iter = n_iter)
  return(output)
}
