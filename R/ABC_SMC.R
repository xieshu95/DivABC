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
  ABC_list <- list()
  # ss_reject <- c()
  # ss_accept <- c()


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
    # ss_logic <- c()

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
        } else if(length(idparsopt) == 4){
          to_change <- sample(idparsopt, 1, TRUE, c(0.2,0.3,0.2,0.3))
        } else {
          to_change <- sample(idparsopt, 1)
          # to_change <- sample(idparsopt, 1, TRUE,c(0.2,0.3,0.2,0.3))
        }

        sigma_temp <- sigma * exp(-0.5 * (i - 1))
        if(to_change == 3 || to_change == 7){
          sigma_temp <- sigma_temp/10
        } else {
          sigma_temp <- sigma_temp
        }
        # perturb the parameter a little bit
        parameters[to_change] <- parameters[to_change] + stats::rnorm(1, 0, sigma_temp)
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

        # ss_logic_particle <- c()
        # for (k in seq_along(df_stats)) {
        #   if (as.numeric(df_stats[k]) > epsilon[i, k]) {
        #     ss_logic_particle[k] <- FALSE
        #   } else {
        #     ss_logic_particle[k] <- TRUE
        #   }
        # }
        #
        #
        # if(sum(ss_logic_particle == TRUE) < 4 && i > 1) {
        #   accept <- FALSE
        # }
        #
        #
        # ss_logic <- rbind(ss_logic,ss_logic_particle)

        if (accept) {
          number_accepted <- number_accepted + 1
          new_params[[number_accepted]] <- parameters
          accepted_weight <- 1
          #calculate the weight
          if (i > 1) {
            accepted_weight <- calc_weight(previous_weights,
                                           previous_params,
                                           parameters,
                                           sigma_temp,
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
      if (tried > (1 / stop_rate) & n_iter > 5) {
        if ((number_accepted / tried) < stop_rate) {
          stoprate_reached <- TRUE
          break
        }
      }
    }
    # ss_reject <- rbind(ss_reject,apply(ss_logic,2,function(x) sum(x == FALSE)))
    # ss_accept <- rbind(ss_accept,apply(ss_logic,2,function(x) sum(x == TRUE)))
    ABC <- c()
    for (k in seq_along(new_params)) {
      add <- c()
      for (m in seq_along(parameters)) {
        add <- c(add, new_params[[k]][m])
      }
      ABC <- rbind(ABC, add)
    }
    ABC_list[[i]] <- ABC

    if (stoprate_reached) {
      break
    }


  }
  message("tried times: ", tried)

  # ABC <- c()
  # for (k in seq_along(previous_params)) {
  #   add <- c()
  #   for (m in seq_along(parameters)) {
  #     add <- c(add, previous_params[[k]][m])
  #   }
  #   ABC <- rbind(ABC, add)
  # }
  output <- list(ABC = ABC_list,
                 n_iter = n_iter)
  # ss_reject = ss_reject,
  # ss_accept = ss_accept)
  return(output)
}