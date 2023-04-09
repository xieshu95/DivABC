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
  calc_ss_function,
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
  fixpars,
  ss_set = 1
) {
  #just to get the number of parameters to be estimated.
  parameters <- prior_generating_function(fixpars,idparsopt)

  #generate a matrix with epsilon values
  #we assume that the SMC algorithm converges within 50 iterations
  epsilon <- matrix(nrow = 20, ncol = length(init_epsilon_values))
  epsilon[1,] <- init_epsilon_values

  #store weights
  new_weights <- c()
  new_params <- list(c(seq_along(parameters)))
  previous_weights <- c()
  previous_params  <- list(c(seq_along(parameters)))
  indices <- 1:number_of_particles
  n_iter <- 0
  ABC_list <- list()
  sim_list <- list()
  ss_diff_list <- list()

  #convergence is expected within 50 iterations
  #usually convergence occurs within 20 iterations
  for (i in 1:num_iterations) {
    ss_diff <- c()
    n_iter <- n_iter + 1
    cat("\nGenerating Particles for iteration\t", i, "\n")
    cat("0--------25--------50--------75--------100\n")
    cat("*")
    utils::flush.console()

    print_frequency <- 20
    tried <- 0
    number_accepted <- 0
    sigma_temp <- sigma * exp(-0.1 * (i - 1))

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
          parameters[idparsopt] <- exp(log(parameters[idparsopt]) +
                                         stats::rnorm(1, 0, sigma_temp))
      }

      #reject if outside the prior
      if (prior_density_function(parameters,idparsopt) > 0) {
        #simulate a new tree, given the proposed parameters
        new_sim <- sim_function(parameters = parameters,
                                K = K,
                                replicates = replicates)


        accept <- TRUE
        # for secsse
        if ("phy" %in% names(new_sim[[1]])) {
          if (length(new_sim[[1]]$examTraits) < 20 ||
              length(new_sim[[1]]$examTraits) >= 400 ||
              length(unique(new_sim[[1]]$examTraits)) < 2 ||
              sum(new_sim[[1]]$examTraits == 1) < 2 ||
              sum(new_sim[[1]]$examTraits == 2) < 2) {
            accept <- FALSE
          }
        }
        # for traisie constrain that simulated tree has same clade-number as observed
        if(length(obs_data[[1]][[1]]) != length(new_sim[[1]][[1]])) {
          accept <- FALSE
        }

        #calculate the summary statistics for the simulated tree
        if (accept) {
          df_stats <- calc_ss_function (sim1 = obs_data[[1]],
                                    sim2 = new_sim[[1]],
                                    ss_set = ss_set)

          # #check if the summary statistics are sufficiently
          for (k in seq_along(df_stats)) {
            if (as.numeric(df_stats[k]) > epsilon[i, k]) {
              accept <- FALSE
              #the first step always accepts
              # if (i == 1) accept <- TRUE
              # break
            }
          }
        }


        if (accept) {
          number_accepted <- number_accepted + 1
          new_params[[number_accepted]] <- parameters
          sim_list[[number_accepted]] <- new_sim[[1]]
          accepted_weight <- 1
          ss_diff <- rbind(ss_diff,df_stats)

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
      if (tried > (1 / stop_rate) & n_iter > 4) {
        if ((number_accepted / tried) < stop_rate) {
          stoprate_reached <- TRUE
          break
        }
      }
    }

    ss_diff_list[[i]] <- ss_diff
    if (stoprate_reached == FALSE) {
      epsilon[i + 1, ] <- apply(ss_diff, 2, quantile, probs = 0.7) #0.5
      # if("phy" %in% names(obs_data[[1]])){
      #   epsilon[i + 1, ] <- apply(ss_diff, 2, quantile, probs = 0.65) #0.5
      # } else {
      #   epsilon[i + 1, ] <- apply(ss_diff, 2, quantile, probs = 0.55) #0.5
      # }
    }
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
    if(n_iter >= 3) {
      save_output(
        output = list(sim_list = sim_list,
                      ABC = ABC_list,
                      n_iter = n_iter,
                      epsilon = epsilon,
                      obs_sim = obs_data,
                      ss_diff_list = ss_diff_list),
        param_space_name = param_space_name,
        param_set = param_set,
        ss_set = ss_set
      )
    }
  }
  message("tried times: ", tried)

  output <- list(sim_list = sim_list,
                 ABC = ABC_list,
                 n_iter = n_iter,
                 epsilon = epsilon,
                 obs_sim = obs_data,
                 ss_diff_list = ss_diff_list)
  return(output)
}
