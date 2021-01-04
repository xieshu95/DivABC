## The structure of ABC-SMC algorithm
set.seed(1)
obs_sim <- DAISIE::DAISIE_sim_constant_rate(
  time = 5,
  M = 1000,
  pars = c(0.4,0.8,40,0.02,0.5),
  replicates = 1,
  sample_freq  = Inf,
  plot_sims = FALSE,
  verbose = TRUE,
  cond = 5
)

# create a function to sample parameters from prior distribution
prior_distribution_function <- function(n,min,max){
  return(runif(n,min,max))
}

calc_statistic <- function(phylo_data){
   ### return a vector or a list with calculated summary statistics
   nltt <- ## compare nltt or calculate nltt_error
   num_transition <-
   num_endemic <-
}

calculate_weight <- function() {
}



abc_smc <- function( # nolint indeed a complex function
  observed_tree,
  num_statistics,
  num_iterations,
  num_particles = 1000,
  init_epsilon_values   # length(init_epsilon_values) = num_statistics
) {

  #generate initial parameters using
  # parameters <- prior_distribution_function()

  obs_data <- observed_tree
  # compute the observed statistics as a vector
  obs_statistics <- calc_statistic(obs_data)


  #generate a matrix with epsilon values
  #run 50 iterations as an example
  epsilon <- matrix(nrow = num_iterations, ncol = length(init_epsilon_values))
  for (j in seq_along(init_epsilon_values)) {
    for (i in seq_len(num_iterations)) {
      epsilon[i, j] <- init_epsilon_values[j] * exp(-0.5 * (i - 1))
    }
  }

  #store weights
  lamc <- prior_distribution_function(1,0,1)
  mu <- prior_distribution_function(1,0,1)
  K <- prior_distribution_function(1,0,40)
  gam <- prior_distribution_function(1,0,0.1)
  lama <- prior_distribution_function(1,0,1)
  parameters <- c(lamc, mu, K, gam, lama)
  new_weights <- c()
  new_params <- list(c(seq_along(parameters)))
  previous_weights <- c()
  previous_params  <- list(c(seq_along(parameters)))
  indices <- 1:num_particles

  #convergence is expected within 50 iterations
  #usually convergence occurs within 20 iterations
  for (i in 1:num_iterations) {
    number_accepted <- 0

    #replace all vectors
    if (i > 1) {
      #normalize the weights and store them as previous weights.
      previous_weights <- new_weights / sum(new_weights)
      new_weights <- c() #remove all currently stored weights
      previous_params <- new_params #store found params
      new_params <- list(c(seq_along(parameters))) #clear new params
    }

    while (number_accepted < num_particles) {
      #in this initial step, generate parameters from the prior
      if (i == 1) {
        parameters <- parameters
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
        to_change <- sample(seq_along(parameters), 1)

        # perturb the parameter a little bit
        parameters[to_change] <- parameters[to_change] + stats::rnorm(1, 0, 0.05)
      }

      #reject if outside the prior
      if () {
        #simulate a new tree, given the proposed parameters
        new_tree <- DAISIE::DAISIE_sim_trait_dependent()
        accept <- TRUE
        #calculate the summary statistics for the simulated tree
        new_statistics <- calc_statistic(new_tree)

        #check if the summary statistics are sufficiently
        #close to the observed summary statistics
        for (k in 1:num_statistics) {
          if (abs(new_statistics[k] - obs_statistics[k]) > epsilon[i, k]) {
            accept <- FALSE
            #the first step always accepts
            if (i == 1) accept <- TRUE
            break
          }
        }

        if (accept) {
          number_accepted <- number_accepted + 1
          new_params[[number_accepted]] <- parameters
          if (i = 1){
            accepted_weight <- 1
          }else if (i > 1) {
            accepted_weight <- calculate_weight()
          }
          new_weights[number_accepted] <- accepted_weight
          }
        }
      }
    }
  }

  output <- c()
  for (k in seq_along(previous_params)) {
    add <- c()
    for (m in seq_along(parameters)) {
      add <- c(add, previous_params[[k]][m])
    }
    output <- rbind(output, add)
  }
  return(output)
}
