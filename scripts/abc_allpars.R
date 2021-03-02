#### ABC for all pars with 5 summary statistics

set.seed(1)
obs_sim <- DAISIE::DAISIE_sim_constant_rate(
  time = 5,
  M = 1000,
  pars = c(0.5,0.3,40,0.02,0.5),
  replicates = 1,
  sample_freq  = Inf,
  plot_sims = FALSE,
  verbose = TRUE,
  cond = 0
)

# create a function to sample parameters from prior distribution
##first step set only one variable(lac)
prior_gen <- function(){
  lac <- runif(1,0,2)
  mu <- runif(1,0,2)
  gam <- runif(1,0,0.1)
  laa <- runif(1,0,2)
  param <- c(lac,mu,gam,laa)
  return(param)
}

prior_dens <- function(x) {
  return(dunif(x[1],0,2) * dunif(x[2],0,2) * dunif(x[3],0,0.1) * dunif(x[4],0,2))
}


# calc_statistic <- nltt_within_param (obs_rep = 1,
#                                      sim1 = obs_sim,
#                                      sim2 = simulated_data,
#                                      replicates = replicates)

# calculate_weight <- nLTT:::calculate_weight(
#   weights = weights,
#   particles = particles,
#   current = current,
#   sigma = sigma,
#   prior_density_function = prior_density_function)
calculate_weight <- function(weights, particles,
                             current, sigma, prior_density_function) {
  vals <- c()
  for (i in seq_along(particles)) {
    vals[i] <- weights[i]
    for (j in seq_along(current)) {
      diff <- log(current[j]) - log(particles[[i]][j])
      vals[i] <- vals[i] * stats::dnorm(diff, mean = 0, sd = sigma)
    }
  }

  numerator <- prior_density_function(current)

  return(numerator / sum(vals))
}

simulation_function <- function(parameters, replicates){
  sim <- list()
  for (j in seq_len(replicates)) {
    sim[[j]] <- DAISIE::DAISIE_sim_constant_rate(
      time = 5,
      M = 1000,
      pars = c(parameters[1],parameters[2],40,parameters[3],parameters[4]),
      replicates = 1,
      sample_freq  = Inf,
      plot_sims = FALSE,
      verbose = FALSE,
      cond = 0
    )
  }
  return(sim)
}


abc_smc_nltt <- function( # nolint indeed a complex function
  datalist,
  simulation_function,
  init_epsilon_values,
  prior_generating_function,
  prior_density_function,
  number_of_particles = 1000,
  sigma = 0.05,
  stop_rate = 1e-5,
  replicates  ## simulation replicates for each parameter set
) {
  #just to get the number of parameters to be estimated.
  parameters <- prior_generating_function()

  # # compute the observed statistics (no need)
  # obs_statistics <- c()
  # for (i in seq_along(statistics)) {
  #   obs_statistics[i] <- statistics[[i]](datalist)
  # }
  #
  stats <- c()

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

  #convergence is expected within 50 iterations
  #usually convergence occurs within 20 iterations
  for (i in 1:50) {
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
        parameters <- prior_generating_function()  ## one value for lac
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

        # perturb the parameter a little bit,
        #on log scale, so parameter doesn't go < 0
        eta <- log(parameters[to_change]) + stats::rnorm(1, 0, sigma)
        parameters[to_change] <- exp(eta)
      }

      #reject if outside the prior
      if (prior_density_function(parameters) > 0) {
        #simulate a new tree, given the proposed parameters
        new_tree <- simulation_function(parameters,replicates)
        accept <- TRUE

        #calculate the summary statistics for the simulated tree
        df_stats <- nltt_within_param (obs_rep = 1,
                                       sim1 = datalist,
                                       sim2 = new_tree,
                                       replicates = replicates)

        # #check if the summary statistics are sufficiently
        # #close to the observed summary statistics
        # for (k in seq_along(statistics)) {
        #   if (abs(stats[k] - obs_statistics[k]) > epsilon[i, k]) {
        #     accept <- FALSE
        #     #the first step always accepts
        #     if (i == 1) accept <- TRUE
        #     break
        #   }
        # }

        # Firstly try to use only one statistic (spec_nltt)
        mean_df <- lapply(df_stats,mean)
        for (k in seq_along(mean_df)) {
          if (as.numeric(mean_df[k]) > epsilon[i, k]) {
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
            accepted_weight <- calculate_weight(previous_weights,
                                                previous_params, parameters,
                                                sigma, prior_density_function)
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
      if (tried > (1 / stop_rate)) {
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

# datalist = obs_sim
# simulation_function = simulation_function
# init_epsilon_values = 100
# prior_generating_function = prior_gen
# prior_density_function = prior_dens
# number_of_particles = 1000
# sigma = 0.05
# stop_rate = 0.01
# replicates = 100


t1 <- Sys.time()
abc <- abc_smc_nltt(
  datalist = obs_sim,
  simulation_function = simulation_function,
  init_epsilon_values = c(300,200,100,300,100),
  prior_generating_function = prior_gen,
  prior_density_function = prior_dens,
  number_of_particles = 1000,
  sigma = 0.05,
  stop_rate = 0.01,
  replicates = 5
)
t2 <- Sys.time()
t2-t1
# save(abc,file = "G:/R/Traisie-ABC/results/abc_allpars1.RData")
abc_df <- as.data.frame(abc)
hist(abc_df[,1], breaks = seq(0, 2, by = 0.05), col = "grey", main = "Lambda")
abline(v = 0.5, lty = 2, col = "blue", lwd = 2)

hist(abc_df[,2], breaks = seq(0, 2, by = 0.05), col = "grey", main = "Lambda")
abline(v = 0.5, lty = 2, col = "blue", lwd = 2)

params <- list()
for(i in 1:100){
  params[[i]] <- prior_gen()
}
