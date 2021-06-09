set.seed(1)
clade_size_sd <- function(sim){
  clade_size <- c()
  if (length(sim[[1]][[1]]) == 1){
    clade_size_sd <- 0
  } else {
    for(i in 2:length(sim[[1]][[1]])){ ##clades
        clade_size[i - 1] <- length(sim[[1]][[1]][[i]]$branching_times) - 1
    }
    if(length(clade_size) == 1){
      clade_size_sd <- 0
    } else{
      clade_size_sd <- sd(clade_size)
    }
  }
  return(clade_size_sd)
}

calc_clade_size_error <- function(sim_1, sim_2){
  sim1_cs_sd <- clade_size_sd(sim_1)
  sim2_cs_sd <- clade_size_sd(sim_2)
  clade_size_error <- abs(sim1_cs_sd - sim2_cs_sd)
  return(clade_size_error)
}

nltt_within_param <- function(sim1, sim2, replicates){
  spec_nltt_error <- c()
  num_spec_error <- c()
  num_col_error <- c()
  endemic_nltt_error <- c()
  nonendemic_nltt_error <- c()
  for(i in 1:replicates){
    s <- DAISIErobustness:::calc_error(sim_1 = sim1[[1]],
                                       sim_2 = sim2[[i]],
                                       replicates = 1,
                                       distance_method = "abs")
    spec_nltt_error <- append(spec_nltt_error,
                              s$spec_nltt_error)
    num_spec_error <- append(num_spec_error,
                             s$num_spec_error)
    num_col_error <- append(num_col_error,
                            s$num_col_error)
    endemic_nltt_error <- append(endemic_nltt_error,
                                 s$endemic_nltt_error)
    nonendemic_nltt_error <- append(nonendemic_nltt_error,
                                    s$nonendemic_nltt_error)
  }
  clade_size_error <- calc_clade_size_error(sim_1 = sim1,
                                            sim_2 = sim2)
  list_s <- list(spec_nltt_error = spec_nltt_error,
                 endemic_nltt_error = endemic_nltt_error,
                 nonendemic_nltt_error = nonendemic_nltt_error,
                 num_spec_error = num_spec_error,
                 num_col_error = num_col_error,
                 clade_size_error = clade_size_error)
  return(list_s)
}

# create a function to sample parameters from prior distribution
##first step set only one variable(lac)
prior_gen <- function(){
  lac <- stats::runif(1,0,0.5)
  mu <- stats::runif(1,0,0.5)
  gam <- stats::runif(1,0,0.01)
  laa <- stats::runif(1,0,0.5)
  param <- c(lac,mu,gam,laa)
  return(param)
}

prior_dens <- function(x) {
  return(stats::dunif(x[1],0,0.5) * stats::dunif(x[2],0,0.5) * stats::dunif(x[3],0,0.01) * stats::dunif(x[4],0,0.5))
}

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
      cond = 1
    )
  }
  return(sim)
}

obs_sim <- simulation_function(parameters = c(0.2,0.1,0.005,0.1),
                               replicates = 1)

abc_smc_nltt <- function( # nolint indeed a complex function
  datalist,
  simulation_function,
  init_epsilon_values,
  prior_generating_function,
  prior_density_function,
  number_of_particles = 1000,
  sigma = 0.05,
  stop_rate = 1e-5,
  replicates,  ## simulation replicates for each parameter set
  num_iterations
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
  for (i in 1:num_iterations) {
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
        df_stats <- nltt_within_param (sim1 = datalist,
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
        median_df <- base::lapply(df_stats,median)
        for (k in seq_along(median_df)) {
          if (as.numeric(median_df[k]) > epsilon[i, k]) {
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

abc <- abc_smc_nltt(
  datalist = obs_sim,
  simulation_function = simulation_function,
  init_epsilon_values = c(150,150,50,150,50,10),
  prior_generating_function = prior_gen,
  prior_density_function = prior_dens,
  number_of_particles = 100,
  sigma = 0.05,
  stop_rate = 0.01,
  replicates = 1,
  num_iterations = 2
)
