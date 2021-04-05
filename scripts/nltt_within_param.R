## input sim is a list with 1000 replicates (islands).

### obs_sim is available to be chosen from sim1[[i]] (i = obs_rep)
## If calculate the dispersion within the same param_set, sim1 = sim2;
#  if compare different param_sets, sim1 is not the same as sim2

#' @param obs_rep If obs_sim has more than one replicate, use "obs_rep" to
#'   select which replicate as the reference observed simulation.
#' @param sim1 A datalist which used to create observed data.
#' @param sim2 A datalist simulated using DAISIE simulation model.
#' @param replicates The number of replicates used for calculating summary
#'   statistics.

#output:spec_nltt_error,num_spec_error,num_col_error,endemic_nltt_error,
#nonendemic_nltt_error


nltt_within_param <- function(obs_rep, sim1, sim2, replicates){
  if (length(sim1) == 1) {
    obs_sim <- sim1
  } else {
    obs_sim <- sim1[[obs_rep]]
  }
  spec_nltt_error <- c()
  num_spec_error <- c()
  num_col_error <- c()
  endemic_nltt_error <- c()
  nonendemic_nltt_error <- c()
  for(i in 1:replicates){
    parallel_sim <- sim2[[i]]
    s <- DAISIErobustness:::calc_error(sim_1 = obs_sim,
                    sim_2 = parallel_sim,
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
  list_s <- list(spec_nltt_error = spec_nltt_error,
                 num_spec_error = num_spec_error,
                 num_col_error = num_col_error,
                 endemic_nltt_error = endemic_nltt_error,
                 nonendemic_nltt_error = nonendemic_nltt_error)
  return(list_s)
}


### length(sim1)>= length(sim2), alaculate nltts between parallel replicate
nltt_within_param_parallel <- function(sim1, sim2, replicates){
  spec_nltt_error <- c()
  num_spec_error <- c()
  num_col_error <- c()
  endemic_nltt_error <- c()
  nonendemic_nltt_error <- c()
  for(i in 1:replicates){
    obs_sim <- sim1[[i]]
    parallel_sim <- sim2[[i]]
    s <- DAISIErobustness:::calc_error(sim_1 = obs_sim,
                                       sim_2 = parallel_sim,
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
  list_s <- list(spec_nltt_error = spec_nltt_error,
                 num_spec_error = num_spec_error,
                 num_col_error = num_col_error,
                 endemic_nltt_error = endemic_nltt_error,
                 nonendemic_nltt_error = nonendemic_nltt_error)
  return(list_s)
}


### example
# simulation_function <- function(parameters, replicates){
#   sim <- list()
#   for (j in seq_len(replicates)) {
#     sim[[j]] <- DAISIE::DAISIE_sim_constant_rate(
#       time = 5,
#       M = 1000,
#       pars = c(parameters,0.3,40,0.02,0.5),
#       replicates = 1,
#       sample_freq  = Inf,
#       plot_sims = FALSE,
#       verbose = TRUE,
#       cond = 0
#     )
#   }
#   return(sim)
# }
#
# set.seed(1)
# obs_sim1 <- simulation_function(parameters = 0.5,
#                                 replicates = 5)
# obs_sim2 <- simulation_function(parameters = 1,
#                                 replicates = 5)
# nltt_within_param_parallel(sim1 = obs_sim1, sim2 = obs_sim2, replicates = 5)

# param1 <- sim_gam1
# a <- nltt_within_param(sim1 = param1,sim2 = param1, replicates = 500)
# spec_nltt <- a$spec_nltt_error
# num_spec <- a$num_spec_error
# num_col <- a$num_col_error
# endemic_nltt <- a$endemic_nltt_error
# nonendemic_nltt <- a$nonendemic_nltt_error
# plot(spec_nltt)
# plot(num_spec)
# plot(num_col)
# plot(endemic_nltt)
# plot(nonendemic_nltt)





#
#
# param100<-output
# b <- nltt_within_param(sim = param100,replicates = 1000)
# spec_nltt_100 <- b$spec_nltt_error
# num_spec_100 <- b$num_spec_error
# num_col_100 <- b$num_col_error
# endemic_nltt_100 <- b$endemic_nltt_error
# nonendemic_nltt_100 <- b$nonendemic_nltt_error
# plot(spec_nltt_100)
# plot(num_spec_100)
# plot(num_col_100)
# plot(endemic_nltt_100)
# plot(nonendemic_nltt_100)
