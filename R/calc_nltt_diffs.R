## input sim is a list with 1000 replicates (islands).

### obs_sim is available to be chosen from sim1[[i]] (i = obs_rep)
## If calculate the dispersion within the same param_set, sim1 = sim2;
#  if compare different param_sets, sim1 is not the same as sim2
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
