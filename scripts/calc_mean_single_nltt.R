### add a function to calculate the mean(nedian) of 500 nltts !!!!

calc_mean_single_nltt <- function(sim1,sim2,replicates){
  spec_nltt_mean <- c()
  num_spec_mean <- c()
  num_col_mean <- c()
  endemic_nltt_mean <- c()
  nonendemic_nltt_mean <- c()
  for (i in 1:replicates) {
    nltt_list <- nltt_within_param (obs_rep = i,
                                    sim1 = sim1,
                                    sim2 = sim2,
                                    replicates = replicates)

    spec_nltt_mean[i] <- mean(nltt_list$spec_nltt_error)
    num_spec_mean[i] <- mean(nltt_list$num_spec_error)
    num_col_mean[i] <- mean(nltt_list$num_col_error)
    endemic_nltt_mean[i] <- mean(nltt_list$endemic_nltt_error)
    nonendemic_nltt_mean[i] <- mean(nltt_list$nonendemic_nltt_error)

  }
  list_s_mean <- list(spec_nltt_mean = spec_nltt_mean,
                      num_spec_mean = num_spec_mean,
                      num_col_mean = num_col_mean,
                      endemic_nltt_mean = endemic_nltt_mean,
                      nonendemic_nltt_mean = nonendemic_nltt_mean)
  return(list_s_mean)
}

mean_nltt <- calc_mean_single_nltt(sim1 = sim_gam1,
                                   sim2 = sim_gam1,
                                   replicates = 500)
plot(mean_nltt$spec_nltt_mean)
plot(mean_nltt$num_spec_mean)
plot(mean_nltt$num_col_mean)
plot(mean_nltt$endemic_nltt_mean)
plot(mean_nltt$nonendemic_nltt_mean)
