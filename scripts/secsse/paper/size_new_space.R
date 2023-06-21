## check tree size of the new space
calc_num <- function(sim){
  num_state1 <- length(which(sim$obs_traits == 1))
  num_state2 <- length(which(sim$obs_traits == 2))
  total_spec <- num_state1 + num_state2
  tip_ratio <- max(num_state1,num_state2)/min(num_state1,num_state2)
  return(
    list(state1 = num_state1,
         state2 = num_state2,
         total_spec = total_spec,
         tip_ratio = tip_ratio)
  )
}

calc_epsilon_init_secsse_test <- function(sim){
  ss <- calc_num(sim[[1]]) #calc_ss_secsse_test
  eps_init <- as.numeric(unlist(ss)) * 1
  return(eps_init)
}

param_space_name <- paste0("secsse_ABC_test")
param_space <- load_param_space(param_space_name = param_space_name)
ss <- c()
obs_sim <- list()
set.seed(1)
for(i in 1:300){
  set.seed(i)
  message("set: ", i)
  obs_sim_pars <- c(0.4,0.4,0,0,0.1,0.1)
  obs_sim[[i]] <- get_secsse_sim(parameters = as.numeric(obs_sim_pars),
                                 pool_init_states = NULL,
                                 replicates = 1) ## replicates = 30
  init_epsilon <- calc_epsilon_init_secsse_test(sim = obs_sim[[i]])
  ss<-rbind(ss,init_epsilon)
}

plot(hist(ss[,3],breaks = 30))
