library(TraisieABC)
# treck the likelihood trace with different mu
calc_loglik_secsse <- function(params, datalist) {
  pars <- secsse::id_paramPos(traits = datalist$examTraits,num_concealed_states = 2)
  pars[[1]][] <- c(params[1],params[2],params[1],params[2])
  pars[[2]][] <- c(params[3],params[4],params[3],params[4])
  masterBlock <- matrix(c(params[5],params[6]),
                        ncol=2,nrow=2,byrow=TRUE)
  diag(masterBlock) <- NA
  q <-secsse::q_doubletrans(c(1,2),masterBlock,diff.conceal=F)
  q[1,3]<- q[2,4] <- q[3,1] <- q[4,2] <- 0
  pars[[3]][] <- q
  log_lik <- secsse::secsse_loglik(
    parameter = pars,
    phy = datalist$phy,
    traits = datalist$examTraits,
    num_concealed_states = 2,
    sampling_fraction = c(1,1),
    cond = "proper_cond"
  )
  return(log_lik)
}

## run
param_space <- readr::read_csv2("/home/p286026/TraisieABC/data/secsse_ABC_test1.csv")
param_space <- readr::read_csv2("data/secsse_ABC_test1.csv")
load(paste0("/home/p286026/TraisieABC/scripts/loglik_test/obs_ss_test1.RData"))
load(paste0("/home/p286026/TraisieABC/scripts/loglik_test/whole_df_MLE1.RData"))

## 3D mu1_mu2_loglik
for(i in 1:100) {
  message("set",i)
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_secsse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                       K = Inf,
                                       replicates = 1)
  ml_pars <- as.numeric(whole_df_MLE[i,7:12])
  loglik <- c() # fix mu2 only change mu1
  mu1 <- seq(0,1,0.025)
  mu2 <- seq(0,1,0.025)
  mu_comb <- data.frame(expand.grid(mu1,mu2))
  colnames(mu_comb) <-c("mu1","mu2")
  for(n in 1:nrow(mu_comb)){
    pars <- c(ml_pars[1],ml_pars[2],mu_comb[n,1],mu_comb[n,2],ml_pars[5],ml_pars[6])
    loglik[n] <- calc_loglik_secsse(pars,obs_sim[[1]])
  }
  pars_ll <- data.frame(mu_comb,loglik)
  save(pars_ll,file = paste0("/home/p286026/results/loglik1/pars_loglik",i,".RData"))
}