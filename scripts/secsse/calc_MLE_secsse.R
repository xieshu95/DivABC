param_space <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC_long.csv")
lam1_MLE<- c()
lam2_MLE <-c()
mu1_MLE <- c()
mu2_MLE <-c()
q12_MLE <- c()
q21_MLE <-c()


create_ML_idpars <- function(traits,num_concealed_states) {
  idparslist <- secsse::id_paramPos(traits, num_concealed_states)
  idparslist[[1]][] <- c(1,2,1,2)
  idparslist[[2]][] <- c(3,4,3,4)
  masterBlock <- matrix(c(5,6),
                        ncol=2,nrow=2,byrow=TRUE)
  diag(masterBlock) <- NA
  q <-secsse::q_doubletrans(c(1,2),masterBlock,diff.conceal=F)
  q[1,3]<- q[2,4] <- q[3,1] <- q[4,2] <- 0
  states <- names(idparslist$mus)
  dimnames(q)[1:2]<-list(states)
  idparslist$Q <- q

  return(idparslist)
}

## MLE, 5 different initial for each obs-data
for(i in 1:70) {
  message("set",i)
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_secsse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                       K = Inf,
                                       replicates = 1)
  startingpoint <- DDD::bd_ML(brts = ape::branching.times(obs_sim[[1]]$phy))

  rep <- 1
  while(rep <= 5) {
    message("rep",rep)
    # initparsopt <- obs_sim_pars
    initparsopt <- c(startingpoint$lambda0,startingpoint$lambda0,
                     startingpoint$mu0,startingpoint$mu0,
                     0.1,0.1)
    for(n in 1:6){
      initparsopt[n]<-exp(log(initparsopt[n]) +
                            stats::rnorm(1, 0, 1))
    }
    idparsopt = c(1,2,3,4,5,6)

    skip <- FALSE
    tryCatch(MLE <- secsse::secsse_ml(
      phy = obs_sim[[1]]$phy,
      traits = obs_sim[[1]]$examTraits,
      num_concealed_states = 2,
      idparslist = create_ML_idpars(obs_sim[[1]]$examTraits,2),
      idparsopt = idparsopt,
      initparsopt = initparsopt,
      idparsfix = c(0),
      parsfix = c(0),
      cond = 'proper_cond',
      root_state_weight = 'proper_weights',
      sampling_fraction = c(1,1),
      tol = c(1e-04, 1e-05, 1e-07),
      maxiter = 1000 * round((1.25)^length(idparsopt)),
      optimmethod = 'subplex',
      num_cycles = 1,
      verbose = FALSE
    ), error=function(e) {
      print("Optimization has not converged. Try again with different initial values.")
      skip <<- TRUE
    })
    if(skip == FALSE){
      rep <- rep + 1
      lam1_MLE <- c(lam1_MLE,MLE$MLpars[[1]][1])
      lam2_MLE <- c(lam2_MLE,MLE$MLpars[[1]][2])
      mu1_MLE <- c(mu1_MLE,MLE$MLpars[[2]][1])
      mu2_MLE <- c(mu2_MLE,MLE$MLpars[[2]][2])
      q12_MLE <- c(q12_MLE,MLE$MLpars[[3]][1,2])
      q21_MLE <- c(q21_MLE,MLE$MLpars[[3]][2,1])
    }
  }
}
param_data2<-param_space[rep(seq_len(nrow(param_space)), each=5),]
MLE_all <- data.frame(lam1_MLE,lam2_MLE,
                      mu1_MLE,mu2_MLE,q12_MLE,q21_MLE)
save(MLE_all,file = "G:/results/project 2/tip_info/round4/adap_secsse/MLE_secsse_ABC.RData")

load("G:/results/project 2/tip_info/round4/adap_secsse/MLE_secsse_ABC.RData")



## MLE for only one set of initial value
for(i in 1:70) {
  message("set",i)
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_secsse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                       K = Inf,
                                       replicates = 1)
  startingpoint <- DDD::bd_ML(brts = ape::branching.times(obs_sim[[1]]$phy))
  # initparsopt <- obs_sim_pars
  initparsopt <- c(startingpoint$lambda0,startingpoint$lambda0,
                   startingpoint$mu0,startingpoint$mu0,
                   0.1,0.1)
  idparsopt = c(1,2,3,4,5,6)

  tryCatch(MLE <- secsse::secsse_ml(
    phy = obs_sim[[1]]$phy,
    traits = obs_sim[[1]]$examTraits,
    num_concealed_states = 2,
    idparslist = create_ML_idpars(obs_sim[[1]]$examTraits,2),
    idparsopt = idparsopt,
    initparsopt = initparsopt,
    idparsfix = c(0),
    parsfix = c(0),
    cond = 'proper_cond',
    root_state_weight = 'proper_weights',
    sampling_fraction = c(1,1),
    tol = c(1e-04, 1e-05, 1e-07),
    maxiter = 1000 * round((1.25)^length(idparsopt)),
    optimmethod = 'subplex',
    num_cycles = 1,
    verbose = FALSE
  ), error=function(e) {
    print("Optimization has not converged. Try again with different initial values.")
  })

  lam1_MLE[i] <- MLE$MLpars[[1]][1]
  lam2_MLE[i] <- MLE$MLpars[[1]][2]
  mu1_MLE[i] <- MLE$MLpars[[2]][1]
  mu2_MLE[i] <- MLE$MLpars[[2]][2]
  q12_MLE[i] <- MLE$MLpars[[3]][1,2]
  q21_MLE[i] <- MLE$MLpars[[3]][2,1]
}
# MLE_all <- data.frame(param_space,lam1_MLE,lam2_MLE,
#                       mu1_MLE,mu2_MLE,q12_MLE,q21_MLE)
# MLE_all$dlam <- (MLE_all$lam2-MLE_all$lam1)/(MLE_all$lam2+MLE_all$lam1)
# MLE_all$dlam_MLE <- (MLE_all$lam2_MLE-MLE_all$lam1_MLE)/(MLE_all$lam2_MLE+MLE_all$lam1_MLE)
# MLE_all$dmu <- (MLE_all$mu2-MLE_all$mu1)/(MLE_all$mu2+MLE_all$mu1)
# MLE_all$dmu_MLE <- (MLE_all$mu2_MLE-MLE_all$mu1_MLE)/(MLE_all$mu2_MLE+MLE_all$mu1_MLE)
# MLE_all$dq <- (MLE_all$q12-MLE_all$q21)/(MLE_all$q12+MLE_all$q21)
# MLE_all$dq_MLE <- (MLE_all$q12_MLE-MLE_all$q21_MLE)/(MLE_all$q12_MLE+MLE_all$q21_MLE)
#
# MLE_all$net_div1 <- (MLE_all$lam1-MLE_all$mu1)
# MLE_all$net_div2 <- (MLE_all$lam2-MLE_all$mu2)
# MLE_all$net_div_MLE1 <- (MLE_all$lam1_MLE-MLE_all$mu1_MLE)
# MLE_all$net_div_MLE2 <- (MLE_all$lam2_MLE-MLE_all$mu2_MLE)
#
# MLE_all$dmu[1:40] <- 0
# save(MLE_all,file = "G:/results/project 2/tip_info/round4/secsse_long_2/MLE_secsse_ABC.RData")

load("G:/results/project 2/tip_info/round4/secsse_long_2/MLE_secsse_ABC.RData")


