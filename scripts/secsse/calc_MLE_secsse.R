param_space <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC_test1.csv")
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


for(i in 1:200) {
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
 MLE_all <- data.frame(param_space,lam1_MLE,lam2_MLE,
                      mu1_MLE,mu2_MLE,q12_MLE,q21_MLE)
save(MLE_all,file = "G:/results/project 2/tip_info/round4/secsse_long_2/MLE_secsse_ABC.RData")

load("G:/results/project 2/tip_info/round4/secsse_long_2/MLE_secsse_ABC.RData")
