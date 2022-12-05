## test the estimation accuracy of secsse MLE
library(TraisieABC)
param_space <- readr::read_csv2("/home/p290559/TraisieABC/data/secsse_ABC.csv")

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

lam1_MLE<- c()
lam2_MLE <-c()
mu1_MLE <- c()
mu2_MLE <-c()
q12_MLE <- c()
q21_MLE <-c()
sim_list <- list()
state1<-c()
state2<-c()
total<-c()
## MLE, 500 reps for each parameter set
rep <- 1
while(rep <= 500) {
  message("rep",rep)
  seed_mle <-as.integer(Sys.time()) %% 1000000L * sample(1:10,1)
  set.seed(seed_mle)
  message("seed_mle: ", seed_mle)
  obs_sim_pars <- param_space[6,]
  obs_sim <- get_secsse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                       K = Inf,
                                       replicates = 1)
  startingpoint <- DDD::bd_ML(brts = ape::branching.times(obs_sim[[1]]$phy))
  # initparsopt <- obs_sim_pars
  initparsopt <- c(startingpoint$lambda0,startingpoint$lambda0,
                   startingpoint$mu0,startingpoint$mu0,
                   0.1,0.1)
  for(n in 1:6){
    initparsopt[n]<-exp(log(initparsopt[n]) +
                          stats::rnorm(1, 0, 1))
  }
  idparsopt = c(1,2,3,4,5,6)
  message("initial pars:", initparsopt)
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
    lam1_MLE[rep] <- MLE$MLpars[[1]][1]
    lam2_MLE[rep] <- MLE$MLpars[[1]][2]
    mu1_MLE[rep] <- MLE$MLpars[[2]][1]
    mu2_MLE[rep] <- MLE$MLpars[[2]][2]
    q12_MLE[rep] <- MLE$MLpars[[3]][1,2]
    q21_MLE[rep] <- MLE$MLpars[[3]][2,1]
    sim_list[[rep]] <- obs_sim[[1]]
    num_state1 <- length(which(obs_sim[[1]]$examTraits == 1))
    num_state2 <- length(which(obs_sim[[1]]$examTraits == 2))
    total_spec <- num_state1 + num_state2
    state1[rep] <- num_state1
    state2[rep] <- num_state2
    total[rep] <- total_spec
    rep <- rep + 1
  }
}

MLE_all <- data.frame(lam1_MLE,lam2_MLE,mu1_MLE,mu2_MLE,q12_MLE,q21_MLE,
                      state1,state2,total)
MLE_test_list <- list(MLE_all = MLE_all,
                      sim_list = sim_list)
save(MLE_test_list, file = paste0("/home/p290559/results/MLE_secsse_test6.RData"))

