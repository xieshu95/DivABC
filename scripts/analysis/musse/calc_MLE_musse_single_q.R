library(DivABC)
param_space <- param_data <- load_param_space(param_space_name = paste0("musse_ABC_test"))
lam1_MLE<- c()
lam2_MLE <-c()
lam3_MLE <-c()
mu1_MLE <- c()
mu2_MLE <-c()
mu3_MLE <-c()
q_MLE <- c()
max_ll<- c()

create_ML_idpars <- function(traits,num_concealed_states) {
  idparslist <- secsse::id_paramPos(traits, num_concealed_states)
  idparslist[[1]][] <- c(1,2,3,1,2,3,1,2,3)
  idparslist[[2]][] <- c(4,5,6,4,5,6,4,5,6)

  t_ETD <- secsse::create_default_shift_matrix(state_names = c("1", "2", "3"),
                                               num_concealed_states = 3,
                                               mu_vector = c(4,5,6,4,5,6,4,5,6))
  q_ETD <- secsse::create_q_matrix(state_names = c("1", "2", "3"),
                                   num_concealed_states = 3,
                                   shift_matrix = t_ETD,
                                   diff.conceal = TRUE)

  params <- c(1:7,7,7,7,7,7,0,0,0,0,0,0)
  q <- secsse::fill_in(q_ETD, params)
  idparslist$Q <- q
  return(idparslist)
}

## MLE
for(i in 1:90) {
  message("set",i)
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  param_space_name <- "musse_ABC_test"
  obs_sim <- load_obs_sim(param_space_name = param_space_name)[[i]]
  startingpoint <- DDD::bd_ML(brts = ape::branching.times(obs_sim[[1]]$phy))

  rep <- 1
  while(rep < 2) {
    message("rep",rep)
    initparsopt <- c(startingpoint$lambda0,startingpoint$lambda0,startingpoint$lambda0,
                     startingpoint$mu0,startingpoint$mu0,startingpoint$mu0,
                     0.1)
    seed_mle <-as.integer(Sys.time()) %% 1000000L * sample(1:10,1)
    set.seed(seed_mle)
    message("seed_mle: ", seed_mle)
    for(n in 1:7){
      initparsopt[n]<-exp(log(initparsopt[n]) +
                            stats::rnorm(1, 0, 0.005))+ 0.00001
    }
    idparsopt = 1:7
    message("initial pars:", initparsopt)

    skip <- FALSE
    tryCatch(MLE <- secsse::secsse_ml(
      phy = obs_sim[[1]]$phy,
      traits = obs_sim[[1]]$obs_traits,
      num_concealed_states = 3,
      idparslist = create_ML_idpars(obs_sim[[1]]$obs_traits,3),
      idparsopt = idparsopt,
      initparsopt = initparsopt,
      idparsfix = c(0),
      parsfix = c(0),
      cond = 'proper_cond',
      root_state_weight = 'proper_weights',
      sampling_fraction = c(1,1,1),
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
      lam3_MLE <- c(lam3_MLE,MLE$MLpars[[1]][3])
      mu1_MLE <- c(mu1_MLE,MLE$MLpars[[2]][1])
      mu2_MLE <- c(mu2_MLE,MLE$MLpars[[2]][2])
      mu3_MLE <- c(mu3_MLE,MLE$MLpars[[2]][3])
      q_MLE <- c(q_MLE,MLE$MLpars[[3]][1,2])
      max_ll<- c(max_ll,MLE$ML)
    }
  }
}
MLE_all <- data.frame(lam1_MLE,lam2_MLE,lam3_MLE,mu1_MLE,mu2_MLE,mu3_MLE,q_MLE,max_ll)
save(MLE_all, file = paste0("results/musse/test_MLE_musse_single_q.RData"))

