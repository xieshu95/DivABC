library(DivABC)
library(diversitree)
param_space <- param_data <- load_param_space(param_space_name = paste0("geosse_ABC_test"))
lam1_MLE<- c()
lam2_MLE <-c()
lam3_MLE <-c()
mu1_MLE <- c()
mu2_MLE <-c()
q1_MLE <- c()
q2_MLE <-c()
max_ll<- c()

for(i in 1:200) {
  message("set",i)
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  param_space_name <- "geosse_ABC_test"
  obs_sim <- load_obs_sim(param_space_name = param_space_name)[[i]]

  lik.g <- diversitree::make.geosse(obs_sim[[1]],
                                    obs_sim[[1]]$tip.state)
  rep <- 1
  while(rep < 2) {
    message("rep",rep)
    seed_mle <-as.integer(Sys.time()) %% 1000000L * sample(1:10,1)
    set.seed(seed_mle)
    message("seed_mle: ", seed_mle)
    initparsopt <- starting.point.geosse(obs_sim[[1]])
    message("initial pars:", initparsopt)

    skip <- FALSE
    tryCatch(MLE <- find.mle(lik.g, initparsopt, method = "subplex"), error=function(e) {
      print("Optimization has not converged. Try again with different initial values.")
      skip <<- TRUE
    })
    if(skip == FALSE){
      rep <- rep + 1
      lam1_MLE <- c(lam1_MLE,MLE$par[1])
      lam2_MLE <- c(lam2_MLE,MLE$par[2])
      lam3_MLE <- c(lam3_MLE,MLE$par[3])
      mu1_MLE <- c(mu1_MLE,MLE$par[4])
      mu2_MLE <- c(mu2_MLE,MLE$par[5])
      q1_MLE <- c(q1_MLE,MLE$par[6])
      q2_MLE <- c(q2_MLE,MLE$par[7])
      max_ll<- c(max_ll,MLE$lnLik)
    }
  }
}
MLE_all <- data.frame(lam1_MLE,lam2_MLE,lam3_MLE,mu1_MLE,mu2_MLE,q1_MLE,q2_MLE,max_ll)
save(MLE_all, file = paste0("results/geosse/test_MLE_geosse.RData"))

