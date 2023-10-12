# calculate MLE with different initials (10 replicates for each parameter set)
library(TraisieABC)
scenario <- "DAISIE_ABC_short_DI"
param_space <- load_param_space(scenario = "DAISIE_ABC_short_DI")
lac_MLE <- c()
mu_MLE <- c()
gam_MLE <- c()
laa_MLE <- c()
max_ll<- c()

init_lac<-c()
init_mu<-c()
init_gam<-c()
init_laa<-c()
init_K<-c()
for(i in 1:160) {
  message("set",i)
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- load_obs_sim(scenario = scenario)[[i]]
  rep <- 1
  while(rep < 2) {
    message("rep:", rep)
    initparsopt <- as.numeric(obs_sim_pars[c(1,2,5,3,4)]) + 0.00001
    seed_mle <-as.integer(Sys.time()) %% 1000000L * sample(1:10,1)
    set.seed(seed_mle)
    message("seed_mle: ", seed_mle)
    # for(n in 1:5){
    #   initparsopt[n]<-exp(log(initparsopt[n]) +
    #                         stats::rnorm(1, 0, 0.05))+0.00001
    # }
    message("initial pars:", initparsopt)
    MLE_allpars <- DAISIE::DAISIE_ML(
      datalist = obs_sim[[1]][[1]],
      initparsopt = initparsopt[c(1,2,4,5)],
      idparsopt = c(1,2,4,5),
      parsfix = Inf,
      idparsfix = 3,
      ddmodel = 0,
      cond = 1,
      methode = "lsodes",
      optimmethod = "subplex",
      jitter = 1e-5
    )

    # MLE_allpars <- DAISIE::DAISIE_ML(
    #   datalist = obs_sim[[1]][[1]],
    #   initparsopt = initparsopt,
    #   idparsopt = 1:5,
    #   parsfix = NULL,
    #   idparsfix = NULL,
    #   ddmodel = 11,
    #   cond = 1,
    #   methode = "lsodes",
    #   optimmethod = "subplex",
    #   jitter = 1e-5
    # )

    if(!is.na(MLE_allpars$lambda_c)) {
      init_lac<-c(init_lac,initparsopt[1])
      init_mu<-c(init_mu,initparsopt[2])
      # init_K<-c(init_K,initparsopt[3])
      init_gam<-c(init_gam,initparsopt[4])
      init_laa<-c(init_laa,initparsopt[5])

      lac_MLE<- c(lac_MLE,MLE_allpars$lambda_c)
      mu_MLE <-c(mu_MLE,MLE_allpars$mu)
      gam_MLE <- c(gam_MLE,MLE_allpars$gamma)
      laa_MLE <-c(laa_MLE,MLE_allpars$lambda_a)
      max_ll<- c(max_ll,MLE_allpars$loglik)
      # K <- c(K,MLE_allpars$K)
      rep <- rep + 1
    }
  }
}
MLE_all <- data.frame(lac_MLE, mu_MLE, gam_MLE, laa_MLE,max_ll, #K,init_K
                      init_lac,init_mu,init_gam,init_laa)

save(MLE_all, file = paste0("/home3/p286026/results/MLE_",seed_mle,".RData"))
