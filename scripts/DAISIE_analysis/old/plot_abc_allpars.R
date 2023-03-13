## plot allpars
simulation_function <- function(parameters, replicates){
  sim <- list()
  for (j in seq_len(replicates)) {
    sim[[j]] <- DAISIE::DAISIE_sim_constant_rate(
      time = 5,
      M = 1000,
      pars = c(parameters[1],parameters[2],40,parameters[3],parameters[4]),
      replicates = 1,
      sample_freq  = Inf,
      plot_sims = FALSE,
      verbose = FALSE,
      cond = 0
    )
  }
  return(sim)
}
t1 <- Sys.time()
set.seed(1)
obs_sim <- simulation_function(parameters = c(0.3,0.2,0.008,0.2),
                               replicates = 100)
t2 <- Sys.time()
dt <- t2 - t1
dt
save(obs_sim, file=paste0("G:/R/Traisie-ABC/results/ABC_MLE/obs_sim_DD.RData"))
# load(file=paste0("G:/R/Traisie-ABC/results/ABC_MLE/obs_sim_DI.RData"))

MLE_DD <- list()
for(i in 1:50){
  MLE_DD[[i]] <- DAISIE::DAISIE_ML(
    datalist = obs_sim[[i]][[1]],
    initparsopt = c(0.3,0.2,0.008,0.2),
    idparsopt = c(1,2,4,5),
    parsfix = 40,
    idparsfix = 3,
    ddmodel = 11,
    cond = 0,
    eqmodel = 0,
    x_E = 0.95,
    x_I = 0.98,
    tol = c(1e-04, 1e-05, 1e-07),
    maxiter = 1000 * round((1.25) ^ 4),
    methode = "lsodes",
    optimmethod = "subplex"
  )
}
save(MLE_DD, file=paste0("G:/R/Traisie-ABC/results/ABC_MLE/MLE_DD.RData"))
load(file=paste0("G:/R/Traisie-ABC/results/ABC_MLE/MLE_DD.RData"))


MLE_DI <- list()
for(i in 1:50){
  MLE_DI[[i]] <- DAISIE::DAISIE_ML(
    datalist = obs_sim[[i]][[1]],
    initparsopt = c(0.3,0.2,0.008,0.2),
    idparsopt = c(1,2,4,5),
    parsfix = Inf,
    idparsfix = 3,
    ddmodel = 0,
    cond = 0,
    eqmodel = 0,
    x_E = 0.95,
    x_I = 0.98,
    tol = c(1e-04, 1e-05, 1e-07),
    maxiter = 1000 * round((1.25) ^ 4),
    methode = "lsodes",
    optimmethod = "subplex"
  )
}
save(MLE_DI, file=paste0("G:/R/Traisie-ABC/results/ABC_MLE/MLE_DI.RData"))
load(file=paste0("G:/R/Traisie-ABC/results/ABC_MLE/MLE_DI.RData"))

#### only make plots
## load MLE results
load(file=paste0("G:/R/Traisie-ABC/results/ABC_MLE/MLE_DI_low_rates.RData"))
lac_MLE<- c()
mu_MLE <-c()
gam_MLE <- c()
laa_MLE <-c()
for(i in 1:100){
  lac_MLE[i] <- MLE_DI[[i]]$lambda_c
  mu_MLE[i] <- MLE_DI[[i]]$mu
  gam_MLE[i] <- MLE_DI[[i]]$gamma
  laa_MLE[i] <- MLE_DI[[i]]$lambda_a
}

## Load MCMC results
# DI
load("G:/R/Traisie-ABC/results/MCMC_version4/results/mcmc_allpars_10reps.RData")
# DD
# load("G:/R/Traisie-ABC/results/MCMC_version5/results/mcmc_allpars_10reps.RData")
mcmc_com <- c()
for(i in 1:10){
  mcmc_rep <- mcmc_list[[i]]
  mcmc_com <- rbind(mcmc_com,mcmc_rep)
}
mcmca<- coda::as.mcmc(mcmc_com)
lac_mcmc <- median(mcmca[,1])
mu_mcmc <- median(mcmca[,2])
gam_mcmc <- median(mcmca[,3])
laa_mcmc <- median(mcmca[,4])

## load abc data
i = 5
n = 5
ddmodel = "DI"
### combine 10 reps and plot
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep1.RData"))
abc1<- abc
# load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep2.RData"))
# abc2<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep3.RData"))
abc3<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep4.RData"))
abc4<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep5.RData"))
abc5<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep6.RData"))
abc6<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep7.RData"))
abc7<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep8.RData"))
abc8<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep9.RData"))
abc9<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep10.RData"))
abc10<- abc

n = 7
ddmodel = "DI"
### combine 10 reps and plot
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep1.RData"))
abc11<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep2.RData"))
abc12<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep3.RData"))
abc13<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep4.RData"))
abc14<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep5.RData"))
abc15<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep6.RData"))
abc16<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep7.RData"))
abc17<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep8.RData"))
abc18<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep9.RData"))
abc19<- abc
load(paste0("G:/R/Traisie-ABC/results/ABC_version",n,"_highrates_",ddmodel,"_10reps/results/allpars_",ddmodel,"_ss",i,"_rep10.RData"))
abc20<- abc
# abc <- rbind(abc1,abc2,abc3,abc4,abc6,abc7,abc9)
# abc <- rbind(abc1,abc2,abc4,abc5,abc6,abc7,abc8,abc9,abc10)
# abc <- rbind(abc1,abc2,abc3,abc4,abc5,abc6,abc7,abc8,abc9,abc10,
#              abc11,abc12,abc13,abc14,abc15,abc16,abc17,abc18,abc19,abc20)
abc <- rbind(abc1,abc3,abc4,abc5,abc6,abc7,abc8,abc9,abc10)

lac_abc <- as.data.frame(abc)[,1]
mu_abc <- as.data.frame(abc)[,2]
gam_abc <- as.data.frame(abc)[,3]
laa_abc <- as.data.frame(abc)[,4]


png(paste0("G:/R/Traisie-ABC/plots/ABC_MCMC_MLE_lowrates_DI/ss",i,"_lac.png"))
hist(lac_abc, breaks = seq(0, 0.5, by = 0.01), col = "grey", main = "lac")
abline(v = 0.2, lty = 2, col = "blue", lwd = 2)
abline(v = mean(lac_MLE), lty = 2, col = "green", lwd = 2)
abline(v = lac_mcmc, lty = 2, col = "red", lwd = 2)
legend("right", c("MLE", "True","MCMC"),
       lty = c(2, 2, 2),
       col = c("green", "blue", "red"), lwd = 2)
dev.off()

png(paste0("G:/R/Traisie-ABC/plots/ABC_MCMC_MLE_lowrates_DI/ss",i,"_mu.png"))
hist(mu_abc, breaks = seq(0, 0.5, by = 0.01), col = "grey", main = "mu")
abline(v = 0.1, lty = 2, col = "blue", lwd = 2)
abline(v = mean(mu_MLE), lty = 2, col = "green", lwd = 2)
abline(v = mu_mcmc, lty = 2, col = "red", lwd = 2)
legend("right", c("MLE", "True","MCMC"),
       lty = c(2, 2, 2),
       col = c("green", "blue", "red"), lwd = 2)
dev.off()

png(paste0("G:/R/Traisie-ABC/plots/ABC_MCMC_MLE_lowrates_DI/ss",i,"_gam.png"))
hist(gam_abc, breaks = seq(0, 0.01, by = 0.0002), col = "grey", main = "gam")
abline(v = 0.005, lty = 2, col = "blue", lwd = 2)
abline(v = mean(gam_MLE), lty = 2, col = "green", lwd = 2)
abline(v = gam_mcmc, lty = 2, col = "red", lwd = 2)
legend("right", c("MLE", "True","MCMC"),
       lty = c(2, 2, 2),
       col = c("green", "blue", "red"), lwd = 2)
dev.off()

png(paste0("G:/R/Traisie-ABC/plots/ABC_MCMC_MLE_lowrates_DI/ss",i,"_laa.png"))
hist(laa_abc, breaks = seq(0, 0.5, by = 0.01), col = "grey", main = "laa")
abline(v = 0.1, lty = 2, col = "blue", lwd = 2)
abline(v = mean(laa_MLE), lty = 2, col = "green", lwd = 2)
abline(v = laa_mcmc, lty = 2, col = "red", lwd = 2)
legend("right", c("MLE", "True","MCMC"),
       lty = c(2, 2, 2),
       col = c("green", "blue", "red"), lwd = 2)
dev.off()

