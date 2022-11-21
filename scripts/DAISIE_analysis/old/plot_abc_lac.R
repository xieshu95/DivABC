# plot lac_ss1~ss5
simulation_function <- function(parameters, replicates){
  sim <- list()
  for (j in seq_len(replicates)) {
    sim[[j]] <- DAISIE::DAISIE_sim_constant_rate(
      time = 5,
      M = 1000,
      pars = c(parameters,0.3,40,0.02,0.5),
      replicates = 1,
      sample_freq  = Inf,
      plot_sims = FALSE,
      verbose = FALSE,
      cond = 0
    )
  }
  return(sim)
}

set.seed(1)
obs_sim <- simulation_function(parameters = 0.5,
                               replicates = 20)

MLE_DD <- list()
for(i in 1:20){
  MLE_DD[[i]] <- DAISIE::DAISIE_ML(
    datalist = obs_sim[[i]][[1]],
    initparsopt = 0.5,
    idparsopt = 1,
    parsfix = c(0.3,40,0.02,0.5),
    idparsfix = 2:5,
    ddmodel = 11,
    cond = 0,
    eqmodel = 0,
    x_E = 0.95,
    x_I = 0.98,
    tol = c(1e-04, 1e-05, 1e-07),
    maxiter = 1000 * round((1.25) ^ 1),
    methode = "lsodes",
    optimmethod = "subplex"
  )
}
MLE_lac<- c()
for(i in 1:20){
  MLE_lac[i] <- MLE_DD[[i]]$lambda_c
}
save(MLE_lac, file=paste0("G:/R/Traisie-ABC/results/ABC_MLE/MLE_lac_DD_highrates.RData"))


## MCMC
load("G:/R/Traisie-ABC/results/MCMC_version2/MCMC/results/mcmc_lac_10reps.RData")
mcmc_com <- c()
for(i in 1:10){
  mcmc_rep <- mcmc_list[[i]]
  mcmc_com <- rbind(mcmc_com,mcmc_rep)
}
mcmca<- coda::as.mcmc(mcmc_com)
lac_mcmc <- median(mcmca)



# plot all
hist(abc, breaks = seq(0, 2, by = 0.05), col = "grey", main = "SS5-lac")
abline(v = 0.5, lty = 2, col = "blue", lwd = 2)
abline(v = mean(MLE_lac), lty = 2, col = "green", lwd = 2)
abline(v = lac_mcmc, lty = 2, col = "red", lwd = 2)
legend("right", c("ML", "True","MCMC"),
       lty = c(2, 2, 2),
       col = c("green", "blue","red"), lwd = 2)


# plot mu_ss1~ss5
simulation_function <- function(parameters, replicates){
  sim <- list()
  for (j in seq_len(replicates)) {
    sim[[j]] <- DAISIE::DAISIE_sim_constant_rate(
      time = 5,
      M = 1000,
      pars = c(0.5,parameters,40,0.02,0.5),
      replicates = 1,
      sample_freq  = Inf,
      plot_sims = FALSE,
      verbose = FALSE,
      cond = 0
    )
  }
  return(sim)
}

set.seed(1)
obs_sim <- simulation_function(parameters = 0.3,
                               replicates = 20)

MLE_DD <- list()
for(i in 1:20){
  MLE_DD[[i]] <- DAISIE::DAISIE_ML(
    datalist = obs_sim[[i]][[1]],
    initparsopt = 0.3,
    idparsopt = 2,
    parsfix = c(0.5,40,0.02,0.5),
    idparsfix = c(1,3,4,5),
    ddmodel = 11,
    cond = 0,
    eqmodel = 0,
    x_E = 0.95,
    x_I = 0.98,
    tol = c(1e-04, 1e-05, 1e-07),
    maxiter = 1000 * round((1.25) ^ 1),
    methode = "lsodes",
    optimmethod = "subplex"
  )
}
MLE_mu<- c()
for(i in 1:20){
  MLE_mu[i] <- MLE_DD[[i]] $mu
}



hist(abc, breaks = seq(0, 1, by = 0.05), col = "grey", main = "Mu")
abline(v = 0.3, lty = 2, col = "blue", lwd = 2)
abline(v = mean(MLE_mu), lty = 2, col = "green", lwd = 2)
legend("right", c("ML", "True"),
       lty = c(2, 2),
       col = c("green", "blue"), lwd = 2)

