### create simulations for plotting nltt
set.seed(1)
t1 <- Sys.time()
replicates <- 500
novel_sim <- list()
for (i in seq_len(replicates)) {
  novel_sim[[i]] <- DAISIE::DAISIE_sim_constant_rate(
    time = 5,
    M = 1000,
    pars = c(0.4,0.8,40,0.01,0.5),
    replicates = 1,
    sample_freq  = Inf,
    plot_sims = FALSE,
    verbose = TRUE,
    cond = 5
  )
}
t2 <- Sys.time()
dt <- t2 - t1
dt
sim_gam2 <- novel_sim
save(sim_gam2,file = "G:/R/Traisie-ABC/results/sim_gam2.RData")

set.seed(1)
t1 <- Sys.time()
replicates <- 500
novel_sim <- list()
for (i in seq_len(replicates)) {
  novel_sim[[i]] <- DAISIE::DAISIE_sim_constant_rate(
    time = 5,
    M = 1000,
    pars = c(0.4,0.8,40,0.03,0.5),
    replicates = 1,
    sample_freq  = Inf,
    plot_sims = FALSE,
    verbose = FALSE,
    cond = 5
  )
}
t2 <- Sys.time()
dt <- t2 - t1
dt
sim_gam4 <- novel_sim
save(sim_gam4,file = "G:/R/Traisie-ABC/results/sim_gam4.RData")

set.seed(1)
t1 <- Sys.time()
replicates <- 500
novel_sim <- list()
for (i in seq_len(replicates)) {
  novel_sim[[i]] <- DAISIE::DAISIE_sim_constant_rate(
    time = 5,
    M = 1000,
    pars = c(0.4,0.8,40,0.04,0.5),
    replicates = 1,
    sample_freq  = Inf,
    plot_sims = FALSE,
    verbose = FALSE,
    cond = 5
  )
}
t2 <- Sys.time()
dt <- t2 - t1
dt
sim_gam5 <- novel_sim
save(sim_gam5,file = "G:/R/Traisie-ABC/results/sim_gam5.RData")


### Create simulation results with different lac for Traisie ABC analysis
set.seed(1)
lac <- runif(50,0,1)
replicates <- 100
novel_sim <- list()
for (i in 1:length(lac)) {
  for (j in seq_len(replicates)) {
    novel_sim[[j]] <- DAISIE::DAISIE_sim_constant_rate(
      time = 5,
      M = 1000,
      pars = c(lac[i],0.8,Inf,0.02,0.5),
      replicates = 1,
      sample_freq  = Inf,
      plot_sims = FALSE,
      verbose = FALSE,
      cond = 5
    )
  }
  file_name <- paste0("sim_lac_Inf",i,".RData")
  save(novel_sim,file = paste0("G:/R/Traisie-ABC/results/lac_Inf/",file_name))
}
### Create simulation results with different mu for Traisie ABC analysis
set.seed(1)
mu <- runif(50,0,1)
replicates <- 100
novel_sim <- list()
for (i in 1:length(mu)) {
  for (j in seq_len(replicates)) {
    novel_sim[[j]] <- DAISIE::DAISIE_sim_constant_rate(
      time = 5,
      M = 1000,
      pars = c(0.4,mu[i],Inf,0.02,0.5),
      replicates = 1,
      sample_freq  = Inf,
      plot_sims = FALSE,
      verbose = FALSE,
      cond = 5
    )
  }
  file_name <- paste0("sim_mu_Inf",i,".RData")
  save(novel_sim,file = paste0("G:/R/Traisie-ABC/results/mu_Inf/",file_name))
}
### Create simulation results with different gam for Traisie ABC analysis
t1 <- Sys.time()
set.seed(1)
gam <- runif(50,0,0.1)
replicates <- 100
novel_sim <- list()
for (i in 1:length(gam)) {
  for (j in seq_len(replicates)) {
    novel_sim[[j]] <- DAISIE::DAISIE_sim_constant_rate(
      time = 5,
      M = 1000,
      pars = c(0.4,0.8,Inf,gam[i],0.5),
      replicates = 1,
      sample_freq  = Inf,
      plot_sims = FALSE,
      verbose = FALSE,
      cond = 5
    )
  }
  file_name <- paste0("sim_gam_Inf",i,".RData")
  save(novel_sim,file = paste0("G:/R/Traisie-ABC/results/gam_Inf/",file_name))
}
t2 <- Sys.time()
dt <- t2 - t1
dt
### Create simulation results with different laa for Traisie ABC analysis
t3 <- Sys.time()
set.seed(1)
laa <- runif(50,0,1)
replicates <- 100
novel_sim <- list()
for (i in 1:length(laa)) {
  for (j in seq_len(replicates)) {
    novel_sim[[j]] <- DAISIE::DAISIE_sim_constant_rate(
      time = 5,
      M = 1000,
      pars = c(0.4,0.8,Inf,0.02,laa[i]),
      replicates = 1,
      sample_freq  = Inf,
      plot_sims = FALSE,
      verbose = FALSE,
      cond = 5
    )
  }
  file_name <- paste0("sim_laa_Inf",i,".RData")
  save(novel_sim,file = paste0("G:/R/Traisie-ABC/results/laa_Inf/",file_name))
}
t4 <- Sys.time()
dt2 <- t4 - t3
dt2

### Create simulation results with different K for Traisie ABC analysis
## dd version K = 40, DI simulations K = INF