
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