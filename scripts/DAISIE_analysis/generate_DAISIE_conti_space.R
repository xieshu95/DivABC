calc_ss_no_ext <- function(sim,
                           replicates,
                           distance_method = "abs") {

  # Spec error
  brt <- lapply(sim[[1]][-1],"[[", "branching_times")
  ltt <- full_ltt(sim,brt)
  sim_0 <- rep(0,length(ltt$brt))

  total_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = ltt$brt,
    species_number = ltt$n_spec,
    event_times2 = ltt$brt,
    species_number2 = sim_0,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )

  # Clades number error
  clade_ltt <- clade_ltt(sim, brt)
  sim_0 <- rep(0,length(clade_ltt$colon_time))
  clade_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = clade_ltt$colon_time,
    species_number = clade_ltt$n_clade,
    event_times2 = clade_ltt$colon_time,
    species_number2 = sim_0,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )


  stt_last_row_sim <-
    length(sim[[1]][[1]]$stt_all[, "present"])

  num_singleton <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "nA"])

  num_clado <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "nC"])

  num_nonend <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "nI"])


  num_end <- num_singleton + num_clado
  num_total <- num_singleton + num_clado + num_nonend

  clade_size_sd <- clade_size_sd(sim = sim)
  colon_time_sd <- colon_time_sd(sim = sim)

  ## added nonend_ltt and singleton-ltt
  end_ltt <- end_ltt(sim,brt)
  nonend_ltt <- end_ltt$nonend_ltt
  singleton_ltt <- end_ltt$singleton_ltt
  if(nonend_ltt[1,1] == 0) {
    nonend_nltt <- 0
  } else {
    sim_0 <- rep(0,length(nonend_ltt$nonend_brt))
    nonend_nltt <- nLTT::nltt_diff_exact_extinct(
      event_times = nonend_ltt$nonend_brt,
      species_number = nonend_ltt$n_nonend,
      event_times2 = nonend_ltt$nonend_brt,
      species_number2 = sim_0,
      distance_method = distance_method,
      time_unit = "ago",
      normalize = FALSE
    )
  }

  if(singleton_ltt[1,1] == 0) {
    singleton_nltt <- 0
  } else {
    sim_0 <- rep(0,length(singleton_ltt$singleton_brt))
    singleton_nltt <- nLTT::nltt_diff_exact_extinct(
      event_times = singleton_ltt$singleton_brt,
      species_number = singleton_ltt$n_singleton,
      event_times2 = singleton_ltt$singleton_brt,
      species_number2 = sim_0,
      distance_method = distance_method,
      time_unit = "ago",
      normalize = FALSE
    )
  }

  num_clades <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "present"])

  num_total <- num_singleton + num_clado + num_nonend

  # cla_length_sim <- lapply(sim[[1]][-1],"[[", "branching_times")
  largest_clade <- max(sapply(brt,length))
  return(
    list(clade_nltt = clade_nltt, #
         total_nltt = total_nltt,
         singleton_nltt = singleton_nltt,
         nonend_nltt = nonend_nltt,
         colon_time = colon_time_sd,
         num_clades = num_clades, #
         num_total = num_total,
         num_end = num_end,
         num_nonend = num_nonend,
         clade_size = clade_size_sd,
         num_singleton = num_singleton,
         largest_clade = largest_clade #
    )
  )
}

calc_epsilon_init <- function(sim){
  ss <- calc_ss_no_ext(sim[[1]],1)
  eps_init <- as.numeric(unlist(ss)) * 1
  return(eps_init)
}


pars <- c()
ss<-c()
obs_sim <- list()
set <- 1
set.seed(1)
while(set < 301){
  message("set",set)
  lac <- stats::runif(1,0,1)
  mu <- stats::runif(1,0,0.5)
  gam <- stats::runif(1,0,0.01)
  laa <- stats::runif(1,0,1)
  obs_sim_pars <- c(lac,mu,gam,laa)
  obs_sim[[set]] <- get_DAISIE_sim(parameters = c(obs_sim_pars[1],
                                           obs_sim_pars[2],
                                           obs_sim_pars[3],
                                           obs_sim_pars[4],
                                           Inf),
                            replicates = 1)
  init_epsilon <- calc_epsilon_init(sim = obs_sim[[set]])
  if(init_epsilon[7] >= 10 && init_epsilon[7] < 500){
    pars <- rbind(pars,obs_sim_pars)
    ss <- rbind(ss,init_epsilon)
    set <- set + 1
  } else {
    pars <- pars
    ss <- ss
    set <- set
  }

}

colnames(pars) <- c("lac","mu","gam","laa")
rownames(pars) <- 1:300
DAISIE_ABC_DI <- data.frame(pars)
DAISIE_ABC_DI$K <- Inf
save(DAISIE_ABC_DI, file = "inst/extdata/DAISIE_ABC_DI_conti.rda")
DAISIE_MCMC_DI = DAISIE_ABC_DI
save(DAISIE_MCMC_DI, file = "inst/extdata/DAISIE_MCMC_DI_conti.rda")

save(obs_sim,file = paste0("inst/extdata/obs_sims_DAISIE_ABC_DI_conti.rda"))
save(obs_sim,file = paste0("inst/extdata/obs_sims_DAISIE_MCMC_DI_conti.rda"))

colnames(ss) <- c("clade-nltt","total-nltt","singleton-nltt","nonend-nltt","ctsd",
                  "num-clade","total","end","nonend","cssd","singelton","largest-clade")
rownames(ss) <- 1:300
ss<-data.frame(ss)
save(ss,file = "inst/extdata/obs_ss_DAISIE_ABC_DI_conti.RData")


