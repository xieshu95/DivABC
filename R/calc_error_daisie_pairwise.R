#' This is specific for DAISIE related models
calc_error_pairwise <- function(sim_1,
                              sim_2,
                              replicates,
                              distance_method,
                              pairwise_method = 1) {
  # Spec error
  ltt_1 <- full_ltt(sim_1)
  ltt_2 <- full_ltt(sim_2)

  # total number species nltt error
  total_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = ltt_1$brt,
    species_number = ltt_1$n_spec,
    event_times2 = ltt_2$brt,
    species_number2 = ltt_2$n_spec,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )


  # Clades number nltt error
  clade_ltt_1 <- clade_ltt(sim_1)
  clade_ltt_2 <- clade_ltt(sim_2)

  clade_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = clade_ltt_1$colon_time,
    species_number = clade_ltt_1$n_clade,
    event_times2 = clade_ltt_2$colon_time,
    species_number2 = clade_ltt_2$n_clade,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )

  ## tip info
  stt_last_row_sim_1 <-
    length(sim_1[[1]][[1]]$stt_all[, "present"])
  stt_last_row_sim_2 <-
    length(sim_2[[1]][[1]]$stt_all[, "present"])

  ## singleton clade sepcies number error(anagensis)
  num_ana_sim_1 <-
    as.numeric(sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nA"])
  num_ana_sim_2 <-
    as.numeric(sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nA"])
  num_ana <-
    abs(num_ana_sim_1 - num_ana_sim_2)

  ##  multiple-species clade sepcies number error(cladogenesis)
  num_clado_sim_1 <-
    as.numeric(sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nC"])
  num_clado_sim_2 <-
    as.numeric(sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nC"])
  num_clado <-
    abs(num_clado_sim_1 - num_clado_sim_2)

  ##  nonendemic sepcies number error
  num_nonend_sim_1 <-
    as.numeric(sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nI"])
  num_nonend_sim_2 <-
    as.numeric(sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nI"])
  num_nonend <-
    abs(num_nonend_sim_1 - num_nonend_sim_2)

  clade_size <- calc_clade_size_error(sim_1,sim_2)
  colon_time <- calc_colon_time_error(sim_1,sim_2)

  # pairwise
  if(pairwise_method == 1) {
    pairwise <- pairwise_cross(sim_1,sim_2)
    pw_nltt <- pairwise$pw_nltt
    pw_clade_size <- pairwise$pw_clade_size
    # pw_colon_time <- pairwise$pw_colon_time
    error <- c(total_nltt,
               clade_nltt,
               num_ana,
               num_clado,
               num_nonend,
               pw_nltt,
               pw_clade_size)

  } else if (pairwise_method == 2) {
    pairwise <- pairwise_order(sim_1,sim_2)
    pw_nltt <- pairwise$pw_nltt
    pw_clade_size <- pairwise$pw_clade_size
    pw_nltt_sd <- pairwise$pw_nltt_sd
    pw_clade_size_sd <- pairwise$pw_clade_size_sd
    error <- c(total_nltt,
               clade_nltt,
               num_ana,
               num_clado,
               num_nonend,
               pw_nltt,
               pw_clade_size,
               # pw_nltt_sd,
               # pw_clade_size_sd)
               clade_size = clade_size,
               colon_time = colon_time)
  } else {
    pairwise <- pairwise_order_per_clade(sim_1,sim_2)
    error <- c(total_nltt,
               clade_nltt,
               num_ana,
               num_clado,
               num_nonend,
               pairwise)
  }
  return(error)
}

#' Get the pairwise distance across all the clades
pairwise_cross <- function(sim1,sim2){
  brts1 <- lapply(sim1[[1]][-1],"[[", "branching_times")
  brts2 <- lapply(sim2[[1]][-1],"[[", "branching_times")
  rep <- length(brts1)
  nltt_error <- matrix(NA, rep, rep)
  clade_size_error <- matrix(NA, rep, rep)
  colon_time_error <- matrix(NA, rep, rep)
  for(i in 1:rep) {
    brt1 <- c(brts1[[i]],0)
    n_spec1 <- c(seq(0,length(brt1)-2),length(brt1)-2)
    for (j in 1:rep) {
      brt2 <- c(brts2[[j]],0)
      n_spec2 <- c(seq(0,length(brt2)-2),length(brt2)-2)
      nltt_error[i,j] <- nLTT::nltt_diff_exact_extinct(
        event_times = brt1,
        species_number = n_spec1,
        event_times2 = brt2,
        species_number2 = n_spec2,
        distance_method = "abs",
        time_unit = "ago",
        normalize = FALSE
      )
      clade_size_error[i,j] <- abs(length(brt1) - length(brt2))
      # colon_time_error[i,j] <- abs(brt1[2] - brt2[2])
    }
  }
  pairwise <- list(pw_nltt = mean(nltt_error),
                   pw_clade_size = mean(clade_size_error))
  return(pairwise)
}

#' Get the pairwise distance between reordered clades
reorder_ltt <- function(sim) {
  brts <- lapply(sim[[1]][-1],"[[", "branching_times")
  brts_reorder<-brts[order(sapply(brts,length),decreasing = T)]
  # brts_add<−mapply(append,brts_reorder,0,SIMPLIFY=FALSE)
  return(brts_reorder)
}

pairwise_order <- function(sim1, sim2){
  brts1 <- reorder_ltt(sim1)  ## order based on the length of each clade
  brts2 <- reorder_ltt(sim2)
  clade_size_error <- c()
  nltt_error <- c()
  for(i in 1:length(brts1)){
    brt1<- c(brts1[[i]],0)
    brt2<- c(brts2[[i]],0)
    nltt_error[i] <- nLTT::nltt_diff_exact_extinct(
      event_times = brt1,
      species_number = c(seq(0,length(brt1)-2),length(brt1)-2),
      event_times2 = brt2,
      species_number2 = c(seq(0,length(brt2)-2),length(brt2)-2),
      distance_method = "abs",
      time_unit = "ago",
      normalize = FALSE
    )
    clade_size_error[i] <- abs(length(brt1) - length(brt2))
  }
  pairwise <- list(pw_nltt = mean(nltt_error),
                   pw_clade_size = mean(clade_size_error),
                   pw_nltt_sd = sd(nltt_error),
                   pw_clade_size_sd = sd(clade_size_error))
  return(pairwise)
}

#' #' each clade as a metric
#' pairwise_order_per_clade <- function(brts1, brts2){
#'   # clade_size_error <- c()
#'   nltt_error <- c()
#'   for(i in 1:length(brts1)){
#'     brt1<- c(brts1[[i]],0)
#'     brt2<- c(brts2[[i]],0)
#'     nltt_error[i] <- nLTT::nltt_diff_exact_extinct(
#'       event_times = brt1,
#'       species_number = c(seq(0,length(brt1)-2),length(brt1)-2),
#'       event_times2 = brt2,
#'       species_number2 = c(seq(0,length(brt2)-2),length(brt2)-2),
#'       distance_method = "abs",
#'       time_unit = "ago",
#'       normalize = FALSE
#'     )
#'     # clade_size_error[i] <- abs(length(brt1) - length(brt2))
#'   }
#'   # pairwise <- list(pw_nltt = nltt_error)
#'   return(mean(nltt_error))
#' }


# pars = c(0.4,0,0.001,0.1)
# set.seed(1)
# sim_1 <- get_DAISIE_sim(parameters = pars,
#                        K = 20,
#                        replicates = 1)[[1]]
# set.seed(2)
# sim_2 <- get_DAISIE_sim(parameters = pars,
#                        K = 20,
#                        replicates = 1)[[1]]
# pairwise_order_per_clade(brts1 = brt1_reorder,
#                          brts2 = brt2_reorder)
#
# pars = c(0.1,0,0.01,0.1)
# set.seed(1)
# sim <- get_DAISIE_sim(parameters = pars,
#                        K = 20,
#                        replicates = 10)

