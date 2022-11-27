#' Calculates error metrics between two simulations

calc_error_pairwise <- function(sim_1,
                              sim_2,
                              replicates,
                              distance_method,
                              pairwise_method = 1) {


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


  # pairwise
  if(pairwise_method == 1) {
    pairwise_cross <- pairwise_cross(sim_1,sim_2)
    pw_nltt <- pairwise_cross$pw_nltt
    pw_clade_size <- pairwise_cross$pw_clade_size
    pw_colon_time <- pairwise_cross$pw_colon_time
    error <- c(num_ana,
               num_clado,
               num_nonend,
               pw_nltt,
               pw_clade_size,
               pw_colon_time)
  }


  return(error)
}

#' Get the number of exist colonist through time from
#' the branching times from each exist clade.
#'
#' @param sim1 A datalist of observed data with multiple replicate.
#' @param sim2 A datalist of observed data with multiple replicate.
#'
#' @author Shu Xie
#' @export
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
      clade_size_error[i,j] <- length(brt1) - length(brt2)
      colon_time_error[i,j] <- brt1[2] - brt2[2]
    }
  }
  pairwise <- list(pw_nltt = mean(nltt_error),
                   pw_clade_size = abs(mean(clade_size_error)),
                   pw_colon_time = abs(mean(colon_time_error)))
  return(pairwise)
}


#' Get the number of exist colonist through time from
#' the branching times from each exist clade.
#'
#' @param sim1 A datalist of observed data with multiple replicate.
#' @param sim2 A datalist of observed data with multiple replicate.
#'
#' @author Shu Xie
#' @export
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
      clade_size_error[i,j] <- length(brt1) - length(brt2)
      colon_time_error[i,j] <- brt1[2] - brt2[2]
    }
  }
  pairwise <- list(pw_nltt = mean(nltt_error),
                   pw_clade_size = abs(mean(clade_size_error)),
                   pw_colon_time = abs(mean(colon_time_error)))
  return(pairwise)
}
# pars = c(0.4,0,0.001,0.1)
# set.seed(1)
# sim1 <- get_DAISIE_sim(parameters = pars,
#                        K = 20,
#                        replicates = 1)[[1]]
# set.seed(2)
# sim2 <- get_DAISIE_sim(parameters = pars,
#                        K = 20,
#                        replicates = 1)[[1]]
# pairwise_error(sim1,sim2)

