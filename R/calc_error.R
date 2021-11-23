## calc error of SRTT,ESRTT,NESRTT, but not delta nltt
calc_error <- function(sim_1,
                       sim_2,
                       replicates,
                       distance_method) {
  num_spec_error <- c()
  num_col_error <- c()
  endemic_error <- c()
  nonendemic_error <- c()

  stt_last_row_sim_1 <-
    length(sim_1[[1]][[1]]$stt_all[, "present"])
  num_spec_sim_1 <-
    as.numeric(
      sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nI"] +
        sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nA"] +
        sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nC"])
  stt_last_row_sim_2 <-
    length(sim_2[[1]][[1]]$stt_all[, "present"])
  num_spec_sim_2 <-
    as.numeric(
      sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nI"] +
        sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nA"] +
        sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nC"])
  num_spec_error <-
    abs(num_spec_sim_1 - num_spec_sim_2)
  num_col_sim_1 <-
    as.numeric(sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "present"])
  num_col_sim_2 <-
    as.numeric(sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "present"])
  num_col_error <-
    abs(num_col_sim_1 - num_col_sim_2)

  # Endemic error
  endemic_sim_1 <-
    as.numeric(
        sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nA"] +
        sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nC"])
  endemic_sim_2 <-
    as.numeric(
        sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nA"] +
        sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nC"])
  endemic_error <-
    abs(endemic_sim_1 - endemic_sim_2)

  # Nonendemic error
  nonendemic_sim_1 <-
    as.numeric(
      sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nI"])
  nonendemic_sim_2 <-
    as.numeric(
      sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nI"])
  nonendemic_error <-
    abs(nonendemic_sim_1 - nonendemic_sim_2)

  return(
    list(num_spec_error = num_spec_error,
         num_col_error = num_col_error,
         endemic_error = endemic_error,
         nonendemic_error = nonendemic_error)
  )
}

#' Calculate clade size standard deviation among clades in a DAISIE simulation.
#'
#' @param sim DAISIE simulation.
#'
#' @return A numeric for clade size standard  ddeviation.
#' @author Shu Xie
#' @export


# clade_size_sd <- function(sim){
#   clade_size <- c()
#   if (length(sim[[1]][[1]]) == 1){
#     clade_size_sd <- 0
#   } else {
#     for(i in 2:length(sim[[1]][[1]])){ ##clades
#       clade_size[i - 1] <- length(sim[[1]][[1]][[i]]$branching_times) - 1
#     }
#     if(length(clade_size) == 1){
#       clade_size_sd <- 0
#     } else{
#       clade_size_sd <- sd(clade_size)
#     }
#   }
#   return(clade_size_sd)
# }
clade_size_sd <- function(sim){
  clade_size <- c()
  if (length(sim[[1]]) == 1){
    clade_size_sd <- 0
  } else {
    for(i in 2:length(sim[[1]])){ ##clades
      clade_size[i - 1] <- length(sim[[1]][[i]]$branching_times) - 1
    }
    if(length(clade_size) == 1){
      clade_size_sd <- 0
    } else{
      clade_size_sd <- sd(clade_size)
    }
  }
  return(clade_size_sd)
}

#' Calculate error of clade size standard deviation between two simulated data.
#'
#' @param sim_1 DAISIE simulation.
#' @param sim_2 DAISIE simulation.
#' @return A numeric for clade size standard  ddeviation.
#' @author Shu Xie
#' @export
calc_clade_size_error <- function(sim_1, sim_2){
  sim1_cs_sd <- clade_size_sd(sim_1)
  sim2_cs_sd <- clade_size_sd(sim_2)
  clade_size_error <- abs(sim1_cs_sd - sim2_cs_sd)
  return(clade_size_error)
}

#' Calculate colonization time standard deviation among clades in a DAISIE simulation.
#'
#' @param sim DAISIE simulation.
#'
#' @return A numeric for colonization time standard deviation.
#' @author Shu Xie
#' @export

colon_time_sd <- function(sim){
  colon_time <- c()
  if (length(sim[[1]]) == 1){
    colon_time_sd <- 0
  } else {
    for(i in 2:length(sim[[1]])){ ##clades
      colon_time[i - 1] <- sim[[1]][[i]]$branching_times[2]
    }
    if(length(colon_time) == 1){
      colon_time_sd <- 0
    } else{
      colon_time_sd <- sd(colon_time)
    }
  }
  return(colon_time_sd)
}

#' Calculate error of colonization time standard deviation between two simulated data.
#'
#' @param sim_1 DAISIE simulation.
#' @param sim_2 DAISIE simulation.
#' @return A numeric for colonization time standard  deviation.
#' @author Shu Xie
#' @export
calc_colon_time_error <- function(sim_1, sim_2){
  sim1_ct_sd <- colon_time_sd(sim_1)
  sim2_ct_sd <- colon_time_sd(sim_2)
  colon_time_error <- abs(sim1_ct_sd - sim2_ct_sd)
  return(colon_time_error)
}