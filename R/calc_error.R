#' Calculates error metrics between two simulations
#'
#' Calculates error in number of species and colonists, number of
#' species-through-time, number of endemics-through-time and
#' non-endemics-through-time.
#'
#' @return A list with five error metrics:
#' \describe{
#'   \item{\code{$spec_nltt_error}}{Numeric vector with the nltt error between
#'     the two simulations.}
#'   \item{\code{$num_spec_error}}{Numeric vector with the difference in the
#'     number of species at the end of the simulation between the two
#'     simulations.}
#'   \item{\code{$num_col_error}}{Numeric vector with the difference in the
#'     number of colonists at the end of the simulation between the two
#'     simulations.}
#'   \item{\code{$endemic_nltt_error}}{Numeric vector with the nltt error of the
#'     endemic species between the two simulations.}
#'   \item{\code{$nonendemic_nltt_error}}{Numeric vector with the nltt error of
#'     the non-endemic species between the two simulations.}
#' }
#'
calc_error <- function(sim_1,
                       sim_2,
                       replicates,
                       distance_method) {
  # Spec error
  sim_1_event_times <-
    sim_1[[1]][[1]]$stt_all[, "Time"]
  sim_2_event_times <-
    sim_2[[1]][[1]]$stt_all[, "Time"]
  # sim_1_num_spec <-
  #   sim_1[[1]][[1]]$stt_all[, "nI"] +
  #   sim_1[[1]][[1]]$stt_all[, "nA"] +
  #   sim_1[[1]][[1]]$stt_all[, "nC"]
  # sim_2_num_spec <-
  #   sim_2[[1]][[1]]$stt_all[, "nI"] +
  #   sim_2[[1]][[1]]$stt_all[, "nA"] +
  #   sim_2[[1]][[1]]$stt_all[, "nC"]
  # spec_nltt_error <- nLTT::nltt_diff_exact_extinct(
  #   event_times = sim_1_event_times,
  #   species_number = sim_1_num_spec,
  #   event_times2 = sim_2_event_times,
  #   species_number2 = sim_2_num_spec,
  #   distance_method = distance_method,
  #   time_unit = "ago",
  #   normalize = FALSE
  # )

  # Anagenesis Endemic error
  sim_1_ana_endemic_spec <-
    sim_1[[1]][[1]]$stt_all[, "nA"]
  sim_2_ana_endemic_spec <-
    sim_2[[1]][[1]]$stt_all[, "nA"]
  ana_endemic_nltt_error <- nLTT::nltt_diff_exact_extinct(
    event_times = sim_1_event_times,
    species_number = sim_1_ana_endemic_spec,
    event_times2 = sim_2_event_times,
    species_number2 = sim_2_ana_endemic_spec,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )

  # Cladogenesis Endemic error
  sim_1_clado_endemic_spec <-
    sim_1[[1]][[1]]$stt_all[, "nC"]
  sim_2_clado_endemic_spec <-
    sim_2[[1]][[1]]$stt_all[, "nC"]
  clado_endemic_nltt_error <- nLTT::nltt_diff_exact_extinct(
    event_times = sim_1_event_times,
    species_number = sim_1_clado_endemic_spec,
    event_times2 = sim_2_event_times,
    species_number2 = sim_2_clado_endemic_spec,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )

  # Nonendemic error
  sim_1_nonendemic_spec <-
    sim_1[[1]][[1]]$stt_all[, "nI"]
  sim_2_nonendemic_spec <-
    sim_2[[1]][[1]]$stt_all[, "nI"]
  nonendemic_nltt_error <- nLTT::nltt_diff_exact_extinct(
    event_times = sim_1_event_times,
    species_number = sim_1_nonendemic_spec,
    event_times2 = sim_2_event_times,
    species_number2 = sim_2_nonendemic_spec,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )

  # Clades number error
  sim_1_clade <-
    sim_1[[1]][[1]]$stt_all[, "present"]
  sim_2_clade <-
    sim_2[[1]][[1]]$stt_all[, "present"]
  clade_nltt_error <- nLTT::nltt_diff_exact_extinct(
    event_times = sim_1_event_times,
    species_number = sim_1_clade,
    event_times2 = sim_2_event_times,
    species_number2 = sim_2_clade,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )


  stt_last_row_sim_1 <-
    length(sim_1[[1]][[1]]$stt_all[, "present"])
  stt_last_row_sim_2 <-
    length(sim_2[[1]][[1]]$stt_all[, "present"])
  # num_spec_sim_1 <-
  #   as.numeric(
  #     sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nI"] +
  #       sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nA"] +
  #       sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nC"])
  # num_spec_sim_2 <-
  #   as.numeric(
  #     sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nI"] +
  #       sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nA"] +
  #       sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nC"])
  # num_spec_error <-
  #   abs(num_spec_sim_1 - num_spec_sim_2)

  num_col_sim_1 <-
    as.numeric(sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "present"])
  num_col_sim_2 <-
    as.numeric(sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "present"])
  num_col_error <-
    abs(num_col_sim_1 - num_col_sim_2)

  # Anagenesis Endemic error
  # ana_endemic_sim_1 <-
  #   as.numeric(
  #     sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nA"] +
  #       sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nC"])
  # ana_endemic_sim_2 <-
  #   as.numeric(
  #     sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nA"] +
  #       sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nC"])
  # ana_endemic_error <-
  #   abs(ana_endemic_sim_1 - ana_endemic_sim_2)
  #
  # # Cladogenesis Endemic error
  # clado_endemic_sim_1 <-
  #   as.numeric(
  #     sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nA"] +
  #       sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nC"])
  # clado_endemic_sim_2 <-
  #   as.numeric(
  #     sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nA"] +
  #       sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nC"])
  # clado_endemic_error <-
  #   abs(clado_endemic_sim_1 - clado_endemic_sim_2)
  #
  # # Nonendemic error
  # nonendemic_sim_1 <-
  #   as.numeric(
  #     sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nI"])
  # nonendemic_sim_2 <-
  #   as.numeric(
  #     sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nI"])
  # nonendemic_error <-
  #   abs(nonendemic_sim_1 - nonendemic_sim_2)


  return(
    list(clade_nltt_error = clade_nltt_error,
         ana_endemic_nltt_error = ana_endemic_nltt_error,
         clado_endemic_nltt_error = clado_endemic_nltt_error,
         nonendemic_nltt_error = nonendemic_nltt_error,
         # num_spec_error = num_spec_error,
         # ana_endemic_error = ana_endemic_error,
         # clado_endemic_error = clado_endemic_error,
         # nonendemic_error = nonendemic_error,
         num_col_error = num_col_error)
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