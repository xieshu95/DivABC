#' Calculates error metrics between two TraiSIE simulations
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
calc_error_trait <- function(sim_1,
                             sim_2,
                             replicates,
                             distance_method) {

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


  ### 5. number of singleton speciation difference between states
  stt_last_row_sim_1 <-
    length(sim_1[[1]][[1]]$stt_two_states[, "present"])
  num_singleton_sim_1_state1 <-
    as.numeric(
        sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nA"])
  num_singleton_sim_1_state2 <-
    as.numeric(
        sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nA2"])
  num_singleton_ratio_sim1 <- num_singleton_sim_1_state1/num_singleton_sim_1_state2

  stt_last_row_sim_2 <-
    length(sim_2[[1]][[1]]$stt_two_states[, "present"])
  num_singleton_sim_2_state1 <-
    as.numeric(
        sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nA"])
  num_singleton_sim_2_state2 <-
    as.numeric(
        sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nA2"])
  num_singleton_ratio_sim2 <- num_singleton_sim_2_state1/num_singleton_sim_2_state2

  num_singleton_ratio_error <-
    abs(num_singleton_ratio_sim1 - num_singleton_ratio_sim2)


  ### 6. number of multiple lineage speciation(cladogenesis)
  num_multi_sim_1_state1 <-
    as.numeric(
      sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nC"])
  num_multi_sim_1_state2 <-
    as.numeric(
      sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nC2"])
  num_multi_ratio_sim1 <- num_multi_sim_1_state1/num_multi_sim_1_state2

  num_multi_sim_2_state1 <-
    as.numeric(
      sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nC"])
  num_multi_sim_2_state2 <-
    as.numeric(
      sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nC2"])
  num_multi_ratio_sim2 <- num_multi_sim_2_state1/num_multi_sim_2_state2

  num_multi_ratio_error <-
    abs(num_multi_ratio_sim1 - num_multi_ratio_sim2)

  # 7. number of Nonendemic
  nonend_sim_1_state1 <-
    as.numeric(
      sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nI"])
  nonend_sim_1_state2 <-
    as.numeric(
      sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nI2"])
  nonend_ratio_sim1 <- nonend_sim_1_state1/nonend_sim_1_state2

  nonend_sim_2_state1 <-
    as.numeric(
      sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nI"])
  nonend_sim_2_state2 <-
    as.numeric(
      sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nI2"])
  nonend_ratio_sim2 <- nonend_sim_2_state1/nonend_sim_2_state2

  nonend_ratio_error <-
    abs(nonend_ratio_sim1 - nonend_ratio_sim2)


  return(
    list(total_nltt = total_nltt,
         clade_nltt = clade_nltt,
         num_singleton_ratio_error = num_singleton_ratio_error,
         num_multi_ratio_error = num_multi_ratio_error,
         nonend_ratio_error = nonend_ratio_error
         )
  )
}
