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

  # Spec nltt error
  sim_1_event_times <-
    sim_1[[1]][[1]]$stt_two_states[, "Time"]
  sim_1_num_spec_state1 <-
    sim_1[[1]][[1]]$stt_two_states[, "nI"] +
    sim_1[[1]][[1]]$stt_two_states[, "nA"] +
    sim_1[[1]][[1]]$stt_two_states[, "nC"]
  sim_1_num_spec_state2 <-
    sim_1[[1]][[1]]$stt_two_states[, "nI2"] +
    sim_1[[1]][[1]]$stt_two_states[, "nA2"] +
    sim_1[[1]][[1]]$stt_two_states[, "nC2"]

  sim_2_event_times <-
    sim_2[[1]][[1]]$stt_two_states[, "Time"]
  sim_2_num_spec_state1 <-
    sim_2[[1]][[1]]$stt_two_states[, "nI"] +
    sim_2[[1]][[1]]$stt_two_states[, "nA"] +
    sim_2[[1]][[1]]$stt_two_states[, "nC"]
  sim_2_num_spec_state2 <-
    sim_2[[1]][[1]]$stt_two_states[, "nI2"] +
    sim_2[[1]][[1]]$stt_two_states[, "nA2"] +
    sim_2[[1]][[1]]$stt_two_states[, "nC2"]

  spec_nltt_error_state1 <- nLTT::nltt_diff_exact_extinct(
    event_times = sim_1_event_times,
    species_number = sim_1_num_spec_state1,
    event_times2 = sim_2_event_times,
    species_number2 = sim_2_num_spec_state1,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )
  spec_nltt_error_state2 <- nLTT::nltt_diff_exact_extinct(
    event_times = sim_1_event_times,
    species_number = sim_1_num_spec_state2,
    event_times2 = sim_2_event_times,
    species_number2 = sim_2_num_spec_state2,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )

  # Endemic nltt error
  sim_1_event_times <-
    sim_1[[1]][[1]]$stt_two_states[, "Time"]
  sim_1_endemic_spec_state1 <-
    sim_1[[1]][[1]]$stt_two_states[, "nA"] +
    sim_1[[1]][[1]]$stt_two_states[, "nC"]
  sim_1_endemic_spec_state2 <-
    sim_1[[1]][[1]]$stt_two_states[, "nA2"] +
    sim_1[[1]][[1]]$stt_two_states[, "nC2"]
  sim_2_event_times <-
    sim_2[[1]][[1]]$stt_two_states[, "Time"]
  sim_2_endemic_spec_state1 <-
    sim_2[[1]][[1]]$stt_two_states[, "nA"] +
    sim_2[[1]][[1]]$stt_two_states[, "nC"]
  sim_2_endemic_spec_state2 <-
    sim_2[[1]][[1]]$stt_two_states[, "nA2"] +
    sim_2[[1]][[1]]$stt_two_states[, "nC2"]

  endemic_nltt_error_state1 <- nLTT::nltt_diff_exact_extinct(
    event_times = sim_1_event_times,
    species_number = sim_1_endemic_spec_state1,
    event_times2 = sim_2_event_times,
    species_number2 = sim_2_endemic_spec_state1,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )
  endemic_nltt_error_state2 <- nLTT::nltt_diff_exact_extinct(
    event_times = sim_1_event_times,
    species_number = sim_1_endemic_spec_state2,
    event_times2 = sim_2_event_times,
    species_number2 = sim_2_endemic_spec_state2,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )

  # Nonendemic nltt error
  sim_1_event_times <-
    sim_1[[1]][[1]]$stt_two_states[, "Time"]
  sim_1_nonendemic_spec_state1 <-
    sim_1[[1]][[1]]$stt_two_states[, "nI"]
  sim_1_nonendemic_spec_state2 <-
    sim_1[[1]][[1]]$stt_two_states[, "nI2"]
  sim_2_event_times <-
    sim_2[[1]][[1]]$stt_two_states[, "Time"]
  sim_2_nonendemic_spec_state1 <-
    sim_2[[1]][[1]]$stt_two_states[, "nI"]
  sim_2_nonendemic_spec_state2 <-
    sim_2[[1]][[1]]$stt_two_states[, "nI2"]
  nonendemic_nltt_error_state1 <- nLTT::nltt_diff_exact_extinct(
    event_times = sim_1_event_times,
    species_number = sim_1_nonendemic_spec_state1,
    event_times2 = sim_2_event_times,
    species_number2 = sim_2_nonendemic_spec_state1,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )
  nonendemic_nltt_error_state2 <- nLTT::nltt_diff_exact_extinct(
    event_times = sim_1_event_times,
    species_number = sim_1_nonendemic_spec_state2,
    event_times2 = sim_2_event_times,
    species_number2 = sim_2_nonendemic_spec_state2,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )
  ### number of species
  stt_last_row_sim_1 <-
    length(sim_1[[1]][[1]]$stt_two_states[, "present"])
  num_spec_sim_1_state1 <-
    as.numeric(
      sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nI"] +
        sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nA"] +
        sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nC"])
  num_spec_sim_1_state2 <-
    as.numeric(
      sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nI2"] +
        sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nA2"] +
        sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nC2"])

  stt_last_row_sim_2 <-
    length(sim_2[[1]][[1]]$stt_two_states[, "present"])
  num_spec_sim_2_state1 <-
    as.numeric(
      sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nI"] +
        sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nA"] +
        sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nC"])
  num_spec_sim_2_state2 <-
    as.numeric(
      sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nI2"] +
        sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nA2"] +
        sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nC2"])
  num_spec_error_state1 <-
    abs(num_spec_sim_1_state1 - num_spec_sim_2_state1)
  num_spec_error_state2 <-
    abs(num_spec_sim_1_state2 - num_spec_sim_2_state2)
  tip_ratio_sim1 <- num_spec_sim_1_state1 / num_spec_sim_1_state2
  tip_ratio_sim2 <- num_spec_sim_2_state1 / num_spec_sim_2_state2
  tip_ratio_error <- abs(tip_ratio_sim1 - tip_ratio_sim2)

  #colonist number
  num_col_sim_1 <-
    as.numeric(sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "present"])
  num_col_sim_2 <-
    as.numeric(sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "present"])
  num_col_error <-
    abs(num_col_sim_1 - num_col_sim_2)

  # number of endemic
  endemic_sim_1_state1 <-
    as.numeric(
      sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nA"] +
        sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nC"])
  endemic_sim_1_state2 <-
    as.numeric(
      sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nA2"] +
        sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nC2"])

  endemic_sim_2_state1 <-
    as.numeric(
      sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nA"] +
        sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nC"])

  endemic_sim_2_state2 <-
    as.numeric(
      sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nA2"] +
        sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nC2"])

  endemic_error_state1 <-
    abs(endemic_sim_1_state1 - endemic_sim_2_state1)
  endemic_error_state2 <-
    abs(endemic_sim_1_state2 - endemic_sim_2_state2)

  # number of Nonendemic
  nonendemic_sim_1_state1 <-
    as.numeric(
      sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nI"])
  nonendemic_sim_1_state2 <-
    as.numeric(
      sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nI2"])

  nonendemic_sim_2_state1 <-
    as.numeric(
      sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nI"])
  nonendemic_sim_2_state2 <-
    as.numeric(
      sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nI2"])
  nonendemic_error_state1 <-
    abs(nonendemic_sim_1_state1 - nonendemic_sim_2_state1)
  nonendemic_error_state2 <-
    abs(nonendemic_sim_1_state2 - nonendemic_sim_2_state2)


  return(
    list(spec_nltt_error_state1 = spec_nltt_error_state1,
         spec_nltt_error_state2 = spec_nltt_error_state2,
         endemic_nltt_error_state1 = endemic_nltt_error_state1,
         endemic_nltt_error_state2 = endemic_nltt_error_state2,
         nonendemic_nltt_error_state1 = nonendemic_nltt_error_state1,
         nonendemic_nltt_error_state2 = nonendemic_nltt_error_state2,
         num_spec_error_state1 = num_spec_error_state1,
         num_spec_error_state2 = num_spec_error_state2,
         endemic_error_state1 = endemic_error_state1,
         endemic_error_state2 = endemic_error_state2,
         nonendemic_error_state1 = nonendemic_error_state1,
         nonendemic_error_state2 = nonendemic_error_state2,
         tip_ratio_error = tip_ratio_error,
         num_col_error = num_col_error)
  )
}
