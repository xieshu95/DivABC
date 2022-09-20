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


  sim_1_event_times <-
    sim_1[[1]][[1]]$stt_all[, "Time"]
  sim_2_event_times <-
    sim_2[[1]][[1]]$stt_all[, "Time"]

  # 1. Anagenesis Endemic nltt
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

  # 2. Cladogenesis Endemic nltt
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

  # 3. Nonendemic nltt
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

  # 4. Clades number nltt
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

  ### 5. number of singleton speciation
  stt_last_row_sim_1 <-
    length(sim_1[[1]][[1]]$stt_two_states[, "present"])
  num_ana_sim_1_state1 <-
    as.numeric(
        sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nA"])
  num_ana_sim_1_state2 <-
    as.numeric(
        sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nA2"])

  stt_last_row_sim_2 <-
    length(sim_2[[1]][[1]]$stt_two_states[, "present"])
  num_ana_sim_2_state1 <-
    as.numeric(
        sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nA"])
  num_ana_sim_2_state2 <-
    as.numeric(
        sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nA2"])
  num_ana_error_state1 <-
    abs(num_ana_sim_1_state1 - num_ana_sim_2_state1)
  num_ana_error_state2 <-
    abs(num_ana_sim_1_state2 - num_ana_sim_2_state2)

  ### 6. number of multiple lineage speciation(cladogenesis)
  num_clado_sim_1_state1 <-
    as.numeric(
      sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nC"])
  num_clado_sim_1_state2 <-
    as.numeric(
      sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "nC2"])

  num_clado_sim_2_state1 <-
    as.numeric(
      sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nC"])
  num_clado_sim_2_state2 <-
    as.numeric(
      sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "nC2"])
  num_clado_error_state1 <-
    abs(num_clado_sim_1_state1 - num_clado_sim_2_state1)
  num_clado_error_state2 <-
    abs(num_clado_sim_1_state2 - num_clado_sim_2_state2)

  # 7. number of Nonendemic
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
  num_nonend_error_state1 <-
    abs(nonendemic_sim_1_state1 - nonendemic_sim_2_state1)
  num_nonend_error_state2 <-
    abs(nonendemic_sim_1_state2 - nonendemic_sim_2_state2)

  # #8. colonist number
  # num_col_sim_1 <-
  #   as.numeric(sim_1[[1]][[1]]$stt_two_states[stt_last_row_sim_1, "present"])
  # num_col_sim_2 <-
  #   as.numeric(sim_2[[1]][[1]]$stt_two_states[stt_last_row_sim_2, "present"])
  # num_col_error <-
  #   abs(num_col_sim_1 - num_col_sim_2)


  #9. transition times
  stt1 <- sim_1[[1]][[1]]$stt_two_states
  ds1_sim1 <- diff(stt1[,2] + stt1[,3] + stt1[,4])
  ds2_sim1 <- diff(stt1[,5] + stt1[,6] + stt1[,7])
  ds1<- data.frame(ds1_sim1,ds2_sim1)
  num_trans12_sim1 = sum(ds1$ds1_sim1 == -1 & ds1$ds2_sim1 == 1)
  num_trans21_sim1 = sum(ds1$ds1_sim1 == 1 & ds1$ds2_sim1 == -1)

  stt2 <- sim_2[[1]][[1]]$stt_two_states
  ds1_sim2 <- diff(stt2[,2] + stt2[,3] + stt2[,4])
  ds2_sim2 <- diff(stt2[,5] + stt2[,6] + stt2[,7])
  ds2<- data.frame(ds1_sim2,ds2_sim2)
  num_trans12_sim2 = sum(ds2$ds1_sim2 == -1 & ds2$ds2_sim2 == 1)
  num_trans21_sim2 = sum(ds2$ds1_sim2 == 1 & ds2$ds2_sim2 == -1)

  num_trans12_error = abs(num_trans12_sim1 - num_trans12_sim2)
  num_trans21_error = abs(num_trans21_sim1 - num_trans21_sim2)

  return(
    list(ana_endemic_nltt_error = ana_endemic_nltt_error,
         clado_endemic_nltt_error = clado_endemic_nltt_error,
         nonendemic_nltt_error = nonendemic_nltt_error,
         clade_nltt_error = clade_nltt_error,
         num_ana_error_state1 = num_ana_error_state1,
         num_ana_error_state2 = num_ana_error_state2,
         num_clado_error_state1 = num_clado_error_state1,
         num_clado_error_state2 = num_clado_error_state2,
         num_nonend_error_state1 = num_nonend_error_state1,
         num_nonend_error_state2 = num_nonend_error_state2,
         num_trans12_error = num_trans12_error,
         num_trans21_error = num_trans21_error)
  )
}
