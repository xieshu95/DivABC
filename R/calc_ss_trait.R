### calculate entire ss values rather than ss difference
#' Calculate summary statistic differences between simulated data and a specific
#' replicate of (simulated) observed data.
#'
#' @param sim A datalist of observed data with more than one replicate.
#' @param replicates The number of replicates used for calculating summary
#'   statistics.
#'
#' @return A list with numeric vectors of diff statistics for:
#' \itemize{
#'   \item{\code{$ana_endemic_nltt}}
#'   \item{\code{$clado_endemic_nltt}}
#'   \item{\code{$nonendemic_nltt}}
#'   \item{\code{$num_col_sim}}
#'   \item{\code{$clade_size_sd}}
#'   \item{\code{$colon_time_sd}}
#' }
#' @author Shu Xie
#' @export
calc_ss_trait <- function(sim,
                    replicates,
                    distance_method = "abs") {

  # Spec error
  sim_event_times <-
    sim[[1]][[1]]$stt_all[, "Time"]

  # Anagenesis Endemic error
  sim_ana_endemic_spec <-
    sim[[1]][[1]]$stt_all[, "nA"]
  sim_0 <- rep(0,length(sim_event_times))

  ana_endemic_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = sim_event_times,
    species_number = sim_ana_endemic_spec,
    event_times2 = sim_event_times,
    species_number2 = sim_0,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )

  # Cladogenesis Endemic error
  sim_clado_endemic_spec <-
    sim[[1]][[1]]$stt_all[, "nC"]

  clado_endemic_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = sim_event_times,
    species_number = sim_clado_endemic_spec,
    event_times2 = sim_event_times,
    species_number2 = sim_0,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )

  # Nonendemic error
  sim_nonendemic_spec <-
    sim[[1]][[1]]$stt_all[, "nI"]

  nonendemic_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = sim_event_times,
    species_number = sim_nonendemic_spec,
    event_times2 = sim_event_times,
    species_number2 = sim_0,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )

  # Clades number error
  sim_clade <-
    sim[[1]][[1]]$stt_all[, "present"]
  clade_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = sim_event_times,
    species_number = sim_clade,
    event_times2 = sim_event_times,
    species_number2 = sim_0,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )


  stt_last_row_sim <-
    length(sim[[1]][[1]]$stt_two_states[, "present"])

  num_ana_state1 <-
    as.numeric(sim[[1]][[1]]$stt_two_states[stt_last_row_sim, "nA"])
  num_ana_state2 <-
    as.numeric(sim[[1]][[1]]$stt_two_states[stt_last_row_sim, "nA2"])

  num_clado_state1 <-
    as.numeric(sim[[1]][[1]]$stt_two_states[stt_last_row_sim, "nC"])
  num_clado_state2 <-
    as.numeric(sim[[1]][[1]]$stt_two_states[stt_last_row_sim, "nC2"])

  num_nonend_state1 <-
    as.numeric(sim[[1]][[1]]$stt_two_states[stt_last_row_sim, "nI"])
  num_nonend_state2 <-
    as.numeric(sim[[1]][[1]]$stt_two_states[stt_last_row_sim, "nI2"])

  stt <- sim[[1]][[1]]$stt_two_states
  ds1 <- diff(stt[,2] + stt[,3] + stt[,4])
  ds2 <- diff(stt[,5] + stt[,6] + stt[,7])
  ds<- data.frame(ds1,ds2)
  num_trans12 = sum(ds$ds1 == -1 & ds$ds2 == 1)
  num_trans21 = sum(ds$ds1 == 1 & ds$ds2 == -1)

  return(
    as.numeric(c(ana_endemic_nltt,
                 clado_endemic_nltt,
                 nonendemic_nltt,
                 clade_nltt,
                 num_ana_state1,
                 num_ana_state2,
                 num_clado_state1,
                 num_clado_state2,
                 num_nonend_state1,
                 num_nonend_state2,
                 num_trans12,
                 num_trans21))
  )
}