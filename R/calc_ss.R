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
calc_ss_no_ext <- function(sim,
                             replicates,
                             distance_method = "abs") {

  # Spec error
  ltt <- full_ltt(sim)
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
  clade_ltt <- clade_ltt(sim)
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

  num_ana <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "nA"])

  num_clado <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "nC"])

  num_nonend <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "nI"])


  clade_size_sd <- clade_size_sd(sim = sim)
  colon_time_sd <- colon_time_sd(sim = sim)

  num_col_sim <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "present"])

  num_total <- num_ana + num_clado + num_nonend
  return(
  list(total_nltt = total_nltt,
       clade_nltt = clade_nltt,
       num_ana = num_ana,
       num_clado = num_clado,
       num_nonend = num_nonend,
       num_col_sim = num_col_sim,
       clade_size = clade_size_sd,
       colon_time = colon_time_sd,
       num_total = num_total)
  )
}


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
calc_ss_with_ext <- function(sim,
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
      length(sim[[1]][[1]]$stt_all[, "present"])

    num_col_sim <-
      as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "present"])

    clade_size_sd <- clade_size_sd(sim = sim)
    colon_time_sd <- colon_time_sd(sim = sim)

    return(
      as.numeric(c(clade_nltt,
                   ana_endemic_nltt,
                   clado_endemic_nltt,
                   nonendemic_nltt,
                   num_col_sim,
                   clade_size_sd,
                   colon_time_sd))
    )
}

# list(clade_nltt = clade_nltt,
#      ana_endemic_nltt = ana_endemic_nltt,
#      clado_endemic_nltt = clado_endemic_nltt,
#      nonendemic_nltt = nonendemic_nltt,
#      num_col_sim = num_col_sim,
#      clade_size_sd = clade_size_sd,
#      colon_time_sd = colon_time_sd)


### calculate entire ss values rather than ss difference
#' Calculate summary statistic differences between simulated data and a specific
#' replicate of (simulated) observed data.
#'
#' @param sim A datalist of observed data with more than one replicate.
#' @param replicates The number of replicates used for calculating summary
#'   statistics.
#'
#' @author Shu Xie
#' @export
calc_num_specs <- function(sim,
                           replicates,
                           distance_method = "abs") {

  # Spec error
  stt_last_row_sim <-
    length(sim[[1]][[1]]$stt_all[, "Time"])

  num_spec <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "nI"]+
                 sim[[1]][[1]]$stt_all[stt_last_row_sim, "nA"]+
                 sim[[1]][[1]]$stt_all[stt_last_row_sim, "nC"])

  num_ana <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "nA"])

  num_clado <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "nC"])

  num_nonend <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "nI"])

  num_clade <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "present"])

  return(
    as.numeric(c(num_spec,
                 num_ana,
                 num_clado,
                 num_nonend,
                 num_clade))
  )
}


