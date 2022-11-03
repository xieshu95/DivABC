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
calc_error_no_ext <- function(sim_1,
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

  ## tip info
  stt_last_row_sim_1 <-
    length(sim_1[[1]][[1]]$stt_all[, "present"])
  stt_last_row_sim_2 <-
    length(sim_2[[1]][[1]]$stt_all[, "present"])

  # ## clade number error
  # num_col_sim_1 <-
  #   as.numeric(sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "present"])
  # num_col_sim_2 <-
  #   as.numeric(sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "present"])
  # num_col_error <-
  #   abs(num_col_sim_1 - num_col_sim_2)

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

  return(
    c(total_nltt,
         clade_nltt,
         num_ana,
         num_clado,
         num_nonend,
         clade_size,
         colon_time)
  )
}


#' Get the NLTT dataframe from the branching times from each exist clade.
#'
#' @param sim A datalist of observed data with more than one replicate.
#'
#' @author Shu Xie
#' @export

full_ltt <- function (sim) {
  brt <- lapply(sim[[1]][-1],"[[", "branching_times")
  recolon <-lapply(sim[[1]],"[[", "all_colonisations")
  recolon <- recolon[!unlist(lapply(recolon, is.null))]
  recolon_brt <- list()
  if(length(recolon) > 0){
    for (i in 1: length(recolon)){
      recolon_brt <- append(recolon_brt,lapply(recolon[[i]],"[[", "event_times"))
    }
  }
  brt_full <- append(brt, recolon_brt)
  brt <- c(unique(sort(unlist(brt_full), decreasing = TRUE)), 0)
  n_spec <- c(seq(0,length(brt)-2),length(brt)-2)
  full_ltt <- data.frame(brt, n_spec)
  return(full_ltt)
}


#' Get the number of exist colonist through time from
#' the branching times from each exist clade.
#'
#' @param sim A datalist of observed data with more than one replicate.
#'
#' @author Shu Xie
#' @export

clade_ltt <- function (sim) {
  brt <- lapply(sim[[1]][-1],"[[", "branching_times")
  colon_time <- sapply(brt,function(x) x[2])
  island_age <- sim[[1]][[2]]$branching_times[1]
  colon_time <- c(island_age, unique(sort(colon_time, decreasing = TRUE)), 0)
  n_clade <- c(seq(0,length(colon_time)-2),length(colon_time)-2)
  clade_ltt <- data.frame(colon_time,n_clade)
  return(clade_ltt)
}

