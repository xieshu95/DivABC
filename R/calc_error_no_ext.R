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
# nltt
calc_error_no_ext_nltt <- function(sim_1,
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


  # # Clades number nltt error
  # clade_ltt_1 <- clade_ltt(sim_1)
  # clade_ltt_2 <- clade_ltt(sim_2)
  #
  # clade_nltt <- nLTT::nltt_diff_exact_extinct(
  #   event_times = clade_ltt_1$colon_time,
  #   species_number = clade_ltt_1$n_clade,
  #   event_times2 = clade_ltt_2$colon_time,
  #   species_number2 = clade_ltt_2$n_clade,
  #   distance_method = distance_method,
  #   time_unit = "ago",
  #   normalize = FALSE
  # )

  # clade_size <- calc_clade_size_error(sim_1,sim_2)
  colon_time <- calc_colon_time_error(sim_1,sim_2)

  ## nonendemic_nltt and singleton-endemic-nltt
  end_ltt_1 <- end_ltt(sim_1)
  end_ltt_2 <- end_ltt(sim_2)

  nonend_ltt_1 <- end_ltt_1$nonend_ltt
  nonend_ltt_2 <- end_ltt_2$nonend_ltt
  # total number species nltt error
  if(nonend_ltt_1[1,1] == 0 && nonend_ltt_2[1,1] == 0) {
    nonend_nltt  <- 0
  } else {
    nonend_nltt <- nLTT::nltt_diff_exact_extinct(
      event_times = nonend_ltt_1$nonend_brt,
      species_number = nonend_ltt_1$n_nonend,
      event_times2 = nonend_ltt_2$nonend_brt,
      species_number2 = nonend_ltt_2$n_nonend,
      distance_method = distance_method,
      time_unit = "ago",
      normalize = FALSE
    )}

  singleton_ltt_1 <- end_ltt_1$singleton_ltt
  singleton_ltt_2 <- end_ltt_2$singleton_ltt
  # total number species nltt error
  if(singleton_ltt_1[1,1] == 0 && singleton_ltt_2[1,1] == 0) {
    singleton_nltt  <- 0
  } else {
    singleton_nltt <- nLTT::nltt_diff_exact_extinct(
      event_times = singleton_ltt_1$singleton_brt,
      species_number = singleton_ltt_1$n_singleton,
      event_times2 = singleton_ltt_2$singleton_brt,
      species_number2 = singleton_ltt_2$n_singleton,
      distance_method = distance_method,
      time_unit = "ago",
      normalize = FALSE
    )
  }


  return(
    c(total_nltt,
      # clade_nltt,
      singleton_nltt,
      nonend_nltt,
      # clade_size,
      colon_time)
  )
}

# tips
calc_error_no_ext_tips <- function(sim_1,
                              sim_2,
                              replicates,
                              distance_method) {

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
  num_sington_sim_1 <-
    as.numeric(sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nA"])
  num_sington_sim_2 <-
    as.numeric(sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nA"])
  num_sington <-
    abs(num_sington_sim_1 - num_sington_sim_2)

  ##  multiple-species clade sepcies number error(cladogenesis)
  num_multi_sim_1 <-
    as.numeric(sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nC"])
  num_multi_sim_2 <-
    as.numeric(sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nC"])
  num_multi <-
    abs(num_multi_sim_1 - num_multi_sim_2)

  ##  nonendemic sepcies number error
  num_nonend_sim_1 <-
    as.numeric(sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nI"])
  num_nonend_sim_2 <-
    as.numeric(sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nI"])
  num_nonend <-
    abs(num_nonend_sim_1 - num_nonend_sim_2)

  num_total_sim_1 <- num_sington_sim_1 + num_multi_sim_1 + num_nonend_sim_1
  num_total_sim_2 <- num_sington_sim_2 + num_multi_sim_2 + num_nonend_sim_2
  num_total <- abs(num_total_sim_1 - num_total_sim_2)

  clade_size <- calc_clade_size_error(sim_1,sim_2)
  # colon_time <- calc_colon_time_error(sim_1,sim_2)

  return(
    c(num_total,
      num_sington,
      num_nonend,
      clade_size)
      # colon_time,)
      # num_col_error
  )
}

# nltt + tips
calc_error_no_ext_all <- function(sim_1,
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


  # # Clades number nltt error
  # clade_ltt_1 <- clade_ltt(sim_1)
  # clade_ltt_2 <- clade_ltt(sim_2)
  #
  # clade_nltt <- nLTT::nltt_diff_exact_extinct(
  #   event_times = clade_ltt_1$colon_time,
  #   species_number = clade_ltt_1$n_clade,
  #   event_times2 = clade_ltt_2$colon_time,
  #   species_number2 = clade_ltt_2$n_clade,
  #   distance_method = distance_method,
  #   time_unit = "ago",
  #   normalize = FALSE
  # )

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
  num_sington_sim_1 <-
    as.numeric(sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nA"])
  num_sington_sim_2 <-
    as.numeric(sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nA"])
  num_sington <-
    abs(num_sington_sim_1 - num_sington_sim_2)

  ##  multiple-species clade sepcies number error(cladogenesis)
  num_multi_sim_1 <-
    as.numeric(sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nC"])
  num_multi_sim_2 <-
    as.numeric(sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nC"])
  num_multi <-
    abs(num_multi_sim_1 - num_multi_sim_2)

  ##  nonendemic sepcies number error
  num_nonend_sim_1 <-
    as.numeric(sim_1[[1]][[1]]$stt_all[stt_last_row_sim_1, "nI"])
  num_nonend_sim_2 <-
    as.numeric(sim_2[[1]][[1]]$stt_all[stt_last_row_sim_2, "nI"])
  num_nonend <-
    abs(num_nonend_sim_1 - num_nonend_sim_2)

  num_total_sim_1 <- num_sington_sim_1 + num_multi_sim_1 + num_nonend_sim_1
  num_total_sim_2 <- num_sington_sim_2 + num_multi_sim_2 + num_nonend_sim_2
  num_total <- abs(num_total_sim_1 - num_total_sim_2)

  clade_size <- calc_clade_size_error(sim_1,sim_2)
  colon_time <- calc_colon_time_error(sim_1,sim_2)

  ## nonendemic_nltt and singleton-endemic-nltt
  end_ltt_1 <- end_ltt(sim_1)
  end_ltt_2 <- end_ltt(sim_2)

  nonend_ltt_1 <- end_ltt_1$nonend_ltt
  nonend_ltt_2 <- end_ltt_2$nonend_ltt
  # total number species nltt error
  if(nonend_ltt_1[1,1] == 0 && nonend_ltt_2[1,1] == 0) {
    nonend_nltt  <- 0
  } else {
    nonend_nltt <- nLTT::nltt_diff_exact_extinct(
      event_times = nonend_ltt_1$nonend_brt,
      species_number = nonend_ltt_1$n_nonend,
      event_times2 = nonend_ltt_2$nonend_brt,
      species_number2 = nonend_ltt_2$n_nonend,
      distance_method = distance_method,
      time_unit = "ago",
      normalize = FALSE
    )}

  singleton_ltt_1 <- end_ltt_1$singleton_ltt
  singleton_ltt_2 <- end_ltt_2$singleton_ltt
  # total number species nltt error
  if(singleton_ltt_1[1,1] == 0 && singleton_ltt_2[1,1] == 0) {
    singleton_nltt  <- 0
  } else {
    singleton_nltt <- nLTT::nltt_diff_exact_extinct(
      event_times = singleton_ltt_1$singleton_brt,
      species_number = singleton_ltt_1$n_singleton,
      event_times2 = singleton_ltt_2$singleton_brt,
      species_number2 = singleton_ltt_2$n_singleton,
      distance_method = distance_method,
      time_unit = "ago",
      normalize = FALSE
    )
  }


  return(
    c(total_nltt,
      # clade_nltt,
      singleton_nltt,
      nonend_nltt,
      colon_time,
      clade_size,
      num_total,
      num_sington,
      num_nonend)
      # num_col_error)
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


#' Get the number of clades through time from
#' the colonization times from each exist clade.
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


end_ltt <- function(sim) {
  brt <- lapply(sim[[1]][-1],"[[", "branching_times")
  stac <- unlist(lapply(sim[[1]][-1],"[[", "stac"))
  nonend_brt <- c(unique(sort(unlist(brt[which(stac ==4)]),
                              decreasing = TRUE)), 0)
  if(length(nonend_brt) == 1) {
    nonend_brt <- 0
    n_nonend <- 0
  } else {
    n_nonend <- c(seq(0,length(nonend_brt)-2),length(nonend_brt)-2)
  }
  nonend_ltt <- data.frame(nonend_brt, n_nonend)

  brt_length <- unlist(lapply(brt, length))
  singleton_brt <-c(unique(sort(unlist(brt[which(stac ==2 & brt_length ==2)]),
                                decreasing = TRUE)), 0)
  if(length(singleton_brt) == 1) {
    singleton_brt <- 0
    n_singleton <- 0
  } else {
    n_singleton <- c(seq(0,length(singleton_brt)-2),length(singleton_brt)-2)
  }
  singleton_ltt <- data.frame(singleton_brt, n_singleton)
  end_ltt <- list(nonend_ltt = nonend_ltt,
                  singleton_ltt = singleton_ltt)
  return(end_ltt)
}


calc_error_no_ext_nltt2 <- function(sim_1,
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


  # # Clades number nltt error
  # clade_ltt_1 <- clade_ltt(sim_1)
  # clade_ltt_2 <- clade_ltt(sim_2)
  #
  # clade_nltt <- nLTT::nltt_diff_exact_extinct(
  #   event_times = clade_ltt_1$colon_time,
  #   species_number = clade_ltt_1$n_clade,
  #   event_times2 = clade_ltt_2$colon_time,
  #   species_number2 = clade_ltt_2$n_clade,
  #   distance_method = distance_method,
  #   time_unit = "ago",
  #   normalize = FALSE
  # )

  clade_size <- calc_clade_size_error(sim_1,sim_2)
  colon_time <- calc_colon_time_error(sim_1,sim_2)

  ## nonendemic_nltt and singleton-endemic-nltt
  end_ltt_1 <- end_ltt(sim_1)
  end_ltt_2 <- end_ltt(sim_2)

  nonend_ltt_1 <- end_ltt_1$nonend_ltt
  nonend_ltt_2 <- end_ltt_2$nonend_ltt
  # total number species nltt error
  if(nonend_ltt_1[1,1] == 0 && nonend_ltt_2[1,1] == 0) {
    nonend_nltt  <- 0
  } else {
    nonend_nltt <- nLTT::nltt_diff_exact_extinct(
      event_times = nonend_ltt_1$nonend_brt,
      species_number = nonend_ltt_1$n_nonend,
      event_times2 = nonend_ltt_2$nonend_brt,
      species_number2 = nonend_ltt_2$n_nonend,
      distance_method = distance_method,
      time_unit = "ago",
      normalize = FALSE
    )}

  singleton_ltt_1 <- end_ltt_1$singleton_ltt
  singleton_ltt_2 <- end_ltt_2$singleton_ltt
  # total number species nltt error
  if(singleton_ltt_1[1,1] == 0 && singleton_ltt_2[1,1] == 0) {
    singleton_nltt  <- 0
  } else {
    singleton_nltt <- nLTT::nltt_diff_exact_extinct(
      event_times = singleton_ltt_1$singleton_brt,
      species_number = singleton_ltt_1$n_singleton,
      event_times2 = singleton_ltt_2$singleton_brt,
      species_number2 = singleton_ltt_2$n_singleton,
      distance_method = distance_method,
      time_unit = "ago",
      normalize = FALSE
    )
  }


  return(
    c(total_nltt,
      # clade_nltt,
      singleton_nltt,
      nonend_nltt,
      clade_size,
      colon_time)
  )
}