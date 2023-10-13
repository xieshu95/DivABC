#' Calculate summary statistic differences between simulated data and a specific
#' replicate of (simulated) observed data.
#'
#' @param sim1 A datalist of observed data with more than one replicate.
#' @param sim2 A datalist of simulated data created by DAISIE simulation model.
#' @param replicates The number of replicates used for calculating summary
#'   statistics.
#'
#' @return A list with numeric vectors of diff statistics for:
#' \itemize{
#'   \item{\code{$spec_nltt_error}}
#'   \item{\code{$num_spec_error}}
#'   \item{\code{$num_col_error}}
#'   \item{\code{$endemic_nltt_error}}
#'   \item{\code{$nonendemic_nltt_error}}
#' }
#' @author Shu Xie
#' @export

calc_ss_diff_traisie <- function(sim1, sim2, ss_set){
  ss <- calc_error_trait(sim_1 = sim1,
                         sim_2 = sim2,
                         replicates = 1,
                         distance_method = "abs")
  ss_diff <- select_ss_DAISIE(ss,ss_set)
  return(ss_diff)
}


calc_ss_diff_daisie <- function(sim1, sim2, ss_set){
  if (ss_set == 0){ ## use all ss: nltt+tips 10ss
    ss <- calc_error_all(sim_1 = sim1,   ##calc_error
                            sim_2 = sim2,
                            replicates = 1,
                            distance_method = "abs")
  } else if(ss_set == 1) { # nltt+cssd
    ss <- calc_error_phylo(sim_1 = sim1,   ##calc_error
                                sim_2 = sim2,
                                replicates = 1,
                                distance_method = "abs")
  } else if (ss_set == 2){ # tips
    ss <- calc_error_tips(sim_1 = sim1,   ##calc_error
                                 sim_2 = sim2,
                                 replicates = 1,
                                 distance_method = "abs")
  } else if (ss_set == 3){ # nltt
    ss <- calc_error_nltt(sim_1 = sim1,   ##calc_error
                                 sim_2 = sim2,
                                 replicates = 1,
                                 distance_method = "abs")
  }
  ss_diff <- as.numeric(ss)

  return(ss_diff)
}


calc_ss_diff_secsse <- function(sim1, sim2, ss_set){
  if (ss_set == 0){ # nltt + nltt1 + nltt2 + D
    ss <- calc_error_secsse(sim_1 = sim1,
                            sim_2 = sim2,
                            distance_method = "abs")
  } else if(ss_set == 1) { # nltt + nltt1 + nltt2
    ss <- calc_error_secsse_nltt(sim_1 = sim1,
                                 sim_2 = sim2,
                                 distance_method = "abs")
  } else if(ss_set == 2) { # nltt + D
    ss <- calc_error_secsse_D_nltt(sim_1 = sim1,
                                 sim_2 = sim2,
                                 distance_method = "abs")
  } else if(ss_set == 3) { # D
    ss <- calc_error_secsse_D(sim_1 = sim1,
                                 sim_2 = sim2,
                                 distance_method = "abs")
  }
  ss_diff <- as.numeric(ss)
  return(ss_diff)
}




# all ss trait:
# s$spec_nltt_error_state1,
# s$spec_nltt_error_state2,
# s$endemic_nltt_error_state1,
# s$endemic_nltt_error_state2,
# s$nonendemic_nltt_error_state1,
# s$nonendemic_nltt_error_state2,
# s$num_spec_error_state1,
# s$num_spec_error_state2,
# s$endemic_error_state1,
# s$endemic_error_state2,
# s$nonendemic_error_state1,
# s$nonendemic_error_state2,
# s$num_col_error,
# s$tip_ratio_error,
# clade_size_error,
# colon_time_error


#' calculate the initial epsilon
#'
#' @author Shu Xie
#' @return
#' @export
calc_epsilon_init <- function(sim){
  ss <- calc_ss(sim[[1]],1)
  eps_init <- as.numeric(unlist(ss)) * 1
  return(eps_init)
}


#' calculate the initial epsilon
#'
#' @author Shu Xie
#' @return
#' @export
calc_epsilon_init_secsse <- function(sim){
  ss <- calc_ss_secsse(sim[[1]])
  eps_init <- as.numeric(unlist(ss)) * 1
  return(eps_init)
}


#' Select the combination of summary statitsics
#'
#' @param ss A vector contains all the calculated summary statistics
#' @param ss_set A numeric to choose which combination of summary statistics
#'
#' @author Shu Xie
#' @export

select_ss_DAISIE <- function(ss,ss_set){
  if(ss_set == 0){
    select_ss <- as.numeric(ss)
  } else if(ss_set > 10){
    select_ss <- as.numeric(ss)
  }  else {
    select_ss <- as.numeric(ss[-ss_set])
  }
  return(select_ss)
}


#' Select the combination of summary statitsics
#'
#' @param ss A vector contains all the calculated summary statistics
#' @param ss_set A numeric to choose which combination of summary statistics
#'
#' @author Shu Xie
#' @export
select_ss_secsse <- function (ss,ss_set){
  if(ss_set == 0){
    select_ss <- as.numeric(ss)
  } else if(ss_set > 10){
    select_ss <- as.numeric(ss)
  } else {
    select_ss <-as.numeric(ss[-ss_set])
  }
  return(select_ss)
}
