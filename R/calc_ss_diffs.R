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

# calc_ss_diff_daisie <- function(sim1, sim2, ss_set){
#   ss <- calc_error_no_ext(sim_1 = sim1,   ##calc_error
#                           sim_2 = sim2,
#                           replicates = 1,
#                           distance_method = "abs")
#   ss_diff <- select_ss_DAISIE(ss,ss_set)
#
#   return(ss_diff)
# }

calc_ss_diff_daisie <- function(sim1, sim2, ss_set){
  if (ss_set == 0){ ## all
    ss <- calc_error_all(sim_1 = sim1,
                         sim_2 = sim2)
  } else if(ss_set == 1) { # phylogenetic
    ss <- calc_error_phylo(sim_1 = sim1,
                           sim_2 = sim2)
  } else if (ss_set == 2){ # tips
    ss <- calc_error_tips(sim_1 = sim1,
                          sim_2 = sim2)
  } else if (ss_set == 3){ # nltt
    ss <- calc_error_nltt(sim_1 = sim1,
                          sim_2 = sim2)
  }
  ss_diff <- as.numeric(ss)

  return(ss_diff)
}

calc_ss_diff_secsse <- function(sim1, sim2, ss_set){
  if (ss_set == 0){ # nltt + nltt1 + nltt2 + D
    ss <- calc_error_secsse(sim_1 = sim1,
                            sim_2 = sim2)
  } else if(ss_set == 1) { # nltt + nltt1 + nltt2
    ss <- calc_error_secsse_nltts(sim_1 = sim1,
                                  sim_2 = sim2)
  } else if(ss_set == 2) { # nltt + D
    ss <- calc_error_secsse_D_nltt(sim_1 = sim1,
                                   sim_2 = sim2)
  } else if(ss_set == 3) { # D
    ss <- calc_error_secsse_D(sim_1 = sim1,
                              sim_2 = sim2)
  } else if(ss_set == 4) { # mpd1 + mpd2 + D
    ss <- calc_error_secsse_mpd_D(sim_1 = sim1,
                                  sim_2 = sim2)
  } else if(ss_set == 5) { # mntd1 + mntd2 + D
    ss <- calc_error_secsse_mntd_D(sim_1 = sim1,
                                   sim_2 = sim2)
  } else if(ss_set == 6) { # colless1 + colless2 + D
    ss <- calc_error_secsse_colless_D(sim_1 = sim1,
                                      sim_2 = sim2)
  } else if(ss_set == 7) { # mpd1 + mpd2 + nltt
    ss <- calc_error_secsse_mpd_nltt(sim_1 = sim1,
                                     sim_2 = sim2)
  } else if(ss_set == 8) { # nltt + nltt1 + nltt2 + D + num1 + num2
    ss <- calc_error_secsse_num(sim_1 = sim1,
                                sim_2 = sim2)
  }

  ss_diff <- as.numeric(ss)
  return(ss_diff)
}


# all ss for DAISIE
# spec_nltt_error = spec_nltt_error,
# ana_endemic_nltt_error = ana_endemic_nltt_error,
# clado_endemic_nltt_error = clado_endemic_nltt_error,
# nonendemic_nltt_error = nonendemic_nltt_error,
# num_spec_error = num_spec_error,
# ana_endemic_error = ana_endemic_error,
# clado_endemic_error = clado_endemic_error,
# nonendemic_error = nonendemic_error,
# num_col_error = num_col_error





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
# calc_epsilon_init <- function(sim,ss_set){
#   ss_diff_pairs <- c()
#   replicates <- length(sim)
#   for (i in 1:(replicates-1)){
#     for (j in (i + 1):replicates){
#       ss_diff <- calc_ss_diff(sim1 = sim[[i]],
#                               sim2 = sim[[j]],
#                               ss_set = ss_set)
#       ss_diff_pairs <- data.frame(rbind(ss_diff_pairs,ss_diff))
#     }
#   }
#   ss_diff_pairs_median <- apply(ss_diff_pairs,2,mean)
#   epsilon_init <- 8*ss_diff_pairs_median ##9 for DAISIE
#   return(epsilon_init)
# }
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
