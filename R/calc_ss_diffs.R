#' Calculate summary statistic differences between simulated data and a specific
#' replicate of (simulated) observed data.
#'
#' @param sim1 A datalist of observed data with more than one replicate.
#' @param sim2 A datalist of simulated data created by DAISIE simulation model.
#' @param replicates The number of replicates used for calculating summary
#'   statistics.
#'
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

calc_ss_diff_bisse <- function(sim1, sim2, ss_set){
  if (ss_set == 0){ # nltt + nltt1 + nltt2 + D
    ss <- calc_error_bisse(sim_1 = sim1,
                           sim_2 = sim2)
  } else if(ss_set == 1) { # nltt + nltt1 + nltt2
    ss <- calc_error_bisse_nltts(sim_1 = sim1,
                                 sim_2 = sim2)
  } else if(ss_set == 2) { # nltt + D
    ss <- calc_error_bisse_D_nltt(sim_1 = sim1,
                                  sim_2 = sim2)
  } else if(ss_set == 3) { # D
    ss <- calc_error_bisse_D(sim_1 = sim1,
                             sim_2 = sim2)
  } else if(ss_set == 4) { # mpd1 + mpd2 + D
    ss <- calc_error_bisse_mpd_D(sim_1 = sim1,
                                 sim_2 = sim2)
  } else if(ss_set == 5) { # mntd1 + mntd2 + D
    ss <- calc_error_bisse_mntd_D(sim_1 = sim1,
                                  sim_2 = sim2)
  } else if(ss_set == 6) { # colless1 + colless2 + D
    ss <- calc_error_bisse_colless_D(sim_1 = sim1,
                                     sim_2 = sim2)
  } else if(ss_set == 7) { # mpd1 + mpd2 + nltt
    ss <- calc_error_bisse_mpd_nltt(sim_1 = sim1,
                                    sim_2 = sim2)
  } else if(ss_set == 8) { # nltt + nltt1 + nltt2 + D + num1 + num2
    ss <- calc_error_bisse_num(sim_1 = sim1,
                               sim_2 = sim2)
  } else if(ss_set == 9) { # nltt
    ss <- calc_error_bisse_nltt(sim_1 = sim1,
                                sim_2 = sim2)
  } else if(ss_set == 10) { # mntd1 + mntd2 + nltt
    ss <- calc_error_bisse_mntd_nltt(sim_1 = sim1,
                                     sim_2 = sim2)
  } else if(ss_set == 11) { # colless1 + colless2 + nltt
    ss <- calc_error_bisse_colless_nltt(sim_1 = sim1,
                                        sim_2 = sim2)
  } else if(ss_set == 12) { # tip-ratio + nltt
    ss <- calc_error_bisse_ratio_nltt(sim_1 = sim1,
                                      sim_2 = sim2)
  } else if(ss_set == 13) { # tip-ratio + nltt
    ss <- calc_error_bisse_Delta(sim_1 = sim1,
                                 sim_2 = sim2)
  } else if(ss_set == 14) { # tip-ratio + nltt
    ss <- calc_error_bisse_M(sim_1 = sim1,
                             sim_2 = sim2)
  }
  ss_diff <- as.numeric(ss)
  return(ss_diff)
}


calc_ss_diff_musse <- function(sim1, sim2, ss_set){
  if (ss_set == 0){ # nltt + nltt1 + nltt2 + nltt3 + D12+D13+D23
    ss <- calc_error_musse_D(sim_1 = sim1,
                                 sim_2 = sim2)
  } else if(ss_set == 1) { # nltt + nltt1 + nltt2 + nltt3 + M
    ss <- calc_error_musse_M(sim_1 = sim1,
                             sim_2 = sim2)
  }
  ss_diff <- as.numeric(ss)
  return(ss_diff)
}


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
calc_epsilon_init_bisse <- function(sim){
  ss <- calc_ss_bisse(sim[[1]])
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
select_ss_bisse <- function (ss,ss_set){
  if(ss_set == 0){
    select_ss <- as.numeric(ss)
  } else if(ss_set > 10){
    select_ss <- as.numeric(ss)
  } else {
    select_ss <-as.numeric(ss[-ss_set])
  }
  return(select_ss)
}
