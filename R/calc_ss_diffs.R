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

calc_ss_diff <- function(sim1, sim2){

  clade_size_error <- calc_clade_size_error(sim_1 = sim1,
                                            sim_2 = sim2)
  colon_time_error <- calc_colon_time_error(sim_1 = sim1,
                                            sim_2 = sim2)
  if("stt_two_states" %in% names(sim1[[1]][[1]])){  ##TraiSIE
    s <- calc_error_trait(sim_1 = sim1,
                          sim_2 = sim2,
                          replicates = 1,
                          distance_method = "abs")
    ss_diff <-as.numeric(c(s$ana_endemic_nltt_error,
                           s$clado_endemic_nltt_error,
                           s$nonendemic_nltt_error,
                           s$clade_nltt_error,
                           s$num_end_error_state1,
                           s$num_end_error_state2,
                           s$num_nonend_error_state1,
                           s$num_nonend_error_state2))
  } else { ## DAISIE
    s <- calc_error(sim_1 = sim1,   ##calc_error
                    sim_2 = sim2,
                    replicates = 1,
                    distance_method = "abs")
    ss_diff <-as.numeric(c(s$clade_nltt_error,
                           s$ana_endemic_nltt_error,
                           s$clado_endemic_nltt_error,
                           s$nonendemic_nltt_error,
                           s$num_col_error,
                           clade_size_error,
                           colon_time_error))
  }
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
calc_epsilon_init <- function(sim){
  ss_diff_pairs <- c()
  replicates <- length(sim)
  for (i in 1:(replicates-1)){
    for (j in (i + 1):replicates){
      ss_diff <- calc_ss_diff(sim1 = sim[[i]], sim2 =sim[[j]])
      ss_diff_pairs <- data.frame(rbind(ss_diff_pairs,ss_diff))
    }
  }
  ss_diff_pairs_median <- apply(ss_diff_pairs,2,median)
  epsilon_init <- 8*ss_diff_pairs_median ##9 for DAISIE
  return(epsilon_init)
}