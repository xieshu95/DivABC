#' Calculate summary statistic differences between simulated data and a specific
#' replicate of (simulated) observed data.
#'
#' @param obs_rep A numeric which means a specific replicate of observed data
#'   used to calculate difference of summary statistics.
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

calc_ss_diff <- function(obs_rep, sim1, sim2, replicates){
  spec_nltt_error <- c()
  num_spec_error <- c()
  num_col_error <- c()
  endemic_nltt_error <- c()
  nonendemic_nltt_error <- c()
  for(i in 1:replicates){
    obs_sim <- sim1[[obs_rep]]   ##change to [[1]]
    parallel_sim <- sim2[[i]]
    s <- DAISIErobustness:::calc_error(sim_1 = obs_sim,
                                       sim_2 = parallel_sim,
                                       replicates = 1,
                                       distance_method = "abs")
    spec_nltt_error <- append(spec_nltt_error,
                              s$spec_nltt_error)
    num_spec_error <- append(num_spec_error,
                             s$num_spec_error)
    num_col_error <- append(num_col_error,
                            s$num_col_error)
    endemic_nltt_error <- append(endemic_nltt_error,
                                 s$endemic_nltt_error)
    nonendemic_nltt_error <- append(nonendemic_nltt_error,
                                    s$nonendemic_nltt_error)
  }
  list_s <- list(spec_nltt_error = spec_nltt_error,
                 num_spec_error = num_spec_error,
                 num_col_error = num_col_error,
                 endemic_nltt_error = endemic_nltt_error,
                 nonendemic_nltt_error = nonendemic_nltt_error)
  return(list_s)
}

