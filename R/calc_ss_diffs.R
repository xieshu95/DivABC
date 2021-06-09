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


calc_ss_diff <- function(sim1, sim2, replicates){
  spec_nltt_error <- c()
  num_spec_error <- c()
  num_col_error <- c()
  endemic_nltt_error <- c()
  nonendemic_nltt_error <- c()
  for(i in 1:replicates){
    s <- DAISIErobustness:::calc_error(sim_1 = sim1[[1]],
                                       sim_2 = sim2[[i]],
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
  clade_size_error <- calc_clade_size_error(sim_1 = sim1,
                                            sim_2 = sim2)
  list_s <- list(spec_nltt_error = spec_nltt_error,
                 endemic_nltt_error = endemic_nltt_error,
                 nonendemic_nltt_error = nonendemic_nltt_error,
                 num_spec_error = num_spec_error,
                 num_col_error = num_col_error,
                 clade_size_error = clade_size_error)
  return(list_s)
}