#' Create reference tables for abcrf
#' @param save_output A boolean to determine whether to save or return output.
#' @param id A numeric of id of the reference table.
#' @author Shu Xie
#' @return
#' @export

create_ref_table <- function(
    save_output = TRUE,
    id = 1){

  library(abcrf)
  library(treestats)
  library(ape)
  library(R.utils)
  library(MASS)
  set.seed(100)
  n_sim   <- 3

  # generate training data (reference table)
  tmp <- get_bisse_sim_create_obs(parameters = c(0.2,0.1,0.1,0.1,0.1,0.1),
                                  pool_init_states = c("1","2"))
  # calc phylogenetic SS of treestats(Thijs)
  tmp_s1 <- treestats::calc_all_stats(tmp[[1]]$phy)
  # calc trait-related SS
  tmp_s2 <- unlist(calc_ss_bisse(tmp[[1]]))
  tmp_s <- c(tmp_s1,tmp_s2)
  stat_names <- names(tmp_s)
  ref_mat <- matrix(NA, nrow = n_sim, ncol = 6 + length(stat_names))
  colnames(ref_mat) <- c("lambda1","lambda2","mu1","mu2","q12","q21", stat_names)
  i <- 1
  while(i <= n_sim) {
    message("sim: ", i)
    params <- prior_gen_bisse (pars = rep(0.1,6),idparsopt = 1:6)
    message("params: ", params)
    # sim a tree with n_tips extant (rphylo tries to produce n extant)
    tr <- tryCatch(withTimeout({get_bisse_sim_create_obs(parameters = params,
                                                         pool_init_states = c("1","2"))},
                               timeout = 30, onTimeout = "error"), error = function(e) NULL)
    if(is.null(tr)) next
    st1 <- tryCatch(treestats::calc_all_stats(tr[[1]]$phy), error = function(e) NA)
    st2 <- tryCatch(unlist(calc_ss_bisse(tr[[1]])), error = function(e) NA)
    st <- c(st1,st2)
    if(any(is.na(st))) next
    ref_mat[i, ] <- c(params, unname(st))
    i <- i + 1
  }
  ref_df <- as.data.frame(ref_mat)
  ref_df <- ref_df[complete.cases(ref_df), ]
  rownames(ref_df) <- NULL
  cat("Generated", nrow(ref_df), "simulations for reference table\n")

  if (save_output == TRUE) {
    save_ref_table(ref_df = ref_df,
                   id = id)
  } else {
    return(ref_df)
  }
}
