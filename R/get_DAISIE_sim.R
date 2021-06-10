#' Simulation fucntion to create simulated data as observed data in ABC.
#'
#' @param parameters A vector for CES rates.
#' @param K Carrying capacity, Inf for diverdity-independent models.
#' @param replicates The number of replicates(islands) for DAISIE simulation.
#'
#' @return A list contains simulated islands
#' @author Shu Xie
#' @export


get_DAISIE_sim <-  function(parameters, K, replicates){
  sim <- list()
  for (j in seq_len(replicates)) {
    sim[[j]] <- DAISIE::DAISIE_sim_constant_rate(
      time = 5,
      M = 1000,
      pars = c(parameters[1],parameters[2],K,parameters[3],parameters[4]),
      replicates = 1,
      sample_freq  = Inf,
      plot_sims = FALSE,
      verbose = FALSE,
      cond = 0
    )
  }
  return(sim)
}