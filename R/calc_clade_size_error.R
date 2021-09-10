#' Calculate clade size standard deviation among clades in a DAISIE simulation.
#'
#' @param sim DAISIE simulation.
#'
#' @return A numeric for clade size standard  ddeviation.
#' @author Shu Xie
#' @export


# clade_size_sd <- function(sim){
#   clade_size <- c()
#   if (length(sim[[1]][[1]]) == 1){
#     clade_size_sd <- 0
#   } else {
#     for(i in 2:length(sim[[1]][[1]])){ ##clades
#       clade_size[i - 1] <- length(sim[[1]][[1]][[i]]$branching_times) - 1
#     }
#     if(length(clade_size) == 1){
#       clade_size_sd <- 0
#     } else{
#       clade_size_sd <- sd(clade_size)
#     }
#   }
#   return(clade_size_sd)
# }
clade_size_sd <- function(sim){
  clade_size <- c()
  if (length(sim[[1]]) == 1){
    clade_size_sd <- 0
  } else {
    for(i in 2:length(sim[[1]])){ ##clades
      clade_size[i - 1] <- length(sim[[1]][[i]]$branching_times) - 1
    }
    if(length(clade_size) == 1){
      clade_size_sd <- 0
    } else{
      clade_size_sd <- sd(clade_size)
    }
  }
  return(clade_size_sd)
}

#' Calculate error of clade size standard deviation between two simulated data.
#'
#' @param sim_1 DAISIE simulation.
#' @param sim_2 DAISIE simulation.
#' @return A numeric for clade size standard  ddeviation.
#' @author Shu Xie
#' @export
calc_clade_size_error <- function(sim_1, sim_2){
  sim1_cs_sd <- clade_size_sd(sim_1)
  sim2_cs_sd <- clade_size_sd(sim_2)
  clade_size_error <- abs(sim1_cs_sd - sim2_cs_sd)
  return(clade_size_error)
}
