#' Default parameter documentation
#' @param sim A list of simulated data from DAISIE or BiSSE or TraiSIE model.
#' @param sim_1 A list of observed data from DAISIE or BiSSE or TraiSIE model.
#' @param sim_2 A list of simulated data from DAISIE or BiSSE or TraiSIE model.
#' @param brt A list of vectors containing the branching times in each clade.
#' @param scenario A string of a scenario name which contains a series of
#' parameter sets.
#' @param param_set A numeric with the line corresponding to parameter set to
#'  run, as found in the file named in \code{param_space}.
#' @param parameter A numeric vector of the parameters for generating simulations.
#'  \cr \cr \code{pars1[1]} corresponds to lambda^c (cladogenesis rate)
#'  \cr \code{pars1[2]} corresponds to mu (extinction rate)
#'  \cr \code{pars1[3]} corresponds to gamma (immigration rate)
#'  \cr \code{pars1[4]} corresponds to lambda^a (anagenesis rate).
#' @param replicates A numeric for the number of replicates for the
#'  simulations.
#' @param distance_method The method of calculating nLTT statistics, choose from
#' absolute distance or squared distance.
#' @param weights A vector of weights
#' @param particles A list of parameter combinations
#' @param current A vector of current parameter combination to determine the
#'                weight.
#' @param sigma Standard deviation of the pertubation.
#' @param prior_density_function Function to calculate the prior probability.
#'
#'
#' @author Shu Xie
#'
#'
default_params_doc <- function(
  sim,
  sim_1,
  sim_2,
  brt,
  scenario,
  param_set,
  parameter,
  replicates,
  distance_method,
  weights,
  particles,
  current,
  sigma,
  prior_density_function
) {
  # Nothing
}
