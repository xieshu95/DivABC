#' Loads the parameter space into the environment for simulations
#'
#' @inheritParams default_params_doc
#' @author Shu Xie
#' @return list of observed data.
#' @export
load_obs_sim <- function(sim_model) {
  file_domain <-
    "https://raw.githubusercontent.com/xieshu95/Traisie-ABC/daisie/data/obs_sims_" # nolint
  file <- paste0(file_domain, sim_model, ".rds")
  obs_sim <- readRDS(url(file))
  return(obs_sim)
}
