#' Loads the parameter space into the environment for simulations
#'
#' @inheritParams default_params_doc
#' @author Shu Xie
#' @return list of observed data.
#' @export
load_obs_sim <- function(param_space_name) {
  file_domain <-
    "https://raw.githubusercontent.com/xieshu95/Traisie-ABC/daisie/data/obs_sims_" # nolint
  file <- paste0(file_domain, param_space_name, ".rds")
  obs_sim <- readRDS(url(file))
  return(obs_sim)
}
