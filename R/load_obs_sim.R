#' Loads the parameter space into the environment for simulations
#'
#' @inheritParams default_params_doc
#' @author Shu Xie
#' @return list of observed data.
#' @export
load_obs_sim <- function(scenario) {
  # file_domain <-
  #   "https://raw.githubusercontent.com/xieshu95/Traisie-ABC/daisie/data/obs_sims_" # nolint
  # file <- paste0(file_domain, scenario, ".rds")
  # obs_sim <- readRDS(url(file))

  loaded_name <- load(system.file(
    "extdata", paste0("obs_sims_",scenario, ".rda"), package = "TraisieABC")
  )
  assign("obs_sim", get(loaded_name))
  return(obs_sim)
}

