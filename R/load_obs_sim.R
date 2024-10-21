#' Loads the parameter space into the environment for simulations
#'
#' @inheritParams default_params_doc
#' @author Shu Xie
#' @return list of observed data.
#' @export
load_obs_sim <- function(param_space_name) {
  loaded_name <- load(system.file(
    "extdata", paste0("obs_sims_",param_space_name, ".rda"), package = "DivABC")
  )
  assign("obs_sim", get(loaded_name))
  return(obs_sim)
}

