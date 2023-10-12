#' Loads the parameter sets into the environment for simulations
#'
#' @inheritParams default_params_doc
#' @author Shu Xie
#' @return tibble parameter space in a certain scenario.
#' @export
load_scenario <- function(scenario) {
  loaded_name <- load(system.file(
    "extdata", paste0(scenario, ".rda"), package = "DivABC")
  )
  assign("scenario", get(loaded_name))
  return(scenario)
}
