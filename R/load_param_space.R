#' Loads the parameter space into the environment for simulations
#'
#' @inheritParams default_params_doc
#' @author Shu Xie
#' @return parameter space.
#' @export
load_param_space <- function(param_space_name) {
  loaded_name <- load(system.file(
    "extdata", paste0(param_space_name, ".rda"), package = "DivABC")
  )
  assign("param_space", get(loaded_name))
  return(param_space)
}
