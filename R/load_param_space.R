#' Loads the parameter space into the environment for simulations
#'
#' @inheritParams default_params_doc
#' @author Shu Xie
#' @return tibble parameter space.
#' @export
load_param_space <- function(param_space_name) {
  # file_domain <-
  #   "https://raw.githubusercontent.com/xieshu95/TraisieABC/daisie/data/" # nolint
  # file <- paste0(file_domain, param_space_name, ".csv")
  # param_space <- readr::read_csv2(
  #   file = file
  # )

  loaded_name <- load(system.file(
    "extdata", paste0(param_space_name, ".rda"), package = "TraisieABC")
  )
  assign("param_space", get(loaded_name))
  return(param_space)
}
