#' Run ABC in parallel
#'
#' @author Tianjian Qin, Shu Xie
#' @return
#' @export

run_mcmc_DAISIE_parallel <- function(param_space_name,
                                    idparsopt,
                                    save_output = TRUE,
                                    strategy = future::multisession,
                                    workers = NULL){

  obs_sim_pars <- load_param_space(param_space_name = param_space_name)
  obs_sim_pars <- obs_sim_pars[1:16, ]
  obs_sim_pars <- split(obs_sim_pars, seq(nrow(obs_sim_pars)))
  rep <- as.numeric(param_space[param_set,1])
  set.seed(rep)

  message("Param space name: ", param_space_name)
  message("Running param set: ", param_set)
  message("seed: ", rep)

  check_create_folders(
    param_space_name = param_space_name,
    save_output = save_output
  )

  if (is.null(workers)) {
    stop("Number of workers not set")
  }

  if (identical(future::multisession, strategy)){
    message(paste0(
      "Running multisession simulation with ",
      workers,
      " workers"
    ))
    future::plan(strategy, workers = workers)
    future_opts <- furrr::furrr_options(seed = TRUE)
    output <- furrr::future_map(
      .x = obs_sim_pars,
      .f = TraisieABC::run_mcmc_DAISIE_wrapper,
      .options = future_opts,
      idparsopt = idparsopt
    )
  } else if (identical(future::sequential, strategy)) {
    message("Running sequential simulation")
    output <- purrr::map(
      .x = obs_sim_pars,
      .f = TraisieABC::run_mcmc_DAISIE_wrapper,
      idparsopt = idparsopt
    )
  }

  return(output)
}

run_mcmc_DAISIE_wrapper <- function(obs_sim_pars, idparsopt) {
  obs_sim <- get_DAISIE_sim(
    parameters = c(
      obs_sim_pars$lac,
      obs_sim_pars$mu,
      obs_sim_pars$gam,
      obs_sim_pars$laa
    ),
    K = as.numeric(obs_sim_pars$K),
    replicates = 1
  )

  mcmc <- MCMC_DAISIE(obs_sim[[1]][[1]], calc_loglik,
                      parameters = as.numeric(c(obs_sim_pars$lac,
                                                obs_sim_pars$mu,
                                                obs_sim_pars$gam,
                                                obs_sim_pars$laa)),
                      iterations = 5,
                      burnin = 8,
                      thinning = 1,
                      sigma = 1,
                      idparsopt = idparsopt)

  return(mcmc)
}