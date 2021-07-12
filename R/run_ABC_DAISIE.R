#' Run ABC
#'
#' @author Shu Xie
#' @return
#' @export

run_ABC <- function(param_space_name,
                    param_set,
                    save_output = TRUE){

  testit::assert(is.character(param_space_name))

  param_space <- load_param_space(param_space_name = param_space_name)

  message(Sys.time())
  message("Param space name: ", param_space_name)
  message("Running param set: ", param_set)
  message("CAUTION: Do not submit jobs simultaneously in order for jobs to have
          different seeds.")

  check_create_folders(
    param_space_name = param_space_name,
    save_output = save_output
  )

  rep <- as.numeric(param_space[param_set,1])
  set.seed(rep * 100)

  obs_sim_pars <- param_space[param_set,]
  obs_sim <- get_DAISIE_sim(parameters = as.numeric(obs_sim_pars[2:5]),
                            K = as.numeric(obs_sim_pars[6]),
                            replicates = 1)
  prior_gen <- function(){
    lac <- stats::runif(1,0,0.5)
    mu <- stats::runif(1,0,0.5)
    gam <- stats::runif(1,0,0.02)
    laa <- stats::runif(1,0,0.5)
    param <- c(lac,mu,gam,laa)
    return(param)
  }

  prior_dens <- function(x) {
    return(stats::dunif(x[1],0,0.5) * stats::dunif(x[2],0,0.5) * stats::dunif(x[3],0,0.02) * stats::dunif(x[4],0,0.5))
  }

  abc <- ABC_SMC_DAISIE (
    obs_data = obs_sim,
    sim_function = get_DAISIE_sim,
    init_epsilon_values = c(150,150,50,50,50,20),
    prior_generating_function = prior_gen,
    prior_density_function = prior_dens,
    number_of_particles = 5000,
    sigma = 0.05,
    stop_rate = 1e-3,
    replicates = 1,  ## simulation replicates for each parameter set
    num_iterations = 10,
    K = as.numeric(obs_sim_pars[6])
  )
  if (save_output == TRUE) {
    save_output(
      output = abc,
      param_space_name = param_space_name,
      param_set = param_set,
      rep = rep
    )
  } else {
    return(abc)
  }
  return(abc)
}

