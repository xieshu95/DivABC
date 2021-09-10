#' Run ABC_TRAISIE
#'
#' @author Shu Xie
#' @return
#' @export

run_ABC_trait_DAISIE <- function(param_space_name,
                                 param_set,
                                 save_output = TRUE,
                                 with_trait = FALSE){

  testit::assert(is.character(param_space_name))

  param_space <- load_param_space(param_space_name = param_space_name)

  #random <- sample(1:10000,1)
  rep <- as.numeric(param_space[param_set,1])
  seed <- rep
  set.seed(seed)

  message(Sys.time())
  message("Param space name: ", param_space_name)
  message("Running param set: ", param_set)
  message("seed: ", seed)

  check_create_folders(
    param_space_name = param_space_name,
    save_output = save_output
  )

  obs_sim_pars <- param_space[param_set,]
  obs_sim <- get_trait_DAISIE_sim(parameters = as.numeric(c(obs_sim_pars$lac,
                                                 obs_sim_pars$mu,
                                                 obs_sim_pars$gam,
                                                 obs_sim_pars$laa,
                                                 obs_sim_pars$lac2,
                                                 obs_sim_pars$mu2,
                                                 obs_sim_pars$gam2,
                                                 obs_sim_pars$laa2,
                                                 obs_sim_pars$trans,
                                                 obs_sim_pars$trans2)),
                                  K = as.numeric(obs_sim_pars$K),
                                  replicates = 1)
  prior_gen <- function(){
    lac <- stats::runif(1,0,1)
    mu <- stats::runif(1,0,1)
    gam <- stats::runif(1,0,0.05)
    laa <- stats::runif(1,0,1)
    lac2 <- stats::runif(1,0,1)
    mu2 <- stats::runif(1,0,1)
    gam2 <- stats::runif(1,0,0.05)
    laa2 <- stats::runif(1,0,1)
    trans <- stats::runif(1,0,1)
    trans2 <- stats::runif(1,0,1)
    param <- c(lac,mu,gam,laa,lac2,mu2,gam2,laa2,trans,trans2)
    return(param)
  }

  prior_dens <- function(x) {
    return(stats::dunif(x[1],0,1) * stats::dunif(x[2],0,1) *
             stats::dunif(x[3],0,0.05) * stats::dunif(x[4],0,1) *
             stats::dunif(x[5],0,1) * stats::dunif(x[6],0,1) *
             stats::dunif(x[7],0,0.05) * stats::dunif(x[8],0,1) *
             stats::dunif(x[9],0,1) * stats::dunif(x[10],0,1))
  }

  abc <- ABC_SMC_DAISIE (
    obs_data = obs_sim,
    sim_function = get_DAISIE_sim,
    init_epsilon_values = c(150,150,40,40,30,10),
    prior_generating_function = prior_gen,
    prior_density_function = prior_dens,
    number_of_particles = 2000,
    sigma = 0.05,
    stop_rate = 0.002,
    replicates = 1,  ## simulation replicates for each parameter set
    num_iterations = 10,
    K = as.numeric(obs_sim_pars$K)
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
}

