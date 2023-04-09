#' prior density function
#'
#' @return Density of the given parameter set
#' @export
prior_dens <- function(pars,idparsopt) {
  if(1 %in% idparsopt){
    dens_lac <- stats::dunif(pars[1],0,1)
  } else {
    dens_lac <- 1
  }
  if(2 %in% idparsopt){
    dens_mu <- stats::dunif(pars[2],0,0.5)
  } else {
    dens_mu <- 1
  }
  if(3 %in% idparsopt){
    dens_gam <- stats::dunif(pars[3],0,0.02)
  } else {
    dens_gam <- 1
  }
  if(4 %in% idparsopt){
    dens_laa <- stats::dunif(pars[4],0,0.5)
  } else {
    dens_laa <- 1
  }
  return(dens_lac * dens_mu * dens_gam * dens_laa)
}


#'prior function to generate parameters
#'
#' @return a vector of parameters
#' @export
prior_gen <- function(pars,idparsopt){
  if(1 %in% idparsopt){
    lac <- stats::runif(1,0,1)
  } else {
    lac <- pars[1]
  }
  if(2 %in% idparsopt){
    mu <- stats::runif(1,0,0.5)
  } else {
    mu <- pars[2]
  }
  if(3 %in% idparsopt){
    gam <- stats::runif(1,0,0.02)
  } else {
    gam <- pars[3]
  }
  if(4 %in% idparsopt){
    laa <- stats::runif(1,0,0.5)
  } else {
    laa <- pars[4]
  }
  return(as.numeric(c(lac,mu,gam,laa)))
}

# plot(density(rexp(1000,3)))
# median(stats::rexp(10000,5))
