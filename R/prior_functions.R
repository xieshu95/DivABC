#' prior density function
#'
#' @return Density of the given parameter set
#' @export
prior_dens <- function(pars,idparsopt) {
  if(1 %in% idparsopt){
    dens_lac <- stats::dexp(pars[1],1.5)
  } else {
    dens_lac <- 1
  }
  if(2 %in% idparsopt){
    dens_mu <- stats::dexp(pars[2],5)
  } else {
    dens_mu <- 1
  }
  if(3 %in% idparsopt){
    dens_gam <- stats::dexp(pars[3],25)
  } else {
    dens_gam <- 1
  }
  if(4 %in% idparsopt){
    dens_laa <- stats::dexp(pars[4],3)
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
    lac <- 10
    while (lac > 2) {
      lac <- stats::rexp(1,1.5)
    }
  } else {
    lac <- pars[1]
  }
  if(2 %in% idparsopt){
    mu <- stats::rexp(1,5)
  } else {
    mu <- pars[2]
  }
  if(3 %in% idparsopt){
    gam <- 1
    while (gam > 0.04 || gam < 0.001) {
      gam <- stats::rexp(1,25)
    }
  } else {
    gam <- pars[3]
  }
  if(4 %in% idparsopt){
    laa <- stats::rexp(1,3)
  } else {
    laa <- pars[4]
  }
  return(as.numeric(c(lac,mu,gam,laa)))
}

# plot(density(rexp(1000,3)))
# median(stats::rexp(10000,5))
