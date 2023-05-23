#' prior density function
#'
#' @return Density of the given parameter set
#' @export
prior_dens_secsse <- function(pars,idparsopt) {
  if(1 %in% idparsopt){
    # dens_lam1 <- stats::dexp(pars[1],2.5)
    dens_lam1 <- stats::dunif(pars[1],0,2)
  } else {
    dens_lam1 <- 1
  }
  if(2 %in% idparsopt){
    dens_lam2 <- stats::dunif(pars[2],0,2)
  } else {
    dens_lam2 <- 1
  }
  if(3 %in% idparsopt){
    dens_mu1 <- stats::dunif(pars[3],0,2)
  } else {
    dens_mu1 <- 1
  }
  if(4 %in% idparsopt){
    dens_mu2 <- stats::dunif(pars[4],0,2)
  } else {
    dens_mu2 <- 1
  }
  if(5 %in% idparsopt){
    dens_q12 <- stats::dunif(pars[5],0,2)
  } else {
    dens_q12 <- 1
  }
  if(6 %in% idparsopt){
    dens_q21 <- stats::dunif(pars[6],0,2)
  } else {
    dens_q21 <- 1
  }
  return(dens_lam1 * dens_lam2 * dens_mu1 * dens_mu2 * dens_q12 * dens_q21)
}


#'prior function to generate parameters
#'
#' @return a vector of parameters
#' @export
prior_gen_secsse <- function(pars,idparsopt){
  if(1 %in% idparsopt){
    lam1 <- stats::runif(1,0,2)
  } else {
    lam1 <- pars[1]
  }
  if(2 %in% idparsopt){
    lam2 <- stats::runif(1,0,2)
  } else {
    lam2 <- pars[2]
  }
  if(3 %in% idparsopt){
    mu1 <- stats::runif(1,0,2)
  } else {
    mu1 <- pars[3]
  }
  if(4 %in% idparsopt){
    mu2 <- stats::runif(1,0,2)
  } else {
    mu2 <- pars[4]
  }
  if(5 %in% idparsopt){
    q12 <- stats::runif(1,0,2)
  } else {
    q12 <- pars[5]
  }
  if(6 %in% idparsopt){
    q21 <- stats::runif(1,0,2)
  } else {
    q21 <- pars[6]
  }
  return(as.numeric(c(lam1,lam2,mu1,mu2,q12,q21)))
}

# plot(density(rexp(1000,3)))
