#' prior density function for TraiSIE
#'
#' @return a numeric represents the log likelihood
#' @export
prior_dens_trait <- function(pars,idparsopt) {
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
  if(5 %in% idparsopt){
    dens_lac2 <- stats::dunif(pars[5],0,1)
  } else {
    dens_lac2 <- 1
  }
  if(6 %in% idparsopt){
    dens_mu2 <- stats::dunif(pars[6],0,0.5)
  } else {
    dens_mu2 <- 1
  }
  if(7 %in% idparsopt){
    dens_gam2 <- stats::dunif(pars[7],0,0.02)
  } else {
    dens_gam2 <- 1
  }
  if(8 %in% idparsopt){
    dens_laa2 <- stats::dunif(pars[8],0,0.5)
  } else {
    dens_laa2 <- 1
  }
  if(9 %in% idparsopt){
    dens_trans <- stats::dunif(pars[9],0,0.5)
  } else {
    dens_trans <- 1
  }
  if(10 %in% idparsopt){
    dens_trans2 <- stats::dunif(pars[10],0,0.5)
  } else {
    dens_trans2 <- 1
  }

  return(dens_lac * dens_mu * dens_gam * dens_laa *
           dens_lac2 * dens_mu2 * dens_gam2 * dens_laa2 *
           dens_trans * dens_trans2 )
}


#'prior function to generate TraiSIE parameters
#'
#' @return a numeric represents the log likelihood
#' @export
prior_gen_trait <- function(pars,idparsopt){
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
  if(5 %in% idparsopt){
    lac2 <- stats::runif(1,0,1)
  } else {
    lac2 <- pars[5]
  }
  if(6 %in% idparsopt){
    mu2 <- stats::runif(1,0,0.5)
  } else {
    mu2 <- pars[6]
  }
  if(7 %in% idparsopt){
    gam2 <- stats::runif(1,0,0.02)
  } else {
    gam2 <- pars[7]
  }
  if(8 %in% idparsopt){
    laa2 <- stats::runif(1,0,0.5)
  } else {
    laa2 <- pars[8]
  }
  if(9 %in% idparsopt){
    trans <- stats::runif(1,0,0.5)
  } else {
    trans <- pars[9]
  }
  if(10 %in% idparsopt){
    trans2 <- stats::runif(1,0,0.5)
  } else {
    trans2 <- pars[10]
  }

  return(as.numeric(c(lac,mu,gam,laa,lac2,mu2,gam2,laa2,trans,trans2)))
}
