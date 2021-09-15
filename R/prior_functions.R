#' prior density function
#'
#' @return a numeric represents the log likelihood
#' @export
prior_dens <- function(x,idparsopt) {
  if(1 %in% idparsopt){
    dens_lac <- stats::dunif(x[1],0,1)
  } else {
    dens_lac <- 1
  }
  if(2 %in% idparsopt){
    dens_mu <- stats::dunif(x[2],0,1)
  } else {
    dens_mu <- 1
  }
  if(3 %in% idparsopt){
    dens_gam <- stats::dunif(x[3],0,0.05)
  } else {
    dens_gam <- 1
  }
  if(4 %in% idparsopt){
    dens_laa <- stats::dunif(x[4],0,1)
  } else {
    dens_laa <- 1
  }
  return(dens_lac * dens_mu * dens_gam * dens_laa)
}


#'prior function to generate parameters
#'
#' @return a numeric represents the log likelihood
#' @export
prior_gen <- function(idparsopt){
  if(1 %in% idparsopt){
    lac <- stats::runif(1,0,1)
  } else {
    lac <- 0.4
  }
  if(2 %in% idparsopt){
    mu <- stats::runif(1,0,1)
  } else {
    mu <- 0.2
  }
  if(3 %in% idparsopt){
    gam <- stats::runif(1,0,0.05)
  } else {
    gam <- 0.01
  }
  if(4 %in% idparsopt){
    laa <- stats::runif(1,0,1)
  } else {
    laa <- 0.4
  }
  return(as.numeric(c(lac,mu,gam,laa)))
}
