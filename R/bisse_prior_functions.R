#' prior density function for bisse model
#'
#' @return Density of the given parameter set
#' @export
prior_dens_bisse <- function(pars,idparsopt) {
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
    dens_q12 <- stats::dunif(pars[5],0,1)
  } else {
    dens_q12 <- 1
  }
  if(6 %in% idparsopt){
    dens_q21 <- stats::dunif(pars[6],0,1)
  } else {
    dens_q21 <- 1
  }
  return(dens_lam1 * dens_lam2 * dens_mu1 * dens_mu2 * dens_q12 * dens_q21)
}


#'prior function to generate parameters for bisse
#'
#' @return a vector of parameters
#' @export
prior_gen_bisse <- function(pars,idparsopt){
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
    q12 <- stats::runif(1,0,1)
  } else {
    q12 <- pars[5]
  }
  if(6 %in% idparsopt){
    q21 <- stats::runif(1,0,1)
  } else {
    q21 <- pars[6]
  }
  return(as.numeric(c(lam1,lam2,mu1,mu2,q12,q21)))
}


#' prior density function for musse model
#'
#' @return Density of the given parameter set
#' @export
prior_dens_musse <- function(pars,idparsopt) {
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
    dens_lam3 <- stats::dunif(pars[3],0,2)
  } else {
    dens_lam3 <- 1
  }
  if(4 %in% idparsopt){
    dens_mu1 <- stats::dunif(pars[4],0,2)
  } else {
    dens_mu1 <- 1
  }
  if(5 %in% idparsopt){
    dens_mu2 <- stats::dunif(pars[5],0,2)
  } else {
    dens_mu2 <- 1
  }
  if(6 %in% idparsopt){
    dens_mu3 <- stats::dunif(pars[6],0,2)
  } else {
    dens_mu3 <- 1
  }
  if(7 %in% idparsopt){
    dens_q12 <- stats::dunif(pars[7],0,1)
  } else {
    dens_q12 <- 1
  }
  if(8 %in% idparsopt){
    dens_q13 <- stats::dunif(pars[8],0,1)
  } else {
    dens_q13 <- 1
  }
  if(9 %in% idparsopt){
    dens_q21 <- stats::dunif(pars[9],0,1)
  } else {
    dens_q21 <- 1
  }
  if(10 %in% idparsopt){
    dens_q23 <- stats::dunif(pars[10],0,1)
  } else {
    dens_q23 <- 1
  }
  if(11 %in% idparsopt){
    dens_q31 <- stats::dunif(pars[11],0,1)
  } else {
    dens_q31 <- 1
  }
  if(12 %in% idparsopt){
    dens_q32 <- stats::dunif(pars[12],0,1)
  } else {
    dens_q32 <- 1
  }
  return(dens_lam1 * dens_lam2 * dens_lam3 *
           dens_mu1 * dens_mu2 * dens_mu3 *
           dens_q12 * dens_q13*
           dens_q21 * dens_q23*
           dens_q31 * dens_q32)
}


#'prior function to generate parameters for musse
#'
#' @return a vector of parameters
#' @export
prior_gen_musse <- function(pars,idparsopt){
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
    lam3 <- stats::runif(1,0,2)
  } else {
    lam3 <- pars[3]
  }
  if(4 %in% idparsopt){
    mu1 <- stats::runif(1,0,2)
  } else {
    mu1 <- pars[4]
  }
  if(5 %in% idparsopt){
    mu2 <- stats::runif(1,0,2)
  } else {
    mu2 <- pars[5]
  }
  if(6 %in% idparsopt){
    mu3 <- stats::runif(1,0,2)
  } else {
    mu3 <- pars[6]
  }
  if(7 %in% idparsopt){
    q12 <- stats::runif(1,0,1)
  } else {
    q12 <- pars[7]
  }
  if(8 %in% idparsopt){
    q13 <- stats::runif(1,0,1)
  } else {
    q13 <- pars[8]
  }
  if(9 %in% idparsopt){
    q21 <- stats::runif(1,0,1)
  } else {
    q21 <- pars[9]
  }
  if(10 %in% idparsopt){
    q23 <- stats::runif(1,0,1)
  } else {
    q23 <- pars[10]
  }
  if(11 %in% idparsopt){
    q31 <- stats::runif(1,0,1)
  } else {
    q31 <- pars[11]
  }
  if(12 %in% idparsopt){
    q32 <- stats::runif(1,0,1)
  } else {
    q32 <- pars[12]
  }
  return(as.numeric(c(lam1,lam2,lam3,mu1,mu2,mu3,
                      q12,q13,q21,q23,q31,q32)))
}

#' prior density function for geosse model
#'
#' @return Density of the given parameter set
#' @export
prior_dens_geosse <- function(pars,idparsopt) {
  if(1 %in% idparsopt){
    # dens_lam1 <- stats::dexp(pars[1],2.5)
    dens_lam1 <- stats::dunif(pars[1],0,1)
  } else {
    dens_lam1 <- 1
  }
  if(2 %in% idparsopt){
    dens_lam2 <- stats::dunif(pars[2],0,1)
  } else {
    dens_lam2 <- 1
  }
  if(3 %in% idparsopt){
    dens_lam3 <- stats::dunif(pars[3],0,1)
  } else {
    dens_lam3 <- 1
  }
  if(4 %in% idparsopt){
    dens_mu1 <- stats::dunif(pars[4],0,1)
  } else {
    dens_mu1 <- 1
  }
  if(5 %in% idparsopt){
    dens_mu2 <- stats::dunif(pars[5],0,1)
  } else {
    dens_mu2 <- 1
  }
  if(6 %in% idparsopt){
    dens_q1 <- stats::dunif(pars[6],0,1)
  } else {
    dens_q1 <- 1
  }
  if(7 %in% idparsopt){
    dens_q2 <- stats::dunif(pars[7],0,1)
  } else {
    dens_q2 <- 1
  }
  return(dens_lam1 * dens_lam2 * dens_lam3 *
           dens_mu1 * dens_mu2 *
           dens_q1 * dens_q2)
}


#'prior function to generate parameters for geosse
#'
#' @return a vector of parameters
#' @export
prior_gen_geosse <- function(pars,idparsopt){
  if(1 %in% idparsopt){
    lam1 <- stats::runif(1,0,1)
  } else {
    lam1 <- pars[1]
  }
  if(2 %in% idparsopt){
    lam2 <- stats::runif(1,0,1)
  } else {
    lam2 <- pars[2]
  }
  if(3 %in% idparsopt){
    lam3 <- stats::runif(1,0,1)
  } else {
    lam3 <- pars[3]
  }
  if(4 %in% idparsopt){
    mu1 <- stats::runif(1,0,1)
  } else {
    mu1 <- pars[4]
  }
  if(5 %in% idparsopt){
    mu2 <- stats::runif(1,0,1)
  } else {
    mu2 <- pars[5]
  }
  if(6 %in% idparsopt){
    q1 <- stats::runif(1,0,1)
  } else {
    q1 <- pars[6]
  }
  if(7 %in% idparsopt){
    q2 <- stats::runif(1,0,1)
  } else {
    q2 <- pars[7]
  }

  return(as.numeric(c(lam1,lam2,lam3,mu1,mu2,q1,q2)))
}


