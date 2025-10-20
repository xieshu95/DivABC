#' prior density function for bisse model
#'
#' @return Density of the given parameter set
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

# prior_dens_bisse <- function(pars,idparsopt) {
#   if(1 %in% idparsopt){
#     dens_lam1 <- stats::dexp(pars[1],2)
#   } else {
#     dens_lam1 <- 1
#   }
#   if(2 %in% idparsopt){
#     dens_lam2 <- stats::dexp(pars[2],2)
#   } else {
#     dens_lam2 <- 1
#   }
#   if(3 %in% idparsopt){
#     dens_mu1 <- stats::dexp(pars[3],2)
#   } else {
#     dens_mu1 <- 1
#   }
#   if(4 %in% idparsopt){
#     dens_mu2 <- stats::dexp(pars[4],2)
#   } else {
#     dens_mu2 <- 1
#   }
#   if(5 %in% idparsopt){
#     dens_q12 <- stats::dexp(pars[5],2)
#   } else {
#     dens_q12 <- 1
#   }
#   if(6 %in% idparsopt){
#     dens_q21 <- stats::dexp(pars[6],2)
#   } else {
#     dens_q21 <- 1
#   }
#   return(dens_lam1 * dens_lam2 * dens_mu1 * dens_mu2 * dens_q12 * dens_q21)
# }

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

# prior_gen_bisse <- function(pars,idparsopt){
#   if(1 %in% idparsopt){
#     lam1 <- stats::rexp(1,2)
#   } else {
#     lam1 <- pars[1]
#   }
#   if(2 %in% idparsopt){
#     lam2 <- stats::rexp(1,2)
#   } else {
#     lam2 <- pars[2]
#   }
#   if(3 %in% idparsopt){
#     mu1 <- stats::rexp(1,2)
#   } else {
#     mu1 <- pars[3]
#   }
#   if(4 %in% idparsopt){
#     mu2 <- stats::rexp(1,2)
#   } else {
#     mu2 <- pars[4]
#   }
#   if(5 %in% idparsopt){
#     q12 <- stats::rexp(1,2)
#   } else {
#     q12 <- pars[5]
#   }
#   if(6 %in% idparsopt){
#     q21 <- stats::rexp(1,2)
#   } else {
#     q21 <- pars[6]
#   }
#   return(as.numeric(c(lam1,lam2,mu1,mu2,q12,q21)))
# }

#' prior density function for musse model
#'
#' @return Density of the given parameter set
prior_dens_musse <- function(pars,idparsopt) {
  if(1 %in% idparsopt){
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
    dens_mu3 <- stats::dunif(pars[6],0,1)
  } else {
    dens_mu3 <- 1
  }
  if(7 %in% idparsopt){
    dens_q <- stats::dunif(pars[7],0,1)
  } else {
    dens_q <- 1
  }
  return(dens_lam1 * dens_lam2 * dens_lam3 *
           dens_mu1 * dens_mu2 * dens_mu3 *
           dens_q)
}

#'prior function to generate parameters for musse
prior_gen_musse <- function(pars,idparsopt){
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
    mu3 <- stats::runif(1,0,1)
  } else {
    mu3 <- pars[6]
  }
  if(7 %in% idparsopt){
    q <- stats::runif(1,0,1)
  } else {
    q <- pars[7]
  }
  return(as.numeric(c(lam1,lam2,lam3,mu1,mu2,mu3,q)))
}

# geosse uniform prior distribution
#' prior density function for geosse model
#'
#' @return Density of the given parameter set

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
