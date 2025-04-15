args <- commandArgs(TRUE)

param_space_name <- args[1]
param_set <- as.numeric(args[2])
idparsopt_lam1 <- args[3]
idparsopt_lam2 <- args[4]
idparsopt_lam3 <- args[5]
idparsopt_mu1 <- args[6]
idparsopt_mu2 <- args[7]
idparsopt_mu3 <- args[8]
idparsopt_q12 <- args[9]
idparsopt_q13 <- args[10]
idparsopt_q21 <- args[11]
idparsopt_q23 <- args[12]
idparsopt_q31 <- args[13]
idparsopt_q32 <- args[14]
idparsopt_logical <- c(idparsopt_lam1,idparsopt_lam2,idparsopt_lam3,
                       idparsopt_mu1,idparsopt_mu2,idparsopt_mu3,
                       idparsopt_q12,idparsopt_q13,
                       idparsopt_q21,idparsopt_q23,
                       idparsopt_q31,idparsopt_q32)
idparsopt <- which(idparsopt_logical == 1)
save_output <- TRUE
metadata <- paste0("This is parameter set ", param_set)

library(DivABC)

run_MCMC_musse(
  param_space_name = args[1],
  param_set = as.numeric(args[2]),
  idparsopt = as.numeric(idparsopt),
  save_output = save_output
)
