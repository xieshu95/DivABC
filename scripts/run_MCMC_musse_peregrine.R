args <- commandArgs(TRUE)

param_space_name <- args[1]
param_set <- as.numeric(args[2])
idparsopt_lam1 <- as.numeric(args[3])
idparsopt_lam2 <- as.numeric(args[4])
idparsopt_lam3 <- as.numeric(args[5])
idparsopt_mu1 <- as.numeric(args[6])
idparsopt_mu2 <- as.numeric(args[7])
idparsopt_mu3 <- as.numeric(args[8])
idparsopt_q12 <- as.numeric(args[9])
idparsopt_q13 <- as.numeric(args[10])
idparsopt_q21 <- as.numeric(args[11])
idparsopt_q23 <- as.numeric(args[12])
idparsopt_q31 <- as.numeric(args[13])
idparsopt_q32 <- as.numeric(args[14])
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