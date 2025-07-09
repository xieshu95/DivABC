args <- commandArgs(TRUE)

param_space_name <- args[1]
param_set <- as.numeric(args[2])
idparsopt_lam1 <- args[3]
idparsopt_lam2 <- args[4]
idparsopt_lam3 <- args[5]
idparsopt_mu1 <- args[6]
idparsopt_mu2 <- args[7]
idparsopt_mu3 <- args[8]
idparsopt_q <- args[9]
idparsopt_logical <- c(idparsopt_lam1,idparsopt_lam2,idparsopt_lam3,
                       idparsopt_mu1,idparsopt_mu2,idparsopt_mu3,
                       idparsopt_q)
idparsopt <- which(idparsopt_logical == 1)

save_output <- TRUE
metadata <- paste0("This is parameter set ", param_set)

library(DivABC)

run_MCMC_musse(
  param_space_name = args[1],
  param_set = as.numeric(args[2]),
  idparsopt = 1:7,
  save_output = save_output
)
