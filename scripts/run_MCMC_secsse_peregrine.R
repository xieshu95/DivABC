args <- commandArgs(TRUE)

scenario <- args[1]
param_set <- as.numeric(args[2])
idparsopt_lam1 <- args[3]
idparsopt_lam2 <- args[4]
idparsopt_mu1 <- args[5]
idparsopt_mu2 <- args[6]
idparsopt_q12 <- args[7]
idparsopt_q21 <- args[8]
idparsopt_logical <- c(idparsopt_lam1,idparsopt_lam2,
                       idparsopt_mu1,idparsopt_mu2,
                       idparsopt_q12,idparsopt_q21)
idparsopt <- which(idparsopt_logical == 1)
save_output <- TRUE
metadata <- paste0("This is parameter set ", param_set)

library(TraisieABC)

run_MCMC_secsse(
  scenario = args[1],
  param_set = as.numeric(args[2]),
  idparsopt = as.numeric(idparsopt),
  save_output = save_output
)