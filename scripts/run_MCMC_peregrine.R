args <- commandArgs(TRUE)

param_space_name <- args[1]
param_set <- as.numeric(args[2])
idparsopt_lac <- args[3]
idparsopt_mu <- args[4]
idparsopt_gam <- args[5]
idparsopt_laa <- args[6]
idparsopt_K <- args[7]
idparsopt_logical <- c(idparsopt_lac,idparsopt_mu,idparsopt_gam,idparsopt_laa,idparsopt_K)
idparsopt <- which(idparsopt_logical == 1)
model <- args[8]
save_output <- TRUE
metadata <- paste0("This is parameter set ", param_set)

library(DivABC)
library(DAISIE)

run_MCMC_DAISIE(
  param_space_name = args[1],
  param_set = as.numeric(args[2]),
  idparsopt = as.numeric(idparsopt),
  model = args[8],
  save_output = save_output
)
