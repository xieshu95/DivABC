args <- commandArgs(TRUE)

param_space_name <- args[1]
param_set <- as.numeric(args[2])
idparsopt <- args[3]
save_output <- TRUE
metadata <- paste0("This is parameter set ", param_set)

library(TraisieABC)

run_MCMC_DAISIE(
  param_space_name = args[1],
  param_set = as.numeric(args[2]),
  idparsopt <- args[3],
  save_output = save_output
)