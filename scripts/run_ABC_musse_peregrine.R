args <- commandArgs(TRUE)

param_space_name <- args[1]
param_set <- as.numeric(args[2])
idparsopt_lac1 <- as.numeric(args[3])
idparsopt_lac2 <- as.numeric(args[4])
idparsopt_lac3 <- as.numeric(args[5])
idparsopt_mu1 <- as.numeric(args[6])
idparsopt_mu2 <- as.numeric(args[7])
idparsopt_mu3 <- as.numeric(args[8])
idparsopt_trans <- as.numeric(args[9])
sim_model <- args[10]
ss_set <- as.numeric(args[11])
idparsopt_all <- c(idparsopt_lac1,idparsopt_lac2,idparsopt_lac3,
                   idparsopt_mu1,idparsopt_mu2,idparsopt_mu3,
                   idparsopt_trans)
idparsopt <- which(idparsopt_all == 1)
save_output <- TRUE
metadata <- paste0("This is parameter set ", param_set)

library(DivABC)

run_ABC(
  param_space_name = args[1],
  param_set = as.numeric(args[2]),
  idparsopt = as.numeric(idparsopt),
  sim_model = sim_model,
  save_output = save_output,
  ss_set = ss_set
)