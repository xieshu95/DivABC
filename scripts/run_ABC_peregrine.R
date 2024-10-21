args <- commandArgs(TRUE)

param_space_name <- args[1]
param_set <- as.numeric(args[2])
idparsopt_lac <- as.numeric(args[3])
idparsopt_mu <- as.numeric(args[4])
idparsopt_gam <- as.numeric(args[5])
idparsopt_laa <- as.numeric(args[6])
idparsopt_lac2 <- as.numeric(args[7])
idparsopt_mu2 <- as.numeric(args[8])
idparsopt_gam2 <- as.numeric(args[9])
idparsopt_laa2 <- as.numeric(args[10])
idparsopt_trans <- as.numeric(args[11])
idparsopt_trans2 <- as.numeric(args[12])
sim_model <- args[13]
ss_set <- as.numeric(args[14])
idparsopt_all <- c(idparsopt_lac,idparsopt_mu,idparsopt_gam,idparsopt_laa,
                   idparsopt_lac2,idparsopt_mu2,idparsopt_gam2,idparsopt_laa2,
                   idparsopt_trans,idparsopt_trans2)
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
