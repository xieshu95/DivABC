args <- commandArgs(TRUE)

param_space_name <- args[1]
param_set <- as.numeric(args[2])
idparsopt_lam1 <- as.numeric(args[3])
idparsopt_lam2 <- as.numeric(args[4])
idparsopt_lam3 <- as.numeric(args[5])
idparsopt_mu1 <- as.numeric(args[6])
idparsopt_mu2 <- as.numeric(args[7])
idparsopt_trans12 <- as.numeric(args[8])
idparsopt_trans21 <- as.numeric(args[9])
sim_model <- args[10]
ss_set <- as.numeric(args[11])
idparsopt_all <- c(idparsopt_lam1,idparsopt_lam2,idparsopt_lam3,idparsopt_mu1,idparsopt_mu2,
                   idparsopt_trans12,idparsopt_trans21)
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