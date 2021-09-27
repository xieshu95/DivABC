args <- commandArgs(TRUE)

param_space_name <- args[1]
idparsopt_lac <- args[2]
idparsopt_mu <- args[3]
idparsopt_gam <- args[4]
idparsopt_laa <- args[5]
idparsopt_logical <- c(idparsopt_lac,idparsopt_mu,idparsopt_gam,idparsopt_laa)
idparsopt <- which(idparsopt_logical == TRUE)
save_output <- TRUE

library(TraisieABC)
library(microbenchmark)

bm <- microbenchmark(
  run_mcmc_DAISIE_parallel(
    param_space_name,
    idparsopt,
    strategy = future::sequential,
    workers = 8
  ),
  run_mcmc_DAISIE_parallel(
    param_space_name,
    idparsopt,
    strategy = future::multisession,
    workers = 8
  ),
  times = 5
)

write.csv2(summary(bm), "~/traisieABC/testdata/bm.csv")