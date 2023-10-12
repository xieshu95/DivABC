## create input parameter sets for ABC


K <- c(50)
lac <- c(0.4,0.7)
mu <- c(0,0.3)
gam <- c(0.003,0.009)
laa <- c(0.1,1.0)
DAISIE_ABC_short <- expand.grid(
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)

DAISIE_ABC_short <- DAISIE_ABC_short[rep(seq_len(nrow(DAISIE_ABC_short)), each = 10), ]

save(DAISIE_ABC_short, file = "inst/extdata/DAISIE_ABC_short.rda")


DAISIE_MCMC_short = DAISIE_ABC_short
save(DAISIE_MCMC_short, file = "inst/extdata/DAISIE_MCMC_short.rda")


## DI
K <- Inf
lac <- c(0.4,0.7)
mu <- c(0,0.3)
gam <- c(0.003,0.009)
laa <- c(0.1,1.0)
DAISIE_ABC_short_DI <- expand.grid(
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)
DAISIE_ABC_short_DI <- DAISIE_ABC_short_DI[rep(seq_len(nrow(DAISIE_ABC_short_DI)), each = 10), ]

save(DAISIE_ABC_short_DI, file = "inst/extdata/DAISIE_ABC_short_DI.rda")
# write.csv2(
#   DAISIE_ABC_short_DI,
#   "data/DAISIE_ABC_short_DI.csv",
#   row.names = FALSE
# )


DAISIE_MCMC_short_DI = DAISIE_ABC_short_DI
save(DAISIE_MCMC_short_DI, file = "inst/extdata/DAISIE_MCMC_short_DI.rda")

# write.csv2(
#   DAISIE_MCMC_short_DI,
#   "data/DAISIE_MCMC_short_DI.csv",
#   row.names = FALSE
# )
