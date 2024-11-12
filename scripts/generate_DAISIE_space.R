
### use less parameter sets to try
K <- c(20,50)

lac <- c(0.4,0.7)
mu <- c(0,0.3)
gam <- c(0.003,0.009)
laa <- c(0.1,1.0)
DAISIE_ABC_DD <- expand.grid(
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)

DAISIE_ABC_DD <- DAISIE_ABC_DD[rep(seq_len(nrow(DAISIE_ABC_DD)), each = 10), ]

save(DAISIE_ABC_DD, file = "inst/extdata/DAISIE_ABC_DD.rda")


DAISIE_MCMC_DD = DAISIE_ABC_DD
save(DAISIE_MCMC_DD, file = "inst/extdata/DAISIE_MCMC_DD.rda")


## DI
K <- Inf

lac <- c(0.4,0.7)
mu <- c(0,0.3)
gam <- c(0.003,0.009)
laa <- c(0.1,1.0)
DAISIE_ABC_DI <- expand.grid(
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)
DAISIE_ABC_DI <- DAISIE_ABC_DI[rep(seq_len(nrow(DAISIE_ABC_DI)), each = 10), ]

save(DAISIE_ABC_DI, file = "inst/extdata/DAISIE_ABC_DI.rda")


DAISIE_MCMC_DI = DAISIE_ABC_DI
save(DAISIE_MCMC_DI, file = "inst/extdata/DAISIE_MCMC_DI.rda")

## sampled parameter space
K <- Inf
lac <- stats::runif(300,0,1)
mu <- stats::runif(300,0,0.5)
gam <- stats::runif(300,0,0.01)
laa <- stats::runif(300,0,1)

DAISIE_ABC_DI <- data.frame(lac,mu,gam,laa,K)

save(DAISIE_ABC_DI, file = "inst/extdata/DAISIE_ABC_conti_DI.rda")


DAISIE_MCMC_DI = DAISIE_ABC_DI
save(DAISIE_MCMC_DI, file = "inst/extdata/DAISIE_MCMC_conti_DI.rda")

