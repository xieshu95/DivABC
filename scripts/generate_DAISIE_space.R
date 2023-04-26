## create input parameter sets for ABC
# #1. ABC_DAISIE
#
# rep <- 1:5
# K <- c(40, Inf)
# lac <- c(0.2,0.4)
# mu <- c(0.1,0.2)
# gam <- c(0.005,0.01)
# laa <-c(0.1,0.2)
#
# DAISIE_ABC <- expand.grid(
#   rep = rep,
#   lac = lac,
#   mu = mu,
#   gam = gam,
#   laa = laa,
#   K = K
# )
#
# write.csv2(
#   DAISIE_ABC,
#   "data/DAISIE_ABC.csv",
#   row.names = FALSE
# )


## create input parameter sets for ABC
#2. ABC_DAISIE
rep <- 1:5
K <- Inf
## a. lac
lac <- runif(50,0.05,0.5)
mu <- 0.2
gam <- 0.01
laa <- 0.2
ABC_lac <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)

## b. mu
lac <- 0.3
mu <- runif(50,0.05,0.5)
gam <- 0.01
laa <- 0.2
ABC_mu <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)

## c. gam
lac <- 0.3
mu <- 0.2
gam <- runif(50,0.005,0.02)
laa <- 0.2
ABC_gam <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)
## d. laa
lac <- 0.3
mu <- 0.2
gam <- 0.01
laa <- runif(50,0.05,0.5)
ABC_laa <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)

DAISIE_ABC_single_change <- rbind(
  ABC_lac,
  ABC_mu,
  ABC_gam,
  ABC_laa
)

write.csv2(
  DAISIE_ABC_single_change,
  "data/DAISIE_ABC_single_change.csv",
  row.names = FALSE
)


## rondom sampled parameters from uniform distribution
set.seed(42)
lac <- runif(200,0.1,0.5)
mu <- runif(200,0.1,0.5)
gam <- runif(200,0.005,0.01)
laa <- runif(200,0.1,0.5)
K <- rep(Inf,200)
rep <- rep(1,200)
random_ABC <- data.frame(rep,lac,mu,gam,laa,K,stringsAsFactors = TRUE)

write.csv2(
  random_ABC,
  "data/DAISIE_ABC_random.csv",
  row.names = FALSE
)

random_MCMC = random_ABC
write.csv2(
  random_MCMC,
  "data/DAISIE_MCMC_random.csv",
  row.names = FALSE
)


## create input parameter sets for ABC
#4. ABC_DAISIE default: lac=0.5 mu=0.5 gam=0.02 laa=0.5
rep <- 1:10
K <- Inf
## a. lac
lac <- c(0.2,0.4,0.6,0.8)
mu <- 0.05
gam <- 0.015
laa <- 0.2
ABC_lac <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)

## b. mu
lac <- 0.5
mu <- c(0.05,0.1,0.15,0.2)
gam <- 0.015
laa <- 0.2
ABC_mu <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)

## c. gam
lac <- 0.5
mu <- 0.05
gam <- c(0.01,0.015,0.02,0.025)
laa <- 0.2
ABC_gam <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)
## d. laa
lac <- 0.5
mu <- 0.05
gam <- 0.015
laa <- c(0.2,0.4,0.6,0.8)
ABC_laa <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)

DAISIE_ABC <- rbind(
  ABC_lac,
  ABC_mu,
  ABC_gam,
  ABC_laa
)

write.csv2(
  DAISIE_ABC,
  "data/DAISIE_ABC.csv",
  row.names = FALSE
)



## create input parameter sets for mcmc
#5. mcmc_DAISIE default: lac=0.4 mu=0.2 gam=0.01 laa=0.4
DAISIE_MCMC = DAISIE_ABC

write.csv2(
  DAISIE_MCMC,
  "data/DAISIE_MCMC.csv",
  row.names = FALSE
)


## create input parameter sets for ABC
#4. ABC_DAISIE default: lac=0.5 mu=0.05 gam=0.015 laa=0.2
rep <- 1:10
K <- 40
## a. lac
lac <- c(0.2,0.4,0.6,0.8)
mu <- 0.05
gam <- 0.015
laa <- 0.2
ABC_lac_DD <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)

## b. mu
lac <- 0.5
mu <- c(0.05,0.1,0.15,0.2)
gam <- 0.015
laa <- 0.2
ABC_mu_DD <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)

## c. gam
lac <- 0.5
mu <- 0.05
gam <- c(0.01,0.015,0.02,0.025)
laa <- 0.2
ABC_gam_DD <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)
## d. laa
lac <- 0.5
mu <- 0.05
gam <- 0.015
laa <- c(0.2,0.4,0.6,0.8)
ABC_laa_DD <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)

DAISIE_ABC_DD <- rbind(
  ABC_lac_DD,
  ABC_mu_DD,
  ABC_gam_DD,
  ABC_laa_DD
)

write.csv2(
  DAISIE_ABC_DD,
  "data/DAISIE_ABC_DD.csv",
  row.names = FALSE
)



## create input parameter sets for mcmc
#5. mcmc_DAISIE default: lac=0.4 mu=0.2 gam=0.01 laa=0.4
DAISIE_MCMC_DD = DAISIE_ABC_DD

write.csv2(
  DAISIE_MCMC_DD,
  "data/DAISIE_MCMC_DD.csv",
  row.names = FALSE
)






### use less parameter sets to try
K <- c(50)

# lac <- c(0.3,1.0)
# mu <- c(0,0.3)
# gam <- c(0.003,0.012)
# laa <- c(0.1,1.0)
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

write.csv2(
  DAISIE_ABC_short,
  "data/DAISIE_ABC_short.csv",
  row.names = FALSE
)


DAISIE_MCMC_short = DAISIE_ABC_short

write.csv2(
  DAISIE_MCMC_short,
  "data/DAISIE_MCMC_short.csv",
  row.names = FALSE
)

## DI
K <- Inf
## a. lac
# lac <- c(0.3,0.5,0.7)
# mu <- c(0.05,0.1,0.15)
# gam <- c(0.005,0.0075,0.01)
# laa <- c(0.1,0.2,0.3)

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

write.csv2(
  DAISIE_ABC_short_DI,
  "data/DAISIE_ABC_short_DI.csv",
  row.names = FALSE
)


DAISIE_MCMC_short_DI = DAISIE_ABC_short_DI

write.csv2(
  DAISIE_MCMC_short_DI,
  "data/DAISIE_MCMC_short_DI.csv",
  row.names = FALSE
)


#####
## test
par1 <- c(0.3,0.1,0.009,0.2,20)
par2 <- c(0.3,0,0.009,0.2,20)  ##
par3 <- c(0.5,0.1,0.009,0.2,20)  ##
par4 <- c(0.3,0.1,0.009,0.2,Inf)  ##


DAISIE_ABC_test <- data.frame(rbind(par1,par2,par3,par4))
colnames(DAISIE_ABC_test) <- c("lac","mu","gam","laa","K")

DAISIE_ABC_test1 <- DAISIE_ABC_test[rep(1,each = 50), ]
rownames(DAISIE_ABC_test1) <- 1:nrow(DAISIE_ABC_test1)
write.csv2(
  DAISIE_ABC_test1,
  "data/DAISIE_ABC_test1.csv",
  row.names = FALSE
)
DAISIE_MCMC_test1 = DAISIE_ABC_test1
write.csv2(
  DAISIE_MCMC_test1,
  "data/DAISIE_MCMC_test1.csv",
  row.names = FALSE
)

DAISIE_ABC_test2 <- DAISIE_ABC_test[rep(2,each = 50), ]
rownames(DAISIE_ABC_test2) <- 1:nrow(DAISIE_ABC_test2)
write.csv2(
  DAISIE_ABC_test2,
  "data/DAISIE_ABC_test2.csv",
  row.names = FALSE
)
DAISIE_MCMC_test2 = DAISIE_ABC_test2
write.csv2(
  DAISIE_MCMC_test2,
  "data/DAISIE_MCMC_test2.csv",
  row.names = FALSE
)

DAISIE_ABC_test3 <- DAISIE_ABC_test[rep(3,each = 50), ]
rownames(DAISIE_ABC_test3) <- 1:nrow(DAISIE_ABC_test3)
write.csv2(
  DAISIE_ABC_test3,
  "data/DAISIE_ABC_test3.csv",
  row.names = FALSE
)
DAISIE_MCMC_test3 = DAISIE_ABC_test3
write.csv2(
  DAISIE_MCMC_test3,
  "data/DAISIE_MCMC_test3.csv",
  row.names = FALSE
)

DAISIE_ABC_test4 <- DAISIE_ABC_test[rep(4,each = 50), ]
rownames(DAISIE_ABC_test4) <- 1:nrow(DAISIE_ABC_test4)
write.csv2(
  DAISIE_ABC_test4,
  "data/DAISIE_ABC_test4.csv",
  row.names = FALSE
)
DAISIE_MCMC_test4 = DAISIE_ABC_test4
write.csv2(
  DAISIE_MCMC_test4,
  "data/DAISIE_MCMC_test4.csv",
  row.names = FALSE
)
