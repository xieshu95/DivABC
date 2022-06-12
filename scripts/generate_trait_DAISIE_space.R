## create input parameter sets for TRAISIE ABC
################################
##
#2. ABC_trait default: lac=0.5 mu=0.5 gam=0.02 laa=0.5
rep <- 1:5
K <- 20
## reference rates: 0.5,0.1,0.02,0.2
## a. lac mean = 0.5
lac <- c(0.2,0.3,0.4,0.5)
mu <- 0.1
gam <- 0.02
laa <- 0.2
ABC_trait_lac <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)
ABC_trait_lac1 <- cbind(
  ABC_trait_lac,
  lac2 = 1.0 - ABC_trait_lac[,2],
  mu2 = 0.2 - ABC_trait_lac[,3],
  gam2 = 0.04 - ABC_trait_lac[,4],
  laa2 = 0.4 - ABC_trait_lac[,5],
  trans = 0.02,
  trans2 = 0.02
)

ABC_trait_lac2 <- cbind(
  ABC_trait_lac,
  lac2 = 1.0 - ABC_trait_lac[,2],
  mu2 = 0.2 - ABC_trait_lac[,3],
  gam2 = 0.04 - ABC_trait_lac[,4],
  laa2 = 0.4 - ABC_trait_lac[,5],
  trans = 0.02,
  trans2 = 0.2
)

ABC_trait_lac3 <- cbind(
  ABC_trait_lac,
  lac2 = 1.0 - ABC_trait_lac[,2],
  mu2 = 0.2 - ABC_trait_lac[,3],
  gam2 = 0.04 - ABC_trait_lac[,4],
  laa2 = 0.4 - ABC_trait_lac[,5],
  trans = 0.2,
  trans2 = 0.02
)
## b. mu
lac <- 0.5
mu <- c(0,0.025,0.05,0.1)
gam <- 0.02
laa <- 0.2
ABC_trait_mu <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)
ABC_trait_mu1 <- cbind(
  ABC_trait_mu,
  lac2 = 1.0 - ABC_trait_mu[,2],
  mu2 = 0.2 - ABC_trait_mu[,3],
  gam2 = 0.04 - ABC_trait_mu[,4],
  laa2 = 0.4 - ABC_trait_mu[,5],
  trans = 0.02,
  trans2 = 0.02
)

ABC_trait_mu2 <- cbind(
  ABC_trait_mu,
  lac2 = 1.0 - ABC_trait_mu[,2],
  mu2 = 0.2 - ABC_trait_mu[,3],
  gam2 = 0.04 - ABC_trait_mu[,4],
  laa2 = 0.4 - ABC_trait_mu[,5],
  trans = 0.02,
  trans2 = 0.2
)

ABC_trait_mu3 <- cbind(
  ABC_trait_mu,
  lac2 = 1.0 - ABC_trait_mu[,2],
  mu2 = 0.2 - ABC_trait_mu[,3],
  gam2 = 0.04 - ABC_trait_mu[,4],
  laa2 = 0.4 - ABC_trait_mu[,5],
  trans = 0.2,
  trans2 = 0.02
)
## c. gam
lac <- 0.5
mu <- 0.1
gam <- c(0.005,0.01,0.015,0.02)
laa <- 0.2
ABC_trait_gam <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)
ABC_trait_gam1 <- cbind(
  ABC_trait_gam,
  lac2 = 1.0 - ABC_trait_gam[,2],
  mu2 = 0.2 - ABC_trait_gam[,3],
  gam2 = 0.04 - ABC_trait_gam[,4],
  laa2 = 0.4 - ABC_trait_gam[,5],
  trans = 0.02,
  trans2 = 0.02
)

ABC_trait_gam2 <- cbind(
  ABC_trait_gam,
  lac2 = 1.0 - ABC_trait_gam[,2],
  mu2 = 0.2 - ABC_trait_gam[,3],
  gam2 = 0.04 - ABC_trait_gam[,4],
  laa2 = 0.4 - ABC_trait_gam[,5],
  trans = 0.02,
  trans2 = 0.2
)

ABC_trait_gam3 <- cbind(
  ABC_trait_gam,
  lac2 = 1.0 - ABC_trait_gam[,2],
  mu2 = 0.2 - ABC_trait_gam[,3],
  gam2 = 0.04 - ABC_trait_gam[,4],
  laa2 = 0.4 - ABC_trait_gam[,5],
  trans = 0.2,
  trans2 = 0.02
)
## d. laa
lac <- 0.5
mu <- 0.1
gam <- 0.02
laa <- c(0.05,0.1,0.15,0.2)
ABC_trait_laa <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)
ABC_trait_laa1 <- cbind(
  ABC_trait_laa,
  lac2 = 1.0 - ABC_trait_laa[,2],
  mu2 = 0.2 - ABC_trait_laa[,3],
  gam2 = 0.04 - ABC_trait_laa[,4],
  laa2 = 0.4 - ABC_trait_laa[,5],
  trans = 0.02,
  trans2 = 0.02
)

ABC_trait_laa2 <- cbind(
  ABC_trait_laa,
  lac2 = 1.0 - ABC_trait_laa[,2],
  mu2 = 0.2 - ABC_trait_laa[,3],
  gam2 = 0.04 - ABC_trait_laa[,4],
  laa2 = 0.4 - ABC_trait_laa[,5],
  trans = 0.02,
  trans2 = 0.2
)

ABC_trait_laa3 <- cbind(
  ABC_trait_laa,
  lac2 = 1.0 - ABC_trait_laa[,2],
  mu2 = 0.2 - ABC_trait_laa[,3],
  gam2 = 0.04 - ABC_trait_laa[,4],
  laa2 = 0.4 - ABC_trait_laa[,5],
  trans = 0.2,
  trans2 = 0.02
)

TraiSIE_ABC_DD <- rbind(
  ABC_trait_lac1,
  ABC_trait_lac2,
  ABC_trait_lac3,
  ABC_trait_mu1,
  ABC_trait_mu2,
  ABC_trait_mu3,
  ABC_trait_gam1,
  ABC_trait_gam2,
  ABC_trait_gam3,
  ABC_trait_laa1,
  ABC_trait_laa2,
  ABC_trait_laa3
)

write.csv2(
  TraiSIE_ABC_DD,
  "data/TraiSIE_ABC_DD.csv",
  row.names = FALSE
)


# ########### Diversity dependent
# rep <- 1:5
# K <- 20
# ## a. lac mean = 0.5
# lac <- c(0.1,0.2,0.3,0.4,0.5)
# mu <- 0.5
# gam <- 0.02
# laa <- 0.5
# ABC_trait_lac <- expand.grid(
#   rep = rep,
#   lac = lac,
#   mu = mu,
#   gam = gam,
#   laa = laa,
#   K = K
# )
# ABC_trait_lac1 <- cbind(
#   ABC_trait_lac,
#   lac2 = 1.0 - ABC_trait_lac[,2],
#   mu2 = 1.0 - ABC_trait_lac[,3],
#   gam2 = 0.04 - ABC_trait_lac[,4],
#   laa2 = 1.0 - ABC_trait_lac[,5],
#   trans = 0.001,
#   trans2 = 0.001
# )
#
# ABC_trait_lac2 <- cbind(
#   ABC_trait_lac,
#   lac2 = 1.0 - ABC_trait_lac[,2],
#   mu2 = 1.0 - ABC_trait_lac[,3],
#   gam2 = 0.04 - ABC_trait_lac[,4],
#   laa2 = 1.0 - ABC_trait_lac[,5],
#   trans = 0.02,
#   trans2 = 0.2
# )
#
# ## b. mu
# lac <- 0.5
# mu <- c(0.1,0.2,0.3,0.4,0.5)
# gam <- 0.02
# laa <- 0.5
# ABC_trait_mu <- expand.grid(
#   rep = rep,
#   lac = lac,
#   mu = mu,
#   gam = gam,
#   laa = laa,
#   K = K
# )
# ABC_trait_mu1 <- cbind(
#   ABC_trait_mu,
#   lac2 = 1.0 - ABC_trait_mu[,2],
#   mu2 = 1.0 - ABC_trait_mu[,3],
#   gam2 = 0.04 - ABC_trait_mu[,4],
#   laa2 = 1.0 - ABC_trait_mu[,5],
#   trans = 0.001,
#   trans2 = 0.001
# )
#
# ABC_trait_mu2 <- cbind(
#   ABC_trait_mu,
#   lac2 = 1.0 - ABC_trait_mu[,2],
#   mu2 = 1.0 - ABC_trait_mu[,3],
#   gam2 = 0.04 - ABC_trait_mu[,4],
#   laa2 = 1.0 - ABC_trait_mu[,5],
#   trans = 0.02,
#   trans2 = 0.2
# )
# ## c. gam
# lac <- 0.5
# mu <- 0.5
# gam <- c(0.004,0.008,0.012,0.016,0.02)
# laa <- 0.5
# ABC_trait_gam <- expand.grid(
#   rep = rep,
#   lac = lac,
#   mu = mu,
#   gam = gam,
#   laa = laa,
#   K = K
# )
# ABC_trait_gam1 <- cbind(
#   ABC_trait_gam,
#   lac2 = 1.0 - ABC_trait_gam[,2],
#   mu2 = 1.0 - ABC_trait_gam[,3],
#   gam2 = 0.04 - ABC_trait_gam[,4],
#   laa2 = 1.0 - ABC_trait_gam[,5],
#   trans = 0.001,
#   trans2 = 0.001
# )
#
# ABC_trait_gam2 <- cbind(
#   ABC_trait_gam,
#   lac2 = 1.0 - ABC_trait_gam[,2],
#   mu2 = 1.0 - ABC_trait_gam[,3],
#   gam2 = 0.04 - ABC_trait_gam[,4],
#   laa2 = 1.0 - ABC_trait_gam[,5],
#   trans = 0.02,
#   trans2 = 0.2
# )
# ## d. laa
# lac <- 0.5
# mu <- 0.5
# gam <- 0.02
# laa <- c(0.1,0.2,0.3,0.4,0.5)
# ABC_trait_laa <- expand.grid(
#   rep = rep,
#   lac = lac,
#   mu = mu,
#   gam = gam,
#   laa = laa,
#   K = K
# )
# ABC_trait_laa1 <- cbind(
#   ABC_trait_laa,
#   lac2 = 1.0 - ABC_trait_laa[,2],
#   mu2 = 1.0 - ABC_trait_laa[,3],
#   gam2 = 0.04 - ABC_trait_laa[,4],
#   laa2 = 1.0 - ABC_trait_laa[,5],
#   trans = 0.001,
#   trans2 = 0.001
# )
#
# ABC_trait_laa2 <- cbind(
#   ABC_trait_laa,
#   lac2 = 1.0 - ABC_trait_laa[,2],
#   mu2 = 1.0 - ABC_trait_laa[,3],
#   gam2 = 0.04 - ABC_trait_laa[,4],
#   laa2 = 1.0 - ABC_trait_laa[,5],
#   trans = 0.02,
#   trans2 = 0.2
# )
#
# TraiSIE_ABC_DD <- rbind(
#   ABC_trait_lac1,
#   ABC_trait_lac2,
#   ABC_trait_mu1,
#   ABC_trait_mu2,
#   ABC_trait_gam1,
#   ABC_trait_gam2,
#   ABC_trait_laa1,
#   ABC_trait_laa2
# )
#
# write.csv2(
#   TraiSIE_ABC_DD,
#   "data/TraiSIE_ABC_DD.csv",
#   row.names = FALSE
# )