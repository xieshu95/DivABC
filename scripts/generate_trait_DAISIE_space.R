## create input parameter sets for TRAISIE ABC
rep <- 1:10
K <- Inf
## a. lac
lac <- c(1:10)*0.1
mu <- 0.5
gam <- 0.02
laa <- 0.5
ABC_trait_lac <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)









################################
##
#2. ABC_trait
rep <- 1:5
K <- Inf
## a. lac mean = 0.3
lac <- runif(20,0.05,0.6)
mu <- 0.2
gam <- 0.01
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
  lac2 = 0.6 - ABC_trait_lac[,2],
  mu2 = 0.4 - ABC_trait_lac[,3],
  gam2 = 0.02 - ABC_trait_lac[,4],
  laa2 = 0.4 - ABC_trait_lac[,5],
  trans = 0,
  trans2 = 0
)

ABC_trait_lac2 <- cbind(
  ABC_trait_lac,
  lac2 = 0.6 - ABC_trait_lac[,2],
  mu2 = 0.4 - ABC_trait_lac[,3],
  gam2 = 0.02 - ABC_trait_lac[,4],
  laa2 = 0.4 - ABC_trait_lac[,5],
  trans = 0.2,
  trans2 = 0.2
)

## b. mu
lac <- 0.3
mu <- runif(20,0,0.4)
gam <- 0.01
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
  lac2 = 0.6 - ABC_trait_mu[,2],
  mu2 = 0.4 - ABC_trait_mu[,3],
  gam2 = 0.02 - ABC_trait_mu[,4],
  laa2 = 0.4 - ABC_trait_mu[,5],
  trans = 0,
  trans2 = 0
)

ABC_trait_mu2 <- cbind(
  ABC_trait_mu,
  lac2 = 0.6 - ABC_trait_mu[,2],
  mu2 = 0.4 - ABC_trait_mu[,3],
  gam2 = 0.02 - ABC_trait_mu[,4],
  laa2 = 0.4 - ABC_trait_mu[,5],
  trans = 0.2,
  trans2 = 0.2
)
## c. gam
lac <- 0.3
mu <- 0.2
gam <- runif(20,0.005,0.02)
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
  lac2 = 0.6 - ABC_trait_gam[,2],
  mu2 = 0.4 - ABC_trait_gam[,3],
  gam2 = 0.02 - ABC_trait_gam[,4],
  laa2 = 0.4 - ABC_trait_gam[,5],
  trans = 0,
  trans2 = 0
)

ABC_trait_gam2 <- cbind(
  ABC_trait_gam,
  lac2 = 0.6 - ABC_trait_gam[,2],
  mu2 = 0.4 - ABC_trait_gam[,3],
  gam2 = 0.02 - ABC_trait_gam[,4],
  laa2 = 0.4 - ABC_trait_gam[,5],
  trans = 0.2,
  trans2 = 0.2
)
## d. laa
lac <- 0.3
mu <- 0.2
gam <- 0.01
laa <- runif(20,0.05,0.4)
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
  lac2 = 0.6 - ABC_trait_laa[,2],
  mu2 = 0.4 - ABC_trait_laa[,3],
  gam2 = 0.02 - ABC_trait_laa[,4],
  laa2 = 0.4 - ABC_trait_laa[,5],
  trans = 0,
  trans2 = 0
)

ABC_trait_laa2 <- cbind(
  ABC_trait_laa,
  lac2 = 0.6 - ABC_trait_laa[,2],
  mu2 = 0.4 - ABC_trait_laa[,3],
  gam2 = 0.02 - ABC_trait_laa[,4],
  laa2 = 0.4 - ABC_trait_laa[,5],
  trans = 0.2,
  trans2 = 0.2
)

trait_DAISIE_single_change <- rbind(
  ABC_trait_lac1,
  ABC_trait_lac2,
  ABC_trait_mu1,
  ABC_trait_mu2,
  ABC_trait_gam1,
  ABC_trait_gam2,
  ABC_trait_laa1,
  ABC_trait_laa2
)

write.csv2(
  trait_DAISIE_single_change,
  "data/trait_DAISIE_single_change.csv",
  row.names = FALSE
)



