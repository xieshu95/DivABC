## create input parameter sets for ABC
#1. ABC_DAISIE

rep <- 1:10
K <- c(40, Inf)
lac <- c(0.2,0.4)
mu <- c(0.1,0.2)
gam <- c(0.005,0.01)
laa <-c(0.1,0.2)

DAISIE_ABC <- expand.grid(
  rep = rep,
  lac = lac,
  mu = mu,
  gam = gam,
  laa = laa,
  K = K
)

write.csv2(
  DAISIE_ABC,
  "data/DAISIE_ABC.csv",
  row.names = FALSE
)


