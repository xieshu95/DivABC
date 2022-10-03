## create input parameter sets for scesse_ABC
# two examined states and two concealed states
## 1. ETD_lam
lam1 <- 0.2
lam2 <- c(0.2,0.8)
mu1 <- 0.1
mu2 <- c(0.1,0.2)
q12 <- 0.1
q21 <- c(0.1,0.2)
secsse <- expand.grid(
  lam1 = lam1,
  lam2 = lam2,
  mu1 = mu1,
  mu2 = mu2,
  q12 = q12,
  q21 = q21
)

write.csv2(
  secsse,
  "data/secsse.csv",
  row.names = FALSE
)
