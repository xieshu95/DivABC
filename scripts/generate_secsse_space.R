## create input parameter sets for scesse_ABC
# two examined states and two concealed states
## 1. ETD_lam
par1 <- c(0.5,0.5,0,0,0.1,0.1)
par2 <- c(0.3,0.7,0,0,0.1,0.1)
par3 <- c(0.3,0.7,0,0,0.05,0.1)
par4 <- c(0.3,0.7,0,0,0.1,0.05)
par5 <- c(0.3,0.7,0.05,0.05,0.1,0.1)
par6 <- c(0.3,0.7,0.05,0,0.1,0.1)
secsse <- rbind(par1,par2,par3,par4,par5,par6)
colnames(secsse) <- c("lam1","lam2","mu1","mu2","q12","q21")


write.csv2(
  secsse,
  "data/secsse_ABC.csv",
  row.names = FALSE
)
