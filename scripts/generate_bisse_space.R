## create input parameter sets for bisse_ABC
# two examined states and two concealed states

# latest param space

par1 <- c(0.6,0.6,0.05,0.05,0.05,0.05)
par2 <- c(0.6,0.3,0.05,0.05,0.05,0.05)
par3 <- c(0.6,0.12,0.05,0.05,0.05,0.05)
par4 <- c(0.6,0.6,0.05,0.1,0.05,0.05)
par5 <- c(0.6,0.6,0.05,0.25,0.05,0.05)
par6 <- c(0.6,0.6,0.05,0.05,0.05,0.1)
par7 <- c(0.6,0.6,0.05,0.05,0.05,0.25)

bisse_ABC <- rbind(par1,par2,par3,par4,par5,par6,par7)
colnames(bisse_ABC) <- c("lam1","lam2","mu1","mu2","q12","q21")

bisse_ABC_test <- bisse_ABC[rep(1:7, each = 50), ]
rownames(bisse_ABC_test) <- 1:nrow(bisse_ABC_test)
bisse_MCMC_test = bisse_ABC_test
save(bisse_ABC_test, file = "inst/extdata/bisse_ABC_test.rda")
save(bisse_MCMC_test, file = "inst/extdata/bisse_MCMC_test.rda")


write.csv2(
  bisse_ABC_test,
  "data/bisse_ABC_test.csv",
  row.names = FALSE
)


write.csv2(
  bisse_MCMC_test,
  "data/bisse_MCMC_test.csv",
  row.names = FALSE
)
