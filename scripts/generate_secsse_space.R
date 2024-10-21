## create input parameter sets for scesse_ABC
# two examined states and two concealed states

# latest param space

par1 <- c(0.6,0.6,0.05,0.05,0.05,0.05)
par2 <- c(0.6,0.3,0.05,0.05,0.05,0.05)
par3 <- c(0.6,0.12,0.05,0.05,0.05,0.05)
par4 <- c(0.6,0.6,0.05,0.1,0.05,0.05)
par5 <- c(0.6,0.6,0.05,0.25,0.05,0.05)
par6 <- c(0.6,0.6,0.05,0.05,0.05,0.1)
par7 <- c(0.6,0.6,0.05,0.05,0.05,0.25)

secsse_ABC <- rbind(par1,par2,par3,par4,par5,par6,par7)
colnames(secsse_ABC) <- c("lam1","lam2","mu1","mu2","q12","q21")

secsse_ABC_test <- secsse_ABC[rep(1:7, each = 50), ]
rownames(secsse_ABC_test) <- 1:nrow(secsse_ABC_test)
secsse_MCMC_test = secsse_ABC_test
save(secsse_ABC_test, file = "inst/extdata/secsse_ABC_test.rda")
save(secsse_MCMC_test, file = "inst/extdata/secsse_MCMC_test.rda")


write.csv2(
  secsse_ABC_test,
  "data/secsse_ABC_test.csv",
  row.names = FALSE
)


write.csv2(
  secsse_MCMC_test,
  "data/secsse_MCMC_test.csv",
  row.names = FALSE
)
