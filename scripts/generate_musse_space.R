## create input parameter sets for musse_ABC with three states
# two examined states and two concealed states

# latest param space

par1 <- c(0.6,0.6,0.6,0.05,0.05,0.05,0.05)
par2 <- c(0.6,0.3,0.1,0.05,0.05,0.05,0.05)
par3 <- c(0.6,0.6,0.6,0.05,0.1,0.2,0.05)


musse_ABC <- rbind(par1,par2,par3)
colnames(musse_ABC) <- c("lam1","lam2","lam3","mu1","mu2","mu3","q")

musse_ABC_test <- musse_ABC[rep(1:3, each = 30), ]
rownames(musse_ABC_test) <- 1:nrow(musse_ABC_test)
musse_MCMC_test = musse_ABC_test
save(musse_ABC_test, file = "inst/extdata/musse_ABC_test.rda")
save(musse_MCMC_test, file = "inst/extdata/musse_MCMC_test.rda")


write.csv2(
  musse_ABC_test,
  "data/musse_ABC_test.csv",
  row.names = FALSE
)


write.csv2(
  musse_MCMC_test,
  "data/musse_MCMC_test.csv",
  row.names = FALSE
)

