## create input parameter sets for geosse_ABC with three states
# two examined states and two concealed states

# latest param space

par1 <- c(0.5,0.5,0.5,0.1,0.1,0.2,0.2)
par2 <- c(0.75,0.5,0.25,0.1,0.1,0.2,0.2)
par3 <- c(0.5,0.5,0.5,0.1,0.2,0.2,0.2)
par4 <- c(0.5,0.5,0.5,0.1,0.1,0.2,0.1)

geosse_ABC <- rbind(par1,par2,par3,par4)
colnames(geosse_ABC) <- c("sA", "sB", "sAB", "xA", "xB", "dA", "dB")

geosse_ABC_test <- geosse_ABC[rep(1:4, each = 50), ]
rownames(geosse_ABC_test) <- 1:nrow(geosse_ABC_test)
geosse_MCMC_test = geosse_ABC_test
save(geosse_ABC_test, file = "inst/extdata/geosse_ABC_test.rda")
save(geosse_MCMC_test, file = "inst/extdata/geosse_MCMC_test.rda")


write.csv2(
  geosse_ABC_test,
  "data/geosse_ABC_test.csv",
  row.names = FALSE
)


write.csv2(
  geosse_MCMC_test,
  "data/geosse_MCMC_test.csv",
  row.names = FALSE
)
