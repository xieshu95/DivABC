## create input parameter sets for scesse_ABC
# two examined states and two concealed states

## 1. ETD_lam
par1 <- c(0.5,0.5,0,0,0.1,0.1)
par2 <- c(0.3,0.7,0,0,0.1,0.1)
par3 <- c(0.3,0.7,0,0,0.05,0.1)
par4 <- c(0.3,0.7,0,0,0.1,0.05)
par5 <- c(0.3,0.7,0.05,0.05,0.1,0.1)
par6 <- c(0.3,0.7,0.05,0,0.1,0.1)
secsse_ABC <- rbind(par1,par2,par3,par4,par5,par6)
colnames(secsse_ABC) <- c("lam1","lam2","mu1","mu2","q12","q21")


write.csv2(
  secsse_ABC,
  "data/secsse_ABC.csv",
  row.names = FALSE
)


secsse_MCMC = secsse_ABC
write.csv2(
  secsse_MCMC,
  "data/secsse_MCMC.csv",
  row.names = FALSE
)

## 2. ETD_lam more reps
par1 <- c(0.3,0.3,0,0,0.1,0.1)
par2 <- c(0.2,0.4,0,0,0.1,0.1)
par3 <- c(0.2,0.4,0,0,0.1,0.05)
par4 <- c(0.2,0.4,0,0,0.05,0.1)
par5 <- c(0.2,0.4,0.05,0.05,0.1,0.1)
par6 <- c(0.2,0.4,0.05,0.01,0.1,0.1)
par7 <- c(0.2,0.4,0.01,0.05,0.1,0.1)

secsse_ABC <- rbind(par1,par2,par3,par4,par5,par6,par7)
secsse_ABC <- secsse_ABC[rep(seq_len(nrow(secsse_ABC)), each = 10), ]
colnames(secsse_ABC) <- c("lam1","lam2","mu1","mu2","q12","q21")


write.csv2(
  secsse_ABC,
  "data/secsse_ABC_long.csv",
  row.names = FALSE
)


secsse_MCMC = secsse_ABC
write.csv2(
  secsse_MCMC,
  "data/secsse_MCMC_long.csv",
  row.names = FALSE
)



## 2. ETD_lam more reps
par1 <- c(0.3,0.3,0,0,0.1,0.1)
par2 <- c(0.2,0.4,0,0,0.1,0.1)
par3 <- c(0.2,0.4,0,0,0.1,0.05)
par4 <- c(0.2,0.4,0,0,0.05,0.1)
par5 <- c(0.2,0.4,0.05,0.05,0.1,0.1)
par6 <- c(0.2,0.4,0.05,0.01,0.1,0.1)
par7 <- c(0.2,0.4,0.01,0.05,0.1,0.1)

secsse_ABC <- rbind(par1,par2,par3,par4,par5,par6,par7)
secsse_ABC <- secsse_ABC[rep(seq_len(nrow(secsse_ABC)), each = 10), ]
colnames(secsse_ABC) <- c("lam1","lam2","mu1","mu2","q12","q21")


write.csv2(
  secsse_ABC,
  "data/secsse_ABC_long.csv",
  row.names = FALSE
)


secsse_MCMC = secsse_ABC
write.csv2(
  secsse_MCMC,
  "data/secsse_MCMC_long.csv",
  row.names = FALSE
)


## 3. ETD_lam more reps
lam1 <- 0.2
lam2 <- c(0.2,0.3,0.4)
mu1 <- 0
mu2 <- c(0,0.05,0.1)
q12 <- 0.05
q21 <- c(0.05,0.1,0.2)
secsse_ABC <- expand.grid(
  lam1 = lam1,
  lam2 = lam2,
  mu1 = mu1,
  mu2 = mu2,
  q12 = q12,
  q21 = q21
)

write.csv2(
  secsse_ABC,
  "data/secsse_ABC.csv",
  row.names = FALSE
)


secsse_MCMC = secsse_ABC
write.csv2(
  secsse_MCMC,
  "data/secsse_MCMC.csv",
  row.names = FALSE
)


## 4. secsse test
lam1 <- 0.2
lam2 <- 0.4
mu1 <- 0.05
mu2 <- c(0.05,0.1)
q12 <- 0.05
q21 <- c(0.05,0.1)
secsse_ABC <- expand.grid(
  lam1 = lam1,
  lam2 = lam2,
  mu1 = mu1,
  mu2 = mu2,
  q12 = q12,
  q21 = q21
)
secsse_ABC_test1 <- secsse_ABC[rep(1,100), ]
rownames(secsse_ABC_test1) <- 1:nrow(secsse_ABC_test1)
write.csv2(
  secsse_ABC_test1,
  "data/secsse_ABC_test1.csv",
  row.names = FALSE
)
secsse_MCMC_test1 = secsse_ABC_test1
write.csv2(
  secsse_MCMC_test1,
  "data/secsse_MCMC_test1.csv",
  row.names = FALSE
)


secsse_ABC_test2 <- secsse_ABC[rep(2,100), ]
rownames(secsse_ABC_test2) <- 1:nrow(secsse_ABC_test2)
write.csv2(
  secsse_ABC_test2,
  "data/secsse_ABC_test2.csv",
  row.names = FALSE
)
secsse_MCMC_test2 = secsse_ABC_test2
write.csv2(
  secsse_MCMC_test2,
  "data/secsse_MCMC_test2.csv",
  row.names = FALSE
)


secsse_ABC_test3 <- secsse_ABC[rep(3,100), ]
rownames(secsse_ABC_test3) <- 1:nrow(secsse_ABC_test3)
write.csv2(
  secsse_ABC_test3,
  "data/secsse_ABC_test3.csv",
  row.names = FALSE
)
secsse_MCMC_test3 = secsse_ABC_test3
write.csv2(
  secsse_MCMC_test3,
  "data/secsse_MCMC_test3.csv",
  row.names = FALSE
)


secsse_ABC_test4 <- secsse_ABC[rep(4,100), ]
rownames(secsse_ABC_test4) <- 1:nrow(secsse_ABC_test4)
write.csv2(
  secsse_ABC_test4,
  "data/secsse_ABC_test4.csv",
  row.names = FALSE
)
secsse_MCMC_test4 = secsse_ABC_test4
write.csv2(
  secsse_MCMC_test4,
  "data/secsse_MCMC_test4.csv",
  row.names = FALSE
)

##
# new test
par1 <- c(0.3,0.3,0.05,0.05,0.1,0.1)  ## all symmetric
par2 <- c(0.2,0.4,0.05,0.05,0.1,0.1)  ## asymmetric in lam
par3 <- c(0.2,0.4,0.01,0.01,0.1,0.1)  ## asymmetric in lam, low mu
par4 <- c(0.2,0.4,0.01,0.05,0.1,0.1)  ## asymmetric in lam and mu
par5 <- c(0.2,0.4,0.01,0.01,0.1,0.2)  ## asymmetric in lam and q
par6 <- c(0.2,0.4,0.01,0.01,0.2,0.1)
# par2 <- c(0.3,0.3,0.01,0.05,0.1,0.1)  ## asymmetric in mu
# par2 <- c(0.3,0.3,0.05,0.05,0.2,0.1)  ## asymmetric in mu

secsse_ABC <- data.frame(rbind(par1,par2,par3,par4,par5,par6))
colnames(secsse_ABC) <- c("lam1","lam2","mu1","mu2","q12","q21")


secsse_ABC_test1 <- secsse_ABC[rep(1,100), ]
rownames(secsse_ABC_test1) <- 1:nrow(secsse_ABC_test1)
write.csv2(
  secsse_ABC_test1,
  "data/secsse_ABC_test1.csv",
  row.names = FALSE
)
secsse_MCMC_test1 = secsse_ABC_test1
write.csv2(
  secsse_MCMC_test1,
  "data/secsse_MCMC_test1.csv",
  row.names = FALSE
)


secsse_ABC_test2 <- secsse_ABC[rep(2,100), ]
rownames(secsse_ABC_test2) <- 1:nrow(secsse_ABC_test2)
write.csv2(
  secsse_ABC_test2,
  "data/secsse_ABC_test2.csv",
  row.names = FALSE
)
secsse_MCMC_test2 = secsse_ABC_test2
write.csv2(
  secsse_MCMC_test2,
  "data/secsse_MCMC_test2.csv",
  row.names = FALSE
)


secsse_ABC_test3 <- secsse_ABC[rep(3,100), ]
rownames(secsse_ABC_test3) <- 1:nrow(secsse_ABC_test3)
write.csv2(
  secsse_ABC_test3,
  "data/secsse_ABC_test3.csv",
  row.names = FALSE
)
secsse_MCMC_test3 = secsse_ABC_test3
write.csv2(
  secsse_MCMC_test3,
  "data/secsse_MCMC_test3.csv",
  row.names = FALSE
)


secsse_ABC_test4 <- secsse_ABC[rep(4,100), ]
rownames(secsse_ABC_test4) <- 1:nrow(secsse_ABC_test4)
write.csv2(
  secsse_ABC_test4,
  "data/secsse_ABC_test4.csv",
  row.names = FALSE
)
secsse_MCMC_test4 = secsse_ABC_test4
write.csv2(
  secsse_MCMC_test4,
  "data/secsse_MCMC_test4.csv",
  row.names = FALSE
)


secsse_ABC_test5 <- secsse_ABC[rep(5,100), ]
rownames(secsse_ABC_test5) <- 1:nrow(secsse_ABC_test5)
write.csv2(
  secsse_ABC_test5,
  "data/secsse_ABC_test5.csv",
  row.names = FALSE
)
secsse_MCMC_test5 = secsse_ABC_test5
write.csv2(
  secsse_MCMC_test5,
  "data/secsse_MCMC_test5.csv",
  row.names = FALSE
)

secsse_ABC_test6 <- secsse_ABC[rep(6,100), ]
rownames(secsse_ABC_test6) <- 1:nrow(secsse_ABC_test6)
write.csv2(
  secsse_ABC_test6,
  "data/secsse_ABC_test6.csv",
  row.names = FALSE
)
secsse_MCMC_test6 = secsse_ABC_test6
write.csv2(
  secsse_MCMC_test6,
  "data/secsse_MCMC_test6.csv",
  row.names = FALSE
)
