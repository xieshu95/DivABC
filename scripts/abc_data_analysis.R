lac_abc <- c()
mu_abc <- c()
gam_abc <- c()
laa_abc <- c()

folder_path <- "G:/R/Traisie-ABC/results/ABC_allpars_ss134_highrates/ABC_DD"
files <- list.files(folder_path)
for (i in 1:30){
  file_to_load <- grep(paste0("DD_rep", i, ".RData"),
                       files,
                       value = TRUE,
                       fixed = TRUE)
  abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lac_abc <- c(lac_abc, abc[,1])
    mu_abc <- c(mu_abc, abc[,2])
    gam_abc <- c(gam_abc, abc[,3])
    laa_abc <- c(laa_abc, abc[,4])
  }

}


load(file=paste0("G:/R/Traisie-ABC/results/ABC_MLE/MLE_DD_high_rates.RData"))
lac_MLE<- c()
mu_MLE <-c()
gam_MLE <- c()
laa_MLE <-c()
for(i in 1:50){
  lac_MLE[i] <- MLE_DD[[i]]$lambda_c
  mu_MLE[i] <- MLE_DD[[i]]$mu
  gam_MLE[i] <- MLE_DD[[i]]$gamma
  laa_MLE[i] <- MLE_DD[[i]]$lambda_a
}

# load("G:/R/Traisie-ABC/results/MCMC_version2/MCMC/results/mcmc_allpars_10reps.RData")
# mcmc_com <- c()
# for(i in 1:10){
#   mcmc_rep <- mcmc_list[[i]]
#   mcmc_com <- rbind(mcmc_com,mcmc_rep)
# }
# mcmca<- coda::as.mcmc(mcmc_com)
# lac_mcmc <- median(mcmca[,1])
# mu_mcmc <- median(mcmca[,2])
# gam_mcmc <- median(mcmca[,3])
# laa_mcmc <- median(mcmca[,4])

png(paste0("G:/R/Traisie-ABC/plots/ABC_allpars_allss_highrates_DD/ss134/lac.png"))
hist(lac_abc, breaks = seq(0, 1, by = 0.01), col = "grey", main = "lac")
abline(v = 0.5, lty = 2, col = "blue", lwd = 2)
abline(v = mean(lac_MLE), lty = 2, col = "green", lwd = 2)
legend("right", c("MLE", "True"),
       lty = c(2, 2),
       col = c("green", "blue"), lwd = 2)
dev.off()

png(paste0("G:/R/Traisie-ABC/plots/ABC_allpars_allss_highrates_DD/ss134/mu.png"))
hist(mu_abc, breaks = seq(0, 0.5, by = 0.01), col = "grey", main = "mu")
abline(v = 0.3, lty = 2, col = "blue", lwd = 2)
abline(v = mean(mu_MLE), lty = 2, col = "green", lwd = 2)
legend("right", c("MLE", "True"),
       lty = c(2, 2),
       col = c("green", "blue"), lwd = 2)
dev.off()

png(paste0("G:/R/Traisie-ABC/plots/ABC_allpars_allss_highrates_DD/ss134/gam.png"))
hist(gam_abc, breaks = seq(0, 0.05, by = 0.0002), col = "grey", main = "gam")
abline(v = 0.02, lty = 2, col = "blue", lwd = 2)
abline(v = mean(gam_MLE), lty = 2, col = "green", lwd = 2)
legend("right", c("MLE", "True"),
       lty = c(2, 2),
       col = c("green", "blue"), lwd = 2)
dev.off()


png(paste0("G:/R/Traisie-ABC/plots/ABC_allpars_allss_highrates_DD/ss134/laa.png"))
hist(laa_abc, breaks = seq(0, 1, by = 0.01), col = "grey", main = "laa")
abline(v = 0.5, lty = 2, col = "blue", lwd = 2)
abline(v = mean(laa_MLE), lty = 2, col = "green", lwd = 2)
legend("right", c("MLE", "True"),
       lty = c(2, 2),
       col = c("green", "blue"), lwd = 2)
dev.off()






hist(lac_abc[1:5000], breaks = seq(0, 1, by = 0.01), col = "grey", main = "lac")
abline(v = 0.5, lty = 2, col = "blue", lwd = 2)
legend("right", c("MLE", "True","MCMC"),
       lty = c(2, 2, 2),
       col = c("green", "blue", "red"), lwd = 2)

hist(lac_abc[5001:10000], breaks = seq(0, 1, by = 0.01), col = "grey", main = "lac")
abline(v = 0.5, lty = 2, col = "blue", lwd = 2)
legend("right", c("MLE", "True","MCMC"),
       lty = c(2, 2, 2),
       col = c("green", "blue", "red"), lwd = 2)


hist(mu_abc[1:5000], breaks = seq(0, 0.5, by = 0.02), col = "grey", main = "mu")
abline(v = 0.3, lty = 2, col = "blue", lwd = 2)
legend("right", c("MLE", "True","MCMC"),
       lty = c(2, 2, 2),
       col = c("green", "blue", "red"), lwd = 2)
hist(mu_abc[5001:10000], breaks = seq(0, 0.5, by = 0.02), col = "grey", main = "mu")
abline(v = 0.3, lty = 2, col = "blue", lwd = 2)
legend("right", c("MLE", "True","MCMC"),
       lty = c(2, 2, 2),
       col = c("green", "blue", "red"), lwd = 2)
