## DI model
lac_abc <- c()
mu_abc <- c()
gam_abc <- c()
laa_abc <- c()
### for low rates
folder_path <- "G:/R/Traisie-ABC/results/ABC_version16/ABC_DI"
files <- list.files(folder_path)
for (i in 1:30){
  file_to_load <- grep(paste0("DI_rep", i, ".RData"),
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

load(file=paste0("G:/R/Traisie-ABC/results/ABC_MLE/MLE_DI.RData"))
lac_MLE<- c()
mu_MLE <-c()
gam_MLE <- c()
laa_MLE <-c()
for(i in 1:50){
  lac_MLE[i] <- MLE_DI[[i]]$lambda_c
  mu_MLE[i] <- MLE_DI[[i]]$mu
  gam_MLE[i] <- MLE_DI[[i]]$gamma
  laa_MLE[i] <- MLE_DI[[i]]$lambda_a
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

png(paste0("G:/R/Traisie-ABC/plots/ABC_version16/DI_lac.png"))
hist(lac_abc, breaks = seq(0, 0.5, by = 0.01), col = "grey", main = "DI_lac")
abline(v = 0.3, lty = 2, col = "blue", lwd = 2)
abline(v = mean(lac_MLE), lty = 2, col = "green", lwd = 2)
# abline(v = median(lac_abc), lty = 2, col = "black", lwd = 2)
abline(v = median(lac_abc), lty = 2, col = "red", lwd = 2)
legend("right", c("MLE", "True","mean_ABC"),
       lty = c(2, 2, 2),
       col = c("green", "blue","red"), lwd = 2)
dev.off()

png(paste0("G:/R/Traisie-ABC/plots/ABC_version16/DI_mu.png"))
hist(mu_abc, breaks = seq(0, 0.5, by = 0.01), col = "grey", main = "DI_mu")
abline(v = 0.2, lty = 2, col = "blue", lwd = 2)
abline(v = mean(mu_MLE), lty = 2, col = "green", lwd = 2)
# abline(v = median(mu_abc), lty = 2, col = "black", lwd = 2)
abline(v = median(mu_abc), lty = 2, col = "red", lwd = 2)
legend("right", c("MLE", "True","mean_ABC"),
       lty = c(2, 2, 2),
       col = c("green", "blue","red"), lwd = 2)
dev.off()

png(paste0("G:/R/Traisie-ABC/plots/ABC_version16/DI_gam.png"))
hist(gam_abc, breaks = seq(0, 0.02, by = 0.0002), col = "grey", main = "DI_gam")
abline(v = 0.008, lty = 2, col = "blue", lwd = 2)
abline(v = mean(gam_MLE), lty = 2, col = "green", lwd = 2)
# abline(v = median(gam_abc), lty = 2, col = "black", lwd = 2)
abline(v = median(gam_abc), lty = 2, col = "red", lwd = 2)
legend("right", c("MLE", "True","mean_ABC"),
       lty = c(2, 2, 2),
       col = c("green", "blue","red"), lwd = 2)
dev.off()


png(paste0("G:/R/Traisie-ABC/plots/ABC_version16/DI_laa.png"))
hist(laa_abc, breaks = seq(0, 0.5, by = 0.01), col = "grey", main = "DI_laa")
abline(v = 0.2, lty = 2, col = "blue", lwd = 2)
abline(v = mean(laa_MLE), lty = 2, col = "green", lwd = 2)
# abline(v = median(laa_abc), lty = 2, col = "black", lwd = 2)
abline(v = median(laa_abc), lty = 2, col = "red", lwd = 2)
legend("right", c("MLE", "True","mean_ABC"),
       lty = c(2, 2, 2),
       col = c("green", "blue","red"), lwd = 2)
dev.off()



## DD model
lac_abc <- c()
mu_abc <- c()
gam_abc <- c()
laa_abc <- c()
### for low rates
folder_path <- "G:/R/Traisie-ABC/results/ABC_version16/ABC_DD"
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

load(file=paste0("G:/R/Traisie-ABC/results/ABC_MLE/MLE_DD.RData"))
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

png(paste0("G:/R/Traisie-ABC/plots/ABC_version16/DD_lac.png"))
hist(lac_abc, breaks = seq(0, 0.5, by = 0.01), col = "grey", main = "DD_lac")
abline(v = 0.3, lty = 2, col = "blue", lwd = 2)
abline(v = mean(lac_MLE), lty = 2, col = "green", lwd = 2)
# abline(v = median(lac_abc), lty = 2, col = "black", lwd = 2)
abline(v = mean(lac_abc), lty = 2, col = "red", lwd = 2)
legend("right", c("MLE", "True","mean_ABC"),
       lty = c(2, 2, 2),
       col = c("green", "blue","red"), lwd = 2)
dev.off()

png(paste0("G:/R/Traisie-ABC/plots/ABC_version16/DD_mu.png"))
hist(mu_abc, breaks = seq(0, 0.5, by = 0.01), col = "grey", main = "DD_mu")
abline(v = 0.2, lty = 2, col = "blue", lwd = 2)
abline(v = mean(mu_MLE), lty = 2, col = "green", lwd = 2)
# abline(v = median(mu_abc), lty = 2, col = "black", lwd = 2)
abline(v = mean(mu_abc), lty = 2, col = "red", lwd = 2)
legend("right", c("MLE", "True","mean_ABC"),
       lty = c(2, 2, 2),
       col = c("green", "blue","red"), lwd = 2)
dev.off()

png(paste0("G:/R/Traisie-ABC/plots/ABC_version16/DD_gam.png"))
hist(gam_abc, breaks = seq(0, 0.02, by = 0.0002), col = "grey", main = "DD_gam")
abline(v = 0.008, lty = 2, col = "blue", lwd = 2)
abline(v = mean(gam_MLE), lty = 2, col = "green", lwd = 2)
# abline(v = median(gam_abc), lty = 2, col = "black", lwd = 2)
abline(v = mean(gam_abc), lty = 2, col = "red", lwd = 2)
legend("right", c("MLE", "True","mean_ABC"),
       lty = c(2, 2, 2),
       col = c("green", "blue","red"), lwd = 2)
dev.off()


png(paste0("G:/R/Traisie-ABC/plots/ABC_version16/DD_laa.png"))
hist(laa_abc, breaks = seq(0, 0.5, by = 0.01), col = "grey", main = "DD_laa")
abline(v = 0.2, lty = 2, col = "blue", lwd = 2)
abline(v = mean(laa_MLE), lty = 2, col = "green", lwd = 2)
# abline(v = median(laa_abc), lty = 2, col = "black", lwd = 2)
abline(v = mean(laa_abc), lty = 2, col = "red", lwd = 2)
legend("right", c("MLE", "True","mean_ABC"),
       lty = c(2, 2, 2),
       col = c("green", "blue","red"), lwd = 2)
dev.off()





# ### plot each replicate
# hist(lac_abc[1:5000], breaks = seq(0, 1, by = 0.01), col = "grey", main = "lac")
# abline(v = 0.5, lty = 2, col = "blue", lwd = 2)
# legend("right", c("MLE", "True","MCMC"),
#        lty = c(2, 2, 2),
#        col = c("green", "blue", "red"), lwd = 2)
#
# hist(lac_abc[5001:10000], breaks = seq(0, 1, by = 0.01), col = "grey", main = "lac")
# abline(v = 0.5, lty = 2, col = "blue", lwd = 2)
# legend("right", c("MLE", "True","MCMC"),
#        lty = c(2, 2, 2),
#        col = c("green", "blue", "red"), lwd = 2)
