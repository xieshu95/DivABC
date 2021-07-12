## DI model
lac_abc <- c()
mu_abc <- c()
gam_abc <- c()
laa_abc <- c()
### for low rates
folder_path <- "G:/R/Traisie-ABC/results/DAISIE_ABC"
files <- list.files(folder_path)
param_num = 32
for (i in 1:10){
  param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("DAISIE_ABC_param_set_", param_set,"_rep",i, ".RData"),
                       files,
                       value = TRUE,
                       fixed = TRUE)

  abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lac_abc <- c(lac_abc, output[,1])
    mu_abc <- c(mu_abc, output[,2])
    gam_abc <- c(gam_abc, output[,3])
    laa_abc <- c(laa_abc, output[,4])
  }

}
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
param_select <- param_num * 10

png(paste0("G:/R/Traisie-ABC/plots/DAISIE_ABC/lac_param_",param_num,".png"))
hist(lac_abc, breaks = seq(0, 0.5, by = 0.01), col = "grey", main = "DI_lac")
abline(v = param_data[param_select,2], lty = 2, col = "blue", lwd = 2)
# abline(v = mean(lac_MLE), lty = 2, col = "green", lwd = 2)
# abline(v = median(lac_abc), lty = 2, col = "black", lwd = 2)
abline(v = median(lac_abc), lty = 2, col = "red", lwd = 2)
legend("right", c("True","median_ABC"),
       lty = c(2, 2),
       col = c("blue","red"), lwd = 2)
dev.off()

png(paste0("G:/R/Traisie-ABC/plots/DAISIE_ABC/mu_param_",param_num,".png"))
hist(mu_abc, breaks = seq(0, 0.5, by = 0.01), col = "grey", main = "DI_mu")
abline(v = param_data[param_select,3], lty = 2, col = "blue", lwd = 2)
# abline(v = mean(mu_MLE), lty = 2, col = "green", lwd = 2)
# abline(v = median(mu_abc), lty = 2, col = "black", lwd = 2)
abline(v = median(mu_abc), lty = 2, col = "red", lwd = 2)
legend("right", c("True","median_ABC"),
       lty = c(2, 2),
       col = c("blue","red"), lwd = 2)
dev.off()

png(paste0("G:/R/Traisie-ABC/plots/DAISIE_ABC/gam_param_",param_num,".png"))
hist(gam_abc, breaks = seq(0, 0.02, by = 0.0002), col = "grey", main = "DI_gam")
abline(v = param_data[param_select,4], lty = 2, col = "blue", lwd = 2)
# abline(v = mean(gam_MLE), lty = 2, col = "green", lwd = 2)
# abline(v = median(gam_abc), lty = 2, col = "black", lwd = 2)
abline(v = median(gam_abc), lty = 2, col = "red", lwd = 2)
legend("right", c("True","median_ABC"),
       lty = c(2, 2),
       col = c("blue","red"), lwd = 2)
dev.off()


png(paste0("G:/R/Traisie-ABC/plots/DAISIE_ABC/laa_param_",param_num,".png"))
hist(laa_abc, breaks = seq(0, 0.5, by = 0.01), col = "grey", main = "DI_laa")
abline(v = param_data[param_select,5], lty = 2, col = "blue", lwd = 2)
# abline(v = mean(laa_MLE), lty = 2, col = "green", lwd = 2)
# abline(v = median(laa_abc), lty = 2, col = "black", lwd = 2)
abline(v = median(laa_abc), lty = 2, col = "red", lwd = 2)
legend("right", c("True","median_ABC"),
       lty = c(2, 2),
       col = c("blue","red"), lwd = 2)
dev.off()







