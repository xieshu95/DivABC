## plot all the parameters in one
## DI model
lac_abc <- c()
mu_abc <- c()
gam_abc <- c()
laa_abc <- c()
### for low rates
folder_path <- "G:/R/Traisie-ABC/results/DAISIE_ABC_2"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
for(param_num in 1:32){
  lac_abc <- c()
  mu_abc <- c()
  gam_abc <- c()
  laa_abc <- c()
  for (i in 1:5){
    # param_set = (param_num-1)*5 + i
    file_to_load <- grep(paste0("DAISIE_ABC_param_set_", param_num,"_rep",i, ".RData"),
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
  param_select <- param_num * 5

  png(paste0("G:/R/Traisie-ABC/plots/DAISIE_ABC/lac_param_",param_num,".png"))
  hist(lac_abc, breaks = seq(0, 1, by = 0.01), col = "grey", main = "DI_lac")
  abline(v = param_data[param_select,2], lty = 2, col = "blue", lwd = 2)
  # abline(v = mean(lac_MLE), lty = 2, col = "green", lwd = 2)
  # abline(v = median(lac_abc), lty = 2, col = "black", lwd = 2)
  abline(v = median(lac_abc), lty = 2, col = "red", lwd = 2)
  legend("right", c("True","median_ABC"),
         lty = c(2, 2),
         col = c("blue","red"), lwd = 2)
  dev.off()

  png(paste0("G:/R/Traisie-ABC/plots/DAISIE_ABC/mu_param_",param_num,".png"))
  hist(mu_abc, breaks = seq(0, 1, by = 0.01), col = "grey", main = "DI_mu")
  abline(v = param_data[param_select,3], lty = 2, col = "blue", lwd = 2)
  # abline(v = mean(mu_MLE), lty = 2, col = "green", lwd = 2)
  # abline(v = median(mu_abc), lty = 2, col = "black", lwd = 2)
  abline(v = median(mu_abc), lty = 2, col = "red", lwd = 2)
  legend("right", c("True","median_ABC"),
         lty = c(2, 2),
         col = c("blue","red"), lwd = 2)
  dev.off()

  png(paste0("G:/R/Traisie-ABC/plots/DAISIE_ABC/gam_param_",param_num,".png"))
  hist(gam_abc, breaks = seq(0, 0.1, by = 0.0002), col = "grey", main = "DI_gam")
  abline(v = param_data[param_select,4], lty = 2, col = "blue", lwd = 2)
  # abline(v = mean(gam_MLE), lty = 2, col = "green", lwd = 2)
  # abline(v = median(gam_abc), lty = 2, col = "black", lwd = 2)
  abline(v = median(gam_abc), lty = 2, col = "red", lwd = 2)
  legend("right", c("True","median_ABC"),
         lty = c(2, 2),
         col = c("blue","red"), lwd = 2)
  dev.off()


  png(paste0("G:/R/Traisie-ABC/plots/DAISIE_ABC/laa_param_",param_num,".png"))
  hist(laa_abc, breaks = seq(0, 1, by = 0.01), col = "grey", main = "DI_laa")
  abline(v = param_data[param_select,5], lty = 2, col = "blue", lwd = 2)
  # abline(v = mean(laa_MLE), lty = 2, col = "green", lwd = 2)
  # abline(v = median(laa_abc), lty = 2, col = "black", lwd = 2)
  abline(v = median(laa_abc), lty = 2, col = "red", lwd = 2)
  legend("right", c("True","median_ABC"),
         lty = c(2, 2),
         col = c("blue","red"), lwd = 2)
  dev.off()

}


## calculate MAE and RMSE for each parameter set (combine reps)
folder_path <- "G:/R/Traisie-ABC/results/DAISIE_ABC_2"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
MAE_lac <- c()
MAE_mu <- c()
MAE_gam <- c()
MAE_laa <- c()
RMSE_lac <- c()
RMSE_mu <- c()
RMSE_gam <- c()
RMSE_laa <- c()
for(param_num in 1:32){
  lac_abc <- c()
  mu_abc <- c()
  gam_abc <- c()
  laa_abc <- c()
  for (i in 1:5){
    # param_set = (param_num-1)*5 + i
    file_to_load <- grep(paste0("DAISIE_ABC_param_set_", param_num,"_rep",i, ".RData"),
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
  param_select <- param_num * 5
  real = param_data[param_select,2:5]
  MAE_lac[param_num] <- Metrics::mae(real$lac,lac_abc)
  MAE_mu[param_num] <- Metrics::mae(real$mu,mu_abc)
  MAE_gam[param_num] <- Metrics::mae(real$gam,gam_abc)
  MAE_laa[param_num] <- Metrics::mae(real$laa,laa_abc)
  RMSE_lac[param_num] <- Metrics::rmse(real$lac,lac_abc)
  RMSE_mu[param_num] <- Metrics::rmse(real$mu,mu_abc)
  RMSE_gam[param_num] <- Metrics::rmse(real$gam,gam_abc)
  RMSE_laa[param_num] <- Metrics::rmse(real$laa,laa_abc)
}

true_pars <- param_data[seq(0,nrow(param_data),5),]
whole_df <- data.frame(true_pars,MAE_lac,MAE_mu,MAE_gam,MAE_laa,
                       RMSE_lac,RMSE_mu,RMSE_gam,RMSE_laa,
                       stringsAsFactors = TRUE)
whole_df$K[whole_df$K == "40"] <- "DD"
whole_df$K[whole_df$K == "Inf"] <- "DI"

p <- ggplot2::ggplot(whole_df, ggplot2::aes(x=laa, y=MAE_laa, color = K)) +  ##change y and factor  (shape = K)
  ggplot2::theme_bw() +
  ggplot2::geom_jitter(position = ggplot2::position_jitterdodge(jitter.width = 0.01, dodge.width = 0.1)) +  ##position = ggplot2::position_jitterdodge(0.2)
  ggplot2::scale_color_brewer(palette = "Set2") +
  ggplot2::geom_hline(yintercept = 0.05, linetype = "dashed", size = 0.3) +
  ggplot2::xlab("lac") +  #Rate differential ratio of anagenesis/Diversity dependence
  ggplot2::ylab("Mean absolute error in lac") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.margin = ggplot2::margin(6, 0.2, 6, 0.2)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5))
print(p)


## calc mae and rmae for each replicate
folder_path <- "G:/R/Traisie-ABC/results/DAISIE_ABC_2"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
MAE_lac <- c()
MAE_mu <- c()
MAE_gam <- c()
MAE_laa <- c()
RMSE_lac <- c()
RMSE_mu <- c()
RMSE_gam <- c()
RMSE_laa <- c()
for(i in 1:160){
  lac_abc <- c()
  mu_abc <- c()
  gam_abc <- c()
  laa_abc <- c()
  if(i%%5 == 0){
    param_num <- i%/%5
    rep <- 5
  } else {
    param_num <- i%/%5 + 1
    rep <- i%%5
  }
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("DAISIE_ABC_param_set_", param_num,"_rep",rep, ".RData"),
                       files,
                       value = TRUE,
                       fixed = TRUE)

  abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lac_abc <- output[,1]
    mu_abc <- output[,2]
    gam_abc <- output[,3]
    laa_abc <- output[,4]
  } else {
    lac_abc <- NA
    mu_abc <- NA
    gam_abc <- NA
    laa_abc <- NA
  }
  real = param_data[i,2:5]
  MAE_lac[i] <- Metrics::mae(real$lac,lac_abc)
  MAE_mu[i] <- Metrics::mae(real$mu,mu_abc)
  MAE_gam[i] <- Metrics::mae(real$gam,gam_abc)
  MAE_laa[i] <- Metrics::mae(real$laa,laa_abc)
  RMSE_lac[i] <- Metrics::rmse(real$lac,lac_abc)
  RMSE_mu[i] <- Metrics::rmse(real$mu,mu_abc)
  RMSE_gam[i] <- Metrics::rmse(real$gam,gam_abc)
  RMSE_laa[i] <- Metrics::rmse(real$laa,laa_abc)
}


whole_df <- data.frame(param_data,MAE_lac,MAE_mu,MAE_gam,MAE_laa,
                       RMSE_lac,RMSE_mu,RMSE_gam,RMSE_laa,
                       stringsAsFactors = TRUE)
whole_df$K[whole_df$K == "40"] <- "DD"
whole_df$K[whole_df$K == "Inf"] <- "DI"
p <- ggplot2::ggplot(whole_df, ggplot2::aes(x=lac, y=RMSE_lac, color = K)) +  ##change y and factor  (shape = K)
  ggplot2::theme_bw() +
  ggplot2::geom_jitter(position = ggplot2::position_jitterdodge(jitter.width = 0.01, dodge.width = 0.1)) +  ##position = ggplot2::position_jitterdodge(0.2)
  ggplot2::scale_color_brewer(palette = "Set2") +
  ggplot2::geom_hline(yintercept = 0.05, linetype = "dashed", size = 0.3) +
  ggplot2::xlab("Rate differential ratio") +  #Rate differential ratio of anagenesis/Diversity dependence
  ggplot2::ylab("Mean absolute error in lac") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.margin = ggplot2::margin(6, 0.2, 6, 0.2)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5))
print(p)


# ## DI model
# lac_abc <- c()
# mu_abc <- c()
# gam_abc <- c()
# laa_abc <- c()
# ### for low rates
# folder_path <- "G:/R/Traisie-ABC/results/DAISIE_ABC_2"
# files <- list.files(folder_path)
# param_num = 1
# for (i in 1:5){
#   # param_set = (param_num-1)*5 + i
#   file_to_load <- grep(paste0("DAISIE_ABC_param_set_", param_num,"_rep",i, ".RData"),
#                        files,
#                        value = TRUE,
#                        fixed = TRUE)
#
#   abc <- NULL; rm(abc) # nolint ; hack around global var
#   if (!identical(file_to_load, character())) {
#     load(file.path(folder_path, file_to_load))
#     lac_abc <- c(lac_abc, output[,1])
#     mu_abc <- c(mu_abc, output[,2])
#     gam_abc <- c(gam_abc, output[,3])
#     laa_abc <- c(laa_abc, output[,4])
#   }
#
# }
#
# param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
# param_select <- param_num * 5
#
# png(paste0("G:/R/Traisie-ABC/plots/DAISIE_ABC/lac_param_",param_num,".png"))
# hist(lac_abc, breaks = seq(0, 1, by = 0.01), col = "grey", main = "DI_lac")
# abline(v = param_data[param_select,2], lty = 2, col = "blue", lwd = 2)
# # abline(v = mean(lac_MLE), lty = 2, col = "green", lwd = 2)
# # abline(v = median(lac_abc), lty = 2, col = "black", lwd = 2)
# abline(v = median(lac_abc), lty = 2, col = "red", lwd = 2)
# legend("right", c("True","median_ABC"),
#        lty = c(2, 2),
#        col = c("blue","red"), lwd = 2)
# dev.off()
#
# png(paste0("G:/R/Traisie-ABC/plots/DAISIE_ABC/mu_param_",param_num,".png"))
# hist(mu_abc, breaks = seq(0, 1, by = 0.01), col = "grey", main = "DI_mu")
# abline(v = param_data[param_select,3], lty = 2, col = "blue", lwd = 2)
# # abline(v = mean(mu_MLE), lty = 2, col = "green", lwd = 2)
# # abline(v = median(mu_abc), lty = 2, col = "black", lwd = 2)
# abline(v = median(mu_abc), lty = 2, col = "red", lwd = 2)
# legend("right", c("True","median_ABC"),
#        lty = c(2, 2),
#        col = c("blue","red"), lwd = 2)
# dev.off()
#
# png(paste0("G:/R/Traisie-ABC/plots/DAISIE_ABC/gam_param_",param_num,".png"))
# hist(gam_abc, breaks = seq(0, 0.1, by = 0.0002), col = "grey", main = "DI_gam")
# abline(v = param_data[param_select,4], lty = 2, col = "blue", lwd = 2)
# # abline(v = mean(gam_MLE), lty = 2, col = "green", lwd = 2)
# # abline(v = median(gam_abc), lty = 2, col = "black", lwd = 2)
# abline(v = median(gam_abc), lty = 2, col = "red", lwd = 2)
# legend("right", c("True","median_ABC"),
#        lty = c(2, 2),
#        col = c("blue","red"), lwd = 2)
# dev.off()
#
#
# png(paste0("G:/R/Traisie-ABC/plots/DAISIE_ABC/laa_param_",param_num,".png"))
# hist(laa_abc, breaks = seq(0, 1, by = 0.01), col = "grey", main = "DI_laa")
# abline(v = param_data[param_select,5], lty = 2, col = "blue", lwd = 2)
# # abline(v = mean(laa_MLE), lty = 2, col = "green", lwd = 2)
# # abline(v = median(laa_abc), lty = 2, col = "black", lwd = 2)
# abline(v = median(laa_abc), lty = 2, col = "red", lwd = 2)
# legend("right", c("True","median_ABC"),
#        lty = c(2, 2),
#        col = c("blue","red"), lwd = 2)
# dev.off()










