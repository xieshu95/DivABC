## analyse secsse_test
#####
# 1. formate ABC results
## check new secsse ABC result
for(test in c(1,3,5,6)){
  # formate results
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/cpp_obs_ss_test",test,".RData"))
  ## ABC results
  folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/secsse_ABC_test",test)
  files <- list.files(folder_path)
  param_data <- readr::read_csv2(paste0("data/secsse_ABC_test",test,".csv"))

  param_data2<-param_data[rep(seq_len(nrow(param_data)), each=500),] #500
  lam1_abc <- c()
  lam2_abc <- c()
  mu1_abc <- c()
  mu2_abc <- c()
  q12_abc <- c()
  q21_abc <- c()
  n_iter <- c()
  n_iteration <- c()
  for(i in 1:100){
    file_to_load <- grep(paste0("secsse_ABC_test",test,"_param_set_",i,"_ss_1.RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      if(output$n_iter <= 4){
        lam1_abc <- c(lam1_abc, rep(NA,500))
        lam2_abc <- c(lam2_abc, rep(NA,500))
        mu1_abc <- c(mu1_abc, rep(NA,500))
        mu2_abc <- c(mu2_abc, rep(NA,500))
        q12_abc <- c(q12_abc, rep(NA,500))
        q21_abc <- c(q21_abc, rep(NA,500))
        n_iteration <- c(n_iteration,rep(NA,500))
      } else if (nrow(output$ABC[[output$n_iter]]) == 500) {
        lam1_abc <- c(lam1_abc, output$ABC[[num_iter]][,1])
        lam2_abc <- c(lam2_abc, output$ABC[[num_iter]][,2])
        mu1_abc <- c(mu1_abc, output$ABC[[num_iter]][,3])
        mu2_abc <- c(mu2_abc, output$ABC[[num_iter]][,4])
        q12_abc <- c(q12_abc, output$ABC[[num_iter]][,5])
        q21_abc <- c(q21_abc, output$ABC[[num_iter]][,6])
        n_iteration <- c(n_iteration,rep(num_iter,500))
      } else {
        lam1_abc <- c(lam1_abc, output$ABC[[num_iter-1]][,1])
        lam2_abc <- c(lam2_abc, output$ABC[[num_iter-1]][,2])
        mu1_abc <- c(mu1_abc, output$ABC[[num_iter-1]][,3])
        mu2_abc <- c(mu2_abc, output$ABC[[num_iter-1]][,4])
        q12_abc <- c(q12_abc, output$ABC[[num_iter-1]][,5])
        q21_abc <- c(q21_abc, output$ABC[[num_iter-1]][,6])
        n_iteration <- c(n_iteration,rep(num_iter,500))
      }
    } else {
      lam1_abc <- c(lam1_abc, rep(NA,500))
      lam2_abc <- c(lam2_abc, rep(NA,500))
      mu1_abc <- c(mu1_abc, rep(NA,500))
      mu2_abc <- c(mu2_abc, rep(NA,500))
      q12_abc <- c(q12_abc, rep(NA,500))
      q21_abc <- c(q21_abc, rep(NA,500))
      n_iteration <- c(n_iteration,rep(NA,500))
    }
  }
  whole_df_ABC <- data.frame(param_data2,n_iteration,
                             # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                             lam1_abc,lam2_abc,mu1_abc,mu2_abc,q12_abc,q21_abc)
  save(whole_df_ABC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/whole_df_ABC_test",test,"_ss1.RData"))

  # whole_df_ABC$dlam <- (whole_df_ABC$lam2-whole_df_ABC$lam1)/(whole_df_ABC$lam2+whole_df_ABC$lam1)
  # whole_df_ABC$dlam_ABC <- (whole_df_ABC$lam2_abc-whole_df_ABC$lam1_abc)/(whole_df_ABC$lam2_abc+whole_df_ABC$lam1_abc)
  # whole_df_ABC$dmu <- (whole_df_ABC$mu2-whole_df_ABC$mu1)/(whole_df_ABC$mu2+whole_df_ABC$mu1)
  # whole_df_ABC$dmu_ABC <- (whole_df_ABC$mu2_abc-whole_df_ABC$mu1_abc)/(whole_df_ABC$mu2_abc+whole_df_ABC$mu1_abc)
  # whole_df_ABC$dq <- (whole_df_ABC$q12-whole_df_ABC$q21)/(whole_df_ABC$q12+whole_df_ABC$q21)
  # whole_df_ABC$dq_ABC <- (whole_df_ABC$q12_abc-whole_df_ABC$q21_abc)/(whole_df_ABC$q12_abc+whole_df_ABC$q21_abc)

  whole_df_ABC$net_div1 <- (whole_df_ABC$lam1-whole_df_ABC$mu1)
  whole_df_ABC$net_div2 <- (whole_df_ABC$lam2-whole_df_ABC$mu2)
  whole_df_ABC$net_div_ABC1 <- (whole_df_ABC$lam1_abc-whole_df_ABC$mu1_abc)
  whole_df_ABC$net_div_ABC2 <- (whole_df_ABC$lam2_abc-whole_df_ABC$mu2_abc)


  whole_df_ABC$ext_frac1 <- (whole_df_ABC$mu1)/(whole_df_ABC$lam1)
  whole_df_ABC$ext_frac2 <- (whole_df_ABC$mu2)/(whole_df_ABC$lam2)
  whole_df_ABC$ext_frac_ABC1 <- (whole_df_ABC$mu1_abc)/(whole_df_ABC$lam1_abc)
  whole_df_ABC$ext_frac_ABC2 <- (whole_df_ABC$mu2_abc)/(whole_df_ABC$lam2_abc)
  save(whole_df_ABC,file =
         paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/delta_whole_df_ABC_test",test,"_ss1.RData"))

}

######
# 2. formate MCMC results (only plot the estimation points with ABC results)
# skip
for(test in c(1,3,5,6)){
  param_data <- readr::read_csv2(paste0("data/secsse_ABC_test",test,".csv"))
  param_data3<-param_data[rep(seq_len(nrow(param_data)), each=500),] #5001
  folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/secsse_MCMC_test",test)
  files <- list.files(folder_path)
  lam1_mcmc <- c()
  lam2_mcmc <- c()
  mu1_mcmc <- c()
  mu2_mcmc <- c()
  q12_mcmc <- c()
  q21_mcmc <- c()
  for(i in 1:100){
    file_to_load <- grep(paste0("secsse_MCMC_test",test,"_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                         files,
                         value = TRUE,
                         fixed = TRUE)
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      lam1_mcmc <- c(lam1_mcmc, output[4502:5001,1])
      lam2_mcmc <- c(lam2_mcmc, output[4502:5001,2])
      mu1_mcmc <- c(mu1_mcmc, output[4502:5001,3])
      mu2_mcmc <- c(mu2_mcmc, output[4502:5001,4])
      q12_mcmc <- c(q12_mcmc, output[4502:5001,5])
      q21_mcmc <- c(q21_mcmc, output[4502:5001,6])
    } else {
      lam1_mcmc <- c(lam1_mcmc, rep(NA,500)) #500
      lam2_mcmc <- c(lam2_mcmc, rep(NA,500))
      mu1_mcmc <- c(mu1_mcmc, rep(NA,500))
      mu2_mcmc <- c(mu2_mcmc, rep(NA,500))
      q12_mcmc <- c(q12_mcmc, rep(NA,500))
      q21_mcmc <- c(q21_mcmc, rep(NA,500))
    }
  }
  whole_df_MCMC <- data.frame(param_data3,
                              lam1_mcmc,lam2_mcmc,
                              mu1_mcmc,mu2_mcmc,
                              q12_mcmc,q21_mcmc)

  save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/whole_df_MCMC_test",test,"_fit_ABC.RData"))

  # whole_df_MCMC$dlam <- (whole_df_MCMC$lam2-whole_df_MCMC$lam1)/(whole_df_MCMC$lam2+whole_df_MCMC$lam1)
  # whole_df_MCMC$dlam_mcmc <- (whole_df_MCMC$lam2_mcmc-whole_df_MCMC$lam1_mcmc)/(whole_df_MCMC$lam2_mcmc+whole_df_MCMC$lam1_mcmc)
  # whole_df_MCMC$dmu <- (whole_df_MCMC$mu2-whole_df_MCMC$mu1)/(whole_df_MCMC$mu2+whole_df_MCMC$mu1)
  # whole_df_MCMC$dmu_mcmc <- (whole_df_MCMC$mu2_mcmc-whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$mu2_mcmc+whole_df_MCMC$mu1_mcmc)
  # whole_df_MCMC$dq <- (whole_df_MCMC$q12-whole_df_MCMC$q21)/(whole_df_MCMC$q12+whole_df_MCMC$q21)
  # whole_df_MCMC$dq_mcmc <- (whole_df_MCMC$q12_mcmc-whole_df_MCMC$q21_mcmc)/(whole_df_MCMC$q12_mcmc+whole_df_MCMC$q21_mcmc)

  whole_df_MCMC$net_div1 <- (whole_df_MCMC$lam1-whole_df_MCMC$mu1)
  whole_df_MCMC$net_div2 <- (whole_df_MCMC$lam2-whole_df_MCMC$mu2)
  whole_df_MCMC$net_div_MCMC1 <- (whole_df_MCMC$lam1_mcmc-whole_df_MCMC$mu1_mcmc)
  whole_df_MCMC$net_div_MCMC2 <- (whole_df_MCMC$lam2_mcmc-whole_df_MCMC$mu2_mcmc)

  whole_df_MCMC$ext_frac1 <- (whole_df_MCMC$mu1)/(whole_df_MCMC$lam1)
  whole_df_MCMC$ext_frac2 <- (whole_df_MCMC$mu2)/(whole_df_MCMC$lam2)
  whole_df_MCMC$ext_frac_MCMC1 <- (whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$lam1_mcmc)
  whole_df_MCMC$ext_frac_MCMC2 <- (whole_df_MCMC$mu2_mcmc)/(whole_df_MCMC$lam2_mcmc)

  save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/delta_whole_df_MCMC_test",test,".RData"))
}

# for(test in 1:5){
#   param_data <- readr::read_csv2(paste0("data/secsse_ABC_test",test,".csv"))
#   param_data3<-param_data[rep(seq_len(nrow(param_data)), each=500),] #5001
#   folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/secsse_ABC_test",test)
#   files <- list.files(folder_path)
#   lam1_mcmc <- c()
#   lam2_mcmc <- c()
#   mu1_mcmc <- c()
#   mu2_mcmc <- c()
#   q12_mcmc <- c()
#   q21_mcmc <- c()
#   for(i in 1:100){
#     file_to_load <- grep(paste0("secsse_ABC_test",test,"_param_set_",i,"_ss_0.RData"),  #,"_rep",rep
#                          files,
#                          value = TRUE,
#                          fixed = TRUE)
#
#     if (!identical(file_to_load, character())) {
#       folder_path_mcmc <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/secsse_MCMC_test",test)
#       files_mcmc <- list.files(folder_path_mcmc)
#       file_to_load_mcmc <- grep(paste0("secsse_MCMC_test",test,"_param_set_", i,"_ss_1.RData"), #"_rep",rep,
#                                 files_mcmc,
#                                 value = TRUE,
#                                 fixed = TRUE)
#
#       if (!identical(file_to_load_mcmc, character())) {
#         load(file.path(folder_path_mcmc, file_to_load_mcmc))
#         lam1_mcmc <- c(lam1_mcmc, output[1502:5001,1])
#         lam2_mcmc <- c(lam2_mcmc, output[1502:5001,2])
#         mu1_mcmc <- c(mu1_mcmc, output[1502:5001,3])
#         mu2_mcmc <- c(mu2_mcmc, output[1502:5001,4])
#         q12_mcmc <- c(q12_mcmc, output[1502:5001,5])
#         q21_mcmc <- c(q21_mcmc, output[1502:5001,6])
#       } else {
#         lam1_mcmc <- c(lam1_mcmc, rep(NA,500)) #500
#         lam2_mcmc <- c(lam2_mcmc, rep(NA,500))
#         mu1_mcmc <- c(mu1_mcmc, rep(NA,500))
#         mu2_mcmc <- c(mu2_mcmc, rep(NA,500))
#         q12_mcmc <- c(q12_mcmc, rep(NA,500))
#         q21_mcmc <- c(q21_mcmc, rep(NA,500))
#       }
#     } else {
#       lam1_mcmc <- c(lam1_mcmc, rep(NA,500))
#       lam2_mcmc <- c(lam2_mcmc, rep(NA,500))
#       mu1_mcmc <- c(mu1_mcmc, rep(NA,500))
#       mu2_mcmc <- c(mu2_mcmc, rep(NA,500))
#       q12_mcmc <- c(q12_mcmc, rep(NA,500))
#       q21_mcmc <- c(q21_mcmc, rep(NA,500))
#     }
#   }
#
#   whole_df_MCMC <- data.frame(param_data3,
#                               lam1_mcmc,lam2_mcmc,
#                               mu1_mcmc,mu2_mcmc,
#                               q12_mcmc,q21_mcmc)
#
#   save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/whole_df_MCMC_test",test,"_fit_ABC.RData"))
#
#   # whole_df_MCMC$dlam <- (whole_df_MCMC$lam2-whole_df_MCMC$lam1)/(whole_df_MCMC$lam2+whole_df_MCMC$lam1)
#   # whole_df_MCMC$dlam_mcmc <- (whole_df_MCMC$lam2_mcmc-whole_df_MCMC$lam1_mcmc)/(whole_df_MCMC$lam2_mcmc+whole_df_MCMC$lam1_mcmc)
#   # whole_df_MCMC$dmu <- (whole_df_MCMC$mu2-whole_df_MCMC$mu1)/(whole_df_MCMC$mu2+whole_df_MCMC$mu1)
#   # whole_df_MCMC$dmu_mcmc <- (whole_df_MCMC$mu2_mcmc-whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$mu2_mcmc+whole_df_MCMC$mu1_mcmc)
#   # whole_df_MCMC$dq <- (whole_df_MCMC$q12-whole_df_MCMC$q21)/(whole_df_MCMC$q12+whole_df_MCMC$q21)
#   # whole_df_MCMC$dq_mcmc <- (whole_df_MCMC$q12_mcmc-whole_df_MCMC$q21_mcmc)/(whole_df_MCMC$q12_mcmc+whole_df_MCMC$q21_mcmc)
#
#   whole_df_MCMC$net_div1 <- (whole_df_MCMC$lam1-whole_df_MCMC$mu1)
#   whole_df_MCMC$net_div2 <- (whole_df_MCMC$lam2-whole_df_MCMC$mu2)
#   whole_df_MCMC$net_div_MCMC1 <- (whole_df_MCMC$lam1_mcmc-whole_df_MCMC$mu1_mcmc)
#   whole_df_MCMC$net_div_MCMC2 <- (whole_df_MCMC$lam2_mcmc-whole_df_MCMC$mu2_mcmc)
#
#   whole_df_MCMC$ext_frac1 <- (whole_df_MCMC$mu1)/(whole_df_MCMC$lam1)
#   whole_df_MCMC$ext_frac2 <- (whole_df_MCMC$mu2)/(whole_df_MCMC$lam2)
#   whole_df_MCMC$ext_frac_MCMC1 <- (whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$lam1_mcmc)
#   whole_df_MCMC$ext_frac_MCMC2 <- (whole_df_MCMC$mu2_mcmc)/(whole_df_MCMC$lam2_mcmc)
#
#   save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/delta_whole_df_MCMC_test",test,"_fit_ABC.RData"))
# }




######


# 3. formate MLE results
# skip
for(test in 1:6){
  param_data <- readr::read_csv2(paste0("data/secsse_ABC_test",test,".csv"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/cpp_obs_ss_test",test,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/MLE/test",test,"_MLE_secsse.RData"))
  whole_df_MLE <- data.frame(param_data,MLE_all,ss[,1:4])
  save(whole_df_MLE,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/whole_df_MLE",test,".RData"))
}


# for(test in 1:5){
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/whole_df_MLE",test,".RData"))
#   whole_df_MLE_fit_ABC <- whole_df_MLE
#   folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/secsse_ABC_test",test)
#   files <- list.files(folder_path)
#   for(i in 1:100){
#     # param_set = (param_num-1)*5 + i
#     file_to_load <- grep(paste0("secsse_ABC_test",test,"_param_set_", i,"_ss_0.RData"), #"_rep",rep,
#                          files,
#                          value = TRUE,
#                          fixed = TRUE)
#
#     if (identical(file_to_load, character())) {
#       whole_df_MLE_fit_ABC[i,] <- NA
#     } else {
#       whole_df_MLE_fit_ABC$net_div1 <- (whole_df_MLE_fit_ABC$lam1-whole_df_MLE_fit_ABC$mu1)
#       whole_df_MLE_fit_ABC$net_div2 <- (whole_df_MLE_fit_ABC$lam2-whole_df_MLE_fit_ABC$mu2)
#       whole_df_MLE_fit_ABC$net_div_MLE1 <- (whole_df_MLE_fit_ABC$lam1_MLE-whole_df_MLE_fit_ABC$mu1_MLE)
#       whole_df_MLE_fit_ABC$net_div_MLE2 <- (whole_df_MLE_fit_ABC$lam2_MLE-whole_df_MLE_fit_ABC$mu2_MLE)
#
#       whole_df_MLE_fit_ABC$ext_frac1 <- (whole_df_MLE_fit_ABC$mu1)/(whole_df_MLE_fit_ABC$lam1)
#       whole_df_MLE_fit_ABC$ext_frac2 <- (whole_df_MLE_fit_ABC$mu2)/(whole_df_MLE_fit_ABC$lam2)
#       whole_df_MLE_fit_ABC$ext_frac_MLE1 <- (whole_df_MLE_fit_ABC$mu1_MLE)/(whole_df_MLE_fit_ABC$lam1_MLE)
#       whole_df_MLE_fit_ABC$ext_frac_MLE2 <- (whole_df_MLE_fit_ABC$mu2_MLE)/(whole_df_MLE_fit_ABC$lam2_MLE)
#     }
#   }
#   save(whole_df_MLE_fit_ABC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/whole_df_MLE_test",test,"_fit_ABC.RData"))
# }

######
## combine ABC, MCMC, MLE for each parameter set(use median value)
# for(test in 1:5){
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/delta_whole_df_ABC_test",test,".RData"))
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/delta_whole_df_MCMC_test",test,"_fit_ABC.RData"))
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/whole_df_MLE_test",test,"_fit_ABC.RData"))
#
#   ## get number of iterations and mean values
#   df <- whole_df_ABC
#   n <- 500
#   ABC_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
#
#   df<-whole_df_MCMC
#   n <- 500
#   MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
#
#   MLE_median <- whole_df_MLE_fit_ABC
#
#
#   ## combine ABC MCMC MLE as "AMM"
#   AMM_all_df <- cbind(ABC_median[1:21],
#                       MCMC_median[,c(7:12,15,16,19,20)],
#                       MLE_median[,c(8:13,32,33,36,37)])
#   save(AMM_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/AMM_per_set_test",test,".RData"))
#
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/AMM_per_set_test",test,".RData"))
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/cpp_obs_ss_test",test,".RData"))
#   AMM_all_df$dlam1_abc <- AMM_all_df$lam1_abc - AMM_all_df$lam1
#   AMM_all_df$dlam2_abc <- AMM_all_df$lam2_abc - AMM_all_df$lam2
#   AMM_all_df$dmu1_abc <- AMM_all_df$mu1_abc - AMM_all_df$mu1
#   AMM_all_df$dmu2_abc <- AMM_all_df$mu2_abc - AMM_all_df$mu2
#   AMM_all_df$dq12_abc <- AMM_all_df$q12_abc - AMM_all_df$q12
#   AMM_all_df$dq21_abc <- AMM_all_df$q21_abc - AMM_all_df$q21
#
#   AMM_all_df$dlam1_mcmc <- AMM_all_df$lam1_mcmc - AMM_all_df$lam1
#   AMM_all_df$dlam2_mcmc <- AMM_all_df$lam2_mcmc - AMM_all_df$lam2
#   AMM_all_df$dmu1_mcmc <- AMM_all_df$mu1_mcmc - AMM_all_df$mu1
#   AMM_all_df$dmu2_mcmc <- AMM_all_df$mu2_mcmc - AMM_all_df$mu2
#   AMM_all_df$dq12_mcmc <- AMM_all_df$q12_mcmc - AMM_all_df$q12
#   AMM_all_df$dq21_mcmc <- AMM_all_df$q21_mcmc - AMM_all_df$q21
#
#   AMM_all_df$dlam1_MLE <- AMM_all_df$lam1_MLE - AMM_all_df$lam1
#   AMM_all_df$dlam2_MLE <- AMM_all_df$lam2_MLE - AMM_all_df$lam2
#   AMM_all_df$dmu1_MLE <- AMM_all_df$mu1_MLE - AMM_all_df$mu1
#   AMM_all_df$dmu2_MLE <- AMM_all_df$mu2_MLE - AMM_all_df$mu2
#   AMM_all_df$dq12_MLE <- AMM_all_df$q12_MLE - AMM_all_df$q12
#   AMM_all_df$dq21_MLE <- AMM_all_df$q21_MLE - AMM_all_df$q21
#
#   AMM_all_df$tree_size <- pars_ss$tree_size
#   AMM_all_df$tip_ratio1 <- pars_ss$state1/pars_ss$state2
#   AMM_all_df$tip_ratio <- AMM_all_df$tip_ratio1
#   AMM_all_df$tip_ratio[AMM_all_df$tip_ratio < 1]<- 1/AMM_all_df$tip_ratio[AMM_all_df$tip_ratio < 1]
#   save(AMM_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/AMM_per_set_drate_test",test,"_fit_ABC.RData"))
# }

for(test in c(1,3,5,6)){
  # load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/delta_whole_df_ABC_test",test,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/delta_whole_df_ABC_test",test,"_ss1.RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/delta_whole_df_MCMC_test",test,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/whole_df_MLE",test,".RData"))

  ## get number of iterations and mean values
  df <- whole_df_ABC
  n <- 500
  ABC_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

  df<-whole_df_MCMC
  n <- 500
  MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

  MLE_median <- whole_df_MLE


  ## combine ABC MCMC MLE as "AMM"
  AMM_all_df <- cbind(ABC_median[1:21],
                      MCMC_median[,c(7:12,15,16,19,20)],
                      MLE_median[,c(7:12,20:23)])
  save(AMM_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/AMM_per_set_test",test,"_ss1.RData"))

  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/AMM_per_set_test",test,"_ss1.RData"))
  AMM_all_df$dlam1_abc <- AMM_all_df$lam1_abc - AMM_all_df$lam1
  AMM_all_df$dlam2_abc <- AMM_all_df$lam2_abc - AMM_all_df$lam2
  AMM_all_df$dmu1_abc <- AMM_all_df$mu1_abc - AMM_all_df$mu1
  AMM_all_df$dmu2_abc <- AMM_all_df$mu2_abc - AMM_all_df$mu2
  AMM_all_df$dq12_abc <- AMM_all_df$q12_abc - AMM_all_df$q12
  AMM_all_df$dq21_abc <- AMM_all_df$q21_abc - AMM_all_df$q21

  AMM_all_df$dlam1_mcmc <- AMM_all_df$lam1_mcmc - AMM_all_df$lam1
  AMM_all_df$dlam2_mcmc <- AMM_all_df$lam2_mcmc - AMM_all_df$lam2
  AMM_all_df$dmu1_mcmc <- AMM_all_df$mu1_mcmc - AMM_all_df$mu1
  AMM_all_df$dmu2_mcmc <- AMM_all_df$mu2_mcmc - AMM_all_df$mu2
  AMM_all_df$dq12_mcmc <- AMM_all_df$q12_mcmc - AMM_all_df$q12
  AMM_all_df$dq21_mcmc <- AMM_all_df$q21_mcmc - AMM_all_df$q21

  AMM_all_df$dlam1_MLE <- AMM_all_df$lam1_MLE - AMM_all_df$lam1
  AMM_all_df$dlam2_MLE <- AMM_all_df$lam2_MLE - AMM_all_df$lam2
  AMM_all_df$dmu1_MLE <- AMM_all_df$mu1_MLE - AMM_all_df$mu1
  AMM_all_df$dmu2_MLE <- AMM_all_df$mu2_MLE - AMM_all_df$mu2
  AMM_all_df$dq12_MLE <- AMM_all_df$q12_MLE - AMM_all_df$q12
  AMM_all_df$dq21_MLE <- AMM_all_df$q21_MLE - AMM_all_df$q21

  AMM_all_df$net_div_MLE1 <- AMM_all_df$lam1_MLE-AMM_all_df$mu1_MLE
  AMM_all_df$net_div_MLE2 <- AMM_all_df$lam2_MLE-AMM_all_df$mu2_MLE

  save(AMM_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/AMM_per_set_drate_test",test,"_ss1.RData"))
}

#####
## 4. plot observed treesize /tip ratio vs estimation error
## skip
for(test in c(1,3,5,6)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/AMM_per_set_drate_test",test,"_ss1.RData"))
  color_values <-c("ABC" = "red3","MCMC" = "green2", "MLE" = "yellow2")
  p_lam1 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dlam1_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dlam1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dlam1_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~lambda[1]))+
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_lam2 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dlam2_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dlam2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dlam2_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~lambda[2]))+
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_mu1 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dmu1_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dmu1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dmu1_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~mu[1]))+
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_mu2 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dmu2_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dmu2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dmu2_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~mu[2]))+
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_q12 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dq12_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dq12_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dq12_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~q[12]))+
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_q21 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dq21_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dq21_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dq21_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~q[21]))+
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/rate_error_test",test,".tiff"),
       units="px", width=3000, height=2000,res = 400,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
    align = "hv", nrow = 2, ncol = 3
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()
}

# run drate plots with all the particles(show distribution for each parameter)




#####
## run
library(ggplot2)
for(test in c(1,3,5,6)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/AMM_per_set_drate_test",test,"_ss1.RData"))
  color_values <-c("ABC" = "red3","MCMC" = "green2", "MLE" = "yellow2")
  p_lam1 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(lambda[1]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$lam1[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_lam2 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(lambda[2]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$lam2[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_mu1 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(mu[1]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$mu1[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_mu2 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(mu[2]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$mu2[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_q12 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(q[12]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$q12[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_q21 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(q[21]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$q21[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/exact_rate_test",test,"_ss1.tiff"),
       units="px", width=3000, height=1800,res = 400,compression="lzw")
  params <- cowplot::plot_grid(
    p_lam1+ggplot2::theme(legend.position = "none"),
    p_mu1+ggplot2::theme(legend.position = "none"),
    p_q12+ggplot2::theme(legend.position = "none"),
    p_lam2+ggplot2::theme(legend.position = "none"),
    p_mu2+ggplot2::theme(legend.position = "none"),
    p_q21+ggplot2::theme(legend.position = "none"),
    align = "hv", nrow = 2, ncol = 3
  )
  legend <- cowplot::get_legend(
    p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
  )
  param_estimates <- cowplot::plot_grid(params,legend,
                                        rel_widths = c(3,0.4)
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()
}

## plot tree size VS net diversification rates
for(test in c(1,3,5,6)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/AMM_per_set_drate_test",test,"_ss1.RData"))
  color_values <-c("ABC" = "red3","MCMC" = "green2", "MLE" = "yellow2")
  p_div1 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.05,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MLE1),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MCMC1),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_ABC1),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression("Net Diversification State 1")) +
    ggplot2::xlab("Tree size")+
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))+
    ggplot2::geom_hline(yintercept = AMM_all_df$net_div1[1], linetype = "dashed", size = 0.5)


  p_div2 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.05,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MLE2),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MCMC2),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_ABC2),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression("Net Diversification State 2")) +
    ggplot2::xlab("Tree size")+
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))+
    ggplot2::geom_hline(yintercept = AMM_all_df$net_div2[1], linetype = "dashed", size = 0.5)

  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/net_div_set_",test,"_ss1.tiff"),
       units="px", width=2200, height=1000,res = 400,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_div1,p_div2,
    align = "hv", nrow = 1, ncol = 2
  )
  print(param_estimates)

  params <- cowplot::plot_grid(
    p_div1+ggplot2::theme(legend.position = "none"),
    p_div2+ggplot2::theme(legend.position = "none"),
    align = "hv", nrow = 1, ncol = 2
  )
  legend <- cowplot::get_legend(
    p_div1 + theme(legend.box.margin = margin(0, 0, 0, 6))
  )
  param_estimates <- cowplot::plot_grid(params,legend,
                                        rel_widths = c(3,0.5)
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()
}


## histogram
# 5. plot AMM distribution
# AMM histogram
library(ggplot2)
for(test in c(1,3,5,6)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/delta_whole_df_ABC_test",test,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/delta_whole_df_MCMC_test",test,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/whole_df_MLE",test,".RData"))


  ## get legend first
  param_abc <- whole_df_ABC[1:10,]
  param_mcmc <- whole_df_MCMC[1:10,]
  param_mle <- whole_df_MLE[1:10,]
  p_legend <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = lam1_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = lam1_abc,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_density(data = param_mle,
                          ggplot2::aes(x = lam1_MLE,fill = "MLE"),colour = "green4",
                          alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda[1]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = c("ABC" = "#4D85BD","MCMC" = "#F7903D",  "MLE" = "#59A95A"),
                               labels = c("ABC", "MCMC","MLE"))+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 10)) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 10)) +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lam1), linetype = "dashed", size = 0.5)


  legend_all <- cowplot::get_legend(
    p_legend + theme(legend.box.margin = margin(0, 0, 0, 6))
  )
  color_values <-c("ABC" = "#4D85BD", "MCMC" = "#F7903D", "MLE" = "#59A95A")


  for(i in 1:100){
    param_abc <- whole_df_ABC[((i*500-499)):(i*500),]
    param_mcmc <- whole_df_MCMC[((i*500-499)):(i*500),]
    param_mle <- whole_df_MLE[i,]

    if(!is.na(param_abc[1,7])){
      p_lam1 <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(-0.032,1.0)+ #1
        ggplot2::geom_histogram(data = param_mcmc,
                                ggplot2::aes(x = lam1_mcmc,fill = "MCMC"),
                                alpha = 0.7,bins = 150) + #0.03
        ggplot2::geom_histogram(ggplot2::aes(x = lam1_abc,
                                             fill = "ABC"),
                                alpha = 0.7,bins = 150) +
        ggplot2::geom_vline(data= param_mle,
                            aes(xintercept = lam1_MLE),color = "#59A95A",
                            linetype = "solid", size = 0.6)+
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Frequency") +
        ggplot2::xlab(expression(lambda[1]))+
        ggplot2::scale_fill_manual(name = "Method",
                                   values = color_values,
                                   labels = c("ABC","MCMC", "MLE"))+
        ggplot2::theme(legend.position = "none") +
        ggplot2::geom_vline(data= param_abc, aes(xintercept = lam1), linetype = "dashed", size = 0.5)
      # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
      #                     linetype = "dashed", size = 0.5,color = "red")

      p_lam2 <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(-0.032,1.0)+
        ggplot2::geom_histogram(data = param_mcmc,
                                ggplot2::aes(x = lam2_mcmc,fill = "MCMC"),
                                alpha = 0.7,bins = 150) +
        ggplot2::geom_histogram(ggplot2::aes(x = lam2_abc,
                                             fill = "ABC"),
                                alpha = 0.7,bins = 150) +
        ggplot2::geom_vline(data= param_mle,
                            aes(xintercept = lam2_MLE),color = "#59A95A",
                            linetype = "solid", size = 0.6)+
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Frequency") +
        ggplot2::xlab(expression(lambda[2]))+
        ggplot2::scale_fill_manual(name = "Method",
                                   values = color_values,
                                   labels = c("ABC","MCMC", "MLE"))+
        ggplot2::theme(legend.position = "none") +
        ggplot2::geom_vline(data= param_abc, aes(xintercept = lam2), linetype = "dashed", size = 0.5)
      # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
      #                     linetype = "dashed", size = 0.5,color = "red")

      p_mu1 <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        # ylim(0,300)+
        xlim(-0.008,0.4)+ #0.2
        ggplot2::geom_histogram(data = param_mcmc,
                                ggplot2::aes(x = mu1_mcmc,fill = "MCMC"),
                                alpha = 0.7,bins = 150) +
        ggplot2::geom_histogram(ggplot2::aes(x = mu1_abc,
                                             fill = "ABC"),
                                alpha = 0.7,bins = 150) +
        ggplot2::geom_vline(data= param_mle,
                            aes(xintercept = mu1_MLE),color = "#59A95A",
                            linetype = "solid", size = 0.6)+
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Frequency") +
        ggplot2::xlab(expression(mu[1]))+
        ggplot2::scale_fill_manual(name = "Method",
                                   values = color_values,
                                   labels = c("ABC","MCMC", "MLE"))+
        ggplot2::theme(legend.position = "none") +
        ggplot2::geom_vline(data= param_abc, aes(xintercept = mu1), linetype = "dashed", size = 0.5)

      p_mu2 <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(-0.008,0.4)+
        # ylim(0,200)+
        ggplot2::geom_histogram(data = param_mcmc,
                                ggplot2::aes(x = mu2_mcmc,fill = "MCMC"),
                                alpha = 0.7,bins = 150) +
        ggplot2::geom_histogram(ggplot2::aes(x = mu2_abc,
                                             fill = "ABC"),
                                alpha = 0.7,bins = 150) +
        ggplot2::geom_vline(data= param_mle,
                            aes(xintercept = mu2_MLE),color = "#59A95A",
                            linetype = "solid", size = 0.6)+
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Frequency") +
        ggplot2::xlab(expression(mu[2]))+
        ggplot2::scale_fill_manual(name = "Method",
                                   values = color_values,
                                   labels = c("ABC","MCMC", "MLE"))+
        ggplot2::theme(legend.position = "none") +
        ggplot2::geom_vline(data= param_abc, aes(xintercept = mu2), linetype = "dashed", size = 0.5)

      p_q12 <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(-0.02,0.5)+ #1
        ggplot2::geom_histogram(data = param_mcmc,
                                ggplot2::aes(x = q12_mcmc,fill = "MCMC"),
                                alpha = 0.7,bins = 150) +
        ggplot2::geom_histogram(ggplot2::aes(x = q12_abc,
                                             fill = "ABC"),
                                alpha = 0.7,bins = 150) +
        ggplot2::geom_vline(data= param_mle,
                            aes(xintercept = q12_MLE),color = "#59A95A",
                            linetype = "solid", size = 0.6)+
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Frequency") +
        ggplot2::xlab(expression(q[12]))+
        ggplot2::scale_fill_manual(name = "Method",
                                   values = color_values,
                                   labels = c("ABC","MCMC", "MLE"))+
        ggplot2::theme(legend.position = "none") +
        ggplot2::geom_vline(data= param_abc, aes(xintercept = q12), linetype = "dashed", size = 0.5)


      p_q21 <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(-0.02,0.5)+
        ggplot2::geom_histogram(data = param_mcmc,
                                ggplot2::aes(x = q21_mcmc,fill = "MCMC"),
                                alpha = 0.7,bins = 150) +
        ggplot2::geom_histogram(ggplot2::aes(x = q21_abc,
                                             fill = "ABC"),
                                alpha = 0.7,bins = 150) +
        ggplot2::geom_vline(data= param_mle,
                            aes(xintercept = q21_MLE),color = "#59A95A",
                            linetype = "solid", size = 0.6)+
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Frequency") +
        ggplot2::xlab(expression(q[21]))+
        ggplot2::scale_fill_manual(name = "Method",
                                   values = color_values,
                                   labels = c("ABC","MCMC", "MLE"))+
        ggplot2::theme(legend.position = "none") +
        ggplot2::geom_vline(data= param_abc, aes(xintercept = q21), linetype = "dashed", size = 0.5)


      p_emp <- ggplot() + theme_void()

      tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/cowplot_AMM_3gene/test",test,"AMM_rep_",i,".tiff"),
           units="px", width=3000, height=1800,res = 400,compression="lzw")
      param_estimates <- cowplot::plot_grid(
        p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
        align = "hv", nrow = 2, ncol = 3
      )
      param_est_final <- cowplot::plot_grid(param_estimates,legend_all,rel_widths = c(3, 0.5))
      print(param_est_final)
      while (!is.null(dev.list()))  dev.off()
    }
  }
}


#####
# 6. compare net-diversification
library(ggplot2)
for(test in 1:5){
  # load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse/secsse_cpp_ABC_new_space/delta_whole_df_ABC_ss_set0.RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/delta_whole_df_ABC_test",test,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/delta_whole_df_MCMC_test",test,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_ABC_new/whole_df_MLE_test",test,".RData"))


  ## get legend first
  param_abc <- whole_df_ABC[1:10,]
  param_mcmc <- whole_df_MCMC[1:10,]
  param_mle <- whole_df_MLE_fit_ABC[1:10,]
  p_legend <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = lam1_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = lam1_abc,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_density(data = param_mle,
                          ggplot2::aes(x = lam1_MLE,fill = "MLE"),colour = "green4",
                          alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda[1]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = c( "MCMC" = "#F7903D", "ABC" = "#4D85BD", "MLE" = "#59A95A"),
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 10)) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 10)) +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lam1), linetype = "dashed", size = 0.5)


  legend_all <- cowplot::get_legend(
    p_legend + theme(legend.box.margin = margin(0, 0, 0, 6))
  )
  color_values <-c("MCMC" = "#F7903D", "ABC" = "#4D85BD", "MLE" = "#59A95A")

  #####
  # histogram
  for(i in 1:100){
    param_abc <- whole_df_ABC[((i*500-499)):(i*500),]
    param_mcmc <- whole_df_MCMC[((i*500-299)):(i*500),]
    param_mle <- whole_df_MLE_fit_ABC[i,]
    if(!is.na(param_abc[,7])){
      p_net_div1 <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(-0.032,1.0)+
        ggplot2::geom_histogram(data = param_mcmc,
                                ggplot2::aes(x = net_div_MCMC1,fill = "MCMC"),
                                alpha = 0.9,bins = 50) +
        ggplot2::geom_histogram(ggplot2::aes(x = net_div_ABC1,fill = "ABC"),
                                alpha = 0.9,bins = 50) +
        ggplot2::geom_vline(data= param_mle,
                            aes(xintercept = net_div_MLE1),colour = "green4",
                            linetype = "solid", size = 0.6)+
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Frequency") +
        ggplot2::xlab("Net diversification state 1")+
        ggplot2::scale_fill_manual(name = "Method",
                                   values = color_values,
                                   labels = c("MCMC", "ABC", "MLE"))+
        ggplot2::theme(legend.position = "none") +
        ggplot2::geom_vline(data= param_abc, aes(xintercept = net_div1), linetype = "dashed", size = 0.5)
      # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
      #                     linetype = "dashed", size = 0.5,color = "red")

      p_net_div2 <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(-0.032,1.0)+
        ggplot2::geom_histogram(data = param_mcmc,
                                ggplot2::aes(x = net_div_MCMC2,fill = "MCMC"),
                                alpha = 0.9,bins = 50) +
        ggplot2::geom_histogram(ggplot2::aes(x = net_div_ABC2,fill = "ABC"),
                                alpha = 0.9,bins = 50) +
        ggplot2::geom_vline(data= param_mle,
                            aes(xintercept = net_div_MLE2),colour = "green4",
                            linetype = "solid", size = 0.6)+
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Frequency") +
        ggplot2::xlab("Net diversification state 2")+
        ggplot2::scale_fill_manual(name = "Method",
                                   values = color_values,
                                   labels = c("MCMC", "ABC", "MLE"))+
        ggplot2::theme(legend.position = "none") +
        ggplot2::geom_vline(data= param_abc, aes(xintercept = net_div2), linetype = "dashed", size = 0.5)


      # p_ext_frac1 <-ggplot2::ggplot(data = param_abc) +
      #   ggplot2::theme_bw() +
      #   # xlim(0,0.8)+
      #   ggplot2::geom_histogram(data = param_mcmc,
      #                           ggplot2::aes(x = ext_frac_MCMC1,fill = "MCMC"),
      #                           alpha = 0.9,bins = 50) +
      #   ggplot2::geom_histogram(ggplot2::aes(x = ext_frac_ABC1,fill = "ABC"),
      #                           alpha = 0.9,bins = 50) +
      #   ggplot2::geom_vline(data= param_mle,
      #                       aes(xintercept = ext_frac_MLE1),colour = "green4",
      #                       linetype = "solid", size = 1)+
      #   ggplot2::theme_classic() +
      #   ggplot2::theme(title = ggplot2::element_text(size = 12),
      #                  text = ggplot2::element_text(size = 12)) +
      #   ggplot2::ylab("Frequency") +
      #   ggplot2::xlab("Extinction fraction state 1")+
      #   ggplot2::scale_fill_manual(name = "Method",
      #                              values = color_values,
      #                              labels = c("MCMC", "ABC", "MLE"))+
      #   ggplot2::theme(legend.position = "none") +
      #   ggplot2::geom_vline(data= param_abc, aes(xintercept = ext_frac1), linetype = "dashed", size = 0.5)
      #
      #
      # p_ext_frac2 <-ggplot2::ggplot(data = param_abc) +
      #   ggplot2::theme_bw() +
      #   # xlim(0,0.8)+
      #   ggplot2::geom_histogram(data = param_mcmc,
      #                           ggplot2::aes(x = ext_frac_MCMC2,fill = "MCMC"),
      #                           alpha = 0.9,bins = 50) +
      #   ggplot2::geom_histogram(ggplot2::aes(x = ext_frac_ABC2,fill = "ABC"),
      #                           alpha = 0.9,bins = 50) +
      #   ggplot2::geom_vline(data= param_mle,
      #                       aes(xintercept = ext_frac_MLE2),colour = "green4",
      #                       linetype = "solid", size = 1)+
      #   ggplot2::theme_classic() +
      #   ggplot2::theme(title = ggplot2::element_text(size = 12),
      #                  text = ggplot2::element_text(size = 12)) +
      #   ggplot2::ylab("Frequency") +
      #   ggplot2::xlab("Extinction fraction state 1")+
      #   ggplot2::scale_fill_manual(name = "Method",
      #                              values = color_values,
      #                              labels = c("MCMC", "ABC", "MLE"))+
      #   ggplot2::theme(legend.position = "none") +
      #   ggplot2::geom_vline(data= param_abc, aes(xintercept = ext_frac2), linetype = "dashed", size = 0.5)


      p_emp <- ggplot() + theme_void()

      tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/net_div/test",test,"/net_div_hist_set_",i,".tiff"),
           units="px", width=2200, height=1000,res = 400,compression="lzw")
      param_estimates <- cowplot::plot_grid(
        p_net_div1,p_net_div2,
        # p_ext_frac1,p_ext_frac2,
        align = "hv", nrow = 1, ncol = 2
      )
      param_est_final <- cowplot::plot_grid(param_estimates,legend_all,rel_widths = c(3, .4))
      print(param_est_final)
      while (!is.null(dev.list()))  dev.off()
    }
  }
}


#####
# 7. plot the epsilon through generation
library(ggplot2)
for(test in 1:5){
  folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/secsse_ABC_test",test)
  files <- list.files(folder_path)
  for(set in 1:100){
    message("set", set)
    file_to_load <- grep(paste0("secsse_ABC_test",test,"_param_set_", set,"_ss_0.RData"),
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))

      ss_dist<-c()
      n_gene <- length(output$ss_diff_list)
      if(nrow(output$ss_diff_list[[n_gene]]) < 500){
        n_gene <- n_gene - 1
      }
      for(i in 1:n_gene){
        ss_dist <- rbind(ss_dist,output$ss_diff_list[[i]])
      }

      # colnames(ss_dist) <- c("MPD","MNTD","SDPD","SDNTD",
      #                        "D","Total","Ratio","NLTT")
      colnames(ss_dist) <- c("MPD_12","MPD_S1","MPD_S2",
                             "MNTD_12","MNTD_S1","MNTD_S2",
                             "D","Num_S1","Num_S2","NLTT")
      rownames(ss_dist) <- 1:nrow(ss_dist)
      ss_dist <- as.data.frame(ss_dist)
      ss_dist$generation <- as.factor(rep(1:n_gene, each = 500))

      g1 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = MPD_12)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()
      # print(g1)
      g2 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = MPD_S1)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()
      g3 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = MPD_S2)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()

      g4 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = MNTD_12)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()
      g5 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = MNTD_S1)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()
      g6 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = MNTD_S2)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()
      # print(g2)

      # g7 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = SDPD)) +
      #   ggplot2::theme_bw() +
      #   ggplot2::geom_boxplot()
      # # print(g3)
      #
      # g8 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = SDNTD)) +
      #   ggplot2::theme_bw() +
      #   ggplot2::geom_boxplot()
      # print(g4)

      g7 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = D)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()
      # print(g5)

      g8 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Num_S1)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()

      g9 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Num_S2)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()

      # g7 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Ratio)) +
      #   ggplot2::theme_bw() +
      #   ggplot2::geom_boxplot()
      # print(g7)

      g10 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = NLTT)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()
      # print(g8)
      tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/dss/test",test,"/param_set",set,".tiff"),
           units="px", width=5000, height=2000,res = 400,compression="lzw")
      dss <- cowplot::plot_grid(
        g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,
        align = "hv", nrow = 2, ncol = 5
      )
      print(dss)
      while (!is.null(dev.list()))  dev.off()
    }
  }
}


#####
# 8. plot rate estimations through generation

library(ggplot2)
for(test in 1:5){
  folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/secsse_ABC_test",test)
  files <- list.files(folder_path)
  param_data <- readr::read_csv2(paste0("G:/R/Traisie-ABC/data/secsse_ABC_test",test,".csv"))
  for(set in 1:100){
    message("set", set)
    true_rates <- param_data[set,]
    file_to_load <- grep(paste0("secsse_ABC_test",test,"_param_set_", set,"_ss_0.RData"),
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      ABC_df<-c()
      n_gene <- length(output$ABC)
      if(nrow(output$ABC[[n_gene]]) < 500){
        n_gene <- n_gene - 1
      }
      for(i in 1:n_gene){
        ABC_df <- rbind(ABC_df,output$ABC[[i]])
      }

      # colnames(ss_dist) <- c("MPD","MNTD","SDPD","SDNTD",
      #                        "D","Total","Ratio","NLTT")
      colnames(ABC_df) <- c("lam1","lam2","mu1","mu2","q12","q21")
      rownames(ABC_df) <- 1:nrow(ABC_df)
      ABC_df <- as.data.frame(ABC_df)
      ABC_df$generation <- as.factor(rep(1:n_gene, each = 500))

      g1 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = lam1)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()+
        ggplot2::geom_hline(data= true_rates, aes(yintercept = lam1), linetype = "dashed", size = 0.5)
      # print(g1)
      g2 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = lam2)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()+
        ggplot2::geom_hline(data= true_rates, aes(yintercept = lam2), linetype = "dashed", size = 0.5)

      g3 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = mu1)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()+
        ggplot2::geom_hline(data= true_rates, aes(yintercept = mu1), linetype = "dashed", size = 0.5)


      g4 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = mu2)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()+
        ggplot2::geom_hline(data= true_rates, aes(yintercept = mu2), linetype = "dashed", size = 0.5)

      g5 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = q12)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()+
        ggplot2::geom_hline(data= true_rates, aes(yintercept = q12), linetype = "dashed", size = 0.5)

      g6 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = q21)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()+
        ggplot2::geom_hline(data= true_rates, aes(yintercept = q21), linetype = "dashed", size = 0.5)


      tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/ABC_rep1/rate_each_gene/test",test,"/param_",set,".tiff"),
           units="px", width=3000, height=2000,res = 400,compression="lzw")
      dss <- cowplot::plot_grid(
        g1,g3,g5,g2,g4,g6,
        align = "hv", nrow = 2, ncol = 3
      )
      print(dss)
      while (!is.null(dev.list()))  dev.off()
    }
  }

}


##
for(test in 1:5){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/cpp_obs_ss_test",test,".RData"))
  write.csv(pars_ss,paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new/cpp_obs_ss_test",test,".csv"))
}

## plot treesize vs rates include 27~75 percentile
## secsse
load("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3/ABC_rep1/delta_whole_df_ABC_test1.RData")
load("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3/cpp_obs_ss_test1.RData")
whole_df_ABC$tips <- rep(pars_ss$tree_size,each = 500)
iqr = function(z, lower = 0.05, upper = 0.95) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}
p_lac1 <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = tips, y = net_div_ABC1) ) +
  ggplot2::theme_bw() +
  ggplot2::ylim(0,1)+
  ggplot2::stat_summary(fun.data = iqr) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::ylab(expression(paste(lambda[1]),)) +
  ggplot2::xlab("Tree size")+
  ggplot2::geom_hline(data= whole_df_ABC, aes(yintercept = net_div1), linetype = "dashed", size = 0.5)


