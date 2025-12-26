#1. Combining ABC results
for (num_ss in c(0,1,2)){
  load(paste0("Data/GeoSSE/obs_ss_geosse.rda"))
  ## ABC results
  folder_path <- paste0("Data/GeoSSE/ABC")
  files <- list.files(folder_path)
  param_data <- load_param_space(param_space_name = paste0("geosse_ABC_test"))
  colnames(param_data) <- c("lam1","lam2","lam3","mu1","mu2","q12","q21")
  param_data2<-param_data[rep(seq_len(nrow(param_data)), each=300),]
  lam1_abc <- c()
  lam2_abc <- c()
  lam3_abc <- c()
  mu1_abc <- c()
  mu2_abc <- c()
  q12_abc <- c()
  q21_abc <- c()
  n_iter <- c()
  n_iteration <- c()
  for(i in 1:200){
    file_to_load <- grep(paste0("geosse_ABC_test_param_set_",i,"_ss_",num_ss,".RData"),
                         files,
                         value = TRUE,
                         fixed = TRUE)
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      message("loading set: ", i)
      num_iter <- output$n_iter
      if(output$n_iter <= 2){
        lam1_abc <- c(lam1_abc, rep(NA,300))
        lam2_abc <- c(lam2_abc, rep(NA,300))
        lam3_abc <- c(lam3_abc, rep(NA,300))
        mu1_abc <- c(mu1_abc, rep(NA,300))
        mu2_abc <- c(mu2_abc, rep(NA,300))
        q12_abc <- c(q12_abc, rep(NA,300))
        q21_abc <- c(q21_abc, rep(NA,300))
        n_iteration <- c(n_iteration,rep(NA,300))
      } else if (nrow(output$ABC[[output$n_iter]]) == 300) {
        lam1_abc <- c(lam1_abc, output$ABC[[num_iter]][,1])
        lam2_abc <- c(lam2_abc, output$ABC[[num_iter]][,2])
        lam3_abc <- c(lam3_abc, output$ABC[[num_iter]][,3])
        mu1_abc <- c(mu1_abc, output$ABC[[num_iter]][,4])
        mu2_abc <- c(mu2_abc, output$ABC[[num_iter]][,5])
        q12_abc <- c(q12_abc, output$ABC[[num_iter]][,6])
        q21_abc <- c(q21_abc, output$ABC[[num_iter]][,7])
        n_iteration <- c(n_iteration,rep(num_iter,300))
      } else {
        lam1_abc <- c(lam1_abc, output$ABC[[num_iter-1]][,1])
        lam2_abc <- c(lam2_abc, output$ABC[[num_iter-1]][,2])
        lam3_abc <- c(lam3_abc, output$ABC[[num_iter-1]][,3])
        mu1_abc <- c(mu1_abc, output$ABC[[num_iter-1]][,4])
        mu2_abc <- c(mu2_abc, output$ABC[[num_iter-1]][,5])
        q12_abc <- c(q12_abc, output$ABC[[num_iter-1]][,6])
        q21_abc <- c(q21_abc, output$ABC[[num_iter-1]][,7])
        n_iteration <- c(n_iteration,rep(num_iter,300))
      }
    } else {
      lam1_abc <- c(lam1_abc, rep(NA,300))
      lam2_abc <- c(lam2_abc, rep(NA,300))
      lam3_abc <- c(lam3_abc, rep(NA,300))
      mu1_abc <- c(mu1_abc, rep(NA,300))
      mu2_abc <- c(mu2_abc, rep(NA,300))
      q12_abc <- c(q12_abc, rep(NA,300))
      q21_abc <- c(q21_abc, rep(NA,300))
      n_iteration <- c(n_iteration,rep(NA,300))
    }
  }
  whole_df_ABC <- data.frame(param_data2,n_iteration,
                             lam1_abc,lam2_abc,lam3_abc,mu1_abc,mu2_abc,q12_abc,q21_abc)
  save(whole_df_ABC,file = paste0("Data/GeoSSE/whole_df_ABC_test_ss",num_ss,".RData"))
  whole_df_ABC$net_div1 <- (whole_df_ABC$lam1-whole_df_ABC$mu1)
  whole_df_ABC$net_div2 <- (whole_df_ABC$lam2-whole_df_ABC$mu2)
  whole_df_ABC$net_div_ABC1 <- (whole_df_ABC$lam1_abc-whole_df_ABC$mu1_abc)
  whole_df_ABC$net_div_ABC2 <- (whole_df_ABC$lam2_abc-whole_df_ABC$mu2_abc)
  whole_df_ABC$ext_frac1 <- (whole_df_ABC$mu1)/(whole_df_ABC$lam1)
  whole_df_ABC$ext_frac2 <- (whole_df_ABC$mu2)/(whole_df_ABC$lam2)
  whole_df_ABC$ext_frac_ABC1 <- (whole_df_ABC$mu1_abc)/(whole_df_ABC$lam1_abc)
  whole_df_ABC$ext_frac_ABC2 <- (whole_df_ABC$mu2_abc)/(whole_df_ABC$lam2_abc)
  whole_df_ABC$init_obs <- rep(c(rep(0,25*300),rep(1,25*300)),4)
  save(whole_df_ABC,file =
         paste0("Data/GeoSSE/delta_whole_df_ABC_test_ss",num_ss,".RData"))
}


######
# 2. Combining MCMC results
param_data <- load_param_space(param_space_name = paste0("geosse_ABC_test"))
colnames(param_data) <- c("lam1","lam2","lam3","mu1","mu2","q12","q21")
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=5001),]
folder_path <- paste0("Data/GeoSSE/MCMC")
files <- list.files(folder_path)
lam1_mcmc <- c()
lam2_mcmc <- c()
lam3_mcmc <- c()
mu1_mcmc <- c()
mu2_mcmc <- c()
q12_mcmc <- c()
q21_mcmc <- c()
seq <- seq(1,10001,2)
for(i in 1:200){
  file_to_load <- grep(paste0("geosse_MCMC_test_param_set_", i,"_ss_1.RData"),
                       files,
                       value = TRUE,
                       fixed = TRUE)
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lam1_mcmc <- c(lam1_mcmc, output[seq,1])
    lam2_mcmc <- c(lam2_mcmc, output[seq,2])
    lam3_mcmc <- c(lam3_mcmc, output[seq,3])
    mu1_mcmc <- c(mu1_mcmc, output[seq,4])
    mu2_mcmc <- c(mu2_mcmc, output[seq,5])
    q12_mcmc <- c(q12_mcmc, output[seq,6])
    q21_mcmc <- c(q21_mcmc, output[seq,7])
  } else {
    lam1_mcmc <- c(lam1_mcmc, rep(NA,5001))
    lam2_mcmc <- c(lam2_mcmc, rep(NA,5001))
    lam3_mcmc <- c(lam3_mcmc, rep(NA,5001))
    mu1_mcmc <- c(mu1_mcmc, rep(NA,5001))
    mu2_mcmc <- c(mu2_mcmc, rep(NA,5001))
    q12_mcmc <- c(q12_mcmc, rep(NA,5001))
    q21_mcmc <- c(q21_mcmc, rep(NA,5001))
  }
}
whole_df_MCMC <- data.frame(param_data3,
                            lam1_mcmc,lam2_mcmc,lam3_mcmc,
                            mu1_mcmc,mu2_mcmc,
                            q12_mcmc,q21_mcmc)

save(whole_df_MCMC,file = paste0("Data/GeoSSE/whole_df_MCMC_test.RData"))
whole_df_MCMC$net_div1 <- (whole_df_MCMC$lam1-whole_df_MCMC$mu1)
whole_df_MCMC$net_div2 <- (whole_df_MCMC$lam2-whole_df_MCMC$mu2)
whole_df_MCMC$net_div_MCMC1 <- (whole_df_MCMC$lam1_mcmc-whole_df_MCMC$mu1_mcmc)
whole_df_MCMC$net_div_MCMC2 <- (whole_df_MCMC$lam2_mcmc-whole_df_MCMC$mu2_mcmc)

whole_df_MCMC$ext_frac1 <- (whole_df_MCMC$mu1)/(whole_df_MCMC$lam1)
whole_df_MCMC$ext_frac2 <- (whole_df_MCMC$mu2)/(whole_df_MCMC$lam2)
whole_df_MCMC$ext_frac_MCMC1 <- (whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$lam1_mcmc)
whole_df_MCMC$ext_frac_MCMC2 <- (whole_df_MCMC$mu2_mcmc)/(whole_df_MCMC$lam2_mcmc)
whole_df_MCMC$init_obs <- rep(c(rep(0,25*5001),rep(1,25*5001)),4)
save(whole_df_MCMC,file = paste0("Data/GeoSSE/delta_whole_df_MCMC_test.RData"))

######
# 3. formate MLE results
param_data <- load_param_space(param_space_name = paste0("geosse_ABC_test"))
colnames(param_data) <- c("lam1","lam2","lam3","mu1","mu2","q12","q21")
load(paste0("Data/GeoSSE/obs_ss_geosse.rda"))
load(paste0("Data/GeoSSE/test_MLE_geosse.RData"))
whole_df_MLE <- data.frame(param_data,MLE_all,ss[,1:4])
whole_df_MLE$init_obs <- rep(c(rep(0,25),rep(1,25)),4)
save(whole_df_MLE,file = paste0("Data/GeoSSE/whole_df_MLE.RData"))

## median ABC/MCMC/MLE
for (num_ss in c(0,1,2)){
  load(paste0("Data/GeoSSE/delta_whole_df_ABC_test_ss",num_ss,".RData"))
  load(paste0("Data/GeoSSE/delta_whole_df_MCMC_test.RData"))
  load(paste0("Data/GeoSSE/whole_df_MLE.RData"))
  df <- whole_df_ABC
  n <- 300
  ABC_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  df<-whole_df_MCMC
  n <- 5001
  MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  MLE_median <- whole_df_MLE
  load(paste0("Data/GeoSSE/obs_ss_geosse.rda"))
  ## combine ABC MCMC MLE as "AMM"
  AMM_all_df <- cbind(ABC_median[1:21],
                      MCMC_median[,c(8:14,17,18)],
                      MLE_median[,c(8:14)])
  # AMM_all_df$init_obs <- rep(c(rep(1,25),rep(2,25)),7)
  save(AMM_all_df,file = paste0("Data/GeoSSE/AMM_per_set_test_ss",num_ss,".RData"))
}

for(i in 1:4){
  load(paste0("Data/GeoSSE/whole_df_MLE.RData"))
  whole_df_MLE <- whole_df_MLE[(i*50-49):(i*50),][,1:20]

  ss = "ABC1"
  load(paste0("Data/GeoSSE/delta_whole_df_ABC_test_ss0.RData"))
  whole_df_ABC1 <- whole_df_ABC[(i*15000-14999):(i*15000),]
  whole_df_ABC1$ss = "ABC1"
  whole_df_ABC1 = whole_df_ABC1[,-8]
  whole_df_ABC1$dlam1 <- whole_df_ABC1$lam1_abc - whole_df_ABC1$lam1
  whole_df_ABC1$dlam2 <- whole_df_ABC1$lam2_abc - whole_df_ABC1$lam2
  whole_df_ABC1$dlam3 <- whole_df_ABC1$lam3_abc - whole_df_ABC1$lam3
  whole_df_ABC1$dmu1 <- whole_df_ABC1$mu1_abc - whole_df_ABC1$mu1
  whole_df_ABC1$dmu2 <- whole_df_ABC1$mu2_abc - whole_df_ABC1$mu2
  whole_df_ABC1$dq12 <- whole_df_ABC1$q12_abc - whole_df_ABC1$q12
  whole_df_ABC1$dq21 <- whole_df_ABC1$q21_abc - whole_df_ABC1$q21
  whole_df_ABC1$rep <- rep(rep(1:50, each = 300), 1)
  df <- whole_df_ABC1
  n <- 300
  ABC_median1 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median1$ss = "ABC1"

  ss = "ABC2"
  load(paste0("Data/GeoSSE/delta_whole_df_ABC_test_ss2.RData"))
  whole_df_ABC2 <- whole_df_ABC[(i*15000-14999):(i*15000),]
  whole_df_ABC2$ss = "ABC2"
  whole_df_ABC2 = whole_df_ABC2[,-8]
  whole_df_ABC2$dlam1 <- whole_df_ABC2$lam1_abc - whole_df_ABC2$lam1
  whole_df_ABC2$dlam2 <- whole_df_ABC2$lam2_abc - whole_df_ABC2$lam2
  whole_df_ABC2$dlam3 <- whole_df_ABC2$lam3_abc - whole_df_ABC2$lam3
  whole_df_ABC2$dmu1 <- whole_df_ABC2$mu1_abc - whole_df_ABC2$mu1
  whole_df_ABC2$dmu2 <- whole_df_ABC2$mu2_abc - whole_df_ABC2$mu2
  whole_df_ABC2$dq12 <- whole_df_ABC2$q12_abc - whole_df_ABC2$q12
  whole_df_ABC2$dq21 <- whole_df_ABC2$q21_abc - whole_df_ABC2$q21
  whole_df_ABC2$rep <- rep(rep(1:50, each = 300), 1)
  df <- whole_df_ABC2
  n <- 300
  ABC_median2 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median2$ss = "ABC2"

  ss = "ABC3"
  load(paste0("Data/GeoSSE/delta_whole_df_ABC_test_ss1.RData"))
  whole_df_ABC3 <- whole_df_ABC[(i*15000-14999):(i*15000),]
  whole_df_ABC3$ss = "ABC3"
  whole_df_ABC3 = whole_df_ABC3[,-8]
  whole_df_ABC3$dlam1 <- whole_df_ABC3$lam1_abc - whole_df_ABC3$lam1
  whole_df_ABC3$dlam2 <- whole_df_ABC3$lam2_abc - whole_df_ABC3$lam2
  whole_df_ABC3$dlam3 <- whole_df_ABC3$lam3_abc - whole_df_ABC3$lam3
  whole_df_ABC3$dmu1 <- whole_df_ABC3$mu1_abc - whole_df_ABC3$mu1
  whole_df_ABC3$dmu2 <- whole_df_ABC3$mu2_abc - whole_df_ABC3$mu2
  whole_df_ABC3$dq12 <- whole_df_ABC3$q12_abc - whole_df_ABC3$q12
  whole_df_ABC3$dq21 <- whole_df_ABC3$q21_abc - whole_df_ABC3$q21
  whole_df_ABC3$rep <- rep(rep(1:50, each = 300), 1)
  df <- whole_df_ABC3
  n <- 300
  ABC_median3 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median3$ss = "ABC3"

  load(paste0("Data/GeoSSE/delta_whole_df_MCMC_test.RData"))
  whole_df_MCMC <- whole_df_MCMC[(i*250050-250049):(i*250050),]
  whole_df_MCMC$ss = "MCMC"
  whole_df_MCMC$dlam1 <- whole_df_MCMC$lam1_mcmc - whole_df_MCMC$lam1
  whole_df_MCMC$dlam2 <- whole_df_MCMC$lam2_mcmc - whole_df_MCMC$lam2
  whole_df_MCMC$dlam3 <- whole_df_MCMC$lam3_mcmc - whole_df_MCMC$lam3
  whole_df_MCMC$dmu1 <- whole_df_MCMC$mu1_mcmc - whole_df_MCMC$mu1
  whole_df_MCMC$dmu2 <- whole_df_MCMC$mu2_mcmc - whole_df_MCMC$mu2
  whole_df_MCMC$dq12 <- whole_df_MCMC$q12_mcmc - whole_df_MCMC$q12
  whole_df_MCMC$dq21 <- whole_df_MCMC$q21_mcmc - whole_df_MCMC$q21
  whole_df_MCMC$rep <- rep(rep(1:50, each = 5001), 1)
  df<-whole_df_MCMC
  n <- 5001
  MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

  # MLE
  whole_df_MLE$ss = "MLE"
  whole_df_MLE$dlam1 <- whole_df_MLE$lam1_MLE - whole_df_MLE$lam1
  whole_df_MLE$dlam2 <- whole_df_MLE$lam2_MLE - whole_df_MLE$lam2
  whole_df_MLE$dlam3 <- whole_df_MLE$lam3_MLE - whole_df_MLE$lam3
  whole_df_MLE$dmu1 <- whole_df_MLE$mu1_MLE - whole_df_MLE$mu1
  whole_df_MLE$dmu2 <- whole_df_MLE$mu2_MLE - whole_df_MLE$mu2
  whole_df_MLE$dq12 <- whole_df_MLE$q1_MLE - whole_df_MLE$q12
  whole_df_MLE$dq21 <- whole_df_MLE$q2_MLE - whole_df_MLE$q21
  whole_df_MLE$rep <- rep(rep(1:50, each = 1), 1)

  whole_df_all <- rbind(whole_df_ABC1[,-c(8:22)],
                        whole_df_ABC2[,-c(8:22)],
                        whole_df_ABC3[,-c(8:22)],
                        whole_df_MCMC[,-c(8:22)],
                        whole_df_MLE[,-c(8:19)])
  save(whole_df_all, file = paste0("Data/GeoSSE/whole_df_all_AMM_test",i,".RData"))

  median_all <- rbind(ABC_median1[,-c(8:22)],
                      ABC_median2[,-c(8:22)],
                      ABC_median3[,-c(8:22)],
                      MCMC_median[,-c(8:22)],
                      whole_df_MLE[,-c(8:19)])
  save(median_all, file = paste0("Data/GeoSSE/median_AMM_test",i,".RData"))
}





