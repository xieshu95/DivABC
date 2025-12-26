
for (num_ss in c(0,1,2)){
  load(paste0("Data/MuSSE/obs_ss_musse.rda"))
  ## ABC results
  folder_path <- paste0("Data/MuSSE/ABC")
  files <- list.files(folder_path)
  param_data <- load_param_space(param_space_name = paste0("musse_ABC_test"))
  param_data2<-param_data[rep(seq_len(nrow(param_data)), each=300),]
  lam1_abc <- c()
  lam2_abc <- c()
  lam3_abc <- c()
  mu1_abc <- c()
  mu2_abc <- c()
  mu3_abc <- c()
  q_abc <- c()
  n_iter <- c()
  n_iteration <- c()
  for(i in 1:90){
    file_to_load <- grep(paste0("musse_ABC_test_param_set_",i,"_ss_",num_ss,".RData"),
                         files,
                         value = TRUE,
                         fixed = TRUE)

    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      if (nrow(output$ABC[[output$n_iter]]) == 300) {
        lam1_abc <- c(lam1_abc, output$ABC[[num_iter]][,1])
        lam2_abc <- c(lam2_abc, output$ABC[[num_iter]][,2])
        lam3_abc <- c(lam3_abc, output$ABC[[num_iter]][,3])
        mu1_abc <- c(mu1_abc, output$ABC[[num_iter]][,4])
        mu2_abc <- c(mu2_abc, output$ABC[[num_iter]][,5])
        mu3_abc <- c(mu3_abc, output$ABC[[num_iter]][,6])
        q_abc <- c(q_abc, output$ABC[[num_iter]][,7])
        n_iteration <- c(n_iteration,rep(num_iter,300))
      } else {
        lam1_abc <- c(lam1_abc, output$ABC[[num_iter-1]][,1])
        lam2_abc <- c(lam2_abc, output$ABC[[num_iter-1]][,2])
        lam3_abc <- c(lam3_abc, output$ABC[[num_iter-1]][,3])
        mu1_abc <- c(mu1_abc, output$ABC[[num_iter-1]][,4])
        mu2_abc <- c(mu2_abc, output$ABC[[num_iter-1]][,5])
        mu3_abc <- c(mu3_abc, output$ABC[[num_iter-1]][,6])
        q_abc <- c(q_abc, output$ABC[[num_iter-1]][,7])
        n_iteration <- c(n_iteration,rep(num_iter,300))
      }
    } else {
      lam1_abc <- c(lam1_abc, rep(NA,300))
      lam2_abc <- c(lam2_abc, rep(NA,300))
      lam3_abc <- c(lam3_abc, rep(NA,300))
      mu1_abc <- c(mu1_abc, rep(NA,300))
      mu2_abc <- c(mu2_abc, rep(NA,300))
      mu3_abc <- c(mu3_abc, rep(NA,300))
      q_abc <- c(q_abc, rep(NA,300))
      n_iteration <- c(n_iteration,rep(NA,300))
    }
  }
  whole_df_ABC <- data.frame(param_data2,n_iteration,
                             lam1_abc,lam2_abc,lam3_abc,
                             mu1_abc,mu2_abc,mu3_abc,q_abc)
  save(whole_df_ABC,file = paste0("Data/MuSSE/whole_df_ABC_test_ss",num_ss,".RData"))
  save(whole_df_ABC,file =
         paste0("Data/MuSSE/delta_whole_df_ABC_test_ss",num_ss,".RData"))

}


######
# 2. formate MCMC results
param_data <- load_param_space(param_space_name = paste0("musse_ABC_test"))
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=5001),]
folder_path <- paste0("Data/MuSSE/MCMC")
files <- list.files(folder_path)
lam1_mcmc <- c()
lam2_mcmc <- c()
lam3_mcmc <- c()
mu1_mcmc <- c()
mu2_mcmc <- c()
mu3_mcmc <- c()
q_mcmc <- c()
seq <- seq(1,10001,2)
for(i in 1:90){
  file_to_load <- grep(paste0("musse_MCMC_test_param_set_", i,"_ss_1.RData"),
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
    mu3_mcmc <- c(mu3_mcmc, output[seq,6])
    q_mcmc <- c(q_mcmc, output[seq,7])
  } else {
    lam1_mcmc <- c(lam1_mcmc, rep(NA,5001))
    lam2_mcmc <- c(lam2_mcmc, rep(NA,5001))
    lam3_mcmc <- c(lam3_mcmc, rep(NA,5001))
    mu1_mcmc <- c(mu1_mcmc, rep(NA,5001))
    mu2_mcmc <- c(mu2_mcmc, rep(NA,5001))
    mu3_mcmc <- c(mu3_mcmc, rep(NA,5001))
    q_mcmc <- c(q_mcmc, rep(NA,5001))
  }
}
whole_df_MCMC <- data.frame(param_data3,
                            lam1_mcmc,lam2_mcmc,lam3_mcmc,
                            mu1_mcmc,mu2_mcmc,mu3_mcmc,q_mcmc)

save(whole_df_MCMC,file = paste0("Data/MuSSE/whole_df_MCMC_test.RData"))

save(whole_df_MCMC,file = paste0("Data/MuSSE/delta_whole_df_MCMC_test.RData"))

# MLE
param_data <- load_param_space(param_space_name = paste0("musse_ABC_test"))
load(paste0("Data/MuSSE/obs_ss_musse.rda"))
load(paste0("Data/MuSSE/test_MLE_musse.RData"))
whole_df_MLE <- data.frame(param_data,MLE_all,ss[,1:4])
save(whole_df_MLE,file = paste0("Data/MuSSE/whole_df_MLE.RData"))





## median ABC/MCMC/MLE
for(i in 1:3){
  load(paste0("Data/MuSSE/whole_df_MLE.RData"))
  whole_df_MLE <- whole_df_MLE[(i*30-29):(i*30),][,1:19]

  ss = "ABC1"
  load(paste0("Data/MuSSE/whole_df_ABC_test_ss0.RData"))
  whole_df_ABC1 <- whole_df_ABC[(i*9000-8999):(i*9000),]
  whole_df_ABC1$ss = "ABC1"
  whole_df_ABC1 = whole_df_ABC1[,-8]
  whole_df_ABC1$dlam1 <- whole_df_ABC1$lam1_abc - whole_df_ABC1$lam1
  whole_df_ABC1$dlam2 <- whole_df_ABC1$lam2_abc - whole_df_ABC1$lam2
  whole_df_ABC1$dlam3 <- whole_df_ABC1$lam3_abc - whole_df_ABC1$lam3
  whole_df_ABC1$dmu1 <- whole_df_ABC1$mu1_abc - whole_df_ABC1$mu1
  whole_df_ABC1$dmu2 <- whole_df_ABC1$mu2_abc - whole_df_ABC1$mu2
  whole_df_ABC1$dmu3 <- whole_df_ABC1$mu3_abc - whole_df_ABC1$mu3
  whole_df_ABC1$dq <- whole_df_ABC1$q_abc - whole_df_ABC1$q
  whole_df_ABC1$dnet_div1 <- (whole_df_ABC1$lam1_abc-whole_df_ABC1$mu1_abc)-(whole_df_ABC1$lam1-whole_df_ABC1$mu1)
  whole_df_ABC1$dnet_div2 <- (whole_df_ABC1$lam2_abc-whole_df_ABC1$mu2_abc)-(whole_df_ABC1$lam2-whole_df_ABC1$mu2)
  whole_df_ABC1$dnet_div3 <- (whole_df_ABC1$lam3_abc-whole_df_ABC1$mu3_abc)-(whole_df_ABC1$lam3-whole_df_ABC1$mu3)

  whole_df_ABC1$rep <- rep(rep(1:30, each = 300), 1)
  df <- whole_df_ABC1
  n <- 300
  ABC_median1 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median1$ss = "ABC1"

  ss = "ABC2"
  load(paste0("Data/MuSSE/whole_df_ABC_test_ss2.RData"))
  whole_df_ABC2 <- whole_df_ABC[(i*9000-8999):(i*9000),]
  whole_df_ABC2$ss = "ABC2"
  whole_df_ABC2 = whole_df_ABC2[,-8]
  whole_df_ABC2$dlam1 <- whole_df_ABC2$lam1_abc - whole_df_ABC2$lam1
  whole_df_ABC2$dlam2 <- whole_df_ABC2$lam2_abc - whole_df_ABC2$lam2
  whole_df_ABC2$dlam3 <- whole_df_ABC2$lam3_abc - whole_df_ABC2$lam3
  whole_df_ABC2$dmu1 <- whole_df_ABC2$mu1_abc - whole_df_ABC2$mu1
  whole_df_ABC2$dmu2 <- whole_df_ABC2$mu2_abc - whole_df_ABC2$mu2
  whole_df_ABC2$dmu3 <- whole_df_ABC2$mu3_abc - whole_df_ABC2$mu3
  whole_df_ABC2$dq <- whole_df_ABC2$q_abc - whole_df_ABC2$q
  whole_df_ABC2$dnet_div1 <- (whole_df_ABC2$lam1_abc-whole_df_ABC2$mu1_abc)-(whole_df_ABC2$lam1-whole_df_ABC2$mu1)
  whole_df_ABC2$dnet_div2 <- (whole_df_ABC2$lam2_abc-whole_df_ABC2$mu2_abc)-(whole_df_ABC2$lam2-whole_df_ABC2$mu2)
  whole_df_ABC2$dnet_div3 <- (whole_df_ABC2$lam3_abc-whole_df_ABC2$mu3_abc)-(whole_df_ABC2$lam3-whole_df_ABC2$mu3)
  whole_df_ABC2$rep <- rep(rep(1:30, each = 300), 1)
  df <- whole_df_ABC2
  n <- 300
  ABC_median2 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median2$ss = "ABC2"

  ss = "ABC3"
  load(paste0("Data/MuSSE/whole_df_ABC_test_ss1.RData"))
  whole_df_ABC3 <- whole_df_ABC[(i*9000-8999):(i*9000),]
  whole_df_ABC3$ss = "ABC3"
  whole_df_ABC3 = whole_df_ABC3[,-8]
  whole_df_ABC3$dlam1 <- whole_df_ABC3$lam1_abc - whole_df_ABC3$lam1
  whole_df_ABC3$dlam2 <- whole_df_ABC3$lam2_abc - whole_df_ABC3$lam2
  whole_df_ABC3$dlam3 <- whole_df_ABC3$lam3_abc - whole_df_ABC3$lam3
  whole_df_ABC3$dmu1 <- whole_df_ABC3$mu1_abc - whole_df_ABC3$mu1
  whole_df_ABC3$dmu2 <- whole_df_ABC3$mu2_abc - whole_df_ABC3$mu2
  whole_df_ABC3$dmu3 <- whole_df_ABC3$mu3_abc - whole_df_ABC3$mu3
  whole_df_ABC3$dq <- whole_df_ABC3$q_abc - whole_df_ABC3$q
  whole_df_ABC3$dnet_div1 <- (whole_df_ABC3$lam1_abc-whole_df_ABC3$mu1_abc)-(whole_df_ABC3$lam1-whole_df_ABC3$mu1)
  whole_df_ABC3$dnet_div2 <- (whole_df_ABC3$lam2_abc-whole_df_ABC3$mu2_abc)-(whole_df_ABC3$lam2-whole_df_ABC3$mu2)
  whole_df_ABC3$dnet_div3 <- (whole_df_ABC3$lam3_abc-whole_df_ABC3$mu3_abc)-(whole_df_ABC3$lam3-whole_df_ABC3$mu3)
  whole_df_ABC3$rep <- rep(rep(1:30, each = 300), 1)
  df <- whole_df_ABC3
  n <- 300
  ABC_median3 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median3$ss = "ABC3"


  load(paste0("Data/MuSSE/whole_df_MCMC_test.RData"))
  whole_df_mcmc <- whole_df_MCMC[(i*150030-150029):(i*150030),]
  whole_df_mcmc$ss = "MCMC"
  whole_df_mcmc$dlam1 <- whole_df_mcmc$lam1_mcmc - whole_df_mcmc$lam1
  whole_df_mcmc$dlam2 <- whole_df_mcmc$lam2_mcmc - whole_df_mcmc$lam2
  whole_df_mcmc$dlam3 <- whole_df_mcmc$lam3_mcmc - whole_df_mcmc$lam3
  whole_df_mcmc$dmu1 <- whole_df_mcmc$mu1_mcmc - whole_df_mcmc$mu1
  whole_df_mcmc$dmu2 <- whole_df_mcmc$mu2_mcmc - whole_df_mcmc$mu2
  whole_df_mcmc$dmu3 <- whole_df_mcmc$mu3_mcmc - whole_df_mcmc$mu3
  whole_df_mcmc$dq <- whole_df_mcmc$q_mcmc - whole_df_mcmc$q
  whole_df_mcmc$dnet_div1 <- (whole_df_mcmc$lam1_mcmc-whole_df_mcmc$mu1_mcmc)-(whole_df_mcmc$lam1-whole_df_mcmc$mu1)
  whole_df_mcmc$dnet_div2 <- (whole_df_mcmc$lam2_mcmc-whole_df_mcmc$mu2_mcmc)-(whole_df_mcmc$lam2-whole_df_mcmc$mu2)
  whole_df_mcmc$dnet_div3 <- (whole_df_mcmc$lam3_mcmc-whole_df_mcmc$mu3_mcmc)-(whole_df_mcmc$lam3-whole_df_mcmc$mu3)
  whole_df_mcmc$rep <- rep(rep(1:30, each = 5001), 1)


  df<-whole_df_mcmc
  n <- 5001
  MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]


  whole_df_MLE$ss = "MLE"
  whole_df_MLE$dlam1 <- whole_df_MLE$lam1_MLE - whole_df_MLE$lam1
  whole_df_MLE$dlam2 <- whole_df_MLE$lam2_MLE - whole_df_MLE$lam2
  whole_df_MLE$dlam3 <- whole_df_MLE$lam3_MLE - whole_df_MLE$lam3
  whole_df_MLE$dmu1 <- whole_df_MLE$mu1_MLE - whole_df_MLE$mu1
  whole_df_MLE$dmu2 <- whole_df_MLE$mu2_MLE - whole_df_MLE$mu2
  whole_df_MLE$dmu3 <- whole_df_MLE$mu3_MLE - whole_df_MLE$mu3
  whole_df_MLE$dq <- whole_df_MLE$q_MLE - whole_df_MLE$q
  whole_df_MLE$dnet_div1 <- (whole_df_MLE$lam1_MLE-whole_df_MLE$mu1_MLE)-(whole_df_MLE$lam1-whole_df_MLE$mu1)
  whole_df_MLE$dnet_div2 <- (whole_df_MLE$lam2_MLE-whole_df_MLE$mu2_MLE)-(whole_df_MLE$lam2-whole_df_MLE$mu2)
  whole_df_MLE$dnet_div3 <- (whole_df_MLE$lam3_MLE-whole_df_MLE$mu3_MLE)-(whole_df_MLE$lam3-whole_df_MLE$mu3)
  whole_df_MLE$rep <- rep(rep(1:30, each = 1), 1)

  whole_df_all <- rbind(whole_df_ABC1[,-c(8:14)],
                        whole_df_ABC2[,-c(8:14)],
                        whole_df_ABC3[,-c(8:14)],
                        whole_df_mcmc[,-c(8:14)],
                        whole_df_MLE[,-c(8:19)])
  save(whole_df_all, file = paste0("Data/MuSSE/whole_df_all_AMM_test",i,".RData"))

  median_all <- rbind(ABC_median1[,-c(8:14)],
                      ABC_median2[,-c(8:14)],
                      ABC_median3[,-c(8:14)],
                      MCMC_median[,-c(8:14)],
                      whole_df_MLE[,-c(8:19)])

  save(median_all, file = paste0("Data/MuSSE/median_AMM_test",i,".RData"))
}






