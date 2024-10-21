## data analysis and plots for SSE estimation results
# 1. formate ABC results
for (num_ss in c(1)){
  # formate results
  load(paste0("Data/obs_ss.rda"))
  ## ABC results
  folder_path <- paste0("Data/nltts/ABC")
  files <- list.files(folder_path)
  param_data <- load_param_space(param_space_name = paste0("secsse_ABC_test"))
  param_data2<-param_data[rep(seq_len(nrow(param_data)), each=500),]
  lam1_abc <- c()
  lam2_abc <- c()
  mu1_abc <- c()
  mu2_abc <- c()
  q12_abc <- c()
  q21_abc <- c()
  n_iter <- c()
  n_iteration <- c()
  for(i in 1:350){
    file_to_load <- grep(paste0("secsse_ABC_test_param_set_",i,"_ss_",num_ss,".RData"),
                         files,
                         value = TRUE,
                         fixed = TRUE)

    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      if (nrow(output$ABC[[output$n_iter]]) == 500) {
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
                             lam1_abc,lam2_abc,mu1_abc,mu2_abc,q12_abc,q21_abc)
  save(whole_df_ABC,file = paste0("Data/nltts/whole_df_ABC_test_ss",num_ss,".RData"))

  whole_df_ABC$net_div1 <- (whole_df_ABC$lam1-whole_df_ABC$mu1)
  whole_df_ABC$net_div2 <- (whole_df_ABC$lam2-whole_df_ABC$mu2)
  whole_df_ABC$net_div_ABC1 <- (whole_df_ABC$lam1_abc-whole_df_ABC$mu1_abc)
  whole_df_ABC$net_div_ABC2 <- (whole_df_ABC$lam2_abc-whole_df_ABC$mu2_abc)


  whole_df_ABC$ext_frac1 <- (whole_df_ABC$mu1)/(whole_df_ABC$lam1)
  whole_df_ABC$ext_frac2 <- (whole_df_ABC$mu2)/(whole_df_ABC$lam2)
  whole_df_ABC$ext_frac_ABC1 <- (whole_df_ABC$mu1_abc)/(whole_df_ABC$lam1_abc)
  whole_df_ABC$ext_frac_ABC2 <- (whole_df_ABC$mu2_abc)/(whole_df_ABC$lam2_abc)
  whole_df_ABC$init_obs <- rep(c(rep(0,25*500),rep(1,25*500)),7)
  save(whole_df_ABC,file =
         paste0("Data/nltts/delta_whole_df_ABC_test_ss",num_ss,".RData"))

}


######
# # 2. formate MCMC results
param_data <- load_param_space(param_space_name = paste0("secsse_ABC_test"))
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=5001),]
folder_path <- paste0("Data/MCMC")
files <- list.files(folder_path)
lam1_mcmc <- c()
lam2_mcmc <- c()
mu1_mcmc <- c()
mu2_mcmc <- c()
q12_mcmc <- c()
q21_mcmc <- c()
seq <- seq(1,10001,2)
for(i in 1:350){
  file_to_load <- grep(paste0("secsse_MCMC_test_param_set_", i,"_ss_1.RData"),
                       files,
                       value = TRUE,
                       fixed = TRUE)
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lam1_mcmc <- c(lam1_mcmc, output[seq,1])
    lam2_mcmc <- c(lam2_mcmc, output[seq,2])
    mu1_mcmc <- c(mu1_mcmc, output[seq,3])
    mu2_mcmc <- c(mu2_mcmc, output[seq,4])
    q12_mcmc <- c(q12_mcmc, output[seq,5])
    q21_mcmc <- c(q21_mcmc, output[seq,6])
  } else {
    lam1_mcmc <- c(lam1_mcmc, rep(NA,5001))
    lam2_mcmc <- c(lam2_mcmc, rep(NA,5001))
    mu1_mcmc <- c(mu1_mcmc, rep(NA,5001))
    mu2_mcmc <- c(mu2_mcmc, rep(NA,5001))
    q12_mcmc <- c(q12_mcmc, rep(NA,5001))
    q21_mcmc <- c(q21_mcmc, rep(NA,5001))
  }
}
whole_df_MCMC <- data.frame(param_data3,
                            lam1_mcmc,lam2_mcmc,
                            mu1_mcmc,mu2_mcmc,
                            q12_mcmc,q21_mcmc)

save(whole_df_MCMC,file = paste0("Data/whole_df_MCMC_test.RData"))

whole_df_MCMC$net_div1 <- (whole_df_MCMC$lam1-whole_df_MCMC$mu1)
whole_df_MCMC$net_div2 <- (whole_df_MCMC$lam2-whole_df_MCMC$mu2)
whole_df_MCMC$net_div_MCMC1 <- (whole_df_MCMC$lam1_mcmc-whole_df_MCMC$mu1_mcmc)
whole_df_MCMC$net_div_MCMC2 <- (whole_df_MCMC$lam2_mcmc-whole_df_MCMC$mu2_mcmc)

whole_df_MCMC$ext_frac1 <- (whole_df_MCMC$mu1)/(whole_df_MCMC$lam1)
whole_df_MCMC$ext_frac2 <- (whole_df_MCMC$mu2)/(whole_df_MCMC$lam2)
whole_df_MCMC$ext_frac_MCMC1 <- (whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$lam1_mcmc)
whole_df_MCMC$ext_frac_MCMC2 <- (whole_df_MCMC$mu2_mcmc)/(whole_df_MCMC$lam2_mcmc)
whole_df_MCMC$init_obs <- rep(c(rep(0,25*5001),rep(1,25*5001)),7)

save(whole_df_MCMC,file = paste0("Data/delta_whole_df_MCMC_test.RData"))



## median ABC/MCMC/MLE
for (num_ss in c(0)){
  load(paste0("Data/nltts_D/delta_whole_df_ABC_test_ss",num_ss,".RData"))
  load(paste0("Data/delta_whole_df_MCMC_test.RData"))
  load(paste0("Data/whole_df_MLE.RData"))

  ## get number of iterations and mean values
  df <- whole_df_ABC
  n <- 500
  ABC_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

  df<-whole_df_MCMC
  n <- 5001
  MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

  MLE_median <- whole_df_MLE


  load(paste0("Data/obs_ss.rda"))
  ## combine ABC MCMC MLE as "AMM"
  AMM_all_df <- cbind(ABC_median[1:21],
                      MCMC_median[,c(7:12,15,16,19,20)],
                      MLE_median[,c(7:12,20:23)])
  AMM_all_df$init_obs <- rep(c(rep(0,25),rep(1,25)),7)
  save(AMM_all_df,file = paste0("Data/nltts_D/AMM_per_set_test_ss",num_ss,".RData"))

  load(paste0("Data/nltts_D/AMM_per_set_test_ss",num_ss,".RData"))
  AMM_all_df$dlam1_abc <- AMM_all_df$lam1_abc - AMM_all_df$lam1
  AMM_all_df$dlam2_abc <- AMM_all_df$lam2_abc - AMM_all_df$lam2
  AMM_all_df$dmu1_abc <- AMM_all_df$mu1_abc - AMM_all_df$mu1
  AMM_all_df$dmu2_abc <- AMM_all_df$mu2_abc - AMM_all_df$mu2
  AMM_all_df$dq12_abc <- AMM_all_df$q12_abc - AMM_all_df$q12
  AMM_all_df$dq21_abc <- AMM_all_df$q21_abc - AMM_all_df$q21
  AMM_all_df$tree_size <- ss$tree_size
  AMM_all_df$tip_ratio <- ss$tip_ratio
  AMM_all_df$state1 <- ss$state1
  AMM_all_df$state2 <- ss$state2

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

  AMM_all_df$dnet_div_abc1 <- AMM_all_df$net_div_ABC1-AMM_all_df$net_div1
  AMM_all_df$dnet_div_abc2 <- AMM_all_df$net_div_ABC2-AMM_all_df$net_div2
  AMM_all_df$dnet_div_mcmc1 <- AMM_all_df$net_div_MCMC1-AMM_all_df$net_div1
  AMM_all_df$dnet_div_mcmc2 <- AMM_all_df$net_div_MCMC2-AMM_all_df$net_div2
  AMM_all_df$dnet_div_MLE1 <- AMM_all_df$net_div_MLE1-AMM_all_df$net_div1
  AMM_all_df$dnet_div_MLE2 <- AMM_all_df$net_div_MLE2-AMM_all_df$net_div2

  save(AMM_all_df,file = paste0("Data/nltts_D/AMM_per_set_drate_test_ss",num_ss,".RData"))
}


# plot each replicate (50 reps in one plot)
#####
rgb(red=255, green=208, blue=111, maxColorValue = 255)

iqr = function(z, lower = 0.1, upper = 0.9) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

library(ggplot2)
for(i in 1:7){
  load(paste0("Data/whole_df_MLE.RData"))
  whole_df_MLE <- whole_df_MLE[(i*50-49):(i*50),][,1:24]
  total <- whole_df_MLE$tree_size
  state1 <-whole_df_MLE$state1
  state2 <- whole_df_MLE$state2

  ss = "ABC"
  load(paste0("Data/nltts_D/delta_whole_df_ABC_test_ss0.RData"))
  whole_df_ABC <- whole_df_ABC[(i*25000-24999):(i*25000),]
  whole_df_ABC$ss = "ABC"
  whole_df_ABC = whole_df_ABC[,-7]
  whole_df_ABC$total <- rep(total, each = 500)

  whole_df_ABC$dlam1 <- whole_df_ABC$lam1_abc - whole_df_ABC$lam1
  whole_df_ABC$dlam2 <- whole_df_ABC$lam2_abc - whole_df_ABC$lam2
  whole_df_ABC$dmu1 <- whole_df_ABC$mu1_abc - whole_df_ABC$mu1
  whole_df_ABC$dmu2 <- whole_df_ABC$mu2_abc - whole_df_ABC$mu2
  whole_df_ABC$dq12 <- whole_df_ABC$q12_abc - whole_df_ABC$q12
  whole_df_ABC$dq21 <- whole_df_ABC$q21_abc - whole_df_ABC$q21
  whole_df_ABC$dnet_div1 <- whole_df_ABC$net_div_ABC1 - whole_df_ABC$net_div1
  whole_df_ABC$dnet_div2 <- whole_df_ABC$net_div_ABC2 - whole_df_ABC$net_div2
  whole_df_ABC$dext_frac1 <- whole_df_ABC$ext_frac_ABC1 - whole_df_ABC$ext_frac1
  whole_df_ABC$dext_frac2 <- whole_df_ABC$ext_frac_ABC2 - whole_df_ABC$ext_frac2
  whole_df_ABC$rep <- rep(rep(1:50, each = 500), 1)
  whole_df_ABC$state1 <- rep(state1, each = 500)
  whole_df_ABC$state2 <- rep(state2, each = 500)

  df <- whole_df_ABC
  n <- 500
  ABC_median <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median$ss = "ABC"


  load(paste0("Data/delta_whole_df_MCMC_test.RData"))
  whole_df_MCMC <- whole_df_MCMC[(i*250050-250049):(i*250050),]
  whole_df_MCMC$ss = "MCMC"
  whole_df_MCMC$total <- rep(total, each = 5001)
  whole_df_MCMC$dlam1 <- whole_df_MCMC$lam1_mcmc - whole_df_MCMC$lam1
  whole_df_MCMC$dlam2 <- whole_df_MCMC$lam2_mcmc - whole_df_MCMC$lam2
  whole_df_MCMC$dmu1 <- whole_df_MCMC$mu1_mcmc - whole_df_MCMC$mu1
  whole_df_MCMC$dmu2 <- whole_df_MCMC$mu2_mcmc - whole_df_MCMC$mu2
  whole_df_MCMC$dq12 <- whole_df_MCMC$q12_mcmc - whole_df_MCMC$q12
  whole_df_MCMC$dq21 <- whole_df_MCMC$q21_mcmc - whole_df_MCMC$q21
  whole_df_MCMC$dnet_div1 <- whole_df_MCMC$net_div_MCMC1 - whole_df_MCMC$net_div1
  whole_df_MCMC$dnet_div2 <- whole_df_MCMC$net_div_MCMC2 - whole_df_MCMC$net_div2
  whole_df_MCMC$dext_frac1 <- whole_df_MCMC$ext_frac_MCMC1 - whole_df_MCMC$ext_frac1
  whole_df_MCMC$dext_frac2 <- whole_df_MCMC$ext_frac_MCMC2 - whole_df_MCMC$ext_frac2
  whole_df_MCMC$rep <- rep(rep(1:50, each = 5001), 1)
  whole_df_MCMC$state1 <- rep(state1, each = 5001)
  whole_df_MCMC$state2 <- rep(state2, each = 5001)

  df<-whole_df_MCMC
  n <- 5001
  MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

  # MLE
  whole_df_MLE$net_div1 <- (whole_df_MLE$lam1-whole_df_MLE$mu1)
  whole_df_MLE$net_div2 <- (whole_df_MLE$lam2-whole_df_MLE$mu2)
  whole_df_MLE$net_div_MLE1 <- (whole_df_MLE$lam1_MLE-whole_df_MLE$mu1_MLE)
  whole_df_MLE$net_div_MLE2 <- (whole_df_MLE$lam2_MLE-whole_df_MLE$mu2_MLE)
  whole_df_MLE$ext_frac1 <- (whole_df_MLE$mu1)/(whole_df_MLE$lam1)
  whole_df_MLE$ext_frac2 <- (whole_df_MLE$mu2)/(whole_df_MLE$lam2)
  whole_df_MLE$ext_frac_MLE1 <- (whole_df_MLE$mu1_MLE)/(whole_df_MLE$lam1_MLE)
  whole_df_MLE$ext_frac_MLE2 <- (whole_df_MLE$mu2_MLE)/(whole_df_MLE$lam2_MLE)

  whole_df_MLE$ss = "MLE"
  whole_df_MLE$total <- rep(total, each = 1)
  whole_df_MLE$dlam1 <- whole_df_MLE$lam1_MLE - whole_df_MLE$lam1
  whole_df_MLE$dlam2 <- whole_df_MLE$lam2_MLE - whole_df_MLE$lam2
  whole_df_MLE$dmu1 <- whole_df_MLE$mu1_MLE - whole_df_MLE$mu1
  whole_df_MLE$dmu2 <- whole_df_MLE$mu2_MLE - whole_df_MLE$mu2
  whole_df_MLE$dq12 <- whole_df_MLE$q12_MLE - whole_df_MLE$q12
  whole_df_MLE$dq21 <- whole_df_MLE$q21_MLE - whole_df_MLE$q21
  whole_df_MLE$dnet_div1 <- whole_df_MLE$net_div_MLE1 - whole_df_MLE$net_div1
  whole_df_MLE$dnet_div2 <- whole_df_MLE$net_div_MLE2 - whole_df_MLE$net_div2
  whole_df_MLE$dext_frac1 <- whole_df_MLE$ext_frac_MLE1 - whole_df_MLE$ext_frac1
  whole_df_MLE$dext_frac2 <- whole_df_MLE$ext_frac_MLE2 - whole_df_MLE$ext_frac2
  whole_df_MLE$rep <- rep(rep(1:50, each = 1), 1)
  whole_df_MLE$state1 <- rep(state1, each = 1)
  whole_df_MLE$state2 <- rep(state2, each = 1)


  whole_df_all <- rbind(whole_df_ABC[,c(1:6,13,14,17,18,21:36)],
                        whole_df_MCMC[,c(1:6,13,14,17,18,21:36)],
                        whole_df_MLE[,c(1:6,24,25,26,29,30,33:45,20,21)])
  save(whole_df_all, file = paste0("Data/nltts_D/whole_df_all_AMM_test",i,".RData"))

  median_all <- rbind(ABC_median[,c(1:6,13,14,17,18,21:36)],
                      MCMC_median[,c(1:6,13,14,17,18,21:36)],
                      whole_df_MLE[,c(1:6,24,25,26,29,30,33:45,20,21)])

  save(median_all, file = paste0("Data/nltts_D/median_AMM_test",i,".RData"))
}










