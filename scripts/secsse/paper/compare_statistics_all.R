## combine  all ss combinations
library(ggplot2)
for(i in 1:7){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/whole_df_MLE.RData"))
  whole_df_MLE <- whole_df_MLE[(i*50-49):(i*50),]
  total <- whole_df_MLE$tree_size

  # 1. NLTTs + D
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/delta_whole_df_ABC_test_ss1.RData"))
  whole_df_ABC1 <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC[(i*20000-19999):(i*20000),]
  whole_df_ABC1$ss = "NLTTs+D"
  whole_df_ABC1 = whole_df_ABC1[,-7]
  whole_df_ABC1$total <- rep(total, each = 500)

  # whole_df_ABC1 <- rbind(whole_df_ABC1_old,whole_df_ABC1_new) #whole_df_ABC1_20
  whole_df_ABC1$dlam1 <- whole_df_ABC1$lam1_abc - whole_df_ABC1$lam1
  whole_df_ABC1$dlam2 <- whole_df_ABC1$lam2_abc - whole_df_ABC1$lam2
  whole_df_ABC1$dmu1 <- whole_df_ABC1$mu1_abc - whole_df_ABC1$mu1
  whole_df_ABC1$dmu2 <- whole_df_ABC1$mu2_abc - whole_df_ABC1$mu2
  whole_df_ABC1$dq12 <- whole_df_ABC1$q12_abc - whole_df_ABC1$q12
  whole_df_ABC1$dq21 <- whole_df_ABC1$q21_abc - whole_df_ABC1$q21
  whole_df_ABC1$dnet_div1 <- whole_df_ABC1$net_div_ABC1 - whole_df_ABC1$net_div1
  whole_df_ABC1$dnet_div2 <- whole_df_ABC1$net_div_ABC2 - whole_df_ABC1$net_div2
  whole_df_ABC1$dext_frac1 <- whole_df_ABC1$ext_frac_ABC1 - whole_df_ABC1$ext_frac1
  whole_df_ABC1$dext_frac2 <- whole_df_ABC1$ext_frac_ABC2 - whole_df_ABC1$ext_frac2
  whole_df_ABC1$rep <- rep(rep(1:50, each = 500), 1)


  df <- whole_df_ABC1
  n <- 500
  ABC_median1 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median1$ss = "NLTTs+D"

  # 2. NLTTs
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/NLTTs/delta_whole_df_ABC_test_ss1.RData"))
  whole_df_ABC2 <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC2[(i*20000-19999):(i*20000),]
  whole_df_ABC2$ss = "NLTTs"
  whole_df_ABC2 = whole_df_ABC2[,-7]
  whole_df_ABC2$total <- rep(total, each = 500)

  # whole_df_ABC2 <- rbind(whole_df_ABC2_old,whole_df_ABC2_new) #whole_df_ABC2_20
  whole_df_ABC2$dlam1 <- whole_df_ABC2$lam1_abc - whole_df_ABC2$lam1
  whole_df_ABC2$dlam2 <- whole_df_ABC2$lam2_abc - whole_df_ABC2$lam2
  whole_df_ABC2$dmu1 <- whole_df_ABC2$mu1_abc - whole_df_ABC2$mu1
  whole_df_ABC2$dmu2 <- whole_df_ABC2$mu2_abc - whole_df_ABC2$mu2
  whole_df_ABC2$dq12 <- whole_df_ABC2$q12_abc - whole_df_ABC2$q12
  whole_df_ABC2$dq21 <- whole_df_ABC2$q21_abc - whole_df_ABC2$q21
  whole_df_ABC2$dnet_div1 <- whole_df_ABC2$net_div_ABC1 - whole_df_ABC2$net_div1
  whole_df_ABC2$dnet_div2 <- whole_df_ABC2$net_div_ABC2 - whole_df_ABC2$net_div2
  whole_df_ABC2$dext_frac1 <- whole_df_ABC2$ext_frac_ABC1 - whole_df_ABC2$ext_frac1
  whole_df_ABC2$dext_frac2 <- whole_df_ABC2$ext_frac_ABC2 - whole_df_ABC2$ext_frac2
  whole_df_ABC2$rep <- rep(rep(1:50, each = 500), 1)

  df <- whole_df_ABC2
  n <- 500
  ABC_median2 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median2$ss = "NLTTs"


  # 3. D
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/D/delta_whole_df_ABC_test_ss3.RData"))
  whole_df_ABC3 <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC3[(i*20000-19999):(i*20000),]
  whole_df_ABC3$ss = "D"
  whole_df_ABC3 = whole_df_ABC3[,-7]
  whole_df_ABC3$total <- rep(total, each = 500)

  # whole_df_ABC3 <- rbind(whole_df_ABC3_old,whole_df_ABC3_new) #whole_df_ABC3_20
  whole_df_ABC3$dlam1 <- whole_df_ABC3$lam1_abc - whole_df_ABC3$lam1
  whole_df_ABC3$dlam2 <- whole_df_ABC3$lam2_abc - whole_df_ABC3$lam2
  whole_df_ABC3$dmu1 <- whole_df_ABC3$mu1_abc - whole_df_ABC3$mu1
  whole_df_ABC3$dmu2 <- whole_df_ABC3$mu2_abc - whole_df_ABC3$mu2
  whole_df_ABC3$dq12 <- whole_df_ABC3$q12_abc - whole_df_ABC3$q12
  whole_df_ABC3$dq21 <- whole_df_ABC3$q21_abc - whole_df_ABC3$q21
  whole_df_ABC3$dnet_div1 <- whole_df_ABC3$net_div_ABC1 - whole_df_ABC3$net_div1
  whole_df_ABC3$dnet_div2 <- whole_df_ABC3$net_div_ABC2 - whole_df_ABC3$net_div2
  whole_df_ABC3$dext_frac1 <- whole_df_ABC3$ext_frac_ABC1 - whole_df_ABC3$ext_frac1
  whole_df_ABC3$dext_frac2 <- whole_df_ABC3$ext_frac_ABC2 - whole_df_ABC3$ext_frac2
  whole_df_ABC3$rep <- rep(rep(1:50, each = 500), 1)

  df <- whole_df_ABC3
  n <- 500
  ABC_median3 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median3$ss = "D"


  # 4. NLTT + D
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/NLTT_D/delta_whole_df_ABC_test_ss2.RData"))
  whole_df_ABC4 <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC4[(i*20000-19999):(i*20000),]
  whole_df_ABC4$ss = "NLTT+D"
  whole_df_ABC4 = whole_df_ABC4[,-7]
  whole_df_ABC4$total <- rep(total, each = 500)

  # whole_df_ABC4 <- rbind(whole_df_ABC4_old,whole_df_ABC4_new) #whole_df_ABC4_20
  whole_df_ABC4$dlam1 <- whole_df_ABC4$lam1_abc - whole_df_ABC4$lam1
  whole_df_ABC4$dlam2 <- whole_df_ABC4$lam2_abc - whole_df_ABC4$lam2
  whole_df_ABC4$dmu1 <- whole_df_ABC4$mu1_abc - whole_df_ABC4$mu1
  whole_df_ABC4$dmu2 <- whole_df_ABC4$mu2_abc - whole_df_ABC4$mu2
  whole_df_ABC4$dq12 <- whole_df_ABC4$q12_abc - whole_df_ABC4$q12
  whole_df_ABC4$dq21 <- whole_df_ABC4$q21_abc - whole_df_ABC4$q21
  whole_df_ABC4$dnet_div1 <- whole_df_ABC4$net_div_ABC1 - whole_df_ABC4$net_div1
  whole_df_ABC4$dnet_div2 <- whole_df_ABC4$net_div_ABC2 - whole_df_ABC4$net_div2
  whole_df_ABC4$dext_frac1 <- whole_df_ABC4$ext_frac_ABC1 - whole_df_ABC4$ext_frac1
  whole_df_ABC4$dext_frac2 <- whole_df_ABC4$ext_frac_ABC2 - whole_df_ABC4$ext_frac2
  whole_df_ABC4$rep <- rep(rep(1:50, each = 500), 1)

  df <- whole_df_ABC4
  n <- 500
  ABC_median4 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median4$ss = "NLTT+D"

  # 5. NLTT + mpd
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/NLTT_MPD/delta_whole_df_ABC_test_ss0.RData"))
  whole_df_ABC5 <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC5[(i*20000-19999):(i*20000),]
  whole_df_ABC5$ss = "NLTT+MPD"
  whole_df_ABC5 = whole_df_ABC5[,-7]
  whole_df_ABC5$total <- rep(total, each = 500)

  # whole_df_ABC5 <- rbind(whole_df_ABC5_old,whole_df_ABC5_new) #whole_df_ABC5_20
  whole_df_ABC5$dlam1 <- whole_df_ABC5$lam1_abc - whole_df_ABC5$lam1
  whole_df_ABC5$dlam2 <- whole_df_ABC5$lam2_abc - whole_df_ABC5$lam2
  whole_df_ABC5$dmu1 <- whole_df_ABC5$mu1_abc - whole_df_ABC5$mu1
  whole_df_ABC5$dmu2 <- whole_df_ABC5$mu2_abc - whole_df_ABC5$mu2
  whole_df_ABC5$dq12 <- whole_df_ABC5$q12_abc - whole_df_ABC5$q12
  whole_df_ABC5$dq21 <- whole_df_ABC5$q21_abc - whole_df_ABC5$q21
  whole_df_ABC5$dnet_div1 <- whole_df_ABC5$net_div_ABC1 - whole_df_ABC5$net_div1
  whole_df_ABC5$dnet_div2 <- whole_df_ABC5$net_div_ABC2 - whole_df_ABC5$net_div2
  whole_df_ABC5$dext_frac1 <- whole_df_ABC5$ext_frac_ABC1 - whole_df_ABC5$ext_frac1
  whole_df_ABC5$dext_frac2 <- whole_df_ABC5$ext_frac_ABC2 - whole_df_ABC5$ext_frac2
  whole_df_ABC5$rep <- rep(rep(1:50, each = 500), 1)

  df <- whole_df_ABC5
  n <- 500
  ABC_median5 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median5$ss = "NLTT+MPD"

  # 6. NLTT + mntd
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/NLTT_MNTD/delta_whole_df_ABC_test_ss0.RData"))
  whole_df_ABC6 <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC6[(i*20000-19999):(i*20000),]
  whole_df_ABC6$ss = "NLTT+MNTD"
  whole_df_ABC6 = whole_df_ABC6[,-7]
  whole_df_ABC6$total <- rep(total, each = 500)

  # whole_df_ABC6 <- rbind(whole_df_ABC6_old,whole_df_ABC6_new) #whole_df_ABC6_20
  whole_df_ABC6$dlam1 <- whole_df_ABC6$lam1_abc - whole_df_ABC6$lam1
  whole_df_ABC6$dlam2 <- whole_df_ABC6$lam2_abc - whole_df_ABC6$lam2
  whole_df_ABC6$dmu1 <- whole_df_ABC6$mu1_abc - whole_df_ABC6$mu1
  whole_df_ABC6$dmu2 <- whole_df_ABC6$mu2_abc - whole_df_ABC6$mu2
  whole_df_ABC6$dq12 <- whole_df_ABC6$q12_abc - whole_df_ABC6$q12
  whole_df_ABC6$dq21 <- whole_df_ABC6$q21_abc - whole_df_ABC6$q21
  whole_df_ABC6$dnet_div1 <- whole_df_ABC6$net_div_ABC1 - whole_df_ABC6$net_div1
  whole_df_ABC6$dnet_div2 <- whole_df_ABC6$net_div_ABC2 - whole_df_ABC6$net_div2
  whole_df_ABC6$dext_frac1 <- whole_df_ABC6$ext_frac_ABC1 - whole_df_ABC6$ext_frac1
  whole_df_ABC6$dext_frac2 <- whole_df_ABC6$ext_frac_ABC2 - whole_df_ABC6$ext_frac2
  whole_df_ABC6$rep <- rep(rep(1:50, each = 500), 1)

  df <- whole_df_ABC6
  n <- 500
  ABC_median6 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median6$ss = "NLTT+MNTD"

  # 7. NLTT + TIPRATIO
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/NLTT_ratio/delta_whole_df_ABC_test_ss1.RData"))
  whole_df_ABC7 <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC7[(i*20000-19999):(i*20000),]
  whole_df_ABC7$ss = "NLTT+ratio"
  whole_df_ABC7 = whole_df_ABC7[,-7]
  whole_df_ABC7$total <- rep(total, each = 500)

  # whole_df_ABC7 <- rbind(whole_df_ABC7_old,whole_df_ABC7_new) #whole_df_ABC7_20
  whole_df_ABC7$dlam1 <- whole_df_ABC7$lam1_abc - whole_df_ABC7$lam1
  whole_df_ABC7$dlam2 <- whole_df_ABC7$lam2_abc - whole_df_ABC7$lam2
  whole_df_ABC7$dmu1 <- whole_df_ABC7$mu1_abc - whole_df_ABC7$mu1
  whole_df_ABC7$dmu2 <- whole_df_ABC7$mu2_abc - whole_df_ABC7$mu2
  whole_df_ABC7$dq12 <- whole_df_ABC7$q12_abc - whole_df_ABC7$q12
  whole_df_ABC7$dq21 <- whole_df_ABC7$q21_abc - whole_df_ABC7$q21
  whole_df_ABC7$dnet_div1 <- whole_df_ABC7$net_div_ABC1 - whole_df_ABC7$net_div1
  whole_df_ABC7$dnet_div2 <- whole_df_ABC7$net_div_ABC2 - whole_df_ABC7$net_div2
  whole_df_ABC7$dext_frac1 <- whole_df_ABC7$ext_frac_ABC1 - whole_df_ABC7$ext_frac1
  whole_df_ABC7$dext_frac2 <- whole_df_ABC7$ext_frac_ABC2 - whole_df_ABC7$ext_frac2
  whole_df_ABC7$rep <- rep(rep(1:50, each = 500), 1)

  df <- whole_df_ABC7
  n <- 500
  ABC_median7 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median7$ss = "NLTT+ratio"

  # 8. NLTT + colless
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/NLTT_colless/delta_whole_df_ABC_test_ss1.RData"))
  whole_df_ABC8 <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC8[(i*20000-19999):(i*20000),]
  whole_df_ABC8$ss = "NLTT+colless"
  whole_df_ABC8 = whole_df_ABC8[,-7]
  whole_df_ABC8$total <- rep(total, each = 500)

  # whole_df_ABC8 <- rbind(whole_df_ABC8_old,whole_df_ABC8_new) #whole_df_ABC8_20
  whole_df_ABC8$dlam1 <- whole_df_ABC8$lam1_abc - whole_df_ABC8$lam1
  whole_df_ABC8$dlam2 <- whole_df_ABC8$lam2_abc - whole_df_ABC8$lam2
  whole_df_ABC8$dmu1 <- whole_df_ABC8$mu1_abc - whole_df_ABC8$mu1
  whole_df_ABC8$dmu2 <- whole_df_ABC8$mu2_abc - whole_df_ABC8$mu2
  whole_df_ABC8$dq12 <- whole_df_ABC8$q12_abc - whole_df_ABC8$q12
  whole_df_ABC8$dq21 <- whole_df_ABC8$q21_abc - whole_df_ABC8$q21
  whole_df_ABC8$dnet_div1 <- whole_df_ABC8$net_div_ABC1 - whole_df_ABC8$net_div1
  whole_df_ABC8$dnet_div2 <- whole_df_ABC8$net_div_ABC2 - whole_df_ABC8$net_div2
  whole_df_ABC8$dext_frac1 <- whole_df_ABC8$ext_frac_ABC1 - whole_df_ABC8$ext_frac1
  whole_df_ABC8$dext_frac2 <- whole_df_ABC8$ext_frac_ABC2 - whole_df_ABC8$ext_frac2
  whole_df_ABC8$rep <- rep(rep(1:50, each = 500), 1)

  df <- whole_df_ABC8
  n <- 500
  ABC_median8 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median8$ss = "NLTT+colless"

  whole_df_all <- rbind(whole_df_ABC1[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC2[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC3[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC4[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC5[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC6[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC7[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC8[,c(1:6,13,14,17,18,21:33)])
  save(whole_df_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_test",i,".RData"))


  median_all <- rbind(ABC_median1[,c(1:6,13,14,17,18,21:33)],
                      ABC_median2[,c(1:6,13,14,17,18,21:33)],
                      ABC_median3[,c(1:6,13,14,17,18,21:33)],
                      ABC_median4[,c(1:6,13,14,17,18,21:33)],
                      ABC_median5[,c(1:6,13,14,17,18,21:33)],
                      ABC_median6[,c(1:6,13,14,17,18,21:33)],
                      ABC_median7[,c(1:6,13,14,17,18,21:33)],
                      ABC_median8[,c(1:6,13,14,17,18,21:33)])

  save(median_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
}


### D VS NLTT+D VS NLTTS VS NLTTS+D
library(ggplot2)
for(i in 1:7){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/whole_df_MLE.RData"))
  whole_df_MLE <- whole_df_MLE[(i*50-49):(i*50),]
  total <- whole_df_MLE$tree_size

  # 1. NLTTs + D
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/delta_whole_df_ABC_test_ss1.RData"))
  whole_df_ABC1 <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC[(i*20000-19999):(i*20000),]
  whole_df_ABC1$ss = "NLTTs+D"
  whole_df_ABC1 = whole_df_ABC1[,-7]
  whole_df_ABC1$total <- rep(total, each = 500)

  # whole_df_ABC1 <- rbind(whole_df_ABC1_old,whole_df_ABC1_new) #whole_df_ABC1_20
  whole_df_ABC1$dlam1 <- whole_df_ABC1$lam1_abc - whole_df_ABC1$lam1
  whole_df_ABC1$dlam2 <- whole_df_ABC1$lam2_abc - whole_df_ABC1$lam2
  whole_df_ABC1$dmu1 <- whole_df_ABC1$mu1_abc - whole_df_ABC1$mu1
  whole_df_ABC1$dmu2 <- whole_df_ABC1$mu2_abc - whole_df_ABC1$mu2
  whole_df_ABC1$dq12 <- whole_df_ABC1$q12_abc - whole_df_ABC1$q12
  whole_df_ABC1$dq21 <- whole_df_ABC1$q21_abc - whole_df_ABC1$q21
  whole_df_ABC1$dnet_div1 <- whole_df_ABC1$net_div_ABC1 - whole_df_ABC1$net_div1
  whole_df_ABC1$dnet_div2 <- whole_df_ABC1$net_div_ABC2 - whole_df_ABC1$net_div2
  whole_df_ABC1$dext_frac1 <- whole_df_ABC1$ext_frac_ABC1 - whole_df_ABC1$ext_frac1
  whole_df_ABC1$dext_frac2 <- whole_df_ABC1$ext_frac_ABC2 - whole_df_ABC1$ext_frac2
  whole_df_ABC1$rep <- rep(rep(1:50, each = 500), 1)


  df <- whole_df_ABC1
  n <- 500
  ABC_median1 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median1$ss = "NLTTs+D"

  # 2. NLTTs
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/NLTTs/delta_whole_df_ABC_test_ss1.RData"))
  whole_df_ABC2 <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC2[(i*20000-19999):(i*20000),]
  whole_df_ABC2$ss = "NLTTs"
  whole_df_ABC2 = whole_df_ABC2[,-7]
  whole_df_ABC2$total <- rep(total, each = 500)

  # whole_df_ABC2 <- rbind(whole_df_ABC2_old,whole_df_ABC2_new) #whole_df_ABC2_20
  whole_df_ABC2$dlam1 <- whole_df_ABC2$lam1_abc - whole_df_ABC2$lam1
  whole_df_ABC2$dlam2 <- whole_df_ABC2$lam2_abc - whole_df_ABC2$lam2
  whole_df_ABC2$dmu1 <- whole_df_ABC2$mu1_abc - whole_df_ABC2$mu1
  whole_df_ABC2$dmu2 <- whole_df_ABC2$mu2_abc - whole_df_ABC2$mu2
  whole_df_ABC2$dq12 <- whole_df_ABC2$q12_abc - whole_df_ABC2$q12
  whole_df_ABC2$dq21 <- whole_df_ABC2$q21_abc - whole_df_ABC2$q21
  whole_df_ABC2$dnet_div1 <- whole_df_ABC2$net_div_ABC1 - whole_df_ABC2$net_div1
  whole_df_ABC2$dnet_div2 <- whole_df_ABC2$net_div_ABC2 - whole_df_ABC2$net_div2
  whole_df_ABC2$dext_frac1 <- whole_df_ABC2$ext_frac_ABC1 - whole_df_ABC2$ext_frac1
  whole_df_ABC2$dext_frac2 <- whole_df_ABC2$ext_frac_ABC2 - whole_df_ABC2$ext_frac2
  whole_df_ABC2$rep <- rep(rep(1:50, each = 500), 1)

  df <- whole_df_ABC2
  n <- 500
  ABC_median2 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median2$ss = "NLTTs"


  # 3. D
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/D/delta_whole_df_ABC_test_ss3.RData"))
  whole_df_ABC3 <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC3[(i*20000-19999):(i*20000),]
  whole_df_ABC3$ss = "D"
  whole_df_ABC3 = whole_df_ABC3[,-7]
  whole_df_ABC3$total <- rep(total, each = 500)

  # whole_df_ABC3 <- rbind(whole_df_ABC3_old,whole_df_ABC3_new) #whole_df_ABC3_20
  whole_df_ABC3$dlam1 <- whole_df_ABC3$lam1_abc - whole_df_ABC3$lam1
  whole_df_ABC3$dlam2 <- whole_df_ABC3$lam2_abc - whole_df_ABC3$lam2
  whole_df_ABC3$dmu1 <- whole_df_ABC3$mu1_abc - whole_df_ABC3$mu1
  whole_df_ABC3$dmu2 <- whole_df_ABC3$mu2_abc - whole_df_ABC3$mu2
  whole_df_ABC3$dq12 <- whole_df_ABC3$q12_abc - whole_df_ABC3$q12
  whole_df_ABC3$dq21 <- whole_df_ABC3$q21_abc - whole_df_ABC3$q21
  whole_df_ABC3$dnet_div1 <- whole_df_ABC3$net_div_ABC1 - whole_df_ABC3$net_div1
  whole_df_ABC3$dnet_div2 <- whole_df_ABC3$net_div_ABC2 - whole_df_ABC3$net_div2
  whole_df_ABC3$dext_frac1 <- whole_df_ABC3$ext_frac_ABC1 - whole_df_ABC3$ext_frac1
  whole_df_ABC3$dext_frac2 <- whole_df_ABC3$ext_frac_ABC2 - whole_df_ABC3$ext_frac2
  whole_df_ABC3$rep <- rep(rep(1:50, each = 500), 1)

  df <- whole_df_ABC3
  n <- 500
  ABC_median3 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median3$ss = "D"


  # 4. NLTT + D
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/NLTT_D/delta_whole_df_ABC_test_ss2.RData"))
  whole_df_ABC4 <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC4[(i*20000-19999):(i*20000),]
  whole_df_ABC4$ss = "NLTT+D"
  whole_df_ABC4 = whole_df_ABC4[,-7]
  whole_df_ABC4$total <- rep(total, each = 500)

  # whole_df_ABC4 <- rbind(whole_df_ABC4_old,whole_df_ABC4_new) #whole_df_ABC4_20
  whole_df_ABC4$dlam1 <- whole_df_ABC4$lam1_abc - whole_df_ABC4$lam1
  whole_df_ABC4$dlam2 <- whole_df_ABC4$lam2_abc - whole_df_ABC4$lam2
  whole_df_ABC4$dmu1 <- whole_df_ABC4$mu1_abc - whole_df_ABC4$mu1
  whole_df_ABC4$dmu2 <- whole_df_ABC4$mu2_abc - whole_df_ABC4$mu2
  whole_df_ABC4$dq12 <- whole_df_ABC4$q12_abc - whole_df_ABC4$q12
  whole_df_ABC4$dq21 <- whole_df_ABC4$q21_abc - whole_df_ABC4$q21
  whole_df_ABC4$dnet_div1 <- whole_df_ABC4$net_div_ABC1 - whole_df_ABC4$net_div1
  whole_df_ABC4$dnet_div2 <- whole_df_ABC4$net_div_ABC2 - whole_df_ABC4$net_div2
  whole_df_ABC4$dext_frac1 <- whole_df_ABC4$ext_frac_ABC1 - whole_df_ABC4$ext_frac1
  whole_df_ABC4$dext_frac2 <- whole_df_ABC4$ext_frac_ABC2 - whole_df_ABC4$ext_frac2
  whole_df_ABC4$rep <- rep(rep(1:50, each = 500), 1)

  df <- whole_df_ABC4
  n <- 500
  ABC_median4 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median4$ss = "NLTT+D"

  whole_df_all <- rbind(whole_df_ABC1[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC2[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC3[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC4[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC5[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC6[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC7[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC8[,c(1:6,13,14,17,18,21:33)])
  save(whole_df_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_test",i,".RData"))


  median_all <- rbind(ABC_median1[,c(1:6,13,14,17,18,21:33)],
                      ABC_median2[,c(1:6,13,14,17,18,21:33)],
                      ABC_median3[,c(1:6,13,14,17,18,21:33)],
                      ABC_median4[,c(1:6,13,14,17,18,21:33)],
                      ABC_median5[,c(1:6,13,14,17,18,21:33)],
                      ABC_median6[,c(1:6,13,14,17,18,21:33)],
                      ABC_median7[,c(1:6,13,14,17,18,21:33)],
                      ABC_median8[,c(1:6,13,14,17,18,21:33)])

  save(median_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
}


