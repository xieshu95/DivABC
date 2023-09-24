# compare the nltt and nltt+D
library(ggplot2)
for(i in 1:7){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/whole_df_MLE.RData"))
  whole_df_MLE <- whole_df_MLE[(i*50-49):(i*50),]
  total <- whole_df_MLE$tree_size

  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/delta_whole_df_ABC_test_ss1.RData"))
  whole_df_ABC1 <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC[(i*20000-19999):(i*20000),]
  whole_df_ABC1$ss = "NLTT+D"
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
  ABC_median1$ss = "NLTT+D"


  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/delta_whole_df_ABC_test_ss1.RData"))
  whole_df_ABC2 <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC2[(i*20000-19999):(i*20000),]
  whole_df_ABC2$ss = "NLTT"
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
  ABC_median2$ss = "NLTT"



  whole_df_all <- rbind(whole_df_ABC1[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC2[,c(1:6,13,14,17,18,21:33)])
  save(whole_df_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/ABC_test",i,".RData"))


  median_all <- rbind(ABC_median1[,c(1:6,13,14,17,18,21:33)],
                      ABC_median2[,c(1:6,13,14,17,18,21:33)])

  save(median_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/ABC_median_test",i,".RData"))
}









