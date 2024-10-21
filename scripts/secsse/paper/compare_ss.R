# 1. Estimations for each summary statistic
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

## 2.
## combine  all ss combinations
library(ggplot2)
for(i in 1:7){
  load(paste0("Data/whole_df_MLE.RData"))
  whole_df_MLE <- whole_df_MLE[(i*50-49):(i*50),]
  total <- whole_df_MLE$tree_size

  # 1. D
  load(paste0("Data/D/delta_whole_df_ABC_test_ss3.RData"))

  whole_df_ABC1 <- whole_df_ABC[(i*25000-24999):(i*25000),]
  whole_df_ABC1$ss = "D"
  whole_df_ABC1 = whole_df_ABC1[,-7]
  whole_df_ABC1$total <- rep(total, each = 500)

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
  ABC_median1$ss = "D"

  # 2. NLTT
  load(paste0("Data/nltt/delta_whole_df_ABC_test_ss9.RData"))
  whole_df_ABC2 <- whole_df_ABC[(i*25000-24999):(i*25000),]
  whole_df_ABC2$ss = "nLTT"
  whole_df_ABC2 = whole_df_ABC2[,-7]
  whole_df_ABC2$total <- rep(total, each = 500)

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
  ABC_median2$ss = "nLTT"


  # 3. nltt + D
  load(paste0("Data/nltt_D/delta_whole_df_ABC_test_ss2.RData"))
  whole_df_ABC3 <- whole_df_ABC[(i*25000-24999):(i*25000),]
  whole_df_ABC3$ss = "nLTT-D"
  whole_df_ABC3 = whole_df_ABC3[,-7]
  whole_df_ABC3$total <- rep(total, each = 500)

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
  ABC_median3$ss = "nLTT-D"


  #  4. ratio + nltt
  load(paste0("Data/nltt_ratio/delta_whole_df_ABC_test_ss12.RData"))
  whole_df_ABC4 <- whole_df_ABC[(i*25000-24999):(i*25000),]
  whole_df_ABC4$ss = "nLTT-Ratio"
  whole_df_ABC4 = whole_df_ABC4[,-7]
  whole_df_ABC4$total <- rep(total, each = 500)

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
  ABC_median4$ss = "nLTT-Ratio"

  # 5. mpds + nltt
  load(paste0("Data/nltt_mpds/delta_whole_df_ABC_test_ss7.RData"))
  whole_df_ABC5 <- whole_df_ABC[(i*25000-24999):(i*25000),]
  whole_df_ABC5$ss = "nLTT-MPD"
  whole_df_ABC5 = whole_df_ABC5[,-7]
  whole_df_ABC5$total <- rep(total, each = 500)

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
  ABC_median5$ss = "nLTT-MPD"

  # 6. mntds + nltt
  load(paste0("Data/nltt_mntds/delta_whole_df_ABC_test_ss10.RData"))
  whole_df_ABC6 <- whole_df_ABC[(i*25000-24999):(i*25000),]
  whole_df_ABC6$ss = "nLTT-MNTD"
  whole_df_ABC6 = whole_df_ABC6[,-7]
  whole_df_ABC6$total <- rep(total, each = 500)

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
  ABC_median6$ss = "nLTT-MNTD"

  # 7. colless + nltt
  load(paste0("Data/nltt_colless/delta_whole_df_ABC_test_ss11.RData"))
  whole_df_ABC7 <- whole_df_ABC[(i*25000-24999):(i*25000),]
  whole_df_ABC7$ss = "nLTT-Colless"
  whole_df_ABC7 = whole_df_ABC7[,-7]
  whole_df_ABC7$total <- rep(total, each = 500)

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
  ABC_median7$ss = "nLTT-Colless"

  # 8. nLTTs
  load(paste0("Data/nltts/delta_whole_df_ABC_test_ss1.RData"))

  whole_df_ABC8 <- whole_df_ABC[(i*25000-24999):(i*25000),]
  whole_df_ABC8$ss = "nLTTs"
  whole_df_ABC8 = whole_df_ABC8[,-7]
  whole_df_ABC8$total <- rep(total, each = 500)

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
  ABC_median8$ss = "nLTTs"


  # 9. NLTTs + D
  load(paste0("Data/nltts_D/delta_whole_df_ABC_test_ss0.RData"))

  whole_df_ABC9 <- whole_df_ABC[(i*25000-24999):(i*25000),]
  whole_df_ABC9$ss = "nLTTs-D"
  whole_df_ABC9 = whole_df_ABC9[,-7]
  whole_df_ABC9$total <- rep(total, each = 500)

  whole_df_ABC9$dlam1 <- whole_df_ABC9$lam1_abc - whole_df_ABC9$lam1
  whole_df_ABC9$dlam2 <- whole_df_ABC9$lam2_abc - whole_df_ABC9$lam2
  whole_df_ABC9$dmu1 <- whole_df_ABC9$mu1_abc - whole_df_ABC9$mu1
  whole_df_ABC9$dmu2 <- whole_df_ABC9$mu2_abc - whole_df_ABC9$mu2
  whole_df_ABC9$dq12 <- whole_df_ABC9$q12_abc - whole_df_ABC9$q12
  whole_df_ABC9$dq21 <- whole_df_ABC9$q21_abc - whole_df_ABC9$q21
  whole_df_ABC9$dnet_div1 <- whole_df_ABC9$net_div_ABC1 - whole_df_ABC9$net_div1
  whole_df_ABC9$dnet_div2 <- whole_df_ABC9$net_div_ABC2 - whole_df_ABC9$net_div2
  whole_df_ABC9$dext_frac1 <- whole_df_ABC9$ext_frac_ABC1 - whole_df_ABC9$ext_frac1
  whole_df_ABC9$dext_frac2 <- whole_df_ABC9$ext_frac_ABC2 - whole_df_ABC9$ext_frac2
  whole_df_ABC9$rep <- rep(rep(1:50, each = 500), 1)
  #
  df <- whole_df_ABC9
  n <- 500
  ABC_median9 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median9$ss = "nLTTs-D"

  whole_df_all <- rbind(whole_df_ABC1[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC2[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC3[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC4[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC5[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC6[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC7[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC8[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC9[,c(1:6,13,14,17,18,21:33)])
  save(whole_df_all, file = paste0("Data/ABC_test",i,".RData"))


  median_all <- rbind(ABC_median1[,c(1:6,13,14,17,18,21:33)],
                      ABC_median2[,c(1:6,13,14,17,18,21:33)],
                      ABC_median3[,c(1:6,13,14,17,18,21:33)],
                      ABC_median4[,c(1:6,13,14,17,18,21:33)],
                      ABC_median5[,c(1:6,13,14,17,18,21:33)],
                      ABC_median6[,c(1:6,13,14,17,18,21:33)],
                      ABC_median7[,c(1:6,13,14,17,18,21:33)],
                      ABC_median8[,c(1:6,13,14,17,18,21:33)],
                      ABC_median9[,c(1:6,13,14,17,18,21:33)])

  save(median_all, file = paste0("Data/ABC_median_test",i,".RData"))
}
