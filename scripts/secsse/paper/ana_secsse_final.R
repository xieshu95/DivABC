# 1. formate ABC results  analyze secsse_7
## check new secsse ABC result
for (num_ss in c(1)){
  # formate results
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/obs_ss_test.rda"))
  ## ABC results
  folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/secsse_ABC_test")
  files <- list.files(folder_path)
  param_data <- load_param_space(param_space_name = paste0("secsse_ABC_test"))
  param_data2<-param_data[rep(seq_len(nrow(param_data)), each=500),] #500
  lam1_abc <- c()
  lam2_abc <- c()
  mu1_abc <- c()
  mu2_abc <- c()
  q12_abc <- c()
  q21_abc <- c()
  n_iter <- c()
  n_iteration <- c()
  for(i in 1:350){
    file_to_load <- grep(paste0("secsse_ABC_test_param_set_",i,"_ss_",num_ss,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      if(output$n_iter <= 2){
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
  save(whole_df_ABC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/whole_df_ABC_test_ss",num_ss,".RData"))

  # whole_df_ABC$dlam <- (whole_df_ABC$lam2-whole_df_ABC$lam1)/(whole_df_ABC$lam2+whole_df_ABC$lam1)
  # whole_df_ABC$dlam_ABC <- (whole_df_ABC$lam2_ABC-whole_df_ABC$lam1_ABC)/(whole_df_ABC$lam2_ABC+whole_df_ABC$lam1_ABC)
  # whole_df_ABC$dmu <- (whole_df_ABC$mu2-whole_df_ABC$mu1)/(whole_df_ABC$mu2+whole_df_ABC$mu1)
  # whole_df_ABC$dmu_ABC <- (whole_df_ABC$mu2_ABC-whole_df_ABC$mu1_ABC)/(whole_df_ABC$mu2_ABC+whole_df_ABC$mu1_ABC)
  # whole_df_ABC$dq <- (whole_df_ABC$q12-whole_df_ABC$q21)/(whole_df_ABC$q12+whole_df_ABC$q21)
  # whole_df_ABC$dq_ABC <- (whole_df_ABC$q12_ABC-whole_df_ABC$q21_ABC)/(whole_df_ABC$q12_ABC+whole_df_ABC$q21_ABC)

  whole_df_ABC$net_div1 <- (whole_df_ABC$lam1-whole_df_ABC$mu1)
  whole_df_ABC$net_div2 <- (whole_df_ABC$lam2-whole_df_ABC$mu2)
  whole_df_ABC$net_div_ABC1 <- (whole_df_ABC$lam1_abc-whole_df_ABC$mu1_abc)
  whole_df_ABC$net_div_ABC2 <- (whole_df_ABC$lam2_abc-whole_df_ABC$mu2_abc)


  whole_df_ABC$ext_frac1 <- (whole_df_ABC$mu1)/(whole_df_ABC$lam1)
  whole_df_ABC$ext_frac2 <- (whole_df_ABC$mu2)/(whole_df_ABC$lam2)
  whole_df_ABC$ext_frac_ABC1 <- (whole_df_ABC$mu1_abc)/(whole_df_ABC$lam1_abc)
  whole_df_ABC$ext_frac_ABC2 <- (whole_df_ABC$mu2_abc)/(whole_df_ABC$lam2_abc)
  save(whole_df_ABC,file =
         paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/delta_whole_df_ABC_test_ss",num_ss,".RData"))

}


######
# 2. formate MCMC results (only plot the estimation points with ABC results)
# skip
param_data <- load_param_space(param_space_name = paste0("secsse_ABC_test"))
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=2501),] #2501
folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/secsse_MCMC_test")
files <- list.files(folder_path)
lam1_mcmc <- c()
lam2_mcmc <- c()
mu1_mcmc <- c()
mu2_mcmc <- c()
q12_mcmc <- c()
q21_mcmc <- c()
seq <- seq(1,5001,2)
for(i in 1:350){
  file_to_load <- grep(paste0("secsse_MCMC_test_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lam1_mcmc <- c(lam1_mcmc, output[seq,1]) #4502:2501
    lam2_mcmc <- c(lam2_mcmc, output[seq,2])
    mu1_mcmc <- c(mu1_mcmc, output[seq,3])
    mu2_mcmc <- c(mu2_mcmc, output[seq,4])
    q12_mcmc <- c(q12_mcmc, output[seq,5])
    q21_mcmc <- c(q21_mcmc, output[seq,6])
  } else {
    lam1_mcmc <- c(lam1_mcmc, rep(NA,2501)) #500
    lam2_mcmc <- c(lam2_mcmc, rep(NA,2501))
    mu1_mcmc <- c(mu1_mcmc, rep(NA,2501))
    mu2_mcmc <- c(mu2_mcmc, rep(NA,2501))
    q12_mcmc <- c(q12_mcmc, rep(NA,2501))
    q21_mcmc <- c(q21_mcmc, rep(NA,2501))
  }
}
whole_df_MCMC <- data.frame(param_data3,
                            lam1_mcmc,lam2_mcmc,
                            mu1_mcmc,mu2_mcmc,
                            q12_mcmc,q21_mcmc)

save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/whole_df_MCMC_test.RData"))

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

save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/delta_whole_df_MCMC_test.RData"))

# install.packages("bayesplot")
library(bayesplot)
# plot MCMC trace
# load("G:/results/project 2/tip_info/round4/secsse_long_2/secsse_MCMC_long/secsse_MCMC_long_param_set_1_ss_1.RData")
folder_path <-  paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/secsse_MCMC_test")
files <- list.files(folder_path)
for(i in 1:350){
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("secsse_MCMC_test_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)

  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/MCMC_trace/set_",i,".tiff"),
         units="px", width=2000, height=3000,res = 300,compression="lzw")
    b_mcmc <- coda::as.mcmc(output[,1:6])
    colnames(b_mcmc) <- c("Speciation 1","Speciation 2","Extinction 1","Extinction 2", "Transition 12","Transition 21")
    plot_mcmc <- plot(b_mcmc,trace = T, density = F)
    print(plot_mcmc)
    while (!is.null(dev.list()))  dev.off()
  }
}




######


# 3. formate MLE results
# skip
param_data <- load_param_space(param_space_name = paste0("secsse_ABC_test"))
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/obs_ss_test.rda"))
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/test_MLE_secsse.RData"))
whole_df_MLE <- data.frame(param_data,MLE_all,ss[,1:4])
save(whole_df_MLE,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/whole_df_MLE.RData"))



## median
for (num_ss in c(1)){
  # load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_init_new/delta_whole_df_init_test.RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/delta_whole_df_ABC_test_ss",num_ss,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/delta_whole_df_MCMC_test.RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/whole_df_MLE.RData"))

  ## get number of iterations and mean values
  df <- whole_df_ABC
  n <- 500
  ABC_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

  df<-whole_df_MCMC
  n <- 2501
  MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

  MLE_median <- whole_df_MLE


  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/obs_ss_test.rda"))
  ## combine ABC MCMC MLE as "AMM"
  AMM_all_df <- cbind(ABC_median[1:21],
                      MCMC_median[,c(7:12,15,16,19,20)],
                      MLE_median[,c(7:12,20:23)])
  save(AMM_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/AMM_per_set_test_ss",num_ss,".RData"))

  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/AMM_per_set_test_ss",num_ss,".RData"))
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

  save(AMM_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/AMM_per_set_drate_test_ss",num_ss,".RData"))
}



## run\
library(ggplot2)
for(num_ss in c(1)){
  for(i in 1:7){
    load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/AMM_per_set_drate_test_ss",num_ss,".RData"))
    color_values <-c("ABC" = "#b81f25" ,"MCMC" = "#EFC000", "MLE" = "#4daf4a")
    AMM <- AMM_all_df[(i*50-49):(i*50),]
    p_lam1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,1.05)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_abc),color = "ABC"),shape = 18,alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Total species richness") +
      ggplot2::ylab(expression(lambda[0]))+
      ggplot2::geom_hline(yintercept = AMM$lam1[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_lam2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,1.05)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_abc),color = "ABC"),shape = 18,alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Total species richness") +
      ggplot2::ylab(expression(lambda[1]))+
      ggplot2::geom_hline(yintercept = AMM$lam2[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_mu1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,1.05)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_abc),color = "ABC"),shape = 18,alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Total species richness") +
      ggplot2::ylab(expression(mu[0]))+
      ggplot2::geom_hline(yintercept = AMM$mu1[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_mu2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,1.05)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_abc),color = "ABC"),shape = 18,alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Total species richness") +
      ggplot2::ylab(expression(mu[1]))+
      ggplot2::geom_hline(yintercept = AMM$mu2[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_q12 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,1.05)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_abc),color = "ABC"),shape = 18,alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Total species richness") +
      ggplot2::ylab(expression(q[01]))+
      ggplot2::geom_hline(yintercept = AMM$q12[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_q21 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,1.05)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_abc),color = "ABC"),shape = 18,alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Total species richness") +
      ggplot2::ylab(expression(q[10]))+
      ggplot2::geom_hline(yintercept = AMM$q21[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_div1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.05)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MLE1),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MCMC1),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_ABC1),color = "ABC"),shape = 18, alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 11),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::ylab(expression("Net Diversification 0")) +
      ggplot2::xlab("Total species richness")+
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))+
      ggplot2::geom_hline(yintercept = AMM$net_div1[1], linetype = "dashed", size = 0.5)


    p_div2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.05)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MLE2),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MCMC2),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_ABC2),color = "ABC"),shape = 18,alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 11),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::ylab(expression("Net Diversification 1")) +
      ggplot2::xlab("Total species richness")+
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))+
      ggplot2::geom_hline(yintercept = AMM$net_div2[1], linetype = "dashed", size = 0.5)

    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/exact_rate_test",i,"_ss",num_ss,".tiff"),
         units="px", width=5500, height=2000,res = 400,compression="lzw")
    params <- cowplot::plot_grid(
      p_lam1+ggplot2::theme(legend.position = "none"),
      p_mu1+ggplot2::theme(legend.position = "none"),
      p_q12+ggplot2::theme(legend.position = "none"),
      p_div1+ggplot2::theme(legend.position = "none"),
      p_lam2+ggplot2::theme(legend.position = "none"),
      p_mu2+ggplot2::theme(legend.position = "none"),
      p_q21+ggplot2::theme(legend.position = "none"),
      p_div2+ggplot2::theme(legend.position = "none"),
      align = "hv", nrow = 2, ncol = 4
    )
    legend <- cowplot::get_legend(
      p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
    )
    param_estimates <- cowplot::plot_grid(params,legend,
                                          rel_widths = c(3,0.4)
    )
    param_estimates <- cowplot::add_sub(param_estimates, "Total species richness", hjust = 1)
    print(cowplot::ggdraw(param_estimates))
    while (!is.null(dev.list()))  dev.off()
  }
}


# ## net div
# for(num_ss in c(1)) {
#   for(i in c(2)){
#     load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/AMM_per_set_drate_test_ss",num_ss,".RData"))
#     color_values <-c("ABC" = "#b81f25" ,"MCMC" = "#EFC000", "MLE" = "#4daf4a")
#     # color_values <-c("ABC" = "#e41a1c" ,"MCMC" = "#377eb8", "MLE" = "#8CC269")
#     # color_values <-c("ABC" = "#e41a1c" ,"MCMC" = "#f6b57b", "MLE" = "#2b9fc9")
#
#     # color_values <-c("ABC" = "#39489f","MCMC" = "#f9ed36", "MLE" = "#39bbec")   "#f38466" "#b81f25" "#eebabb" "#4CB391" "#D85356" ,"MCMC" = "#EFC000", "MLE" = "#0073C2"
#     AMM <- AMM_all_df[(i*50-49):(i*50),]
#     p_div1 <-ggplot2::ggplot(data = AMM) +
#       ggplot2::theme_bw() +
#       ggplot2::ylim(-0.1,1.3)+
#       ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MLE1),color = "MLE"),alpha = 0.6) +
#       ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MCMC1),color = "MCMC"),shape = 17,alpha = 0.6) +
#       ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_ABC1),color = "ABC"),shape = 18, alpha = 0.6) +
#       ggplot2::theme_classic() +
#       ggplot2::theme(title = ggplot2::element_text(size = 12),
#                      text = ggplot2::element_text(size = 12)) +
#       ggplot2::ylab(expression("Net Diversification State 1")) +
#       ggplot2::xlab("Total species richness")+
#       ggplot2::scale_color_manual(name = "Method",
#                                   values = color_values,
#                                   labels = c("ABC", "MCMC", "MLE"))+
#       ggplot2::geom_hline(yintercept = AMM$net_div1[1], linetype = "dashed", size = 0.5)
#
#
#     p_div2 <-ggplot2::ggplot(data = AMM) +
#       ggplot2::theme_bw() +
#       ggplot2::ylim(-0.1,1.3)+
#       ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MLE2),color = "MLE"),alpha = 0.9) +
#       ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MCMC2),color = "MCMC"),shape = 17,alpha = 0.8) +
#       ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_ABC2),color = "ABC"),shape = 18, alpha = 0.8) +
#       ggplot2::theme_classic() +
#       ggplot2::theme(title = ggplot2::element_text(size = 12),
#                      text = ggplot2::element_text(size = 12)) +
#       ggplot2::ylab(expression("Net Diversification State 2")) +
#       ggplot2::xlab("Total species richness")+
#       ggplot2::scale_color_manual(name = "Method",
#                                   values = color_values,
#                                   labels = c("ABC", "MCMC", "MLE"))+
#       ggplot2::geom_hline(yintercept = AMM$net_div2[1], linetype = "dashed", size = 0.5)
#
#     tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/net_div_set",i,"_ss",num_ss,".tiff"),
#          units="px", width=2200, height=1000,res = 400,compression="lzw")
#     param_estimates <- cowplot::plot_grid(
#       p_div1,p_div2,
#       align = "hv", nrow = 1, ncol = 2
#     )
#     print(param_estimates)
#
#     params <- cowplot::plot_grid(
#       p_div1+ggplot2::theme(legend.position = "none"),
#       p_div2+ggplot2::theme(legend.position = "none"),
#       align = "hv", nrow = 1, ncol = 2
#     )
#     legend <- cowplot::get_legend(
#       p_div1 + theme(legend.box.margin = margin(0, 0, 0, 6))
#     )
#     param_estimates <- cowplot::plot_grid(params,legend,
#                                           rel_widths = c(3,0.5)
#     )
#     print(param_estimates)
#     while (!is.null(dev.list()))  dev.off()
#   }
# }


## plot each rep
library(ggplot2)
for(i in 1:7){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/whole_df_MLE.RData"))
  whole_df_MLE <- whole_df_MLE[(i*50-49):(i*50),]
  total <- whole_df_MLE$tree_size

  ss = "ABC"
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/delta_whole_df_ABC_test_ss1.RData"))
  whole_df_ABC <- whole_df_ABC[(i*25000-24999):(i*25000),] ## whole_df_ABC[(i*20000-19999):(i*20000),]
  whole_df_ABC$ss = "ABC"
  whole_df_ABC = whole_df_ABC[,-7]
  whole_df_ABC$total <- rep(total, each = 500)

  # whole_df_ABC <- rbind(whole_df_ABC_old,whole_df_ABC_new) #whole_df_ABC_20
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


  df <- whole_df_ABC
  n <- 500
  ABC_median <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median$ss = "ABC"

  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/delta_whole_df_MCMC_test.RData"))
  whole_df_MCMC <- whole_df_MCMC[(i*50050-50049):(i*50050),]
  whole_df_MCMC$ss = "MCMC"
  whole_df_MCMC$total <- rep(total, each = 1001) #2001
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
  whole_df_MCMC$rep <- rep(rep(1:50, each = 1001), 1)


  df<-whole_df_MCMC
  n <- 1001
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


  whole_df_all <- rbind(whole_df_ABC[,c(1:6,13,14,17,18,21:33)],
                        whole_df_MCMC[,c(1:6,13,14,17,18,21:33)],
                        whole_df_MLE[,c(1:6,30,31,34,35,38,39,24:29,40:44)])
  save(whole_df_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/whole_df_all_AMM_test",i,".RData"))

  median_all <- rbind(ABC_median[,c(1:6,13,14,17,18,21:33)],
                      MCMC_median[,c(1:6,13,14,17,18,21:33)],
                      whole_df_MLE[,c(1:6,30,31,34,35,38,39,24:29,40:44)])

  save(median_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/median_AMM_test",i,".RData"))


}

## sum_stat each replicate
iqr = function(z, lower = 0.05, upper = 0.95) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

# lam1_names <- c(
#   `0.1` = 'lambda[1]~"="~0.1',
#   `0.2` = 'lambda[1]~"="~0.2',
#   `0.3` = 'lambda[1]~"="~0.3'
# )
#
# lam2_names <- c(
#   `0.3` = 'lambda[2]~"="~0.3',
#   `0.4` = 'lambda[2]~"="~0.4',
#   `0.5` = 'lambda[2]~"="~0.5'
# )
#
# mu1_names <- c(
#   `0.05` = 'mu[1]~"="~0.05'
# )
#
# mu1_names <- c(
#   `0.01` = 'mu[1]~"="~0.05',
#   `0.05` = 'mu[1]~"="~0.3',
#   `0.1` = 'mu[1]~"="~0'
# )
#
# gam_names <- c(
#   `0.003` = 'gamma~"="~0.003',
#   `0.009` = 'gamma~"="~0.009'
# )
#
# laa_names <- c(
#   `0.1` = 'lambda^a~"="~0.1',
#   `1` = 'lambda^a~"="~1.0'
# )



# for (i in 1:2){
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/whole_df_all_AMM_test",i,".RData"))
#   p_netdiv1 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dnet_div1,y = ss,color = ss)) +
#     ggplot2::stat_summary(fun.data = iqr,alpha = 0.8) +
#     # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
#     ggplot2::theme_bw() +
#     ggplot2::theme_classic() +
#     ggplot2::xlim(-0.5,0.5)+
#     # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#     ggplot2::scale_colour_manual("Method",values = c("#CD534C","#EFC000","#0073C2"))+
#     ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
#                    text = ggplot2::element_text(size = 14,colour = "black"),
#                    strip.text = element_text(size = 14,colour = "black")) +
#     ggplot2::xlab(expression(Delta~Net~diversification~1))+
#     ggplot2::ylab("Method") +
#     ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
#     facet_wrap(~ rep,ncol = 10)
#   tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/each_rep_plots/scen",i,"_drate_net_div1.tiff"),
#        units="px", width=5000, height=3000,res = 320,compression="lzw")
#   print(p_netdiv1)
#   while (!is.null(dev.list()))  dev.off()
#
#
#   p_netdiv2 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dnet_div2,y = ss,color = ss)) +
#     ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
#     # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
#     ggplot2::theme_bw() +
#     ggplot2::theme_classic() +
#     ggplot2::xlim(-1,1)+
#     # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#     ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#     ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
#                    text = ggplot2::element_text(size = 14,colour = "black"),
#                    strip.text = element_text(size = 14,colour = "black")) +
#     ggplot2::xlab(expression(Delta~Net~diversification~2))+
#     ggplot2::ylab("Method") +
#     ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
#     facet_wrap(~ rep,ncol = 10)
#   tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/each_rep_plots/scen",i,"_drate_net_div2.tiff"),
#        units="px", width=5000, height=3000,res = 320,compression="lzw")
#   print(p_netdiv2)
#   while (!is.null(dev.list()))  dev.off()
#
#
#
#   p_lam1 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlam1,y = ss,color = ss)) +
#     ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
#     # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
#     ggplot2::theme_bw() +
#     ggplot2::theme_classic() +
#     ggplot2::xlim(-1,1)+
#     # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#     ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#     ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
#                    text = ggplot2::element_text(size = 14,colour = "black"),
#                    strip.text = element_text(size = 14,colour = "black")) +
#     ggplot2::xlab(expression(Delta~lambda[1]))+
#     ggplot2::ylab("Method") +
#     ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
#     facet_wrap(~ rep,ncol = 10)
#   tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/each_rep_plots/scen",i,"_drate_lam1.tiff"),
#        units="px", width=5000, height=3000,res = 320,compression="lzw")
#   print(p_lam1)
#   while (!is.null(dev.list()))  dev.off()
#
#
#   p_lam2 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlam2,y = ss,color = ss)) +
#     ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
#     # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
#     ggplot2::theme_bw() +
#     ggplot2::theme_classic() +
#     ggplot2::xlim(-1,1)+
#     # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#     ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#     ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
#                    text = ggplot2::element_text(size = 14,colour = "black"),
#                    strip.text = element_text(size = 14,colour = "black")) +
#     ggplot2::xlab(expression(Delta~lambda[2]))+
#     ggplot2::ylab("Method") +
#     ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
#     facet_wrap(~ rep,ncol = 10)
#   tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/each_rep_plots/scen",i,"_drate_lam2.tiff"),
#        units="px", width=5000, height=3000,res = 320,compression="lzw")
#   print(p_lam2)
#   while (!is.null(dev.list()))  dev.off()
#
#   p_mu1<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dmu1,y = ss,color = ss)) +
#     ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
#     # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
#     ggplot2::theme_bw() +
#     ggplot2::theme_classic() +
#     ggplot2::xlim(-0.2,1)+
#     # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#     ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#     ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
#                    text = ggplot2::element_text(size = 14,colour = "black"),
#                    strip.text = element_text(size = 14,colour = "black")) +
#     ggplot2::xlab(expression(Delta~mu[1]))+
#     ggplot2::ylab("Method") +
#     ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
#     facet_wrap(~ rep,ncol = 10)
#   tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/each_rep_plots/scen",i,"_drate_mu1.tiff"),
#        units="px", width=5000, height=3000,res = 320,compression="lzw")
#   print(p_mu1)
#   while (!is.null(dev.list()))  dev.off()
#
#   p_mu2<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dmu2,y = ss,color = ss)) +
#     ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
#     # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
#     ggplot2::theme_bw() +
#     ggplot2::theme_classic() +
#     ggplot2::xlim(-0.2,1)+
#     # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#     ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#     ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
#                    text = ggplot2::element_text(size = 14,colour = "black"),
#                    strip.text = element_text(size = 14,colour = "black")) +
#     ggplot2::xlab(expression(Delta~mu[2]))+
#     ggplot2::ylab("Method") +
#     ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
#     facet_wrap(~ rep,ncol = 10)
#   tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/each_rep_plots/scen",i,"_drate_mu2.tiff"),
#        units="px", width=5000, height=3000,res = 320,compression="lzw")
#   print(p_mu2)
#   while (!is.null(dev.list()))  dev.off()
#
#
#   p_q12<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dq12,y = ss,color = ss)) +
#     ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
#     # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
#     ggplot2::theme_bw() +
#     ggplot2::theme_classic() +
#     ggplot2::xlim(-0.2,1)+
#     # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#     ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#     ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
#                    text = ggplot2::element_text(size = 14,colour = "black"),
#                    strip.text = element_text(size = 14,colour = "black")) +
#     ggplot2::xlab(expression(Delta~q[12]))+
#     ggplot2::ylab("Method") +
#     ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
#     facet_wrap(~ rep,ncol = 10)
#   tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/each_rep_plots/scen",i,"_drate_q12.tiff"),
#        units="px", width=5000, height=3000,res = 320,compression="lzw")
#   print(p_q12)
#   while (!is.null(dev.list()))  dev.off()
#
#
#   p_q21<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dq21,y = ss,color = ss)) +
#     ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
#     # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
#     ggplot2::theme_bw() +
#     ggplot2::theme_classic() +
#     ggplot2::xlim(-0.2,1)+
#     # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#     ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#     ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
#                    text = ggplot2::element_text(size = 14,colour = "black"),
#                    strip.text = element_text(size = 14,colour = "black")) +
#     ggplot2::xlab(expression(Delta~q[21]))+
#     ggplot2::ylab("Method") +
#     ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
#     facet_wrap(~ rep,ncol = 10)
#   tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_7/each_rep_plots/scen",i,"_drate_q21.tiff"),
#        units="px", width=5000, height=3000,res = 320,compression="lzw")
#   print(p_q21)
#   while (!is.null(dev.list()))  dev.off()
#
# }


