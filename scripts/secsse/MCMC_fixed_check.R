## check fixed MCMC
# complete old MCMC results
for(test in c(1,5)){
  param_data <- readr::read_csv2(paste0("data/secsse_ABC_test",test,".csv"))
  param_data3<-param_data[rep(seq_len(nrow(param_data)), each=2001),] #5001
  folder_path <-paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/secsse_MCMC_test",test,"_before_fix")
  files <- list.files(folder_path)
  lam1_mcmc <- c()
  lam2_mcmc <- c()
  mu1_mcmc <- c()
  mu2_mcmc <- c()
  q12_mcmc <- c()
  q21_mcmc <- c()
  for(i in 1:100){
    # param_set = (param_num-1)*5 + i
    file_to_load <- grep(paste0("secsse_MCMC_test",test,"_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                         files,
                         value = TRUE,
                         fixed = TRUE)

    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      lam1_mcmc <- c(lam1_mcmc, output[,1])
      lam2_mcmc <- c(lam2_mcmc, output[,2])
      mu1_mcmc <- c(mu1_mcmc, output[,3])
      mu2_mcmc <- c(mu2_mcmc, output[,4])
      q12_mcmc <- c(q12_mcmc, output[,5])
      q21_mcmc <- c(q21_mcmc, output[,6])
    } else {
      lam1_mcmc <- c(lam1_mcmc, rep(NA,2001))
      lam2_mcmc <- c(lam2_mcmc, rep(NA,2001))
      mu1_mcmc <- c(mu1_mcmc, rep(NA,2001))
      mu2_mcmc <- c(mu2_mcmc, rep(NA,2001))
      q12_mcmc <- c(q12_mcmc, rep(NA,2001))
      q21_mcmc <- c(q21_mcmc, rep(NA,2001))
    }
  }

  whole_df_MCMC <- data.frame(param_data3,
                              lam1_mcmc,lam2_mcmc,
                              mu1_mcmc,mu2_mcmc,
                              q12_mcmc,q21_mcmc)
  #lac_abc,mu_abc,gam_abc,laa_abc,n_iter)
  save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/whole_df_MCMC_test",test,"_before_fix.RData"))

  whole_df_MCMC$net_div1 <- (whole_df_MCMC$lam1-whole_df_MCMC$mu1)
  whole_df_MCMC$net_div2 <- (whole_df_MCMC$lam2-whole_df_MCMC$mu2)
  whole_df_MCMC$net_div_MCMC1 <- (whole_df_MCMC$lam1_mcmc-whole_df_MCMC$mu1_mcmc)
  whole_df_MCMC$net_div_MCMC2 <- (whole_df_MCMC$lam2_mcmc-whole_df_MCMC$mu2_mcmc)

  whole_df_MCMC$ext_frac1 <- (whole_df_MCMC$mu1)/(whole_df_MCMC$lam1)
  whole_df_MCMC$ext_frac2 <- (whole_df_MCMC$mu2)/(whole_df_MCMC$lam2)
  whole_df_MCMC$ext_frac_MCMC1 <- (whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$lam1_mcmc)
  whole_df_MCMC$ext_frac_MCMC2 <- (whole_df_MCMC$mu2_mcmc)/(whole_df_MCMC$lam2_mcmc)
  save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/delta_whole_df_MCMC_test",test,"_before_fix.RData"))

  df<-whole_df_MCMC
  n <- 2001
  MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  save(MCMC_median,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/median_MCMC_test",test,"_before_fix.RData"))
}

# FIXED MCMC NEW RESULTS
# complete old MCMC results
for(test in c(1,5)){
  param_data <- readr::read_csv2(paste0("data/secsse_ABC_test",test,".csv"))
  param_data3<-param_data[rep(seq_len(nrow(param_data)), each=2001),] #5001
  folder_path <-paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/secsse_MCMC_test",test)
  files <- list.files(folder_path)
  lam1_mcmc <- c()
  lam2_mcmc <- c()
  mu1_mcmc <- c()
  mu2_mcmc <- c()
  q12_mcmc <- c()
  q21_mcmc <- c()
  for(i in 1:100){
    # param_set = (param_num-1)*5 + i
    file_to_load <- grep(paste0("secsse_MCMC_test",test,"_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                         files,
                         value = TRUE,
                         fixed = TRUE)

    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      lam1_mcmc <- c(lam1_mcmc, output[,1])
      lam2_mcmc <- c(lam2_mcmc, output[,2])
      mu1_mcmc <- c(mu1_mcmc, output[,3])
      mu2_mcmc <- c(mu2_mcmc, output[,4])
      q12_mcmc <- c(q12_mcmc, output[,5])
      q21_mcmc <- c(q21_mcmc, output[,6])
    } else {
      lam1_mcmc <- c(lam1_mcmc, rep(NA,2001))
      lam2_mcmc <- c(lam2_mcmc, rep(NA,2001))
      mu1_mcmc <- c(mu1_mcmc, rep(NA,2001))
      mu2_mcmc <- c(mu2_mcmc, rep(NA,2001))
      q12_mcmc <- c(q12_mcmc, rep(NA,2001))
      q21_mcmc <- c(q21_mcmc, rep(NA,2001))
    }
  }

  whole_df_MCMC <- data.frame(param_data3,
                              lam1_mcmc,lam2_mcmc,
                              mu1_mcmc,mu2_mcmc,
                              q12_mcmc,q21_mcmc)
  #lac_abc,mu_abc,gam_abc,laa_abc,n_iter)
  save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/whole_df_MCMC_test",test,".RData"))

  whole_df_MCMC$net_div1 <- (whole_df_MCMC$lam1-whole_df_MCMC$mu1)
  whole_df_MCMC$net_div2 <- (whole_df_MCMC$lam2-whole_df_MCMC$mu2)
  whole_df_MCMC$net_div_MCMC1 <- (whole_df_MCMC$lam1_mcmc-whole_df_MCMC$mu1_mcmc)
  whole_df_MCMC$net_div_MCMC2 <- (whole_df_MCMC$lam2_mcmc-whole_df_MCMC$mu2_mcmc)

  whole_df_MCMC$ext_frac1 <- (whole_df_MCMC$mu1)/(whole_df_MCMC$lam1)
  whole_df_MCMC$ext_frac2 <- (whole_df_MCMC$mu2)/(whole_df_MCMC$lam2)
  whole_df_MCMC$ext_frac_MCMC1 <- (whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$lam1_mcmc)
  whole_df_MCMC$ext_frac_MCMC2 <- (whole_df_MCMC$mu2_mcmc)/(whole_df_MCMC$lam2_mcmc)
  save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/delta_whole_df_MCMC_test",test,".RData"))

  df<-whole_df_MCMC
  n <- 2001
  MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  save(MCMC_median,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/median_MCMC_test",test,".RData"))
}

for(test in c(1,5)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/median_MCMC_test",test,"_before_fix.RData"))
  MCMC_median_old <- MCMC_median
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/median_MCMC_test",test,".RData"))
  MCMC_median_fixed <- MCMC_median
  colnames(MCMC_median_fixed)[7] <-"lam1_mcmc_fixed"
  colnames(MCMC_median_fixed)[8] <-"lam2_mcmc_fixed"
  colnames(MCMC_median_fixed)[9] <-"mu1_mcmc_fixed"
  colnames(MCMC_median_fixed)[10] <-"mu2_mcmc_fixed"
  colnames(MCMC_median_fixed)[11] <-"q12_mcmc_fixed"
  colnames(MCMC_median_fixed)[12] <-"q21_mcmc_fixed"
  colnames(MCMC_median_fixed)[15] <-"net_div_MCMC1_fixed"
  colnames(MCMC_median_fixed)[16] <-"net_div_MCMC2_fixed"
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/whole_df_MLE",test,".RData"))

  whole_df_MLE$net_div_MLE1 <- (whole_df_MLE$lam1_MLE-whole_df_MLE$mu1_MLE)
  whole_df_MLE$net_div_MLE2 <- (whole_df_MLE$lam2_MLE-whole_df_MLE$mu2_MLE)
  MCMC_all_df <- cbind(MCMC_median_old[1:16],
                       MCMC_median_fixed[,c(7:12,15,16)],
                       whole_df_MLE[,c(8:13,26:31)])
  save(MCMC_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/compare_MCMC_test",test,".RData"))
}

# plot tree size VS exact rate estimations
library(ggplot2)
for (test in c(1,5)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/compare_MCMC_test",test,".RData"))
  color_values <-c("MCMC" = "red3","MCMC_fixed" = "green2", "MLE" = "yellow2")
  p_lam1 <-ggplot2::ggplot(data = MCMC_all_df) +  #na.omit(MCMC_all_df)
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_mcmc_fixed),color = "MCMC_fixed"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(lambda[1]))+
    ggplot2::geom_hline(yintercept = MCMC_all_df$lam1[1], linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("MCMC", "MCMC fixed", "MLE"))

  p_lam2 <-ggplot2::ggplot(data = MCMC_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_mcmc_fixed),color = "MCMC_fixed"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(lambda[2]))+
    ggplot2::geom_hline(yintercept = MCMC_all_df$lam2[1], linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("MCMC", "MCMC fixed", "MLE"))

  p_mu1 <-ggplot2::ggplot(data = MCMC_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_mcmc_fixed),color = "MCMC_fixed"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(mu[1]))+
    ggplot2::geom_hline(yintercept = MCMC_all_df$mu1[1], linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("MCMC", "MCMC fixed", "MLE"))

  p_mu2 <-ggplot2::ggplot(data = MCMC_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_mcmc_fixed),color = "MCMC_fixed"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(mu[2]))+
    ggplot2::geom_hline(yintercept = MCMC_all_df$mu2[1], linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("MCMC", "MCMC fixed", "MLE"))

  p_q12 <-ggplot2::ggplot(data = MCMC_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_mcmc_fixed),color = "MCMC_fixed"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(q[12]))+
    ggplot2::geom_hline(yintercept = MCMC_all_df$q12[1], linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("MCMC", "MCMC fixed", "MLE"))

  p_q21 <-ggplot2::ggplot(data = MCMC_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_mcmc_fixed),color = "MCMC_fixed"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(q[21]))+
    ggplot2::geom_hline(yintercept = MCMC_all_df$q21[1], linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("MCMC", "MCMC fixed", "MLE"))

  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/MCMC_fix",test,".tiff"),
       units="px", width=3000, height=1500,res = 300,compression="lzw")
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

# add ABC results

library(ggplot2)
for (test in c(1,5)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/compare_MCMC_test",test,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new2/ABC_rep1/delta_whole_df_ABC_test",test,".RData"))
  df <- whole_df_ABC
  n <- 250
  ABC_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  MCMC_all_df<-data.frame(MCMC_all_df,ABC_median[,c(8:13,16:17)])
  color_values <-c("ABC" = "blue2","MCMC" = "red3","MCMC_fixed" = "green2", "MLE" = "yellow2")
  p_lam1 <-ggplot2::ggplot(data = MCMC_all_df) +  #na.omit(MCMC_all_df)
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_mcmc_fixed),color = "MCMC_fixed"),shape = 18) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_abc),color = "ABC"),shape = 4, alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(lambda[1]))+
    ggplot2::geom_hline(yintercept = MCMC_all_df$lam1[1], linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MCMC fixed", "MLE"))

  p_lam2 <-ggplot2::ggplot(data = MCMC_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_mcmc_fixed),color = "MCMC_fixed"),shape = 18) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_abc),color = "ABC"),shape = 4, alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(lambda[2]))+
    ggplot2::geom_hline(yintercept = MCMC_all_df$lam2[1], linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MCMC fixed", "MLE"))

  p_mu1 <-ggplot2::ggplot(data = MCMC_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_mcmc_fixed),color = "MCMC_fixed"),shape = 18) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_abc),color = "ABC"),shape = 4, alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(mu[1]))+
    ggplot2::geom_hline(yintercept = MCMC_all_df$mu1[1], linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MCMC fixed", "MLE"))

  p_mu2 <-ggplot2::ggplot(data = MCMC_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_mcmc_fixed),color = "MCMC_fixed"),shape = 18) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_abc),color = "ABC"),shape = 4, alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(mu[2]))+
    ggplot2::geom_hline(yintercept = MCMC_all_df$mu2[1], linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MCMC fixed", "MLE"))

  p_q12 <-ggplot2::ggplot(data = MCMC_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_mcmc_fixed),color = "MCMC_fixed"),shape = 18) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_abc),color = "ABC"),shape = 4, alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(q[12]))+
    ggplot2::geom_hline(yintercept = MCMC_all_df$q12[1], linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MCMC fixed", "MLE"))

  p_q21 <-ggplot2::ggplot(data = MCMC_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_mcmc_fixed),color = "MCMC_fixed"),shape = 18) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_abc),color = "ABC"),shape = 4, alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(q[21]))+
    ggplot2::geom_hline(yintercept = MCMC_all_df$q21[1], linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MCMC fixed", "MLE"))

  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/AMM_rep1_fix",test,".tiff"),
       units="px", width=3000, height=1500,res = 300,compression="lzw")
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


for(test in c(1,5)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/compare_MCMC_test",test,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new2/ABC_rep1/delta_whole_df_ABC_test",test,".RData"))
  df <- whole_df_ABC
  n <- 250
  ABC_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  MCMC_all_df<-data.frame(MCMC_all_df,ABC_median[,c(8:13,16:17)])
  color_values <-c("ABC" = "blue2","MCMC" = "red3","MCMC_fixed" = "green2", "MLE" = "yellow2")
  p_div1 <-ggplot2::ggplot(data = MCMC_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.05,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MLE1),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MCMC1),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MCMC1_fixed),color = "MCMC_fixed"),shape = 18) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_ABC1),color = "ABC"),shape = 4, alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression("Net Diversification State 1")) +
    ggplot2::xlab("Tree size")+
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MCMC fixed", "MLE"))+
    ggplot2::geom_hline(yintercept = MCMC_all_df$net_div1[1], linetype = "dashed", size = 0.5)


  p_div2 <-ggplot2::ggplot(data = MCMC_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.05,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MLE2),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MCMC2),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MCMC2_fixed),color = "MCMC_fixed"),shape = 18) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_ABC2),color = "ABC"),shape = 4, alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression("Net Diversification State 2")) +
    ggplot2::xlab("Tree size")+
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MCMC fixed", "MLE"))+
    ggplot2::geom_hline(yintercept = MCMC_all_df$net_div2[1], linetype = "dashed", size = 0.5)

  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/net_div_set_",test,".tiff"),
       units="px", width=2500, height=1000,res = 300,compression="lzw")
  params <- cowplot::plot_grid(
    p_div1+ggplot2::theme(legend.position = "none"),
    p_div2+ggplot2::theme(legend.position = "none"),
    align = "hv", nrow = 1, ncol = 2
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


test = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round4/adap_secsse_test3_new3/compare_MCMC_test",test,".RData"))


