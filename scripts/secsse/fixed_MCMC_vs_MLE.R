# this script only check the fixed MCMC vs MLE

for(test in c(1,3,5,6)){
  param_data <- readr::read_csv2(paste0("data/secsse_ABC_test",test,".csv"))
  param_data3<-param_data[rep(seq_len(nrow(param_data)), each=2001),] #5001
  folder_path_mcmc <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/secsse_MCMC_test",test)
  files_mcmc <- list.files(folder_path_mcmc)
  lam1_mcmc <- c()
  lam2_mcmc <- c()
  mu1_mcmc <- c()
  mu2_mcmc <- c()
  q12_mcmc <- c()
  q21_mcmc <- c()
  for(i in 1:100){
      file_to_load_mcmc <- grep(paste0("secsse_MCMC_test",test,"_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                                files_mcmc,
                                value = TRUE,
                                fixed = TRUE)

      if (!identical(file_to_load_mcmc, character())) {
        load(file.path(folder_path_mcmc, file_to_load_mcmc))
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

  save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/whole_df_MCMC_test",test,".RData"))

  whole_df_MCMC$net_div1 <- (whole_df_MCMC$lam1-whole_df_MCMC$mu1)
  whole_df_MCMC$net_div2 <- (whole_df_MCMC$lam2-whole_df_MCMC$mu2)
  whole_df_MCMC$net_div_MCMC1 <- (whole_df_MCMC$lam1_mcmc-whole_df_MCMC$mu1_mcmc)
  whole_df_MCMC$net_div_MCMC2 <- (whole_df_MCMC$lam2_mcmc-whole_df_MCMC$mu2_mcmc)

  whole_df_MCMC$ext_frac1 <- (whole_df_MCMC$mu1)/(whole_df_MCMC$lam1)
  whole_df_MCMC$ext_frac2 <- (whole_df_MCMC$mu2)/(whole_df_MCMC$lam2)
  whole_df_MCMC$ext_frac_MCMC1 <- (whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$lam1_mcmc)
  whole_df_MCMC$ext_frac_MCMC2 <- (whole_df_MCMC$mu2_mcmc)/(whole_df_MCMC$lam2_mcmc)
  save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/delta_whole_df_MCMC_test",test,".RData"))
}

######

for(test in c(1,3,5,6)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/delta_whole_df_MCMC_test",test,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/whole_df_MLE",test,".RData"))

  ## get number of iterations and mean values

  df<-whole_df_MCMC
  n <- 2001
  MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

  MLE_median <- whole_df_MLE


  ## combine ABC MCMC MLE as "AMM"
  AMM_all_df <- cbind(MCMC_median[,c(1:12,15,16,19,20)],
                      MLE_median[,c(7:12,20:23)])
  save(AMM_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/MM_per_set_test",test,".RData"))

  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/MM_per_set_test",test,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/obs_ss_test",test,".RData"))

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


  save(AMM_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/AMM_per_set_drate_test",test,".RData"))
}


### plot
library(ggplot2)
for(test in c(1,3,5,6)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/AMM_per_set_drate_test",test,".RData"))
  color_values <-c("MCMC" = "green2", "MLE" = "yellow2")
  p_lam1 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(lambda[1]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$lam1[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("MCMC", "MLE"))

  p_lam2 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(lambda[2]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$lam2[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("MCMC", "MLE"))

  p_mu1 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(mu[1]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$mu1[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("MCMC", "MLE"))

  p_mu2 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(mu[2]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$mu2[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("MCMC", "MLE"))

  p_q12 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(q[12]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$q12[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("MCMC", "MLE"))

  p_q21 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(q[21]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$q21[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("MCMC", "MLE"))

  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/exact_rate_test",test,".tiff"),
       units="px", width=2000, height=1500,res = 300,compression="lzw")
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
