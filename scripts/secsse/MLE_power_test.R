# i = 1
# load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/obs_ss_test",i,".RData"))

## secsse test MLE
library(ggplot2)
for (i in 1:5){
  param_data <- readr::read_csv2(paste0("G:/R/Traisie-ABC/data/secsse_ABC_test",i,".csv"))
  param_data2<-param_data[rep(seq_len(nrow(param_data)), each=10),]
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/obs_ss_test",i,".RData"))
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/MLE/test",i,"_MLE_secsse.RData"))
  set <- rep(1:100,each = 10)
  MLE_df <- data.frame(set,param_data2,MLE_all)
  MLE_df$dlam1 <- MLE_df$lam1_MLE - MLE_df$lam1
  MLE_df$dlam2 <- MLE_df$lam2_MLE - MLE_df$lam2
  MLE_df$dmu1 <- MLE_df$mu1_MLE - MLE_df$mu1
  MLE_df$dmu2 <- MLE_df$mu2_MLE - MLE_df$mu2
  MLE_df$dq12 <- MLE_df$q12_MLE - MLE_df$q12
  MLE_df$dq21 <- MLE_df$q21_MLE - MLE_df$q21
  n <- 10
  MLE_median <- aggregate(MLE_df, list(rep(1:(nrow(MLE_df) %/% n + 1), each = n, len = nrow(MLE_df))), median,na.rm = TRUE)[-1]
  whole_df_MLE <- data.frame(MLE_median,pars_ss[,7:10])
  save(whole_df_MLE,file = paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/whole_df_MLE",i,".RData"))
  # plot MLE results of each rate with true values
  p_lam1 <-ggplot2::ggplot(data = whole_df_MLE) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size, y = abs(dlam1)),
                        colour = "blue") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",lambda[1]),)) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= whole_df_MLE, aes(yintercept = 0), linetype = "dashed", size = 0.5)


  p_lam2 <-ggplot2::ggplot(data = whole_df_MLE) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size, y = abs(dlam2)),
                        colour = "blue") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",lambda[2]),)) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= whole_df_MLE, aes(yintercept = 0), linetype = "dashed", size = 0.5)

  p_mu1 <-ggplot2::ggplot(data = whole_df_MLE) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size, y = abs(dmu1)),
                        colour = "blue") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",mu[1]),)) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= whole_df_MLE, aes(yintercept = 0), linetype = "dashed", size = 0.5)


  p_mu2 <-ggplot2::ggplot(data = whole_df_MLE) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size, y = abs(dmu2)),
                        colour = "blue") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",mu[2]),)) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= whole_df_MLE, aes(yintercept = 0), linetype = "dashed", size = 0.5)

  p_q12 <-ggplot2::ggplot(data = whole_df_MLE) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size, y = abs(dq12)),
                        colour = "blue") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",q[12]),)) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= whole_df_MLE, aes(yintercept = 0), linetype = "dashed", size = 0.5)

  p_q21 <-ggplot2::ggplot(data = whole_df_MLE) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size, y = abs(dq21)),
                        colour = "blue") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",q[21]),)) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= whole_df_MLE, aes(yintercept = 0), linetype = "dashed", size = 0.5)


  tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/MLE_power_test/tree_size_set_",i,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
    align = "hv", nrow = 2, ncol = 3
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()

}


## plot tree size VS exact rate estimations
library(ggplot2)
for (i in 1:5){
  param_data <- readr::read_csv2(paste0("G:/R/Traisie-ABC/data/secsse_ABC_test",i,".csv"))
  param_data2<-param_data[rep(seq_len(nrow(param_data)), each=10),]
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/obs_ss_test",i,".RData"))
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/MLE/test",i,"_MLE_secsse.RData"))
  set <- rep(1:100,each = 10)
  MLE_df <- data.frame(set,param_data2,MLE_all)
  MLE_df$dlam1 <- MLE_df$lam1_MLE - MLE_df$lam1
  MLE_df$dlam2 <- MLE_df$lam2_MLE - MLE_df$lam2
  MLE_df$dmu1 <- MLE_df$mu1_MLE - MLE_df$mu1
  MLE_df$dmu2 <- MLE_df$mu2_MLE - MLE_df$mu2
  MLE_df$dq12 <- MLE_df$q12_MLE - MLE_df$q12
  MLE_df$dq21 <- MLE_df$q21_MLE - MLE_df$q21
  n <- 10
  MLE_median <- aggregate(MLE_df, list(rep(1:(nrow(MLE_df) %/% n + 1), each = n, len = nrow(MLE_df))), median,na.rm = TRUE)[-1]
  whole_df_MLE <- data.frame(MLE_median,pars_ss[,7:10])
  save(whole_df_MLE,file = paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/whole_df_MLE",i,".RData"))
  # plot MLE results of each rate with true values
  p_lam1 <-ggplot2::ggplot(data = whole_df_MLE) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size, y = abs(lam1_MLE)),
                        colour = "blue") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(lambda[1])) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(yintercept = whole_df_MLE$lam1[1], linetype = "dashed", size = 0.5)


  p_lam2 <-ggplot2::ggplot(data = whole_df_MLE) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size, y = abs(lam2_MLE)),
                        colour = "blue") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(lambda[2])) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(yintercept = whole_df_MLE$lam2[1], linetype = "dashed", size = 0.5)

  p_mu1 <-ggplot2::ggplot(data = whole_df_MLE) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size, y = abs(mu1_MLE)),
                        colour = "blue") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(mu[1])) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(yintercept = whole_df_MLE$mu1[1], linetype = "dashed", size = 0.5)


  p_mu2 <-ggplot2::ggplot(data = whole_df_MLE) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size, y = abs(mu2_MLE)),
                        colour = "blue") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(mu[2])) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(yintercept = whole_df_MLE$mu2[1], linetype = "dashed", size = 0.5)

  p_q12 <-ggplot2::ggplot(data = whole_df_MLE) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size, y = abs(q12_MLE)),
                        colour = "blue") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(q[12])) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(yintercept = whole_df_MLE$q12[1], linetype = "dashed", size = 0.5)

  p_q21 <-ggplot2::ggplot(data = whole_df_MLE) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size, y = abs(q21_MLE)),
                        colour = "blue") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(q[21])) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(yintercept = whole_df_MLE$q21[1], linetype = "dashed", size = 0.5)


  tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/MLE_power_test/tree_size_exact_est_set_",i,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
    align = "hv", nrow = 2, ncol = 3
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()

}



## plot tree size VS net diversification rate
library(ggplot2)
for (i in 1:5){
  param_data <- readr::read_csv2(paste0("G:/R/Traisie-ABC/data/secsse_ABC_test",i,".csv"))
  param_data2<-param_data[rep(seq_len(nrow(param_data)), each=10),]
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/obs_ss_test",i,".RData"))
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/MLE/test",i,"_MLE_secsse.RData"))
  set <- rep(1:100,each = 10)
  MLE_df <- data.frame(set,param_data2,MLE_all)
  MLE_df$dlam1 <- MLE_df$lam1_MLE - MLE_df$lam1
  MLE_df$dlam2 <- MLE_df$lam2_MLE - MLE_df$lam2
  MLE_df$dmu1 <- MLE_df$mu1_MLE - MLE_df$mu1
  MLE_df$dmu2 <- MLE_df$mu2_MLE - MLE_df$mu2
  MLE_df$dq12 <- MLE_df$q12_MLE - MLE_df$q12
  MLE_df$dq21 <- MLE_df$q21_MLE - MLE_df$q21
  MLE_df$net_div1 <- (MLE_df$lam1-MLE_df$mu1)
  MLE_df$net_div2 <- (MLE_df$lam2-MLE_df$mu2)
  MLE_df$net_div_MLE1 <- (MLE_df$lam1_MLE-MLE_df$mu1_MLE)
  MLE_df$net_div_MLE2 <- (MLE_df$lam2_MLE-MLE_df$mu2_MLE)
  n <- 10
  MLE_median <- aggregate(MLE_df, list(rep(1:(nrow(MLE_df) %/% n + 1), each = n, len = nrow(MLE_df))), median,na.rm = TRUE)[-1]
  whole_df_MLE <- data.frame(MLE_median,pars_ss[,7:10])
  # save(whole_df_MLE,file = paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/whole_df_MLE",i,".RData"))

  p_div1 <-ggplot2::ggplot(data = whole_df_MLE) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.05,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size, y = abs(net_div_MLE1)),
                        colour = "blue") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression("Net Diversification State 1")) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(yintercept = whole_df_MLE$net_div1[1], linetype = "dashed", size = 0.5)


  p_div2 <-ggplot2::ggplot(data = whole_df_MLE) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.05,1)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size, y = abs(net_div_MLE2)),
                        colour = "blue") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression("Net Diversification State 2")) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(yintercept = whole_df_MLE$net_div2[1], linetype = "dashed", size = 0.5)

  tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/MLE_power_test/tree_size_net_div_set_",i,".tiff"),
       units="px", width=2200, height=1000,res = 300,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_div1,p_div2,
    align = "hv", nrow = 1, ncol = 2
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()

}



#####
### plot exact rate estiations
library(ggplot2)
for (i in 1:5){
  param_data <- readr::read_csv2(paste0("G:/R/Traisie-ABC/data/secsse_ABC_test",i,".csv"))
  param_data2<-param_data[rep(seq_len(nrow(param_data)), each=10),]
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/obs_ss_test",i,".RData"))
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/MLE/test",i,"_MLE_secsse.RData"))
  set <- rep(1:100,each = 10)
  MLE_df <- data.frame(set,param_data2,MLE_all)
  MLE_df$dlam1 <- MLE_df$lam1_MLE - MLE_df$lam1
  MLE_df$dlam2 <- MLE_df$lam2_MLE - MLE_df$lam2
  MLE_df$dmu1 <- MLE_df$mu1_MLE - MLE_df$mu1
  MLE_df$dmu2 <- MLE_df$mu2_MLE - MLE_df$mu2
  MLE_df$dq12 <- MLE_df$q12_MLE - MLE_df$q12
  MLE_df$dq21 <- MLE_df$q21_MLE - MLE_df$q21
  n <- 10
  MLE_median <- aggregate(MLE_df, list(rep(1:(nrow(MLE_df) %/% n + 1), each = n, len = nrow(MLE_df))), median,na.rm = TRUE)[-1]
  whole_df_MLE <- data.frame(MLE_median,pars_ss[,7:10])
  save(whole_df_MLE,file = paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/whole_df_MLE",i,".RData"))
  # plot MLE results of each rate with true values
  # plot MLE results of each rate with true values
  p_lam1 <-ggplot2::ggplot(data = MLE_df) +
    ggplot2::theme_bw() +
    xlim(0,1)+
    # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_MLE),
    #                       fill = "#009E73",colour = "#009E73",
    #                       alpha = 0.3, binwidth = 0.01) +
    ggplot2::geom_density(ggplot2::aes(x = lam1_MLE),
                          fill = "royalblue",colour = "blue3",
                          alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda[1]))+
    ggplot2::geom_vline(data= MLE_df, aes(xintercept = lam1), linetype = "dashed", size = 0.5)


  p_lam2 <-ggplot2::ggplot(data = MLE_df) +
    ggplot2::theme_bw() +
    xlim(0,1)+
    # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_MLE),
    #                       fill = "#009E73",colour = "#009E73",
    #                       alpha = 0.3, binwidth = 0.01) +
    ggplot2::geom_density(ggplot2::aes(x = lam2_MLE),
                          fill = "royalblue",colour = "blue3",
                          alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda[2]))+
    ggplot2::geom_vline(data= MLE_df, aes(xintercept = lam2), linetype = "dashed", size = 0.5)


  p_mu1 <-ggplot2::ggplot(data = MLE_df) +
    ggplot2::theme_bw() +
    xlim(0,1)+
    # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_MLE),
    #                       fill = "#009E73",colour = "#009E73",
    #                       alpha = 0.3, binwidth = 0.01) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = mu1_MLE),
                          fill = "royalblue",colour = "blue3",
                          alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(mu[1]))+
    ggplot2::geom_vline(data= MLE_df, aes(xintercept = mu1), linetype = "dashed", size = 0.5)

  p_mu2 <-ggplot2::ggplot(data = MLE_df) +
    ggplot2::theme_bw() +
    xlim(0,1)+
    # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_MLE),
    #                       fill = "#009E73",colour = "#009E73",
    #                       alpha = 0.3, binwidth = 0.01) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = mu2_MLE),
                          fill = "royalblue",colour = "blue3",
                          alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(mu[2]))+
    ggplot2::geom_vline(data= MLE_df, aes(xintercept = mu2), linetype = "dashed", size = 0.5)

  p_q12 <-ggplot2::ggplot(data = MLE_df) +
    ggplot2::theme_bw() +
    xlim(0,0.5)+
    # ggplot2::geom_histogram(mapping = ggplot2::aes(x = gam_MLE),
    #                       fill = "#009E73",colour = "#009E73",
    #                       alpha = 0.3, binwidth = 0.0005) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = q12_MLE),
                          fill = "royalblue",colour = "blue3",
                          alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(q[12]))+
    ggplot2::geom_vline(data= MLE_df, aes(xintercept = q12), linetype = "dashed", size = 0.5)


  p_q21 <-ggplot2::ggplot(data = MLE_df) +
    ggplot2::theme_bw() +
    xlim(0,0.5)+
    # ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_MLE),
    #                       fill = "#009E73",colour = "#009E73",
    #                       alpha = 0.3, binwidth = 0.01) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = q21_MLE),
                          fill = "royalblue",colour = "blue3",
                          alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(q[21]))+
    ggplot2::geom_vline(data= MLE_df, aes(xintercept = q21), linetype = "dashed", size = 0.5)

  tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/MLE_power_test/MLE_est_set_",i,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
    align = "hv", nrow = 2, ncol = 3
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()

}
