param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC.csv")
# MLE_all<-round(MLE_all,5)


#####
# plot MLE accuracy
library(ggplot2)
for (set in 1:6){
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE_power_test/MLE/MLE_secsse_test",set,".RData"))
  MLE_all <- MLE_test_list$MLE_all
  param <- param_data[rep(set,500),]
  MLE_all <- data.frame(param, MLE_all)

  # plot MLE results of each rate with true values
  p_lam1 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    xlim(0,0.6)+
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
    ggplot2::geom_vline(data= MLE_all, aes(xintercept = lam1), linetype = "dashed", size = 0.5)


  p_lam2 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    xlim(0,0.6)+
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
    ggplot2::geom_vline(data= MLE_all, aes(xintercept = lam2), linetype = "dashed", size = 0.5)


  p_mu1 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    xlim(0,0.15)+
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
    ggplot2::geom_vline(data= MLE_all, aes(xintercept = mu1), linetype = "dashed", size = 0.5)

  p_mu2 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    xlim(0,0.15)+
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
    ggplot2::geom_vline(data= MLE_all, aes(xintercept = mu2), linetype = "dashed", size = 0.5)

  p_q12 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    xlim(0,0.4)+
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
    ggplot2::geom_vline(data= MLE_all, aes(xintercept = q12), linetype = "dashed", size = 0.5)


  p_q21 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    xlim(0,0.4)+
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
    ggplot2::geom_vline(data= MLE_all, aes(xintercept = q21), linetype = "dashed", size = 0.5)

  tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE_power_test/MLE_test_set_",set,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
    align = "hv", nrow = 2, ncol = 3
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()
}


#####
# plot correlation between accuracy and tip ratio
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC.csv")
library(ggplot2)
for (set in 1:6){
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE_power_test/MLE/MLE_secsse_test",set,".RData"))
  MLE_all <- MLE_test_list$MLE_all
  param <- param_data[rep(set,500),]
  MLE_all <- data.frame(param, MLE_all)
  MLE_all$dlam1 <- MLE_all$lam1_MLE - MLE_all$lam1
  MLE_all$dlam2 <- MLE_all$lam2_MLE - MLE_all$lam2
  MLE_all$dmu1 <- MLE_all$mu1_MLE - MLE_all$mu1
  MLE_all$dmu2 <- MLE_all$mu2_MLE - MLE_all$mu2
  MLE_all$dq12 <- MLE_all$q12_MLE - MLE_all$q12
  MLE_all$dq21 <- MLE_all$q21_MLE - MLE_all$q21
  MLE_all$tip_ratio1 <- MLE_all$state2/MLE_all$state1
  MLE_all$tip_ratio <- MLE_all$tip_ratio1
  MLE_all$tip_ratio[MLE_all$tip_ratio < 1]<- 1/MLE_all$tip_ratio[MLE_all$tip_ratio < 1]

  # plot MLE results of each rate with true values
  p_lam1 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::geom_point(ggplot2::aes(x = tip_ratio, y = abs(dlam1)),
                          colour = "blue3") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",lambda[1]),)) +
    ggplot2::xlab("Tip ratio")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)


  p_lam2 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::geom_point(ggplot2::aes(x = tip_ratio, y = abs(dlam2)),
                        colour = "blue3") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",lambda[2]),)) +
    ggplot2::xlab("Tip ratio")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)

  p_mu1 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::geom_point(ggplot2::aes(x = tip_ratio, y = abs(dmu1)),
                        colour = "blue3") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",mu[1]),)) +
    ggplot2::xlab("Tip ratio")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)


  p_mu2 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::geom_point(ggplot2::aes(x = tip_ratio, y = abs(dmu2)),
                        colour = "blue3") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",mu[2]),)) +
    ggplot2::xlab("Tip ratio")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)

  p_q12 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::geom_point(ggplot2::aes(x = tip_ratio, y = abs(dq12)),
                        colour = "blue3") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",q[12]),)) +
    ggplot2::xlab("Tip ratio")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)

  p_q21 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::geom_point(ggplot2::aes(x = tip_ratio, y = abs(dq21)),
                        colour = "blue3") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",q[21]),)) +
    ggplot2::xlab("Tip ratio")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)

  tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE_power_test/tipratio_drate/set_",set,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
    align = "hv", nrow = 2, ncol = 3
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()
}


#####
# plot correlation between accuracy and tree size
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC.csv")
library(ggplot2)
for (set in 1:6){
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE_power_test/MLE/MLE_secsse_test",set,".RData"))
  MLE_all <- MLE_test_list$MLE_all
  param <- param_data[rep(set,500),]
  MLE_all <- data.frame(param, MLE_all)
  MLE_all$dlam1 <- MLE_all$lam1_MLE - MLE_all$lam1
  MLE_all$dlam2 <- MLE_all$lam2_MLE - MLE_all$lam2
  MLE_all$dmu1 <- MLE_all$mu1_MLE - MLE_all$mu1
  MLE_all$dmu2 <- MLE_all$mu2_MLE - MLE_all$mu2
  MLE_all$dq12 <- MLE_all$q12_MLE - MLE_all$q12
  MLE_all$dq21 <- MLE_all$q21_MLE - MLE_all$q21
  MLE_all$tip_ratio1 <- MLE_all$state2/MLE_all$state1
  MLE_all$tip_ratio <- MLE_all$tip_ratio1
  MLE_all$tip_ratio[MLE_all$tip_ratio < 1]<- 1/MLE_all$tip_ratio[MLE_all$tip_ratio < 1]

  # plot MLE results of each rate with true values
  p_lam1 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1)+
    ggplot2::geom_point(ggplot2::aes(x = total, y = abs(dlam1)),
                        colour = "blue3") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",lambda[1]),)) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5) +
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = lam1), linetype = "dashed", size = 0.5)


  p_lam2 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1)+
    ggplot2::geom_point(ggplot2::aes(x = total, y = abs(dlam2)),
                        colour = "blue3") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",lambda[2]),)) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = lam2), linetype = "dashed", size = 0.5)

  p_mu1 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.5)+
    ggplot2::geom_point(ggplot2::aes(x = total, y = abs(dmu1)),
                        colour = "blue3") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",mu[1]),)) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = mu1), linetype = "dashed", size = 0.5)


  p_mu2 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.5)+
    ggplot2::geom_point(ggplot2::aes(x = total, y = abs(dmu2)),
                        colour = "blue3") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",mu[2]),)) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = mu2), linetype = "dashed", size = 0.5)

  p_q12 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.5)+
    ggplot2::geom_point(ggplot2::aes(x = total, y = abs(dq12)),
                        colour = "blue3") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",q[12]),)) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = q12), linetype = "dashed", size = 0.5)

  p_q21 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.5)+
    ggplot2::geom_point(ggplot2::aes(x = total, y = abs(dq21)),
                        colour = "blue3") +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(paste("Error ",q[21]),)) +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = q21), linetype = "solid", size = 0.5)

  tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE_power_test/treesize_drate/set_",set,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
    align = "hv", nrow = 2, ncol = 3
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()
}

#####
# plot correlation between accuracy and tree size
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC.csv")
library(ggplot2)
for (set in 1:6){
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE_power_test/MLE/MLE_secsse_test",set,".RData"))
  MLE_all <- MLE_test_list$MLE_all
  param <- param_data[rep(set,500),]
  MLE_all <- data.frame(param, MLE_all)
  MLE_all$dlam1 <- MLE_all$lam1_MLE - MLE_all$lam1
  MLE_all$dlam2 <- MLE_all$lam2_MLE - MLE_all$lam2
  MLE_all$dmu1 <- MLE_all$mu1_MLE - MLE_all$mu1
  MLE_all$dmu2 <- MLE_all$mu2_MLE - MLE_all$mu2
  MLE_all$dq12 <- MLE_all$q12_MLE - MLE_all$q12
  MLE_all$dq21 <- MLE_all$q21_MLE - MLE_all$q21
  MLE_all$tip_ratio1 <- MLE_all$state2/MLE_all$state1
  MLE_all$tip_ratio <- MLE_all$tip_ratio1
  MLE_all$tip_ratio[MLE_all$tip_ratio < 1]<- 1/MLE_all$tip_ratio[MLE_all$tip_ratio < 1]

  # plot MLE results of each rate with true values
  p_lam1 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,15)+
    ggplot2::geom_point(ggplot2::aes(x = total, y = tip_ratio, colour = abs(dlam1))) +
    ggplot2::theme_classic() +
    ggplot2::scale_colour_gradient(low = "white", high = "black") +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Tip ratio") +
    ggplot2::xlab("Tree size") +
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5) +
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = lam1), linetype = "dashed", size = 0.5)


  p_lam2 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,15)+
    ggplot2::geom_point(ggplot2::aes(x = total, y = tip_ratio, colour = abs(dlam2))) +
    ggplot2::theme_classic() +
    ggplot2::scale_colour_gradient(low = "white", high = "black") +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Tip ratio") +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = lam2), linetype = "dashed", size = 0.5)

  p_mu1 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,15)+
    ggplot2::geom_point(ggplot2::aes(x = total, y = tip_ratio, colour = abs(dmu1))) +
    ggplot2::theme_classic() +
    ggplot2::scale_colour_gradient(low = "white", high = "black") +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Tip ratio") +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = mu1), linetype = "dashed", size = 0.5)


  p_mu2 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,15)+
    ggplot2::geom_point(ggplot2::aes(x = total, y = tip_ratio, colour = abs(dmu2))) +
    ggplot2::theme_classic() +
    ggplot2::scale_colour_gradient(low = "white", high = "black") +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Tip ratio") +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = mu2), linetype = "dashed", size = 0.5)

  p_q12 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,15)+
    ggplot2::geom_point(ggplot2::aes(x = total, y = tip_ratio, colour = abs(dq12))) +
    ggplot2::theme_classic() +
    ggplot2::scale_colour_gradient(low = "white", high = "black") +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Tip ratio") +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = q12), linetype = "dashed", size = 0.5)

  p_q21 <-ggplot2::ggplot(data = MLE_all) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,15)+
    ggplot2::scale_colour_gradient(low = "white", high = "black") +
    ggplot2::geom_point(ggplot2::aes(x = total, y = tip_ratio, colour = abs(dq21))) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Tip ratio") +
    ggplot2::xlab("Tree size")+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
    ggplot2::geom_hline(data= MLE_all, aes(yintercept = q21), linetype = "solid", size = 0.5)

  tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE_power_test/tr_ts_d/set_",set,".tiff"),
       units="px", width=4000, height=2000,res = 300,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
    align = "hv", nrow = 2, ncol = 3
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()
}
