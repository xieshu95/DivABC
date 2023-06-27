## secsse all particles all parameter sets into one

# change RGB color
rgb(red=016, green=070, blue=128, maxColorValue = 255)

iqr = function(z, lower = 0.1, upper = 0.9) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

library(ggplot2)
for(i in 1:7){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/whole_df_MLE.RData"))
  whole_df_MLE <- whole_df_MLE[(i*50-49):(i*50),]
  total <- whole_df_MLE$tree_size

  ss = "ABC"
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/delta_whole_df_ABC_test_ss1.RData"))
  whole_df_ABC <- whole_df_ABC[(i*25000-24999):(i*25000),]
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

  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/delta_whole_df_MCMC_test.RData"))
  whole_df_MCMC <- whole_df_MCMC[(i*250050-250049):(i*250050),]
  whole_df_MCMC$ss = "MCMC"
  whole_df_MCMC$total <- rep(total, each = 5001) #2001
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
  save(whole_df_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/whole_df_all_AMM_test",i,".RData"))
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



for (i in 1:7){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/whole_df_all_AMM_test",i,".RData"))
  p_netdiv1 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dnet_div1,y = ss,color = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
    # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::xlim(-1,1)+
    # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
    ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
    ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
                   text = ggplot2::element_text(size = 14,colour = "black"),
                   strip.text = element_text(size = 14,colour = "black")) +
    ggplot2::xlab(expression(Delta~Net~diversification~1))+
    ggplot2::ylab("Method") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
    facet_wrap(~ rep,ncol = 10)
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/each_rep_plots/drate_net_div1_scen",i,".tiff"),
       units="px", width=5000, height=3000,res = 320,compression="lzw")
  print(p_netdiv1)
  while (!is.null(dev.list()))  dev.off()


  p_netdiv2 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dnet_div2,y = ss,color = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
    # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::xlim(-1,1)+
    # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
    ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
    ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
                   text = ggplot2::element_text(size = 14,colour = "black"),
                   strip.text = element_text(size = 14,colour = "black")) +
    ggplot2::xlab(expression(Delta~Net~diversification~2))+
    ggplot2::ylab("Method") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
    facet_wrap(~ rep,ncol = 10)
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/each_rep_plots/drate_net_div2_scen",i,".tiff"),
       units="px", width=5000, height=3000,res = 320,compression="lzw")
  print(p_netdiv2)
  while (!is.null(dev.list()))  dev.off()



  p_lam1 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlam1,y = ss,color = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
    # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::xlim(-1,1)+
    # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
    ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
    ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
                   text = ggplot2::element_text(size = 14,colour = "black"),
                   strip.text = element_text(size = 14,colour = "black")) +
    ggplot2::xlab(expression(Delta~lambda[1]))+
    ggplot2::ylab("Method") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
    facet_wrap(~ rep,ncol = 10)
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/each_rep_plots/drate_lam1_scen",i,".tiff"),
       units="px", width=5000, height=3000,res = 320,compression="lzw")
  print(p_lam1)
  while (!is.null(dev.list()))  dev.off()


  p_lam2 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlam2,y = ss,color = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
    # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::xlim(-1,1)+
    # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
    ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
    ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
                   text = ggplot2::element_text(size = 14,colour = "black"),
                   strip.text = element_text(size = 14,colour = "black")) +
    ggplot2::xlab(expression(Delta~lambda[2]))+
    ggplot2::ylab("Method") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
    facet_wrap(~ rep,ncol = 10)
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/each_rep_plots/drate_lam2_scen",i,".tiff"),
       units="px", width=5000, height=3000,res = 320,compression="lzw")
  print(p_lam2)
  while (!is.null(dev.list()))  dev.off()

  p_mu1<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dmu1,y = ss,color = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
    # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::xlim(-0.2,1)+
    # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
    ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
    ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
                   text = ggplot2::element_text(size = 14,colour = "black"),
                   strip.text = element_text(size = 14,colour = "black")) +
    ggplot2::xlab(expression(Delta~mu[1]))+
    ggplot2::ylab("Method") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
    facet_wrap(~ rep,ncol = 10)
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/each_rep_plots/drate_mu1_scen",i,".tiff"),
       units="px", width=5000, height=3000,res = 320,compression="lzw")
  print(p_mu1)
  while (!is.null(dev.list()))  dev.off()

  p_mu2<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dmu2,y = ss,color = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
    # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::xlim(-0.2,1)+
    # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
    ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
    ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
                   text = ggplot2::element_text(size = 14,colour = "black"),
                   strip.text = element_text(size = 14,colour = "black")) +
    ggplot2::xlab(expression(Delta~mu[2]))+
    ggplot2::ylab("Method") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
    facet_wrap(~ rep,ncol = 10)
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/each_rep_plots/drate_mu2_scen",i,".tiff"),
       units="px", width=5000, height=3000,res = 320,compression="lzw")
  print(p_mu2)
  while (!is.null(dev.list()))  dev.off()


  p_q12<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dq12,y = ss,color = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
    # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::xlim(-0.2,1)+
    # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
    ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
    ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
                   text = ggplot2::element_text(size = 14,colour = "black"),
                   strip.text = element_text(size = 14,colour = "black")) +
    ggplot2::xlab(expression(Delta~q[12]))+
    ggplot2::ylab("Method") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
    facet_wrap(~ rep,ncol = 10)
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/each_rep_plots/drate_q12_scen",i,".tiff"),
       units="px", width=5000, height=3000,res = 320,compression="lzw")
  print(p_q12)
  while (!is.null(dev.list()))  dev.off()


  p_q21<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dq21,y = ss,color = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
    # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::xlim(-0.2,1)+
    # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
    ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
    ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
                   text = ggplot2::element_text(size = 14,colour = "black"),
                   strip.text = element_text(size = 14,colour = "black")) +
    ggplot2::xlab(expression(Delta~q[21]))+
    ggplot2::ylab("Method") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
    facet_wrap(~ rep,ncol = 10)
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/each_rep_plots/drate_q21_scen",i,".tiff"),
       units="px", width=5000, height=3000,res = 320,compression="lzw")
  print(p_q21)
  while (!is.null(dev.list()))  dev.off()

}




iqr = function(z, lower = 0.1, upper = 0.9) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

for(test in 1:7){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/whole_df_all_AMM_test",test,".RData"))
  p_netdiv1 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dnet_div1,color = ss,shape = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::ylim(-2,2)+
    ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.5,size = 1.2)+
    ggplot2::scale_colour_manual(values = c("#B72230","#8CC269","#f5cb1f"))+ # "#104680","#4393C3"
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~Net~diversification~state~1))+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)


  p_netdiv2 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dnet_div2,color = ss,shape = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::ylim(-2,2)+
    ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.5,size = 1.2)+
    ggplot2::scale_colour_manual(values = c("#B72230","#8CC269","#f5cb1f"))+
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~Net~diversification~state~2))+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)

  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/drate_all_netdiv_test",test,".tiff"),
       units="px", width=5000, height=2000,res = 350,compression="lzw")
  params <- cowplot::plot_grid(
    p_netdiv1+ggplot2::theme(legend.position = "none"),
    p_netdiv2+ggplot2::theme(legend.position = "none"),
    align = "hv", nrow = 1, ncol = 2
  )
  legend <- cowplot::get_legend(
    p_netdiv1 + theme(legend.box.margin = margin(0, 0, 0, 6))
  )
  param_estimates <- cowplot::plot_grid(params,legend,
                                        rel_widths = c(3,0.4)
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()
}


for(test in c(5)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/whole_df_all_AMM_test",test,".RData"))
  p_lam1 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dlam1,color = ss,shape = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::ylim(-2,2)+
    ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.5,size = 1.2)+
    ggplot2::scale_colour_manual(values = c("#B72230","#8CC269","#f5cb1f"))+ # "#104680","#4393C3"
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~lambda[1]))+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)


  p_lam2 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dlam2,color = ss,shape = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::ylim(-2,2)+
    ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.5,size = 1.2)+
    ggplot2::scale_colour_manual(values = c("#B72230","#8CC269","#f5cb1f"))+ # "#104680","#4393C3"
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~lambda[2]))+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)

  p_mu1 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dmu1,color = ss,shape = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::ylim(-2,2)+
    ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.5,size = 1.2)+
    ggplot2::scale_colour_manual(values = c("#B72230","#8CC269","#f5cb1f"))+ # "#104680","#4393C3"
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~mu[1]))+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)

  p_mu2 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dmu2,color = ss,shape = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::ylim(-2,2)+
    ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.5,size = 1.2)+
    ggplot2::scale_colour_manual(values = c("#B72230","#8CC269","#f5cb1f"))+ # "#104680","#4393C3"
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~mu[2]))+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)


  p_q12 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dq12,color = ss,shape = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::ylim(-2,2)+
    ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.5,size = 1.2)+
    ggplot2::scale_colour_manual(values = c("#B72230","#8CC269","#f5cb1f"))+ # "#104680","#4393C3"
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~q[12]))+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)

  p_q21 <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dq21,color = ss,shape = ss)) +
    ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::ylim(-2,2)+
    ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.5,size = 1.2)+
    ggplot2::scale_colour_manual(values = c("#B72230","#8CC269","#f5cb1f"))+ # "#104680","#4393C3"
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~q[21]))+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)

  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/drate_all_rates_test",test,".tiff"),
       units="px", width=5000, height=2000,res = 350,compression="lzw")
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

