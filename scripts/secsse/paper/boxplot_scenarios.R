# boxplot of scenarios, combine medians of all the replications

## generate data frame combine all the particles/medians
library(ggplot2)
for(i in 1:7){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/whole_df_MLE.RData"))
  whole_df_MLE <- whole_df_MLE[(i*50-49):(i*50),][,1:23]
  total <- whole_df_MLE$tree_size

  ss = "ABC"
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/delta_whole_df_ABC_test_ss1.RData"))
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

  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/delta_whole_df_MCMC_test.RData"))
  whole_df_MCMC <- whole_df_MCMC[(i*125050-125049):(i*125050),]
  whole_df_MCMC$ss = "MCMC"
  whole_df_MCMC$total <- rep(total, each = 2501) #2001
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
  whole_df_MCMC$rep <- rep(rep(1:50, each = 2501), 1)


  df<-whole_df_MCMC
  n <- 2501
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
                        whole_df_MLE_old[,c(1:6,24,25,28,29,32:44)])
                        # whole_df_MLE[,c(1:6,30,31,34,35,38,39,24:29,40:44)])
  save(whole_df_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/whole_df_all_AMM_test",i,".RData"))

  median_all <- rbind(ABC_median[,c(1:6,13,14,17,18,21:33)],
                      MCMC_median[,c(1:6,13,14,17,18,21:33)],
                      whole_df_MLE_old[,c(1:6,24,25,28,29,32:44)])
                      # whole_df_MLE[,c(1:6,30,31,34,35,38,39,24:29,40:44)])

  save(median_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))


}

# install.packages("ggbeeswarm")
library(ggbeeswarm)
library(ggplot2)
i = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
# whole_df_all1<-whole_df_all
whole_df_all1<-median_all
whole_df_all1$Scenario <- 1

i = 2
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
whole_df_all2<-median_all
whole_df_all2$Scenario <- 2

i = 3
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
whole_df_all3<-median_all
whole_df_all3$Scenario <- 3


scen_names1 <- c(
  `1` = 'lambda[1]~"="~0.3~\n~lambda[2]~"="~0.3',
  `2` = 'lambda[1]~"="~0.2~\n~lambda[2]~"="~0.4',
  `3` = 'lambda[1]~"="~0.1~\n~lambda[2]~"="~0.5'
)


scen_names2 <- c(
  `1` = 'mu[1]~"="~0.05~\n~mu[2]~"="~0.05',
  `4` = 'mu[1]~"="~0.05~\n~mu[2]~"="~0.01',
  `5` = 'mu[1]~"="~0.05~\n~mu[2]~"="~0.1'
)


scen_names3 <- c(
  `1` = 'q[12]~"="~0.1~\n~q[21]~"="~0.1',
  `6` = 'q[12]~"="~0.1~\n~q[21]~"="~0.2',
  `7` = 'q[12]~"="~0.1~\n~q[21]~"="~0.02'
)

whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

p_lam1 <-ggplot2::ggplot(data = whole_df_lam) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.0)+ #1
  ggplot2::geom_boxplot(ggplot2::aes(x = ss,y = dlam1,
                                     color = ss,fill =ss),
                        alpha = 0.5) +  #outlier.shape = NA
  # geom_quasirandom(aes(x = ss,y = dlam1, color = ss),cex = 2,size = 0.5,alpha = 0.5)+
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(lambda[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_lam2 <-ggplot2::ggplot(data = whole_df_lam) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.0)+ #1
  ggplot2::geom_boxplot(ggplot2::aes(x = ss,y = dlam2,
                                     color = ss,fill =ss),
                        alpha = 0.5) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(lambda[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_mu1 <-ggplot2::ggplot(data = whole_df_lam) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.0)+ #1
  ggplot2::geom_boxplot(ggplot2::aes(x = ss,y = dmu1,
                                     color = ss,fill =ss),
                        alpha = 0.5) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(mu[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu2 <-ggplot2::ggplot(data = whole_df_lam) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.0)+ #1
  ggplot2::geom_boxplot(ggplot2::aes(x = ss,y = dmu2,
                                     color = ss,fill =ss),
                        alpha = 0.5) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(mu[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q12 <-ggplot2::ggplot(data = whole_df_lam) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.0)+ #1
  ggplot2::geom_boxplot(ggplot2::aes(x = ss,y = dq12,
                                     color = ss,fill =ss),
                        alpha = 0.5) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(q[01])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q21 <-ggplot2::ggplot(data = whole_df_lam) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.0)+ #1
  ggplot2::geom_boxplot(ggplot2::aes(x = ss,y = dq21,
                                     color = ss,fill =ss),
                        alpha = 0.5) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(q[10])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_net1 <-ggplot2::ggplot(data = whole_df_lam) +
  ggplot2::theme_bw() +
  ylim(-1,1.0)+ #1
  ggplot2::geom_boxplot(ggplot2::aes(x = ss,y = dnet_div1,
                                     color = ss,fill =ss),
                        alpha = 0.5) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(Net~Diversification~0)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net2 <-ggplot2::ggplot(data = whole_df_lam) +
  ggplot2::theme_bw() +
  ylim(-1,1.0)+ #1
  ggplot2::geom_boxplot(ggplot2::aes(x = ss,y = dnet_div2,
                                     color = ss,fill =ss),
                        alpha = 0.5) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(Net~Diversification~1)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_emp <- ggplot() + theme_void()

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/boxplot_median_lam.tiff"),
     units="px", width=7200, height=2000,res = 400,compression="lzw")

param_estimates <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_q12+ggplot2::theme(legend.position = "none"),
  p_net1+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_q21+ggplot2::theme(legend.position = "none"),
  p_net2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 4
)
legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_est_final <- cowplot::plot_grid(param_estimates,legend,rel_widths = c(3, 0.5))
print(param_est_final)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()










######
# violin all the particles not only median
library(ggbeeswarm)
library(ggplot2)
i = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/whole_df_all_AMM_test",i,".RData"))
# whole_df_all1<-whole_df_all
whole_df_all1<-whole_df_all
whole_df_all1$Scenario <- 1

i = 6
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/whole_df_all_AMM_test",i,".RData"))
whole_df_all2<-whole_df_all
whole_df_all2$Scenario <- 6

i = 7
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/whole_df_all_AMM_test",i,".RData"))
whole_df_all3<-whole_df_all
whole_df_all3$Scenario <- 7


scen_names1 <- c(
  `1` = 'lambda[1]~"="~0.3~\n~lambda[2]~"="~0.3',
  `2` = 'lambda[1]~"="~0.2~\n~lambda[2]~"="~0.4',
  `3` = 'lambda[1]~"="~0.1~\n~lambda[2]~"="~0.5'
)


scen_names2 <- c(
  `1` = 'mu[1]~"="~0.05~\n~mu[2]~"="~0.05',
  `4` = 'mu[1]~"="~0.05~\n~mu[2]~"="~0.01',
  `5` = 'mu[1]~"="~0.05~\n~mu[2]~"="~0.1'
)


scen_names3 <- c(
  `1` = 'q[12]~"="~0.1~\n~q[21]~"="~0.1',
  `6` = 'q[12]~"="~0.1~\n~q[21]~"="~0.2',
  `7` = 'q[12]~"="~0.1~\n~q[21]~"="~0.02'
)

whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.05, upper = 0.95) {
  data.frame(
    y = mean(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

p_lam1 <-ggplot2::ggplot(data = whole_df_lam, ggplot2::aes(x = ss,y = dlam1,
                                     color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(lambda[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names3,  label_parsed)))


p_lam2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(lambda[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names3,  label_parsed)))


p_mu1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(mu[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names3,  label_parsed)))

p_mu2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(mu[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names3,  label_parsed)))

p_q12 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq12,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(q[01])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names3,  label_parsed)))

p_q21 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq21,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(q[10])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names3,  label_parsed)))


p_net1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div1,
                                     color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.2,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(Net~Diversification~0)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names3,  label_parsed)))

p_net2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div2,
                                     color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.2,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 10)) +
  ggplot2::ylab(expression(Net~Diversification~1)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names3,  label_parsed)))


p_emp <- ggplot() + theme_void()

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/whole_q.tiff"),
     units="px", width=7200, height=2000,res = 400,compression="lzw")

param_estimates <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_q12+ggplot2::theme(legend.position = "none"),
  p_net1+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_q21+ggplot2::theme(legend.position = "none"),
  p_net2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 4
)
legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_est_final <- cowplot::plot_grid(param_estimates,legend,rel_widths = c(3, 0.5))
print(param_est_final)
while (!is.null(dev.list()))  dev.off()

