# violin mean
# violin mean  (seperate rates and net div) for NLTT+D
library(ggbeeswarm)
library(ggplot2)

## lam
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
  `1` = 'lambda[0]~"="~0.3~lambda[1]~"="~0.3',
  `2` = 'lambda[0]~"="~0.2~lambda[1]~"="~0.4',
  `3` = 'lambda[0]~"="~0.1~lambda[1]~"="~0.5'
)


scen_names2 <- c(
  `1` = 'mu[0]~"="~0.05~mu[1]~"="~0.05',
  `4` = 'mu[0]~"="~0.05~mu[1]~"="~0.01',
  `5` = 'mu[0]~"="~0.05~mu[1]~"="~0.1'
)


scen_names3 <- c(
  `1` = 'q["01"]~"="~0.1~q[10]~"="~0.1',
  `6` = 'q["01"]~"="~0.1~q[10]~"="~0.2',
  `7` = 'q["01"]~"="~0.1~q[10]~"="~0.02'
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
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(lambda[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_lam2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(lambda[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_mu1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(mu[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(mu[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q12 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq12,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(q["01"])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q21 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq21,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(q[10])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/violin_median_lam.tiff"),
     units="px", width=7000, height=1800,res = 450,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
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
param_final_lam <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.2))
print(param_final_lam)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()


## mu
i = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
# whole_df_all1<-whole_df_all
whole_df_all1<-median_all
whole_df_all1$Scenario <- 1

i = 4
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
whole_df_all2<-median_all
whole_df_all2$Scenario <- 4

i = 5
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
whole_df_all3<-median_all
whole_df_all3$Scenario <- 5


scen_names1 <- c(
  `1` = 'lambda[0]~"="~0.3~lambda[1]~"="~0.3',
  `2` = 'lambda[0]~"="~0.2~lambda[1]~"="~0.4',
  `3` = 'lambda[0]~"="~0.1~lambda[1]~"="~0.5'
)


scen_names2 <- c(
  `1` = 'mu[0]~"="~0.05~mu[1]~"="~0.05',
  `4` = 'mu[0]~"="~0.05~mu[1]~"="~0.01',
  `5` = 'mu[0]~"="~0.05~mu[1]~"="~0.1'
)


scen_names3 <- c(
  `1` = 'q["01"]~"="~0.1~q[10]~"="~0.1',
  `6` = 'q["01"]~"="~0.1~q[10]~"="~0.2',
  `7` = 'q["01"]~"="~0.1~q[10]~"="~0.02'
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
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(lambda[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))


p_lam2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(lambda[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))


p_mu1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(mu[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))

p_mu2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(mu[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))

p_q12 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq12,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(q["01"])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))

p_q21 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq21,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(q[10])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/violin_median_mu.tiff"),
     units="px", width=7000, height=1800,res = 450,compression="lzw")

param_estimates_mu <- cowplot::plot_grid(
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
param_est_final_mu <- cowplot::plot_grid(param_estimates_mu,legend,rel_widths = c(3, 0.2))
print(param_est_final_mu)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()
#
#

## q
i = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
# whole_df_all1<-whole_df_all
whole_df_all1<-median_all
whole_df_all1$Scenario <- 1

i = 6
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
whole_df_all2<-median_all
whole_df_all2$Scenario <- 6

i = 7
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
whole_df_all3<-median_all
whole_df_all3$Scenario <- 7


scen_names1 <- c(
  `1` = 'lambda[0]~"="~0.3~lambda[1]~"="~0.3',
  `2` = 'lambda[0]~"="~0.2~lambda[1]~"="~0.4',
  `3` = 'lambda[0]~"="~0.1~lambda[1]~"="~0.5'
)


scen_names2 <- c(
  `1` = 'mu[0]~"="~0.05~mu[1]~"="~0.05',
  `4` = 'mu[0]~"="~0.05~mu[1]~"="~0.01',
  `5` = 'mu[0]~"="~0.05~mu[1]~"="~0.1'
)


scen_names3 <- c(
  `1` = 'q["01"]~"="~0.1~q[10]~"="~0.1',
  `6` = 'q["01"]~"="~0.1~q[10]~"="~0.2',
  `7` = 'q["01"]~"="~0.1~q[10]~"="~0.02'
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
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
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
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
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
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
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
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
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
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(q["01"])) +
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
                 axis.title.y = ggplot2::element_text(size = 13),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(q[10])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names3,  label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/violin_median_q.tiff"),
     units="px", width=7000, height=1800,res = 450,compression="lzw")

param_estimates_q <- cowplot::plot_grid(
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
param_est_final_q <- cowplot::plot_grid(param_estimates_q,legend,rel_widths = c(3, 0.2))
print(param_est_final_q)
while (!is.null(dev.list()))  dev.off()

## combine
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/violin_median_all2.tiff"),
     units="px", width=5500, height=4800,res = 500,compression="lzw")
param_estimates_all <- cowplot::plot_grid(
  param_estimates_lam+ggplot2::theme(legend.position = "none"),
  param_estimates_mu+ggplot2::theme(legend.position = "none"),
  param_estimates_q+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 3, ncol = 1,labels = "auto"
)
legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_estimates_all <- cowplot::plot_grid(param_estimates_all,legend,rel_widths = c(3, 0.2))
print(param_estimates_all)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()




#####
# plot net div

# violin mean  (seperate rates and net div)
library(ggbeeswarm)
library(ggplot2)

## lam
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
  `1` = 'lambda[0]~"="~0.3~lambda[1]~"="~0.3',
  `2` = 'lambda[0]~"="~0.2~lambda[1]~"="~0.4',
  `3` = 'lambda[0]~"="~0.1~lambda[1]~"="~0.5'
)


scen_names2 <- c(
  `1` = 'mu[0]~"="~0.05~mu[1]~"="~0.05',
  `4` = 'mu[0]~"="~0.05~mu[1]~"="~0.01',
  `5` = 'mu[0]~"="~0.05~mu[1]~"="~0.1'
)


scen_names3 <- c(
  `1` = 'q["01"]~"="~0.1~q[10]~"="~0.1',
  `6` = 'q["01"]~"="~0.1~q[10]~"="~0.2',
  `7` = 'q["01"]~"="~0.1~q[10]~"="~0.02'
)

whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.05, upper = 0.95) {
  data.frame(
    y = mean(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

p_net1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div1,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.2,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(Net~Diversification~0)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.2,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(Net~Diversification~1)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/violin_median_netdiv_lam.tiff"),
     units="px", width=7000, height=1800,res = 450,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
  p_net1+ggplot2::theme(legend.position = "none"),
  p_net2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 1
)
legend <- cowplot::get_legend(
  p_net1 + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_final_lam <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.2))
print(param_final_lam)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()


## mu
i = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
# whole_df_all1<-whole_df_all
whole_df_all1<-median_all
whole_df_all1$Scenario <- 1

i = 4
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
whole_df_all2<-median_all
whole_df_all2$Scenario <- 4

i = 5
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
whole_df_all3<-median_all
whole_df_all3$Scenario <- 5


scen_names1 <- c(
  `1` = 'lambda[0]~"="~0.3~lambda[1]~"="~0.3',
  `2` = 'lambda[0]~"="~0.2~lambda[1]~"="~0.4',
  `3` = 'lambda[0]~"="~0.1~lambda[1]~"="~0.5'
)


scen_names2 <- c(
  `1` = 'mu[0]~"="~0.05~mu[1]~"="~0.05',
  `4` = 'mu[0]~"="~0.05~mu[1]~"="~0.01',
  `5` = 'mu[0]~"="~0.05~mu[1]~"="~0.1'
)


scen_names3 <- c(
  `1` = 'q["01"]~"="~0.1~q[10]~"="~0.1',
  `6` = 'q["01"]~"="~0.1~q[10]~"="~0.2',
  `7` = 'q["01"]~"="~0.1~q[10]~"="~0.02'
)

whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.05, upper = 0.95) {
  data.frame(
    y = mean(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

p_net1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div1,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.2,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(Net~Diversification~0)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))

p_net2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.2,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(Net~Diversification~1)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/violin_median_netdiv_mu.tiff"),
     units="px", width=7000, height=1800,res = 450,compression="lzw")

param_estimates_mu <- cowplot::plot_grid(
  p_net1+ggplot2::theme(legend.position = "none"),
  p_net2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 1
)
legend <- cowplot::get_legend(
  p_net1 + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_est_final_mu <- cowplot::plot_grid(param_estimates_mu,legend,rel_widths = c(3, 0.2))
print(param_est_final_mu)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()
#
#

## q
i = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
# whole_df_all1<-whole_df_all
whole_df_all1<-median_all
whole_df_all1$Scenario <- 1

i = 6
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
whole_df_all2<-median_all
whole_df_all2$Scenario <- 6

i = 7
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
whole_df_all3<-median_all
whole_df_all3$Scenario <- 7


scen_names1 <- c(
  `1` = 'lambda[0]~"="~0.3~lambda[1]~"="~0.3',
  `2` = 'lambda[0]~"="~0.2~lambda[1]~"="~0.4',
  `3` = 'lambda[0]~"="~0.1~lambda[1]~"="~0.5'
)


scen_names2 <- c(
  `1` = 'mu[0]~"="~0.05~mu[1]~"="~0.05',
  `4` = 'mu[0]~"="~0.05~mu[1]~"="~0.01',
  `5` = 'mu[0]~"="~0.05~mu[1]~"="~0.1'
)


scen_names3 <- c(
  `1` = 'q["01"]~"="~0.1~q[10]~"="~0.1',
  `6` = 'q["01"]~"="~0.1~q[10]~"="~0.2',
  `7` = 'q["01"]~"="~0.1~q[10]~"="~0.02'
)

whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.05, upper = 0.95) {
  data.frame(
    y = mean(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

p_net1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div1,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.2,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 9)) +
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
                 text = ggplot2::element_text(size = 9)) +
  ggplot2::ylab(expression(Net~Diversification~1)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names3,  label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/violin_median_netdiv_q.tiff"),
     units="px", width=7000, height=1800,res = 450,compression="lzw")

param_estimates_q <- cowplot::plot_grid(
  p_net1+ggplot2::theme(legend.position = "none"),
  p_net2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 1
)
legend <- cowplot::get_legend(
  p_net1 + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_est_final_q <- cowplot::plot_grid(param_estimates_q,legend,rel_widths = c(3, 0.2))
print(param_est_final_q)
while (!is.null(dev.list()))  dev.off()

## combine
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/violin_median_netdiv_all.tiff"),
     units="px", width=6000, height=1800,res = 500,compression="lzw")
param_estimates_all <- cowplot::plot_grid(
  param_estimates_lam+ggplot2::theme(legend.position = "none"),
  param_estimates_mu+ggplot2::theme(legend.position = "none"),
  param_estimates_q+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 1, ncol = 3,labels = "auto"
)
legend <- cowplot::get_legend(
  p_net1 + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_estimates_all <- cowplot::plot_grid(param_estimates_all,legend,rel_widths = c(3, 0.2))
print(param_estimates_all)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()



# library(ggbeeswarm)
# library(ggplot2)
# i = 1
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
# # whole_df_all1<-whole_df_all
# whole_df_all1<-median_all
# whole_df_all1$Scenario <- 1
#
# i = 2
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
# whole_df_all2<-median_all
# whole_df_all2$Scenario <- 2
#
# i = 3
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/median_AMM_test",i,".RData"))
# whole_df_all3<-median_all
# whole_df_all3$Scenario <- 3
#
#
# scen_names1 <- c(
#   `1` = 'lambda[1]~"="~0.3~\n~lambda[2]~"="~0.3',
#   `2` = 'lambda[1]~"="~0.2~\n~lambda[2]~"="~0.4',
#   `3` = 'lambda[1]~"="~0.1~\n~lambda[2]~"="~0.5'
# )
#
#
# scen_names2 <- c(
#   `1` = 'mu[1]~"="~0.05~\n~mu[2]~"="~0.05',
#   `4` = 'mu[1]~"="~0.05~\n~mu[2]~"="~0.01',
#   `5` = 'mu[1]~"="~0.05~\n~mu[2]~"="~0.1'
# )
#
#
# scen_names3 <- c(
#   `1` = 'q[12]~"="~0.1~\n~q[21]~"="~0.1',
#   `6` = 'q[12]~"="~0.1~\n~q[21]~"="~0.2',
#   `7` = 'q[12]~"="~0.1~\n~q[21]~"="~0.02'
# )
#
# whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)
#
# iqr = function(z, lower = 0.05, upper = 0.95) {
#   data.frame(
#     y = mean(z),
#     ymin = quantile(z, lower),
#     ymax = quantile(z, upper)
#   )
# }
#
# p_lam1 <-ggplot2::ggplot(data = whole_df_lam, ggplot2::aes(x = ss,y = dlam1,
#                                                            color = ss,fill =ss)) +
#   ggplot2::theme_bw() +
#   ylim(-0.5,1.15)+ #1
#   ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
#   ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(axis.title.x = ggplot2::element_blank(),
#                  axis.title.y = ggplot2::element_text(size = 13),
#                  text = ggplot2::element_text(size = 9)) +
#   ggplot2::ylab(expression(lambda[0])) +
#   ggplot2::xlab("Method")+
#   ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   # ggplot2::theme(legend.position = "none") +
#   ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
#   facet_grid(~Scenario,
#              labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))
#
#
# p_lam2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam2,
#                                                           color = ss,fill =ss)) +
#   ggplot2::theme_bw() +
#   ylim(-0.5,1.15)+ #1
#   ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
#   ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(axis.title.x = ggplot2::element_blank(),
#                  axis.title.y = ggplot2::element_text(size = 13),
#                  text = ggplot2::element_text(size = 9)) +
#   ggplot2::ylab(expression(lambda[1])) +
#   ggplot2::xlab("Method")+
#   ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   # ggplot2::theme(legend.position = "none") +
#   ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
#   facet_grid(~Scenario,
#              labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))
#
#
# p_mu1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu1,
#                                                          color = ss,fill =ss)) +
#   ggplot2::theme_bw() +
#   ylim(-0.5,1.15)+ #1
#   ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
#   ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(axis.title.x = ggplot2::element_blank(),
#                  axis.title.y = ggplot2::element_text(size = 13),
#                  text = ggplot2::element_text(size = 9)) +
#   ggplot2::ylab(expression(mu[0])) +
#   ggplot2::xlab("Method")+
#   ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   # ggplot2::theme(legend.position = "none") +
#   ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
#   facet_grid(~Scenario,
#              labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))
#
# p_mu2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu2,
#                                                          color = ss,fill =ss)) +
#   ggplot2::theme_bw() +
#   ylim(-0.5,1.15)+ #1
#   ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
#   ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(axis.title.x = ggplot2::element_blank(),
#                  axis.title.y = ggplot2::element_text(size = 13),
#                  text = ggplot2::element_text(size = 9)) +
#   ggplot2::ylab(expression(mu[1])) +
#   ggplot2::xlab("Method")+
#   ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   # ggplot2::theme(legend.position = "none") +
#   ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
#   facet_grid(~Scenario,
#              labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))
#
# p_q12 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq12,
#                                                          color = ss,fill =ss)) +
#   ggplot2::theme_bw() +
#   ylim(-0.5,1.15)+ #1
#   ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
#   ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(axis.title.x = ggplot2::element_blank(),
#                  axis.title.y = ggplot2::element_text(size = 13),
#                  text = ggplot2::element_text(size = 9)) +
#   ggplot2::ylab(expression(q["01"])) +
#   ggplot2::xlab("Method")+
#   ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   # ggplot2::theme(legend.position = "none") +
#   ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
#   facet_grid(~Scenario,
#              labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))
#
# p_q21 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq21,
#                                                          color = ss,fill =ss)) +
#   ggplot2::theme_bw() +
#   ylim(-0.5,1.15)+ #1
#   ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
#   ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(axis.title.x = ggplot2::element_blank(),
#                  axis.title.y = ggplot2::element_text(size = 13),
#                  text = ggplot2::element_text(size = 9)) +
#   ggplot2::ylab(expression(q[10])) +
#   ggplot2::xlab("Method")+
#   ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   # ggplot2::theme(legend.position = "none") +
#   ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
#   facet_grid(~Scenario,
#              labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))
#
#
# p_net1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div1,
#                                                           color = ss,fill =ss)) +
#   ggplot2::theme_bw() +
#   ylim(-1.2,1.15)+ #1
#   ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
#   ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(axis.title.x = ggplot2::element_blank(),
#                  axis.title.y = ggplot2::element_text(size = 10),
#                  text = ggplot2::element_text(size = 9)) +
#   ggplot2::ylab(expression(Net~Diversification~0)) +
#   ggplot2::xlab("Method")+
#   ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   # ggplot2::theme(legend.position = "none") +
#   ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
#   facet_grid(~Scenario,
#              labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))
#
# p_net2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div2,
#                                                           color = ss,fill =ss)) +
#   ggplot2::theme_bw() +
#   ylim(-1.2,1.15)+ #1
#   ggplot2::geom_violin(alpha = 0.3) +  #outlier.shape = NA
#   ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(axis.title.x = ggplot2::element_blank(),
#                  axis.title.y = ggplot2::element_text(size = 10),
#                  text = ggplot2::element_text(size = 9)) +
#   ggplot2::ylab(expression(Net~Diversification~1)) +
#   ggplot2::xlab("Method")+
#   ggplot2::scale_colour_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   ggplot2::scale_fill_manual("Method",values = c("#b81f25","#EFC000","#4daf4a"))+
#   # ggplot2::theme(legend.position = "none") +
#   ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
#   facet_grid(~Scenario,
#              labeller = labeller(Scenario  = as_labeller(scen_names2,  label_parsed)))
#
# p_emp <- ggplot() + theme_void()
#
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/violin_median_mu.tiff"),
#      units="px", width=7000, height=1800,res = 450,compression="lzw")
#
# param_estimates <- cowplot::plot_grid(
#   p_lam1+ggplot2::theme(legend.position = "none"),
#   p_mu1+ggplot2::theme(legend.position = "none"),
#   p_q12+ggplot2::theme(legend.position = "none"),
#   p_net1+ggplot2::theme(legend.position = "none"),
#   p_lam2+ggplot2::theme(legend.position = "none"),
#   p_mu2+ggplot2::theme(legend.position = "none"),
#   p_q21+ggplot2::theme(legend.position = "none"),
#   p_net2+ggplot2::theme(legend.position = "none"),
#   align = "hv", nrow = 2, ncol = 4
# )
# legend <- cowplot::get_legend(
#   p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
# )
# param_est_final <- cowplot::plot_grid(param_estimates,legend,rel_widths = c(3, 0.2))
# print(param_est_final)
# # param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# # print(cowplot::ggdraw(param_est_final))
# while (!is.null(dev.list()))  dev.off()
