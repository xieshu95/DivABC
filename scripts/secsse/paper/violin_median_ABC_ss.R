## compare ABC ss (Figure 1)
library(tidyverse)
# install.packages("ggtext")
library(ggtext)
library(ggbeeswarm)
library(ggplot2)

## lam
i = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
# whole_df_all1<-whole_df_all
whole_df_all1<-median_all
whole_df_all1$Scenario <- 1

i = 2
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
whole_df_all2<-median_all
whole_df_all2$Scenario <- 2

i = 3
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
whole_df_all3<-median_all
whole_df_all3$Scenario <- 3


scen_names1 <- c(
  `1` = 'S1~":"~lambda[0]~"="~0.3~lambda[1]~"="~0.3', #Scenario~1~
  `2` = 'S2~":"~lambda[0]~"="~0.2~lambda[1]~"="~0.4',
  `3` = 'S3~":"~lambda[0]~"="~0.1~lambda[1]~"="~0.5'
)


scen_names2 <- c(
  `1` = 'S1~":"~mu[0]~"="~0.05~mu[1]~"="~0.05',
  `4` = 'S4~":"~mu[0]~"="~0.05~mu[1]~"="~0.01',
  `5` = 'S5~":"~mu[0]~"="~0.05~mu[1]~"="~0.1'
)


scen_names3 <- c(
  `1` = 'S1~":"~q["01"]~"="~0.1~q[10]~"="~0.1',
  `6` = 'S6~":"~q["01"]~"="~0.1~q[10]~"="~0.2',
  `7` = 'S7~":"~q["01"]~"="~0.1~q[10]~"="~0.02'
)

whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.05, upper = 0.95) {
  data.frame(
    y = mean(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}
library(RColorBrewer)
# whole_df_lam$ss[whole_df_lam$ss=="NLTT"]<- "nLTT"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+D"]<- "nLTT-D"
# whole_df_lam$ss[whole_df_lam$ss=="NLTTs"]<- "nLTTs"
# whole_df_lam$ss[whole_df_lam$ss=="NLTTs+D"]<- "nLTTs-D"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+MPD"]<- "nLTT-MPD"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+MNTD"]<- "nLTT-MNTD"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+colless"]<- "nLTT-colless"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+ratio"]<- "nLTT-ratio"
pal <- brewer.pal(n = 12, name = "Paired")
cols <- c("nLTT" = pal[1] ,"D"= pal[2],"nLTT-D"= pal[9],
          "nLTTs"= pal[5],"nLTTs-D"= pal[6],"nLTT-MPD"= pal[8],
          "nLTT-MNTD"= pal[12],"nLTT-colless"= pal[3],"nLTT-ratio"= pal[4])

whole_df_lam$ss <- factor(whole_df_lam$ss, levels = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))

p_lam1 <-ggplot2::ggplot(data = whole_df_lam, ggplot2::aes(x = ss,y = dlam1,
                                                           color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(lambda[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::scale_colour_manual("Method",values = c("#377eb8","#b81f25","green","yellow","black","gray","pink","darkblue"))+
  # ggplot2::scale_fill_manual("Method",values = c("#377eb8","#b81f25","green","yellow","black","gray","pink","darkblue"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_lam2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(lambda[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(mu[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(mu[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q12 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq12,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(q["01"])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q21 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq21,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 # axis.text.x = ggplot2::element_text(size = 11),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(q[10])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/violin_ABC_median_lam.tiff"),
     units="px", width=8000, height=4000,res = 350,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_q12+ggplot2::theme(legend.position = "none"),
  p_q21+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 6, ncol = 1
)+ ggtitle("Asymmetry in speciation")+
  ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))

legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 4))
)
param_final_lam <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.4))
print(param_final_lam)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()


# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/violin_ABC_median_lam_lam.tiff"),
#      units="px", width=9000, height=5000,res = 350,compression="lzw")
#
# param_estimates_lam <- cowplot::plot_grid(
#   p_lam1+ggplot2::theme(legend.position = "none"),
#   p_lam2+ggplot2::theme(legend.position = "none"),
#   align = "hv", nrow = 2, ncol = 1
# )
# legend <- cowplot::get_legend(
#   p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
# )
# param_final_lam <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.2))
# print(param_final_lam)
# # param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# # print(cowplot::ggdraw(param_est_final))
# while (!is.null(dev.list()))  dev.off()




### mu
library(ggbeeswarm)
library(ggplot2)

#####
## mu

i = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
# whole_df_all1<-whole_df_all
whole_df_all1<-median_all
whole_df_all1$Scenario <- 1

i = 4
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
whole_df_all2<-median_all
whole_df_all2$Scenario <- 4

i = 5
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
whole_df_all3<-median_all
whole_df_all3$Scenario <- 5

scen_names1 <- c(
  `1` = 'S1~":"~mu[0]~"="~0.05~mu[1]~"="~0.05',
  `4` = 'S4~":"~mu[0]~"="~0.05~mu[1]~"="~0.01',
  `5` = 'S5~":"~mu[0]~"="~0.05~mu[1]~"="~0.1'
)


scen_names3 <- c(
  `1` = 'S1~":"~q["01"]~"="~0.1~q[10]~"="~0.1',
  `6` = 'S6~":"~q["01"]~"="~0.1~q[10]~"="~0.2',
  `7` = 'S7~":"~q["01"]~"="~0.1~q[10]~"="~0.02'
)

whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.05, upper = 0.95) {
  data.frame(
    y = mean(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}
library(RColorBrewer)
# whole_df_lam$ss[whole_df_lam$ss=="NLTT"]<- "nLTT"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+D"]<- "nLTT-D"
# whole_df_lam$ss[whole_df_lam$ss=="NLTTs"]<- "nLTTs"
# whole_df_lam$ss[whole_df_lam$ss=="NLTTs+D"]<- "nLTTs-D"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+MPD"]<- "nLTT-MPD"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+MNTD"]<- "nLTT-MNTD"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+colless"]<- "nLTT-colless"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+ratio"]<- "nLTT-ratio"
pal <- brewer.pal(n = 12, name = "Paired")
cols <- c("nLTT" = pal[1] ,"D"= pal[2],"nLTT-D"= pal[9],
          "nLTTs"= pal[5],"nLTTs-D"= pal[6],"nLTT-MPD"= pal[8],
          "nLTT-MNTD"= pal[12],"nLTT-colless"= pal[3],"nLTT-ratio"= pal[4])

whole_df_lam$ss <- factor(whole_df_lam$ss, levels = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))

p_lam1 <-ggplot2::ggplot(data = whole_df_lam, ggplot2::aes(x = ss,y = dlam1,
                                                           color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(lambda[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::scale_colour_manual("Method",values = c("#377eb8","#b81f25","green","yellow","black","gray","pink","darkblue"))+
  # ggplot2::scale_fill_manual("Method",values = c("#377eb8","#b81f25","green","yellow","black","gray","pink","darkblue"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_lam2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(lambda[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(mu[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(mu[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q12 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq12,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(q["01"])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q21 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq21,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 # axis.text.x = ggplot2::element_text(size = 11),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(q[10])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/violin_ABC_median_mu.tiff"),
     units="px", width=8000, height=4000,res = 350,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_q12+ggplot2::theme(legend.position = "none"),
  p_q21+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 6, ncol = 1
)+ ggtitle("Asymmetry in extinction")+
  ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))

legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 4))
)
param_final_lam <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.4))
print(param_final_lam)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()



#####
## q
i = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
# whole_df_all1<-whole_df_all
whole_df_all1<-median_all
whole_df_all1$Scenario <- 1

i = 6
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
whole_df_all2<-median_all
whole_df_all2$Scenario <- 6

i = 7
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
whole_df_all3<-median_all
whole_df_all3$Scenario <- 7


scen_names1 <- c(
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
library(RColorBrewer)
# whole_df_lam$ss[whole_df_lam$ss=="NLTT"]<- "nLTT"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+D"]<- "nLTT-D"
# whole_df_lam$ss[whole_df_lam$ss=="NLTTs"]<- "nLTTs"
# whole_df_lam$ss[whole_df_lam$ss=="NLTTs+D"]<- "nLTTs-D"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+MPD"]<- "nLTT-MPD"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+MNTD"]<- "nLTT-MNTD"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+colless"]<- "nLTT-colless"
# whole_df_lam$ss[whole_df_lam$ss=="NLTT+ratio"]<- "nLTT-ratio"
pal <- brewer.pal(n = 12, name = "Paired")
cols <- c("nLTT" = pal[1] ,"D"= pal[2],"nLTT-D"= pal[9],
          "nLTTs"= pal[3],"nLTTs-D"= pal[4],"nLTT-MPD"= pal[5],
          "nLTT-MNTD"= pal[6],"nLTT-colless"= pal[8],"nLTT-ratio"= pal[12])

whole_df_lam$ss <- factor(whole_df_lam$ss, levels = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))

p_lam1 <-ggplot2::ggplot(data = whole_df_lam, ggplot2::aes(x = ss,y = dlam1,
                                                           color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = ggplot2::element_text(size = 15,colour = "black"),
                 strip.text = element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="white",fill="lightgray"),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(lambda[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = cols)+
  ggplot2::scale_fill_manual("Method",values = cols)+
  # ggplot2::scale_colour_manual("Method",values = c("#377eb8","#b81f25","green","yellow","black","gray","pink","darkblue"))+
  # ggplot2::scale_fill_manual("Method",values = c("#377eb8","#b81f25","green","yellow","black","gray","pink","darkblue"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_lam2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = ggplot2::element_text(size = 10,colour = "black"),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(lambda[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = cols)+
  ggplot2::scale_fill_manual("Method",values = cols)+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = ggplot2::element_text(size = 10,colour = "black"),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(mu[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = cols)+
  ggplot2::scale_fill_manual("Method",values = cols)+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = ggplot2::element_text(size = 10,colour = "black"),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(mu[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = cols)+
  ggplot2::scale_fill_manual("Method",values = cols)+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q12 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq12,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = ggplot2::element_text(size = 10,colour = "black"),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(q["01"])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = cols)+
  ggplot2::scale_fill_manual("Method",values = cols)+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q21 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq21,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.15)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = ggplot2::element_text(size = 10,colour = "black"),
                 strip.text = ggplot2::element_blank(),
                 # axis.text.x = ggplot2::element_text(size = 11),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(q[10])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = cols)+
  ggplot2::scale_fill_manual("Method",values = cols)+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/violin_ABC_median_q.tiff"),
     units="px", width=8000, height=4000,res = 350,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_q12+ggplot2::theme(legend.position = "none"),
  p_q21+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 6, ncol = 1
) + ggtitle("Asymmtry in Speciation")+
  ggplot2::theme(plot.title = element_text(color="black", size=18,margin = margin(0,0,5,0)))
legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_final_lam <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.4))
print(param_final_lam)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()

######
## net div
library(ggbeeswarm)
library(ggplot2)

## lam
i = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
# whole_df_all1<-whole_df_all
whole_df_all1<-median_all
whole_df_all1$Scenario <- 1

i = 2
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
whole_df_all2<-median_all
whole_df_all2$Scenario <- 2

i = 3
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
whole_df_all3<-median_all
whole_df_all3$Scenario <- 3


scen_names1 <- c(
  `1` = 'S1~":"~lambda[0]~"="~0.3~lambda[1]~"="~0.3', #Scenario~1~
  `2` = 'S2~":"~lambda[0]~"="~0.2~lambda[1]~"="~0.4',
  `3` = 'S3~":"~lambda[0]~"="~0.1~lambda[1]~"="~0.5'
)


scen_names2 <- c(
  `1` = 'S1~":"~mu[0]~"="~0.05~mu[1]~"="~0.05',
  `4` = 'S4~":"~mu[0]~"="~0.05~mu[1]~"="~0.01',
  `5` = 'S5~":"~mu[0]~"="~0.05~mu[1]~"="~0.1'
)


scen_names3 <- c(
  `1` = 'S1~":"~q["01"]~"="~0.1~q[10]~"="~0.1',
  `6` = 'S6~":"~q["01"]~"="~0.1~q[10]~"="~0.2',
  `7` = 'S7~":"~q["01"]~"="~0.1~q[10]~"="~0.02'
)

whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.05, upper = 0.95) {
  data.frame(
    y = mean(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

pal <- brewer.pal(n = 12, name = "Paired")
cols <- c("nLTT" = pal[1] ,"D"= pal[2],"nLTT-D"= pal[9],
          "nLTTs"= pal[5],"nLTTs-D"= pal[6],"nLTT-MPD"= pal[8],
          "nLTT-MNTD"= pal[12],"nLTT-colless"= pal[3],"nLTT-ratio"= pal[4])

whole_df_lam$ss <- factor(whole_df_lam$ss, levels = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))

p_net1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div1,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.1,1.1)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Net~Diversification~0)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.1,1.1)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 # axis.text.x = ggplot2::element_text(size = 15),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Net~Diversification~1)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/violin_median_netdiv_lam.tiff"),
     units="px", width=8000, height=3000,res = 400,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
  p_net1+ggplot2::theme(legend.position = "none"),
  p_net2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 1
)+ ggtitle("Asymmetry in speciation")+
  ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))
legend <- cowplot::get_legend(
  p_net1 + theme(legend.box.margin = margin(0, 0, 0, 4))
)
# p1 <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.3))
p1 <- cowplot::plot_grid(param_estimates_lam)
print(p1)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()

#####
## mu
i = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
# whole_df_all1<-whole_df_all
whole_df_all1<-median_all
whole_df_all1$Scenario <- 1

i = 4
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
whole_df_all2<-median_all
whole_df_all2$Scenario <- 4

i = 5
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
whole_df_all3<-median_all
whole_df_all3$Scenario <- 5
scen_names1 <- c(
  `1` = 'S1~":"~mu[0]~"="~0.05~mu[1]~"="~0.05',
  `4` = 'S4~":"~mu[0]~"="~0.05~mu[1]~"="~0.01',
  `5` = 'S5~":"~mu[0]~"="~0.05~mu[1]~"="~0.1'
)


scen_names3 <- c(
  `1` = 'S1~":"~q["01"]~"="~0.1~q[10]~"="~0.1',
  `6` = 'S6~":"~q["01"]~"="~0.1~q[10]~"="~0.2',
  `7` = 'S7~":"~q["01"]~"="~0.1~q[10]~"="~0.02'
)

whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.05, upper = 0.95) {
  data.frame(
    y = mean(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}
pal <- brewer.pal(n = 12, name = "Paired")
cols <- c("nLTT" = pal[1] ,"D"= pal[2],"nLTT-D"= pal[9],
          "nLTTs"= pal[5],"nLTTs-D"= pal[6],"nLTT-MPD"= pal[8],
          "nLTT-MNTD"= pal[12],"nLTT-colless"= pal[3],"nLTT-ratio"= pal[4])

whole_df_lam$ss <- factor(whole_df_lam$ss, levels = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))

p_net1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div1,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.1,1.1)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Net~Diversification~0)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.1,1.1)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 # axis.text.x = ggplot2::element_text(size = 15),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Net~Diversification~1)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/violin_median_netdiv_mu.tiff"),
     units="px", width=8000, height=3000,res = 400,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
  p_net1+ggplot2::theme(legend.position = "none"),
  p_net2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 1
)+ ggtitle("Asymmetry in extinction")+
  ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))
legend <- cowplot::get_legend(
  p_net1 + theme(legend.box.margin = margin(0, 0, 0, 4))
)
# p2 <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.3))
p2 <- cowplot::plot_grid(param_estimates_lam)
print(p2)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()
#
#

## q
i = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
# whole_df_all1<-whole_df_all
whole_df_all1<-median_all
whole_df_all1$Scenario <- 1

i = 6
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
whole_df_all2<-median_all
whole_df_all2$Scenario <- 6

i = 7
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/ABC_median_test",i,".RData"))
whole_df_all3<-median_all
whole_df_all3$Scenario <- 7


scen_names1 <- c(
  `1` = 'S1~":"~q["01"]~"="~0.1~q[10]~"="~0.1',
  `6` = 'S6~":"~q["01"]~"="~0.1~q[10]~"="~0.2',
  `7` = 'S7~":"~q["01"]~"="~0.1~q[10]~"="~0.02'
)

whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.05, upper = 0.95) {
  data.frame(
    y = mean(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}
pal <- brewer.pal(n = 12, name = "Paired")
cols <- c("nLTT" = pal[1] ,"D"= pal[2],"nLTT-D"= pal[9],
          "nLTTs"= pal[5],"nLTTs-D"= pal[6],"nLTT-MPD"= pal[8],
          "nLTT-MNTD"= pal[12],"nLTT-colless"= pal[3],"nLTT-ratio"= pal[4])

whole_df_lam$ss <- factor(whole_df_lam$ss, levels = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))

p_net1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div1,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.1,1.1)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Net~Diversification~0)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.1,1.1)+ #1
  ggplot2::geom_violin(alpha = 0.7) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 # axis.text.x = ggplot2::element_text(size = 15),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Net~Diversification~1)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                               labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"),
                             labels = c("nLTT","*D*","nLTT-*D*","nLTTs","nLTTs-*D*","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/violin_median_netdiv_q.tiff"),
     units="px", width=8000, height=3000,res = 400,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
  p_net1+ggplot2::theme(legend.position = "none"),
  p_net2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 1
)+ ggtitle("Asymmetry in transition")+
  ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))
legend <- cowplot::get_legend(
  p_net1 + theme(legend.box.margin = margin(0, 0, 0, 4))
)
# p3 <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.3))
p3 <- cowplot::plot_grid(param_estimates_lam)
print(p3)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()

## combine
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/compare_ss/violin_median_netdiv_all.tiff"),
     units="px", width=8000, height=9000,res = 450,compression="lzw")
param_estimates_all <- cowplot::plot_grid(
  p1+ggplot2::theme(legend.position = "none"),
  p2+ggplot2::theme(legend.position = "none"),
  p3+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 3, ncol = 1
)
legend <- cowplot::get_legend(
  p_net1 + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_estimates_all <- cowplot::plot_grid(param_estimates_all,legend,rel_widths = c(3, 0.4))
print(param_estimates_all)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()
