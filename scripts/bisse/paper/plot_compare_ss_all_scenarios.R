# Fig 3: comparison between summary statistic combinations
library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(ggplot2)


i = 1
load(paste0("Data/BiSSE/compare_ss/ABC_median_test",i,".RData"))
whole_df_all1<-median_all
whole_df_all1$Scenario <- 1

i = 2
load(paste0("Data/BiSSE/compare_ss/ABC_median_test",i,".RData"))
whole_df_all2<-median_all
whole_df_all2$Scenario <- 2

i = 3
load(paste0("Data/BiSSE/compare_ss/ABC_median_test",i,".RData"))
whole_df_all3<-median_all
whole_df_all3$Scenario <- 3

i = 4
load(paste0("Data/BiSSE/compare_ss/ABC_median_test",i,".RData"))
whole_df_all4<-median_all
whole_df_all4$Scenario <- 4

i = 5
load(paste0("Data/BiSSE/compare_ss/ABC_median_test",i,".RData"))
whole_df_all5<-median_all
whole_df_all5$Scenario <- 5

i = 6
load(paste0("Data/BiSSE/compare_ss/ABC_median_test",i,".RData"))
whole_df_all6<-median_all
whole_df_all6$Scenario <- 6

i = 7
load(paste0("Data/BiSSE/compare_ss/ABC_median_test",i,".RData"))
whole_df_all7<-median_all
whole_df_all7$Scenario <- 7


whole_df <- rbind(whole_df_all1,whole_df_all2,whole_df_all3,
                      whole_df_all4,whole_df_all5,whole_df_all6,
                      whole_df_all7)

library(RColorBrewer)
iqr = function(z, lower = 0.025, upper = 0.975) {
  data.frame(
    y = mean(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}
library(RColorBrewer)
pal <- brewer.pal(n = 12, name = "Paired")
cols <- c("D" = pal[1] ,"nLTT"= pal[9],"nLTT-D"= pal[2],
          "nLTT-Ratio"= pal[8],"nLTT-MPD"= pal[3],"nLTT-MNTD"= pal[4],
          "nLTT-Colless"= pal[12],"nLTTs"= pal[10],"nLTTs-D"= pal[6],
          "nLTT-MNTD-D"= pal[5],"nLTTs-MNTD-D"= pal[7])

whole_df$ss <- factor(whole_df$ss, levels = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"))

p_lam1 <-ggplot2::ggplot(data = whole_df, ggplot2::aes(x = ss,y = dlam1,
                                                           color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1,2.05)+
  ggplot2::geom_violin(alpha = 0.5) +
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
  ggplot2::ylab(expression(Delta~lambda[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                               labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                             labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::geom_hline(data= whole_df, aes(yintercept = 0), linetype = "dashed", size = 0.5)


p_lam2 <-ggplot2::ggplot(data = whole_df,ggplot2::aes(x = ss,y = dlam2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1,2.05)+
  ggplot2::geom_violin(alpha = 0.5) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~lambda[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                               labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                             labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::geom_hline(data= whole_df, aes(yintercept = 0), linetype = "dashed", size = 0.5)

p_mu1 <-ggplot2::ggplot(data = whole_df,ggplot2::aes(x = ss,y = dmu1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.8,2.0)+
  ggplot2::geom_violin(alpha = 0.5) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~mu[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                               labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                             labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::geom_hline(data= whole_df, aes(yintercept = 0), linetype = "dashed", size = 0.5)

p_mu2 <-ggplot2::ggplot(data = whole_df,ggplot2::aes(x = ss,y = dmu2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.8,2.0)+
  ggplot2::geom_violin(alpha = 0.5) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~mu[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                               labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                             labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::geom_hline(data= whole_df, aes(yintercept = 0), linetype = "dashed", size = 0.5)

p_q12 <-ggplot2::ggplot(data = whole_df,ggplot2::aes(x = ss,y = dq12,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.05)+ #1
  ggplot2::geom_violin(alpha = 0.5) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~q["01"])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                               labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                             labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::geom_hline(data= whole_df, aes(yintercept = 0), linetype = "dashed", size = 0.5)

p_q21 <-ggplot2::ggplot(data = whole_df,ggplot2::aes(x = ss,y = dq21,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.05)+ #1
  ggplot2::geom_violin(alpha = 0.5) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 # axis.text.x = ggplot2::element_text(size = 11),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~q[10])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                               labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                             labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::geom_hline(data= whole_df, aes(yintercept = 0), linetype = "dashed", size = 0.5)

p_net1 <-ggplot2::ggplot(data = whole_df,ggplot2::aes(x = ss,y = dnet_div1,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-2.5,2)+
  ggplot2::geom_violin(alpha = 0.5) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 13),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 # axis.text.x = ggplot2::element_text(size = 11),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~0)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                               labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                             labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df, aes(yintercept = 0), linetype = "dashed", size = 0.5)

p_net2 <-ggplot2::ggplot(data = whole_df,ggplot2::aes(x = ss,y = dnet_div2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-2.5,2)+
  ggplot2::geom_violin(alpha = 0.5) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 13),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 # axis.text.x = ggplot2::element_text(size = 11),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~1)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Summary \n statistic",values = cols,
                               breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                               labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::scale_fill_manual("Summary \n statistic",values = cols,
                             breaks = c("D","nLTT","nLTT-D","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-D","nLTT-MNTD-D","nLTTs-MNTD-D"),
                             labels = c("*D*","nLTT","nLTT-*D*","nLTT-Ratio","nLTT-MPD","nLTT-MNTD","nLTT-Colless","nLTTs","nLTTs-*D*","nLTT-MNTD-*D*","nLTTs-MNTD-*D*"))+
  ggplot2::geom_hline(data= whole_df, aes(yintercept = 0), linetype = "dashed", size = 0.5)

tiff(paste0("Data/BiSSE/compare_ss/violin_ss_median_all_scenarios.tiff"),
     units="in", width=12, height=8,res = 500,compression="lzw")

param_estimates <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_q12+ggplot2::theme(legend.position = "none"),
  p_q21+ggplot2::theme(legend.position = "none"),
  p_net1+ggplot2::theme(legend.position = "none"),
  p_net2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 8, ncol = 1)

legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 4))
)
param_final <- cowplot::plot_grid(param_estimates,legend,rel_widths = c(3, 0.7))
print(param_final)
while (!is.null(dev.list()))  dev.off()

