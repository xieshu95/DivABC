# violin entire posterior distribution
library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(ggplot2)

#####
## lam
i = 1
load(paste0("Data/BiSSE/nltts_D/whole_df_all_AMM_test",i,".RData"))
whole_df_all1<-whole_df_all
whole_df_all1$Scenario <- 1

i = 2
load(paste0("Data/BiSSE/nltts_D/whole_df_all_AMM_test",i,".RData"))
whole_df_all2<-whole_df_all
whole_df_all2$Scenario <- 2

i = 3
load(paste0("Data/BiSSE/nltts_D/whole_df_all_AMM_test",i,".RData"))
whole_df_all3<-whole_df_all
whole_df_all3$Scenario <- 3


scen_names1 <- c(
  `1` = 'S1~":"~lambda[0]~"="~0.6~lambda[1]~"="~0.6',
  `2` = 'S2~":"~lambda[0]~"="~0.6~lambda[1]~"="~0.3',
  `3` = 'S3~":"~lambda[0]~"="~0.6~lambda[1]~"="~0.12'
)


scen_names2 <- c(
  `1` = 'S1~":"~mu[0]~"="~0.05~mu[1]~"="~0.05',
  `4` = 'S4~":"~mu[0]~"="~0.05~mu[1]~"="~0.1',
  `5` = 'S5~":"~mu[0]~"="~0.05~mu[1]~"="~0.25'
)


scen_names3 <- c(
  `1` = 'S1~":"~q["01"]~"="~0.05~q[10]~"="~0.05',
  `6` = 'S6~":"~q["01"]~"="~0.05~q[10]~"="~0.1',
  `7` = 'S7~":"~q["01"]~"="~0.05~q[10]~"="~0.25'
)

whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.025, upper = 0.975) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

library(RColorBrewer)
p_lam1 <-ggplot2::ggplot(data = whole_df_lam, ggplot2::aes(x = ss,y = dlam1,
                                                           color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,2.1)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,   #+init_obs,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_lam2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,2.1)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_mu1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,2.2)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,2.2)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q12 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq12,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.45,1.15)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q21 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq21,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.45,1.15)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 # axis.text.x = ggplot2::element_text(size = 15),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~q[10])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div1,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-2.5,1.8)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div~0)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-2.5,1.8)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(size = 15),
                 # axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div~1)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("Data/BiSSE/nltts_D/violin_lam_all.tiff"),
     units="px", width=6000, height=8000,res = 500,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_q12+ggplot2::theme(legend.position = "none"),
  p_q21+ggplot2::theme(legend.position = "none"),
  p_net1+ggplot2::theme(legend.position = "none"),
  p_net2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 8, ncol = 1
)+ ggtitle("Asymmetry in speciation")+
  ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))

legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 4))
)
param_final_lam <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.4))
print(param_final_lam)
while (!is.null(dev.list()))  dev.off()



## mu
i = 1
load(paste0("Data/BiSSE/nltts_D/whole_df_all_AMM_test",i,".RData"))

whole_df_all1<-whole_df_all
whole_df_all1$Scenario <- 1

i = 4
load(paste0("Data/BiSSE/nltts_D/whole_df_all_AMM_test",i,".RData"))
whole_df_all2<-whole_df_all
whole_df_all2$Scenario <- 4

i = 5
load(paste0("Data/BiSSE/nltts_D/whole_df_all_AMM_test",i,".RData"))
whole_df_all3<-whole_df_all
whole_df_all3$Scenario <- 5



scen_names1 <- c(
  `1` = 'S1~":"~mu[0]~"="~0.05~mu[1]~"="~0.05',
  `4` = 'S4~":"~mu[0]~"="~0.05~mu[1]~"="~0.1',
  `5` = 'S5~":"~mu[0]~"="~0.05~mu[1]~"="~0.25'
)


whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.025, upper = 0.975) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}


p_lam1 <-ggplot2::ggplot(data = whole_df_lam, ggplot2::aes(x = ss,y = dlam1,
                                                           color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,2.1)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_lam2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,2.1)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_mu1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,2.2)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,2.2)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q12 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq12,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.45,1.15)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q21 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq21,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.45,1.15)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 # axis.text.x = ggplot2::element_text(size = 15),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~q[10])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div1,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-2.5,1.8)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div~0)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-2.5,1.8)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(size = 15),
                 # axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div~1)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("Data/BiSSE/nltts_D/violin_mu_all.tiff"),
     units="px", width=6000, height=8000,res = 500,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_q12+ggplot2::theme(legend.position = "none"),
  p_q21+ggplot2::theme(legend.position = "none"),
  p_net1+ggplot2::theme(legend.position = "none"),
  p_net2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 8, ncol = 1
)+ ggtitle("Asymmetry in extinction")+
  ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))

legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 4))
)
param_final_lam <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.4))
print(param_final_lam)
while (!is.null(dev.list()))  dev.off()

#
#

## q
i = 1
load(paste0("Data/BiSSE/nltts_D/whole_df_all_AMM_test",i,".RData"))

whole_df_all1<-whole_df_all
whole_df_all1$Scenario <- 1

i = 6
load(paste0("Data/BiSSE/nltts_D/whole_df_all_AMM_test",i,".RData"))
whole_df_all2<-whole_df_all
whole_df_all2$Scenario <- 6

i = 7
load(paste0("Data/BiSSE/nltts_D/whole_df_all_AMM_test",i,".RData"))
whole_df_all3<-whole_df_all
whole_df_all3$Scenario <- 7



scen_names1 <- c(
  `1` = 'S1~":"~q["01"]~"="~0.05~q[10]~"="~0.05',
  `6` = 'S6~":"~q["01"]~"="~0.05~q[10]~"="~0.1',
  `7` = 'S7~":"~q["01"]~"="~0.05~q[10]~"="~0.25'
)


whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.025, upper = 0.975) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

library(RColorBrewer)

p_lam1 <-ggplot2::ggplot(data = whole_df_lam, ggplot2::aes(x = ss,y = dlam1,
                                                           color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,2.1)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_lam2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,2.1)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_mu1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,2.2)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,2.2)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q12 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq12,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.45,1.15)+
  ggplot2::geom_violin(alpha = 0.4) +
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
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q21 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq21,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.45,1.15)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 # axis.text.x = ggplot2::element_text(size = 15),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~q[10])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div1,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-2.5,1.8)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div~0)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-2.5,1.8)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(size = 15),
                 # axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div~1)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("Data/BiSSE/nltts_D/violin_q_all.tiff"),
     units="px", width=6000, height=8000,res = 500,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_q12+ggplot2::theme(legend.position = "none"),
  p_q21+ggplot2::theme(legend.position = "none"),
  p_net1+ggplot2::theme(legend.position = "none"),
  p_net2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 8, ncol = 1
)+ ggtitle("Asymmetry in transition")+
  ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))

legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 4))
)
param_final_lam <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.4))
print(param_final_lam)
while (!is.null(dev.list()))  dev.off()

