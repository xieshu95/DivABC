# VIOLIN PLOT median
library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(ggplot2)

#####
## lam
i = 1
load(paste0("Data/MuSSE/median_AMM_test",i,".RData"))
whole_df_all1<-median_all
whole_df_all1$Scenario <- 1

i = 2
load(paste0("Data/MuSSE/median_AMM_test",i,".RData"))
whole_df_all2<-median_all
whole_df_all2$Scenario <- 2

i = 3
load(paste0("Data/MuSSE/median_AMM_test",i,".RData"))
whole_df_all3<-median_all
whole_df_all3$Scenario <- 3


scen_names1 <- c(
  `1` = '"Symmetry"',
  `2` = '"Asymmetry"~"in"~"Speciation"',
  `3` = '"Asymmetry"~"in"~"Extinction"'
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
  ylim(-0.5,1.4)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~lambda[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_lam2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.4)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~lambda[2])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_lam3 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam3,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.4)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~lambda[3])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_mu1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,1.4)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~mu[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,1.4)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~mu[2])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu3 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu3,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,1.4)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~mu[3])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))
p_netdiv1 <-ggplot2::ggplot(data = whole_df_lam, ggplot2::aes(x = ss,y = dnet_div1,
                                                           color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1,1)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_netdiv2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1,1)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div[2])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_netdiv3 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div3,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1,1)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div[3])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,0.8)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~q)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("Data/MuSSE/violin_lam_median_rename.tiff"),
     units="px", width=7000, height=6000,res = 600,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_lam3+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_mu3+ggplot2::theme(legend.position = "none"),
  # p_netdiv1+ggplot2::theme(legend.position = "none"),
  # p_netdiv2+ggplot2::theme(legend.position = "none"),
  # p_netdiv3+ggplot2::theme(legend.position = "none"),
  p_q+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 7, ncol = 1
)+
  ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))

legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 4))
)
param_final_lam <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.6))
print(param_final_lam)
while (!is.null(dev.list()))  dev.off()


### violin plot musse whole df
library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(ggplot2)

#####
## lam
i = 1
load(paste0("Data/MuSSE/whole_df_all_AMM_test",i,".RData"))
whole_df_all1<-whole_df_all
whole_df_all1$Scenario <- 1

i = 2
load(paste0("Data/MuSSE/whole_df_all_AMM_test",i,".RData"))
whole_df_all2<-whole_df_all
whole_df_all2$Scenario <- 2

i = 3
load(paste0("Data/MuSSE/whole_df_all_AMM_test",i,".RData"))
whole_df_all3<-whole_df_all
whole_df_all3$Scenario <- 3


scen_names1 <- c(
  `1` = '"Symmetry"',
  `2` = '"Asymmetry"~"in"~"Speciation"',
  `3` = '"Asymmetry"~"in"~"Extinction"'
)

whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.1, upper = 0.9) {
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
  ylim(-0.5,1.4)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~lambda[1])) +
  ggplot2::xlab("Method")+
  gplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_lam2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.4)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~lambda[2])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_lam3 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam3,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,1.4)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~lambda[3])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_mu1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,1.4)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~mu[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,1.4)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~mu[2])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu3 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu3,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,1.4)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~mu[3])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))
p_netdiv1 <-ggplot2::ggplot(data = whole_df_lam, ggplot2::aes(x = ss,y = dnet_div1,
                                                              color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1,1)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_netdiv2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div2,
                                                             color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1,1)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div[2])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_netdiv3 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div3,
                                                             color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1,1)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div[3])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq,
                                                       color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,0.8)+
  ggplot2::geom_violin(alpha = 0.4) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~q)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                               breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                               labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  ggplot2::scale_fill_manual("Method",values =c("#E90F44","purple","pink","#63ADEE","#FFC839"),
                             breaks = c("ABC1","ABC2","ABC3","MCMC","MLE"),
                             labels = c("nLTTs-*D*<sub>subtree</sub>","nLTTs-*D*<sub>merge</sub>","nLTTs-*M*","MCMC","MLE"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("Data/MuSSE/violin_all_rename.tiff"),
     units="px", width=7000, height=6000,res = 600,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_lam3+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_mu3+ggplot2::theme(legend.position = "none"),
  p_q+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 7, ncol = 1
)+
  ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))

legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 4))
)
param_final_lam <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.6))
print(param_final_lam)
while (!is.null(dev.list()))  dev.off()