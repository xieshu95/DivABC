## plot TRAISIE_DD
load("G:/results/project 2/tip_info/round3/TRAISIE_DD/DD_whole_df_ABC.RData")
whole_df_ABC$Transition <- whole_df_ABC$trans
whole_df_ABC$Transition[whole_df_ABC$trans == "0.02" & whole_df_ABC$trans2 == "0.02"] <- "ll"
whole_df_ABC$Transition[whole_df_ABC$trans == "0.02" & whole_df_ABC$trans2 == "0.2"] <- "lh"
whole_df_ABC$Transition[whole_df_ABC$trans == "0.2" & whole_df_ABC$trans2 == "0.02"] <- "hl"

library(ggplot2)
colors <- c("State1"="red","State2"="blue")
whole_df_ABC$Transition <- factor(whole_df_ABC$Transition,
                             levels = c("hl","lh","ll"),
                             labels = c(expression("high"~italic(q)[12]~"low"~italic(q)[21]),
                                        expression("low"~italic(q)[12]~"high"~italic(q)[21]),
                                        expression("low"~italic(q)[12]~"low"~italic(q)[21])))

i = 1
data <- whole_df_ABC[((i*12000-11999)):(i*12000),]
p_lac1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.1,0.6)+
  ggplot2::geom_boxplot(ggplot2::aes(x = lac, y = lac_abc1, group = lac),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac, y = lac),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Cladogenesis state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)
p_lac1


p_lac2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.4,0.9)+
  ggplot2::geom_boxplot(ggplot2::aes(x = lac2, y = lac_abc2, group = lac2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac2, y = lac2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Cladogenesis state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)
p_lac2

tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_lac1,p_lac2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()


i = 2
data <- whole_df_ABC[((i*12000-11999)):(i*12000),]
p_mu1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(-0.02,0.12)+
  ggplot2::geom_boxplot(ggplot2::aes(x = mu, y = mu_abc1, group = mu),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = mu, y = mu),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Extinction state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)



p_mu2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.08,0.22)+
  ggplot2::geom_boxplot(ggplot2::aes(x = mu2, y = mu_abc2, group = mu2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = mu2, y = mu2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Extinction state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)


tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_mu1,p_mu2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()


i = 3
data <- whole_df_ABC[((i*12000-11999)):(i*12000),]
p_gam1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,0.07)+
  xlim(0,0.022)+
  ggplot2::geom_boxplot(ggplot2::aes(x = gam, y = gam_abc1, group = gam),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam, y = gam),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Colonization state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)



p_gam2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,0.07)+
  xlim(0.018,0.04)+
  ggplot2::geom_boxplot(ggplot2::aes(x = gam2, y = gam_abc2, group = gam2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam2, y = gam2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Colonization state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)


tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_gam1,p_gam2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()



i = 4
data <- whole_df_ABC[((i*12000-11999)):(i*12000),]
p_laa1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0,0.25)+
  ggplot2::geom_boxplot(ggplot2::aes(x = laa, y = laa_abc1, group = laa),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa, y = laa),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Anagenesis state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)



p_laa2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.15,0.4)+
  ggplot2::geom_boxplot(ggplot2::aes(x = laa2, y = laa_abc2, group = laa2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa2, y = laa2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Anagenesis state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)


tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_laa1,p_laa2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()



#########################################################################

## plot TRAISIE_DD_with_q
load("G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/DD_whole_df_ABC.RData")
whole_df_ABC$Transition <- whole_df_ABC$trans
whole_df_ABC$Transition[whole_df_ABC$trans == "0.02" & whole_df_ABC$trans2 == "0.02"] <- "ll"
whole_df_ABC$Transition[whole_df_ABC$trans == "0.02" & whole_df_ABC$trans2 == "0.2"] <- "lh"
whole_df_ABC$Transition[whole_df_ABC$trans == "0.2" & whole_df_ABC$trans2 == "0.02"] <- "hl"

library(ggplot2)
colors <- c("State1"="red","State2"="blue")
whole_df_ABC$Transition <- factor(whole_df_ABC$Transition,
                                  levels = c("hl","lh","ll"),
                                  labels = c(expression("high"~italic(q)[12]~"low"~italic(q)[21]),
                                             expression("low"~italic(q)[12]~"high"~italic(q)[21]),
                                             expression("low"~italic(q)[12]~"low"~italic(q)[21])))

i = 1
data <- whole_df_ABC[((i*6000-5999)):(i*6000),]
p_lac1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.1,0.6)+
  ggplot2::geom_boxplot(ggplot2::aes(x = lac, y = lac_abc1, group = lac),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac, y = lac),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Cladogenesis state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)
p_lac1


p_lac2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.4,0.9)+
  ggplot2::geom_boxplot(ggplot2::aes(x = lac2, y = lac_abc2, group = lac2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac2, y = lac2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Cladogenesis state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)
p_lac2


p_trans1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(ggplot2::aes(x = lac, y = trans_abc1, group = lac),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac, y = trans),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Transition state 1 to 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)

p_trans1


p_trans2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(ggplot2::aes(x = lac, y = trans_abc2, group = lac),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac, y = trans2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Transition state 1 to 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)
p_trans2

tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_lac1,p_lac2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()


i = 2
data <- whole_df_ABC[((i*6000-5999)):(i*6000),]
p_mu1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(-0.02,0.12)+
  ggplot2::geom_boxplot(ggplot2::aes(x = mu, y = mu_abc1, group = mu),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = mu, y = mu),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Extinction state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)



p_mu2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.08,0.22)+
  ggplot2::geom_boxplot(ggplot2::aes(x = mu2, y = mu_abc2, group = mu2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = mu2, y = mu2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Extinction state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)


tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_mu1,p_mu2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()


i = 3
data <- whole_df_ABC[((i*6000-5999)):(i*6000),]
p_gam1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,0.07)+
  xlim(0,0.022)+
  ggplot2::geom_boxplot(ggplot2::aes(x = gam, y = gam_abc1, group = gam),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam, y = gam),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Colonization state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)



p_gam2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,0.07)+
  xlim(0.018,0.04)+
  ggplot2::geom_boxplot(ggplot2::aes(x = gam2, y = gam_abc2, group = gam2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam2, y = gam2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Colonization state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)


tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_gam1,p_gam2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()



i = 4
data <- whole_df_ABC[((i*6000-5999)):(i*6000),]
p_laa1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0,0.25)+
  ggplot2::geom_boxplot(ggplot2::aes(x = laa, y = laa_abc1, group = laa),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa, y = laa),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Anagenesis state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)



p_laa2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.15,0.4)+
  ggplot2::geom_boxplot(ggplot2::aes(x = laa2, y = laa_abc2, group = laa2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa2, y = laa2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Anagenesis state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)


tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_laa1,p_laa2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()

