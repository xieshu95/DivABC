# 1. Fig 2 Properties of observed data
library(ggplot2)
library(ggtext)
load(paste0("Data/BiSSE/obs_ss.rda"))
ss$Scenario <- rep(c("S1","S2","S3","S4","S5","S6","S7"),each = 50)
ss$ratio <- 1/ss$tip_ratio
ss$init <- rep(c(rep(0,25),rep(1,25)),7)
p_total <-ggplot2::ggplot(data = ss,ggplot2::aes(x = tree_size,colour = factor(Scenario),fill = factor(Scenario))) +
  ggplot2::theme_bw() +
  ggplot2::xlim(-1,900)+
  ggplot2::geom_density(size = 0.7,alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::xlab("Total species richness") +
  ggplot2::ylab("Density")+
  ggplot2::scale_colour_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  ggplot2::scale_fill_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))


p_ratio <-ggplot2::ggplot(data = ss,ggplot2::aes(x = tip_ratio,colour = factor(Scenario),fill = factor(Scenario))) +
  ggplot2::theme_bw() +
  ggplot2::geom_density(size = 0.7,alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::xlab("Tip ratio") +
  ggplot2::ylab("Density")+
  ggplot2::scale_colour_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  ggplot2::scale_fill_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))

p_s1 <-ggplot2::ggplot(data = ss,ggplot2::aes(x = state1,colour = factor(Scenario),fill = factor(Scenario))) +
  ggplot2::theme_bw() +
  ggplot2::xlim(-1,500)+
  ggplot2::geom_density(size = 0.7,alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::xlab("Richness with state 0") +
  ggplot2::ylab("Density")+
  ggplot2::scale_colour_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  ggplot2::scale_fill_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))

p_s2 <-ggplot2::ggplot(data = ss,ggplot2::aes(x = state2,colour = factor(Scenario),fill = factor(Scenario))) +
  ggplot2::theme_bw() +
  ggplot2::xlim(-1,180)+
  ggplot2::geom_density(size = 0.7,alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::xlab("Richness with state 1") +
  ggplot2::ylab("Density")+
  ggplot2::scale_colour_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  ggplot2::scale_fill_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))

p_D <-ggplot2::ggplot(data = ss,ggplot2::aes(x = D,colour = factor(Scenario),fill = factor(Scenario))) +
  ggplot2::theme_bw() +
  ggplot2::xlim(-1,1)+
  ggplot2::geom_density(size = 0.7,alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::xlab("Phylogenetic signal") +
  ggplot2::ylab("Density")+
  ggplot2::scale_colour_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  ggplot2::scale_fill_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))

p_nltt2 <-ggplot2::ggplot(data = ss,ggplot2::aes(x = nltt2,colour = factor(Scenario),fill = factor(Scenario))) +
  ggplot2::theme_bw() +
  ggplot2::geom_density(size = 0.7,alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::xlab("nLTT state 1") +
  ggplot2::ylab("Density")+
  ggplot2::scale_colour_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  ggplot2::scale_fill_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))

tiff(paste0("Data/BiSSE/nltts_D/plot_obs_data.tiff"),
     units="px", width=8000, height=4000,res = 500,compression="lzw")

obs_ss <- cowplot::plot_grid(
  p_total+ggplot2::theme(legend.position = "none"),
  p_ratio+ggplot2::theme(legend.position = "none"),
  p_s1+ggplot2::theme(legend.position = "none"),
  p_s2+ggplot2::theme(legend.position = "none"),
  p_D+ggplot2::theme(legend.position = "none"),
  p_nltt2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 3, ncol = 2
)+ ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))

legend <- cowplot::get_legend(
  p_total + theme(legend.box.margin = margin(0, 0, 0, 4))
)
obs <- cowplot::plot_grid(obs_ss,legend,rel_widths = c(3, 0.4))
print(obs)
while (!is.null(dev.list()))  dev.off()


#####
## 3. only compare S1 S2 and S3
load(paste0("Data/BiSSE/obs_ss.rda"))
ss <- ss[1:150,]
ss$Scenario <- rep(c("S1","S2","S3"),each = 50)
ss$ratio <- 1/ss$tip_ratio
ss$init <- rep(c(rep(0,25),rep(1,25)),3)
p_total <-ggplot2::ggplot(data = ss,ggplot2::aes(x = tree_size,colour = factor(Scenario),fill = factor(Scenario))) +
  ggplot2::theme_bw() +
  ggplot2::xlim(-1,900)+
  ggplot2::geom_density(size = 0.7,alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::xlab("Total species richness") +
  ggplot2::ylab("Density")+
  ggplot2::scale_colour_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  ggplot2::scale_fill_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  facet_grid(~init)


p_ratio <-ggplot2::ggplot(data = ss,ggplot2::aes(x = tip_ratio,colour = factor(Scenario),fill = factor(Scenario))) +  #ratio
  ggplot2::theme_bw() +
  # ggplot2::xlim(0,1.0)+
  ggplot2::geom_density(size = 0.7,alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::xlab("Tip ratio") +
  ggplot2::ylab("Density")+
  ggplot2::scale_colour_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  ggplot2::scale_fill_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  facet_grid(~init)

p_s1 <-ggplot2::ggplot(data = ss,ggplot2::aes(x = state1,colour = factor(Scenario),fill = factor(Scenario))) +
  ggplot2::theme_bw() +
  ggplot2::xlim(-1,500)+
  ggplot2::geom_density(size = 0.7,alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::xlab("Richness with state 0") +
  ggplot2::ylab("Density")+
  ggplot2::scale_colour_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  ggplot2::scale_fill_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  facet_grid(~init)

p_s2 <-ggplot2::ggplot(data = ss,ggplot2::aes(x = state2,colour = factor(Scenario),fill = factor(Scenario))) +
  ggplot2::theme_bw() +
  ggplot2::xlim(-1,180)+
  ggplot2::geom_density(size = 0.7,alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::xlab("Richness with state 1") +
  ggplot2::ylab("Density")+
  ggplot2::scale_colour_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  ggplot2::scale_fill_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  facet_grid(~init)

p_D <-ggplot2::ggplot(data = ss,ggplot2::aes(x = D,colour = factor(Scenario),fill = factor(Scenario))) +
  ggplot2::theme_bw() +
  ggplot2::xlim(-1,1)+
  ggplot2::geom_density(size = 0.7,alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::xlab("Phylogenetic signal") +
  ggplot2::ylab("Density")+
  ggplot2::scale_colour_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  ggplot2::scale_fill_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  facet_grid(~init)

p_nltt2 <-ggplot2::ggplot(data = ss,ggplot2::aes(x = nltt2,colour = factor(Scenario),fill = factor(Scenario))) +
  ggplot2::theme_bw() +
  ggplot2::geom_density(size = 0.7,alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::xlab("nLTT state 1") +
  ggplot2::ylab("Density")+
  ggplot2::scale_colour_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  ggplot2::scale_fill_manual("Scenario",values = c("#FF7F0eff","#D62728FF","red4","#98DF8AFF","#2CA02CFF","#1F77B4FF","#1A0099FF"))+
  facet_grid(~init)

tiff(paste0("Data/BiSSE/nltts_D/plot_obs_data_init_S123.tiff"),
     units="px", width=8000, height=4000,res = 500,compression="lzw")

obs_ss <- cowplot::plot_grid(
  p_total+ggplot2::theme(legend.position = "none"),
  p_ratio+ggplot2::theme(legend.position = "none"),
  p_s1+ggplot2::theme(legend.position = "none"),
  p_s2+ggplot2::theme(legend.position = "none"),
  p_D+ggplot2::theme(legend.position = "none"),
  p_nltt2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 3, ncol = 2
)+ ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))

legend <- cowplot::get_legend(
  p_total + theme(legend.box.margin = margin(0, 0, 0, 4))
)
obs <- cowplot::plot_grid(obs_ss,legend,rel_widths = c(3, 0.4))
print(obs)
while (!is.null(dev.list()))  dev.off()


