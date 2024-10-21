### plot conti-space error, test rate ratio
library(ggplot2)
library(ggtext)
i = 1
load(paste0("Data/broad/median_AMM_test",i,".RData"))
ratio <-c()
for(m in 1:300){
  ratio[m]<- max(median_all$lam1[m],median_all$lam2[m])/min(median_all$lam1[m],median_all$lam2[m])
}
median_all$ratio <- log(ratio)
p_lam1 <-ggplot2::ggplot(data = median_all, ggplot2::aes(x = ratio,y = dlam1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ggplot2::ylim(-0.5,1.2)+
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_smooth(method=lm) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~lambda[0])) +
  ggplot2::xlab(expression(lambda~Ratio))+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.7)
# ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed",size = 1)

p_lam2 <-ggplot2::ggplot(data = median_all, ggplot2::aes(x = ratio,y = dlam2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ggplot2::ylim(-0.5,1)+
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_smooth(method=lm) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~lambda[1])) +
  ggplot2::xlab(expression(lambda~Ratio))+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.7)

p_mu1 <-ggplot2::ggplot(data = median_all, ggplot2::aes(x = ratio,y = dmu1,
                                                        color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ggplot2::ylim(-0.2,1.2)+
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_smooth(method=lm) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~mu[0])) +
  ggplot2::xlab(expression(lambda~Ratio))+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.7)

p_mu2 <-ggplot2::ggplot(data = median_all, ggplot2::aes(x = ratio,y = dmu2,
                                                        color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ggplot2::ylim(-0.2,1.2)+
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_smooth(method=lm) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~mu[1])) +
  ggplot2::xlab(expression(lambda~Ratio))+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.7)

p_q12 <-ggplot2::ggplot(data = median_all, ggplot2::aes(x = ratio,y = dq12,
                                                        color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ggplot2::ylim(-0.15,0.7)+
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_smooth(method=lm) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~q["01"])) +
  ggplot2::xlab(expression(lambda~Ratio))+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.7)

p_q21 <-ggplot2::ggplot(data = median_all, ggplot2::aes(x = ratio,y = dq21,
                                                        color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ggplot2::ylim(-0.15,0.7)+
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_smooth(method=lm) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~q["10"])) +
  ggplot2::xlab(expression(lambda~Ratio))+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.7)

p_div1 <-ggplot2::ggplot(data = median_all, ggplot2::aes(x = ratio,y = dnet_div1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ggplot2::ylim(-0.8,0.8)+
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_smooth(method=lm) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div[0])) +
  ggplot2::xlab(expression(lambda~Ratio))+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.7)

p_div2 <-ggplot2::ggplot(data = median_all, ggplot2::aes(x = ratio,y = dnet_div2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ggplot2::ylim(-0.8,0.8)+
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_smooth(method=lm) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = ggplot2::element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div[1])) +
  ggplot2::xlab(expression(lambda~Ratio))+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#63ADEE","#FFC839"))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.7)


tiff(paste0("Data/broad/delta_ratiolam_scen",i,"_ss",0,".tiff"),
     units="px", width=8000, height=3000,res = 500,compression="lzw")
params <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_q12+ggplot2::theme(legend.position = "none"),
  p_div1+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_q21+ggplot2::theme(legend.position = "none"),
  p_div2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 4
)+ ggtitle("Asymmetry in speciation")+
  ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))
legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_estimates <- cowplot::plot_grid(params,legend,
                                      rel_widths = c(3,0.4)
)
param_estimates <- cowplot::add_sub(param_estimates, expression(Log(lambda~Ratio)), hjust = 1)
print(cowplot::ggdraw(param_estimates))
while (!is.null(dev.list()))  dev.off()
