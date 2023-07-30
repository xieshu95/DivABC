library(ggplot2)
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/obs_ss_long_with_pars_DI.RData"))

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/delta_whole_df_ABC_ss_set",0,".RData"))
whole_df_ABC$Method = "ABC All"
whole_df_ABC_s0 = whole_df_ABC
whole_df_ABC_s0$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s0$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s0$set <- rep(1:16, each = 5000)
whole_df_ABC_s0$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s0$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)



load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/delta_whole_df_ABC_ss_set",1,".RData"))
whole_df_ABC$Method = "ABC Phylogenetic"
whole_df_ABC_s1 = whole_df_ABC
whole_df_ABC_s1$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s1$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s1$set <- rep(1:16, each = 5000)
whole_df_ABC_s1$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s1$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/delta_whole_df_ABC_ss_set",2,".RData"))
whole_df_ABC$Method = "ABC Diversity"
whole_df_ABC_s2 = whole_df_ABC
whole_df_ABC_s2$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s2$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s2$set <- rep(1:16, each = 5000)
whole_df_ABC_s2$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s2$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/delta_whole_df_ABC_ss_set",3,".RData"))
whole_df_ABC$Method = "ABC NLTT"
whole_df_ABC_s3 = whole_df_ABC
whole_df_ABC_s3$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s3$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s3$set <- rep(1:16, each = 5000)
whole_df_ABC_s3$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s3$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)


whole_df_ABC <- rbind(whole_df_ABC_s0,
                      whole_df_ABC_s1,
                      whole_df_ABC_s2,
                      whole_df_ABC_s3) #whole_df_ABC_20


whole_df_ABC$dlac <- whole_df_ABC$lac_abc - whole_df_ABC$lac
whole_df_ABC$dmu <- whole_df_ABC$mu_abc - whole_df_ABC$mu
whole_df_ABC$dgam <- whole_df_ABC$gam_abc - whole_df_ABC$gam
whole_df_ABC$dlaa <- whole_df_ABC$laa_abc - whole_df_ABC$laa
whole_df_ABC$dnet_div <- whole_df_ABC$net_div_ABC - whole_df_ABC$net_div
whole_df_ABC$dext_frac <- whole_df_ABC$ext_frac_ABC - whole_df_ABC$ext_frac
# whole_df_ABC$total <- rep(rep(pars_ss$total, each = 400), 1) # 400,5
df <- whole_df_ABC
n <- 500
ABC_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
ABC_median$Method <- rep(c("ABC All","ABC Phylogenetic","ABC Diversity","ABC NLTT"),each = 160) #


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/delta_whole_df_MCMC.RData"))
whole_df_MCMC$Method = "MCMC"
whole_df_MCMC$total <- rep(rep(pars_ss$total, each = 2501), 1)
whole_df_MCMC$rep <- rep(rep(1:10, each = 2501), 16)
whole_df_MCMC$set <- rep(1:16, each = 25010)
whole_df_MCMC$num_clade <- rep(rep(pars_ss$num.clade, each = 2501), 1)
whole_df_MCMC$largest_clade <- rep(rep(pars_ss$largest.clade, each = 2501), 1)
whole_df_MCMC$dlac <- whole_df_MCMC$lac_mcmc - whole_df_MCMC$lac
whole_df_MCMC$dmu <- whole_df_MCMC$mu_mcmc - whole_df_MCMC$mu
whole_df_MCMC$dgam <- whole_df_MCMC$gam_mcmc - whole_df_MCMC$gam
whole_df_MCMC$dlaa <- whole_df_MCMC$laa_mcmc - whole_df_MCMC$laa
whole_df_MCMC$dnet_div <- whole_df_MCMC$net_div_mcmc - whole_df_MCMC$net_div
whole_df_MCMC$dext_frac <- whole_df_MCMC$ext_frac_MCMC - whole_df_MCMC$ext_frac

df<-whole_df_MCMC
n <- 2501
MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]


load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space6/DI/whole_df_MLE_DI.RData")
whole_df_MLE$Method = "MLE"
whole_df_MLE$total <- rep(rep(pars_ss$total, each = 1), 1)
whole_df_MLE$rep <- rep(rep(1:10, each = 1), 16)
whole_df_MLE$set <- rep(1:16, each = 10)
whole_df_MLE$num_clade <- rep(rep(pars_ss$num.clade, each = 1), 1)
whole_df_MLE$largest_clade <- rep(rep(pars_ss$largest.clade, each = 1), 1)
whole_df_MLE$dlac <- whole_df_MLE$lac_MLE - whole_df_MLE$lac
whole_df_MLE$dmu <- whole_df_MLE$mu_MLE - whole_df_MLE$mu
whole_df_MLE$dgam <- whole_df_MLE$gam_MLE - whole_df_MLE$gam
whole_df_MLE$dlaa <- whole_df_MLE$laa_MLE - whole_df_MLE$laa
whole_df_MLE$dnet_div <- whole_df_MLE$net_div_MLE - whole_df_MLE$net_div
whole_df_MLE$dext_frac <- whole_df_MLE$ext_frac_MLE - whole_df_MLE$ext_frac


whole_df_all <- rbind(whole_df_ABC[,c(1:5,10,12,14:25)],
                      whole_df_MCMC[,c(1:5,10,12,14:25)],
                      whole_df_MLE[,c(1:5,10,12,14:25)])



lac_names <- c(
  `0.4` = 'lambda^c~"="~0.4',
  `0.7` = 'lambda^c~"="~0.7'
)

mu_names <- c(
  `0` = 'mu~"="~0',
  `0.3` = 'mu~"="~0.3'
)

gam_names <- c(
  `0.003` = 'gamma~"="~0.003',
  `0.009` = 'gamma~"="~0.009'
)

laa_names <- c(
  `0.1` = 'lambda^a~"="~0.1',
  `1` = 'lambda^a~"="~1.0'
)

iqr = function(z, lower = 0.05, upper = 0.95) {
  data.frame(
    y = mean(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

whole_df_all$Method <- factor(whole_df_all$Method, levels = c("ABC All", "ABC Diversity", "ABC NLTT", "ABC Phylogenetic","MCMC","MLE"))

p_netdiv_all <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dnet_div,y  = Method,color  = Method)) +
  # ggplot2:: geom_jitter(position = position_jitter(height = 0.15, width = 0), alpha = .01,size  = 0.1) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_violin()+ #outlier.shape = NA
  # geom_density_ridges(stat = "binline", bins = 20, scale = 2, draw_baseline = TRUE)+
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1.5,1.5)+
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(whole_df_all$Method))))+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 16,colour = "black"),
                 axis.text.x = ggplot2::element_text(size = 14,colour = "black"),
                 axis.text.y = ggplot2::element_blank(),
                 strip.text = element_text(size = 14,colour = "black"),
                 legend.text = element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~Net~diversification))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_net_div.tiff"),
     units="px", width=4000, height=2500,res = 300,compression="lzw")
print(p_netdiv_all)
while (!is.null(dev.list()))  dev.off()


p_lac<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlac,y  = Method,color  = Method)) +
  # ggplot2:: geom_jitter(position = position_jitter(height = 0.15, width = 0), alpha = .012) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1,1.9)+
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(whole_df_all$Method))))+
  # ggplot2::guides(color = guide_legend(reverse = T))+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 20,colour = "black"),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 12,colour = "black"),
                 strip.text = element_text(size = 15,colour = "black")) +
  ggplot2::xlab(expression(Delta~lambda^c))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))

p_mu<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dmu,y  = Method,color  = Method)) +
  # ggplot2:: geom_jitter(position = position_jitter(height = 0.15, width = 0), alpha = .012) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.6,1.9)+
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(whole_df_all$Method))))+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 20,colour = "black"),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 11,colour = "black"),
                 strip.text = element_text(size = 15,colour = "black")) +
  ggplot2::xlab(expression(Delta~mu))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))

p_gam<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dgam,y  = Method,color  = Method)) +
  # ggplot2:: geom_jitter(position = position_jitter(height = 0.15, width = 0), alpha = .012) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.01,0.018)+
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(whole_df_all$Method))))+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 20,colour = "black"),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 11,colour = "black"),
                 strip.text = element_text(size = 15,colour = "black")) +
  ggplot2::xlab(expression(Delta~gamma))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))

p_laa<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlaa,y  = Method,color  = Method)) +
  # ggplot2:: geom_jitter(position = position_jitter(height = 0.15, width = 0), alpha = .012) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.6,1.9)+
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(whole_df_all$Method))))+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 20,colour = "black"),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),axis.text.y = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 11,colour = "black"),
                 strip.text = element_text(size = 15,colour = "black")) +
  ggplot2::xlab(expression(Delta~lambda^a))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_combine2_3.tiff"),
     units="px", width=6200, height=3500,res = 310,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_lac+ggplot2::theme(legend.position = "none"),
  p_mu+ggplot2::theme(legend.position = "none"),
  p_gam+ggplot2::theme(legend.position = "none"),
  p_laa+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 2
)
legend <- cowplot::get_legend(
  p_lac + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_est_final <- cowplot::plot_grid(param_estimates,legend,rel_widths = c(3, 0.3))
print(param_est_final)

while (!is.null(dev.list()))  dev.off()



