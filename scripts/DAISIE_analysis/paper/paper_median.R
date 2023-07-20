## 3 dimentiuons in a 2D plot
### median
library(ggplot2)
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/obs_ss_long_with_pars_DI.RData"))

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/delta_whole_df_ABC_ss_set",0,".RData"))
whole_df_ABC$Method = "ABC All"
whole_df_ABC_s0 = whole_df_ABC
whole_df_ABC_s0$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s0$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s0$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s0$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/delta_whole_df_ABC_ss_set",1,".RData"))
whole_df_ABC$Method = "ABC Phylogenetic"
whole_df_ABC_s1 = whole_df_ABC
whole_df_ABC_s1$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s1$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s1$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s1$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/delta_whole_df_ABC_ss_set",2,".RData"))
whole_df_ABC$Method = "ABC Diversity"
whole_df_ABC_s2 = whole_df_ABC
whole_df_ABC_s2$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s2$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s2$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s2$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/delta_whole_df_ABC_ss_set",3,".RData"))
whole_df_ABC$Method = "ABC NLTT"
whole_df_ABC_s3 = whole_df_ABC
whole_df_ABC_s3$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s3$rep <- rep(rep(1:10, each = 500), 16)
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
ABC_median$Method <- rep(c("ABC All","ABC Phylogenetic","ABC Diversity","ABC NLTT"),each = 160)


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/delta_whole_df_MCMC.RData"))
whole_df_MCMC$Method = "MCMC"
whole_df_MCMC$total <- rep(rep(pars_ss$total, each = 2501), 1)
whole_df_MCMC$rep <- rep(rep(1:10, each = 2501), 16)
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
whole_df_MLE$num_clade <- rep(rep(pars_ss$num.clade, each = 1), 1)
whole_df_MLE$largest_clade <- rep(rep(pars_ss$largest.clade, each = 1), 1)
whole_df_MLE$dlac <- whole_df_MLE$lac_MLE - whole_df_MLE$lac
whole_df_MLE$dmu <- whole_df_MLE$mu_MLE - whole_df_MLE$mu
whole_df_MLE$dgam <- whole_df_MLE$gam_MLE - whole_df_MLE$gam
whole_df_MLE$dlaa <- whole_df_MLE$laa_MLE - whole_df_MLE$laa
whole_df_MLE$dnet_div <- whole_df_MLE$net_div_MLE - whole_df_MLE$net_div
whole_df_MLE$dext_frac <- whole_df_MLE$ext_frac_MLE - whole_df_MLE$ext_frac


whole_df_all <- rbind(ABC_median[,c(1:5,10,12,14:24)],
                      MCMC_median[,c(1:5,10,12,14:24)],
                      whole_df_MLE[,c(1:5,10,12,14:24)])
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
p_netdiv <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total/num_clade,y = largest_clade,shape = Method)) +
  ggplot2::geom_point(aes(color = dnet_div), alpha = 0.7,size = 0.8) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  # ggplot2::ylim(-2,1.2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_color_continuous(low = 'white', high = 'black', name = expression(Delta~Net~diversification))+
  ggplot2::theme(title = ggplot2::element_text(size = 12,colour = "black"),
                 text = ggplot2::element_text(size = 12,colour = "black"),
                 strip.text = element_text(size = 12,colour = "black")) +
  ggplot2::labs("Method") +
  ggplot2::xlab("Average clade size") +
  # ggplot2::xlab(expression(log[10]~(Tree~size))) +
  ggplot2::ylab("Largest")+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/average_largest_median.tiff"),
     units="px", width=4000, height=2000,res = 350,compression="lzw")
print(p_netdiv)
while (!is.null(dev.list()))  dev.off()



p_netdiv <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = total,shape = Method)) +
  ggplot2::geom_point(aes(color = abs(dnet_div)), alpha = 0.8,size = 0.8) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  # ggplot2::ylim(-2,1.2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_color_continuous(low = 'lightgray', high = 'black', name = expression(Delta~Net~diversification))+
  ggplot2::theme(title = ggplot2::element_text(size = 12,colour = "black"),
                 text = ggplot2::element_text(size = 12,colour = "black"),
                 strip.text = element_text(size = 12,colour = "black")) +
  ggplot2::labs("Method") +
  ggplot2::xlab("Number of clades") +
  # ggplot2::xlab(expression(log[10]~(Tree~size))) +
  ggplot2::ylab("Species richness")+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/richness_numclade_median.tiff"),
     units="px", width=4000, height=2000,res = 350,compression="lzw")
print(p_netdiv)
while (!is.null(dev.list()))  dev.off()


p_netdiv <-ggplot2::ggplot(data = ABC_median[1:160,],mapping = aes(x = total,y = num_clade,shape = Method)) +
  ggplot2::geom_point(aes(color = abs(dnet_div)), alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  # ggplot2::ylim(-2,1.2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_color_continuous(low = 'white', high = 'black')+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Number of clades") +
  # ggplot2::xlab(expression(log[10]~(Tree~size))) +
  ggplot2::ylab("Largest")+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/numclade_largest_netdiv_median.tiff"),
     units="px", width=4000, height=2000,res = 350,compression="lzw")
print(p_netdiv)
while (!is.null(dev.list()))  dev.off()

### paper
p_netdiv <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total/num_clade,y = dnet_div,color = Method)) + #y = abs(dnet_div)
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1.9,1.6)+ #(0,1.6)
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12,colour = "black"),
                 text = ggplot2::element_text(size = 12,colour = "black"),
                 strip.text = element_text(size = 12,colour = "black")) +
  ggplot2::xlab("Average clade size") +
  # ggplot2::xlab(expression(log[10]~(Tree~size))) +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/ave_netdiv_median.tiff"),
     units="px", width=4000, height=2000,res = 350,compression="lzw")
print(p_netdiv)
while (!is.null(dev.list()))  dev.off()

p_netdiv <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dnet_div,color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1.9,1.6)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12,colour = "black"),
                 text = ggplot2::element_text(size = 12,colour = "black"),
                 strip.text = element_text(size = 12,colour = "black")) +
  ggplot2::xlab("Species richness") +
  # ggplot2::xlab(expression(log[10]~(Tree~size))) +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/total_netdiv_median.tiff"),
     units="px", width=4000, height=2000,res = 350,compression="lzw")
print(p_netdiv)
while (!is.null(dev.list()))  dev.off()


p_netdiv <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = (dnet_div),color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1.9,1.6)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12,colour = "black"),
                 text = ggplot2::element_text(size = 12,colour = "black"),
                 strip.text = element_text(size = 12,colour = "black")) +
  ggplot2::xlab("Number of clades") +
  # ggplot2::xlab(expression(log[10]~(Tree~size))) +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/numclade_netdiv_median.tiff"),
     units="px", width=4000, height=2000,res = 350,compression="lzw")
print(p_netdiv)
while (!is.null(dev.list()))  dev.off()

p_netdiv <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = largest_clade,y = (dnet_div),color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1.9,1.6)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12,colour = "black"),
                 text = ggplot2::element_text(size = 12,colour = "black"),
                 strip.text = element_text(size = 12,colour = "black")) +
  ggplot2::xlab("Largest clade size") +
  # ggplot2::xlab(expression(log[10]~(Tree~size))) +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/largest_netdiv_median.tiff"),
     units="px", width=4000, height=2000,res = 350,compression="lzw")
print(p_netdiv)
while (!is.null(dev.list()))  dev.off()


## total vs drates for each rate
p_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dlac,color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,1.6)+
  ggplot2::xlim(0,650)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12,colour = "black"),
                 text = ggplot2::element_text(size = 12,colour = "black"),
                 strip.text = element_text(size = 12,colour = "black")) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~lambda^c))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)


p_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dmu, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,2)+
  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12,colour = "black"),
                 text = ggplot2::element_text(size = 12,colour = "black"),
                 strip.text = element_text(size = 12,colour = "black")) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~mu))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)

p_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dgam, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.01,0.02)+
  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12,colour = "black"),
                 text = ggplot2::element_text(size = 12,colour = "black"),
                 strip.text = element_text(size = 12,colour = "black")) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~gamma))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)

p_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dlaa, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1.5,1.9)+
  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12,colour = "black"),
                 text = ggplot2::element_text(size = 12,colour = "black"),
                 strip.text = element_text(size = 12,colour = "black")) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~lambda^a))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)


tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/facet_total_drate_median.tiff"),
     units="px", width=6500, height=3000,res = 350,compression="lzw")
params <- cowplot::plot_grid(
  p_lac+ggplot2::theme(legend.position = "none"),
  p_mu+ggplot2::theme(legend.position = "none"),
  p_gam+ggplot2::theme(legend.position = "none"),
  p_laa+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 2
)
legend <- cowplot::get_legend(
  p_lac + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_estimates <- cowplot::plot_grid(params,legend,
                                      rel_widths = c(3,0.4)
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()



