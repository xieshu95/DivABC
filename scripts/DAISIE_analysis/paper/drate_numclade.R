# 81 all particles comparsion
## plot all particles (ABC-new vs ABC-old vs MCMC VS MLE)
library(ggplot2)
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/obs_ss_long_with_pars_DI.RData"))

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/delta_whole_df_ABC_ss_set",0,".RData"))
whole_df_ABC$ss = "ABC All"
whole_df_ABC_s0 = whole_df_ABC
whole_df_ABC_s0$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s0$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s0$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s0$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/delta_whole_df_ABC_ss_set",1,".RData"))
whole_df_ABC$ss = "ABC Phylogenetic"
whole_df_ABC_s1 = whole_df_ABC
whole_df_ABC_s1$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s1$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s1$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s1$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/delta_whole_df_ABC_ss_set",2,".RData"))
whole_df_ABC$ss = "ABC Diversity"
whole_df_ABC_s2 = whole_df_ABC
whole_df_ABC_s2$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s2$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s2$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s2$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/delta_whole_df_ABC_ss_set",3,".RData"))
whole_df_ABC$ss = "ABC NLTT"
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


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/delta_whole_df_MCMC.RData"))
whole_df_MCMC$ss = "MCMC"
whole_df_MCMC$total <- rep(rep(pars_ss$total, each = 5001), 1)
whole_df_MCMC$rep <- rep(rep(1:10, each = 5001), 16)
whole_df_MCMC$num_clade <- rep(rep(pars_ss$num.clade, each = 5001), 1)
whole_df_MCMC$largest_clade <- rep(rep(pars_ss$largest.clade, each = 5001), 1)
whole_df_MCMC$dlac <- whole_df_MCMC$lac_mcmc - whole_df_MCMC$lac
whole_df_MCMC$dmu <- whole_df_MCMC$mu_mcmc - whole_df_MCMC$mu
whole_df_MCMC$dgam <- whole_df_MCMC$gam_mcmc - whole_df_MCMC$gam
whole_df_MCMC$dlaa <- whole_df_MCMC$laa_mcmc - whole_df_MCMC$laa
whole_df_MCMC$dnet_div <- whole_df_MCMC$net_div_mcmc - whole_df_MCMC$net_div
whole_df_MCMC$dext_frac <- whole_df_MCMC$ext_frac_MCMC - whole_df_MCMC$ext_frac

load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/whole_df_MLE_DI.RData")
whole_df_MLE$ss = "MLE"
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


whole_df_all <- rbind(whole_df_ABC[,c(1:5,10,12,14:24)],
                      whole_df_MCMC[,c(1:5,10,12,14:24)],
                      whole_df_MLE[,c(1:5,10,12,14:24)])
## 81 into 1 net div (combine 10 reps)
iqr = function(z, lower = 0.05, upper = 0.95) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}
library(ggridges)
library(ggplot2)
library(viridis)
# install.packages("hrbrthemes")
library(hrbrthemes)

## link to space3_DI
p_netdiv <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total/num_clade,y = dnet_div,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.7) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-2,1.2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Average clade size") +
  # ggplot2::xlab(expression(log[10]~(Tree~size))) +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
  # facet_grid(~ gam,labeller = labeller(gam  = as_labeller(gam_names,  label_parsed)))
# facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
#                                                mu = as_labeller(mu_names, label_parsed),
#                                                gam = as_labeller(gam_names, label_parsed),
#                                                laa = as_labeller(laa_names, label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/drate_netdiv_per_clade.tiff"),
     units="px", width=4000, height=2000,res = 350,compression="lzw")
print(p_netdiv)
while (!is.null(dev.list()))  dev.off()

p_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total/num_clade,y = dlac, color = ss)) + ##,color = as.factor(gam)
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+ #,color = "red3"
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Average clade size") +
  ggplot2::ylab(expression(Delta~lambda^c))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/drate_lac.tiff"),
#      units="px", width=2500, height=1500,res = 350,compression="lzw")
# print(p_lac)
# while (!is.null(dev.list()))  dev.off()

p_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total/num_clade,y = dmu, color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.5,2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Average clade size") +
  ggplot2::ylab(expression(Delta~mu))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/drate_mu.tiff"),
#      units="px", width=2500, height=1500,res = 350,compression="lzw")
# print(p_mu)
# while (!is.null(dev.list()))  dev.off()

p_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total/num_clade,y = dgam, color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.01,0.02)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Average clade size") +
  ggplot2::ylab(expression(Delta~gamma))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/drate_gam.tiff"),
#      units="px", width=2500, height=1500,res = 350,compression="lzw")
# print(p_gam)
# while (!is.null(dev.list()))  dev.off()

p_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total/num_clade,y = dlaa, color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Average clade size") +
  ggplot2::ylab(expression(Delta~lambda^a))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/drate_all_rates_per_clade.tiff"),
     units="px", width=5500, height=2500,res = 300,compression="lzw")
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





#####
p_netdiv <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = largest_clade,y = dnet_div,color = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.7) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-2,1.2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Largest clade") +
  # ggplot2::xlab(expression(log[10]~(Tree~size))) +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# facet_grid(~ gam,labeller = labeller(gam  = as_labeller(gam_names,  label_parsed)))
# facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
#                                                mu = as_labeller(mu_names, label_parsed),
#                                                gam = as_labeller(gam_names, label_parsed),
#                                                laa = as_labeller(laa_names, label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/drate_netdiv_largest.tiff"),
     units="px", width=4000, height=2000,res = 350,compression="lzw")
print(p_netdiv)
while (!is.null(dev.list()))  dev.off()




p_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = largest_clade,y = dlac, color = Method)) + ##,color = as.factor(gam)
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+ #,color = "red3"
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Largest clade") +
  ggplot2::ylab(expression(Delta~lambda^c))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/drate_lac.tiff"),
#      units="px", width=2500, height=1500,res = 350,compression="lzw")
# print(p_lac)
# while (!is.null(dev.list()))  dev.off()

p_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = largest_clade,y = dmu, color = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.5,2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Largest clade") +
  ggplot2::ylab(expression(Delta~mu))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/drate_mu.tiff"),
#      units="px", width=2500, height=1500,res = 350,compression="lzw")
# print(p_mu)
# while (!is.null(dev.list()))  dev.off()

p_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = largest_clade,y = dgam, color = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.01,0.02)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Largest clade") +
  ggplot2::ylab(expression(Delta~gamma))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/drate_gam.tiff"),
#      units="px", width=2500, height=1500,res = 350,compression="lzw")
# print(p_gam)
# while (!is.null(dev.list()))  dev.off()

p_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = largest_clade,y = dlaa, color = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Largest clade") +
  ggplot2::ylab(expression(Delta~lambda^a))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/drate_all_rates_largest.tiff"),
     units="px", width=5500, height=2500,res = 300,compression="lzw")
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



p_netdiv <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = dnet_div,color = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.7) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-2,1.2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Number of clades") +
  # ggplot2::xlab(expression(log[10]~(Tree~size))) +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
  # facet_grid(~ gam,labeller = labeller(gam  = as_labeller(gam_names,  label_parsed)))
# facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
#                                                mu = as_labeller(mu_names, label_parsed),
#                                                gam = as_labeller(gam_names, label_parsed),
#                                                laa = as_labeller(laa_names, label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/drate_netdiv_numclade.tiff"),
     units="px", width=4000, height=2000,res = 350,compression="lzw")
print(p_netdiv)
while (!is.null(dev.list()))  dev.off()

p_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = dlac, color = Method)) + ##,color = as.factor(gam)
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+ #,color = "red3"
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Number of clades") +
  ggplot2::ylab(expression(Delta~lambda^c))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/drate_lac.tiff"),
#      units="px", width=2500, height=1500,res = 350,compression="lzw")
# print(p_lac)
# while (!is.null(dev.list()))  dev.off()

p_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = dmu, color = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.5,2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Largest clade") +
  ggplot2::ylab(expression(Delta~mu))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/drate_mu.tiff"),
#      units="px", width=2500, height=1500,res = 350,compression="lzw")
# print(p_mu)
# while (!is.null(dev.list()))  dev.off()

p_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = dgam, color = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.01,0.02)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Number of clades") +
  ggplot2::ylab(expression(Delta~gamma))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/drate_gam.tiff"),
#      units="px", width=2500, height=1500,res = 350,compression="lzw")
# print(p_gam)
# while (!is.null(dev.list()))  dev.off()

p_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = dlaa, color = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Number of clades") +
  ggplot2::ylab(expression(Delta~lambda^a))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/drate_all_rates_numclade.tiff"),
     units="px", width=5500, height=2500,res = 300,compression="lzw")
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

