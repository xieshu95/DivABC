## plot all particles (ABC-new vs ABC-old vs MCMC VS MLE)
library(ggplot2)
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DD_nltt/obs_ss_long_with_pars.RData"))

ss = "ABC-old"
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_unif_DD_7ss/delta_whole_df_ABC_ss_set",0,".RData"))
whole_df_ABC$ss = "ABC-old"
whole_df_ABC_old = whole_df_ABC[,-6]
whole_df_ABC_old$total <- rep(rep(pars_ss$total, each = 500), 1)


ss = "ABC-new"
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DD_nltt/delta_whole_df_ABC_ss_set",0,".RData"))
whole_df_ABC$ss = "ABC-new"
whole_df_ABC_new = whole_df_ABC[,-6]
whole_df_ABC_new$total <- rep(rep(pars_ss$total, each = 300), 1)

whole_df_ABC <- rbind(whole_df_ABC_old,whole_df_ABC_new) #whole_df_ABC_20

whole_df_ABC$dlac <- whole_df_ABC$lac_abc - whole_df_ABC$lac
whole_df_ABC$dmu <- whole_df_ABC$mu_abc - whole_df_ABC$mu
whole_df_ABC$dgam <- whole_df_ABC$gam_abc - whole_df_ABC$gam
whole_df_ABC$dlaa <- whole_df_ABC$laa_abc - whole_df_ABC$laa
whole_df_ABC$dnet_div <- whole_df_ABC$net_div_ABC - whole_df_ABC$net_div
whole_df_ABC$dext_frac <- whole_df_ABC$ext_frac_ABC - whole_df_ABC$ext_frac
# whole_df_ABC$total <- rep(rep(pars_ss$total, each = 400), 1) # 500,5


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DD_nltt/delta_whole_df_MCMC.RData"))
whole_df_MCMC$ss = "MCMC"
whole_df_MCMC$total <- rep(rep(pars_ss$total, each = 1001), 1)
whole_df_MCMC$dlac <- whole_df_MCMC$lac_mcmc - whole_df_MCMC$lac
whole_df_MCMC$dmu <- whole_df_MCMC$mu_mcmc - whole_df_MCMC$mu
whole_df_MCMC$dgam <- whole_df_MCMC$gam_mcmc - whole_df_MCMC$gam
whole_df_MCMC$dlaa <- whole_df_MCMC$laa_mcmc - whole_df_MCMC$laa
whole_df_MCMC$dnet_div <- whole_df_MCMC$net_div_mcmc - whole_df_MCMC$net_div
whole_df_MCMC$dext_frac <- whole_df_MCMC$ext_frac_MCMC - whole_df_MCMC$ext_frac

load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DD_nltt/whole_df_MLE.RData")
whole_df_MLE$ss = "MLE"
whole_df_MLE$total <- rep(rep(pars_ss$total, each = 1), 1)
whole_df_MLE$dlac <- whole_df_MLE$lac_MLE - whole_df_MLE$lac
whole_df_MLE$dmu <- whole_df_MLE$mu_MLE - whole_df_MLE$mu
whole_df_MLE$dgam <- whole_df_MLE$gam_MLE - whole_df_MLE$gam
whole_df_MLE$dlaa <- whole_df_MLE$laa_MLE - whole_df_MLE$laa
whole_df_MLE$dnet_div <- whole_df_MLE$net_div_MLE - whole_df_MLE$net_div
whole_df_MLE$dext_frac <- whole_df_MLE$ext_frac_MLE - whole_df_MLE$ext_frac


whole_df_all <- rbind(whole_df_ABC[,c(1:5,10,12,14:21)],
                      whole_df_MCMC[,c(1:5,10,12,14:21)],
                      whole_df_MLE[,c(1:5,10,12,14:21)])

iqr = function(z, lower = 0.1, upper = 0.9) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

# # lables
# lac_names <- c(
#   `0.3` = lambda[c]~"="~"0.3",
#   `0.5` = lambda[c]~"="~"0.5",
#   `0.7` = lambda[c]~"="~"0.7"
# )
#
# mu_names <- c(
#   `0` = mu~"="~"0.3",
#   `0.1` = mu~"="~"0.1",
#   `0.2` = mu~"="~"0.2"
# )
#
# gam_names <- c(
#   `0.006` = gam~"="~"0.006",
#   `0.009` = gam~"="~"0.009",
#   `0.012` = gam~"="~"0.012"
# )
#
# laa_names <- c(
#   `0.1` = lambda[c]~"="~"0.1",
#   `0.2` = lambda[c]~"="~"0.2",
#   `0.3` = lambda[c]~"="~"0.3"
# )

lac_names <- c(
  `0.3` = 'lambda[c]~"="~0.3',
  `0.5` = 'lambda[c]~"="~0.5',
  `0.7` = 'lambda[c]~"="~0.7'
)

mu_names <- c(
  `0` = 'mu~"="~0',
  `0.1` = 'mu~"="~0.1',
  `0.2` = 'mu~"="~0.2'
)

gam_names <- c(
  `0.006` = 'gamma~"="~0.006',
  `0.009` = 'gamma~"="~0.009',
  `0.012` = 'gamma~"="~0.012'
)

laa_names <- c(
  `0.1` = 'lambda[a]~"="~0.1',
  `0.2` = 'lambda[a]~"="~0.2',
  `0.3` = 'lambda[a]~"="~0.3'
)


## 1.only compare ABC-NEW AND ABC-OLD plot delta-rate for all the particles
p_netdiv_lac <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac~ mu,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                         mu = as_labeller(mu_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DD_nltt/drate_ABC_netdiv_lac.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_netdiv_lac)
while (!is.null(dev.list()))  dev.off()

p_netdiv_mu <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(. ~ mu, scales="free_y",
             labeller = label_bquote(.(as.expression(
               eval(parse(text = paste0('mu_names', '$`', mu, '`')))
             ))))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DD_nltt/drate_ABC_netdiv_mu.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_netdiv_mu)
while (!is.null(dev.list()))  dev.off()

p_netdiv_gam <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ gam)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DD_nltt/drate_ABC_netdiv_gam.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_netdiv_gam)
while (!is.null(dev.list()))  dev.off()


p_netdiv_laa <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ laa)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DD_nltt/drate_ABC_netdiv_laa.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_netdiv_laa)
while (!is.null(dev.list()))  dev.off()


## 81 into 1 net div
iqr = function(z, lower = 0.1, upper = 0.9) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}
p_netdiv_all <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dnet_div,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~Net~diversification))+
  ggplot2::ylab("Methods") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DD_nltt/drate_81.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_netdiv_all)
while (!is.null(dev.list()))  dev.off()


p_lac<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlac,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~lambda[c]))+
  ggplot2::ylab("Methods") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DD_nltt/drate_81_lac.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_lac)
while (!is.null(dev.list()))  dev.off()

p_mu<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dmu,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~mu))+
  ggplot2::ylab("Methods") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DD_nltt/drate_81_mu.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_mu)
while (!is.null(dev.list()))  dev.off()

p_gam<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dgam,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.03,0.03)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~gamma))+
  ggplot2::ylab("Methods") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DD_nltt/drate_81_gam.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_gam)
while (!is.null(dev.list()))  dev.off()

p_laa<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlaa,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~lambda[a]))+
  ggplot2::ylab("Methods") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DD_nltt/drate_81_laa.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_laa)
while (!is.null(dev.list()))  dev.off()

