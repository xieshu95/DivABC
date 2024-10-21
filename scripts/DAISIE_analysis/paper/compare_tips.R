library(ggplot2)
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/obs_ss_long_with_pars_DI.RData"))

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space3/DI/delta_whole_df_ABC_ss_set",2,".RData"))
whole_df_ABC$ss = "ABC tip old"
whole_df_ABC_s0 = whole_df_ABC
whole_df_ABC_s0$total <- rep(rep(pars_ss$total, each = 400), 1)
whole_df_ABC_s0$rep <- rep(rep(1:10, each = 400), 16)
whole_df_ABC_s0$num_clade <- rep(rep(pars_ss$num.clade, each = 400), 1)
whole_df_ABC_s0$largest_clade <- rep(rep(pars_ss$largest.clade, each = 400), 1)


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/delta_whole_df_ABC_ss_set",2,".RData"))
whole_df_ABC$ss = "ABC tip new"
whole_df_ABC_s1 = whole_df_ABC
whole_df_ABC_s1$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s1$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s1$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s1$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)

# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space3/DI2/delta_whole_df_ABC_ss_set",2,".RData"))
# whole_df_ABC$ss = "ABC tip old2"
# whole_df_ABC_s2 = whole_df_ABC
# whole_df_ABC_s2$total <- rep(rep(pars_ss$total, each = 500), 1)
# whole_df_ABC_s2$rep <- rep(rep(1:10, each = 500), 16)
# whole_df_ABC_s2$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
# whole_df_ABC_s2$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)


whole_df_ABC <- rbind(whole_df_ABC_s0,
                      whole_df_ABC_s1)

whole_df_ABC$dlac <- whole_df_ABC$lac_abc - whole_df_ABC$lac
whole_df_ABC$dmu <- whole_df_ABC$mu_abc - whole_df_ABC$mu
whole_df_ABC$dgam <- whole_df_ABC$gam_abc - whole_df_ABC$gam
whole_df_ABC$dlaa <- whole_df_ABC$laa_abc - whole_df_ABC$laa
whole_df_ABC$dnet_div <- whole_df_ABC$net_div_ABC - whole_df_ABC$net_div
whole_df_ABC$dext_frac <- whole_df_ABC$ext_frac_ABC - whole_df_ABC$ext_frac
# whole_df_ABC$total <- rep(rep(pars_ss$total, each = 400), 1) # 400,5


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space6/DI/delta_whole_df_MCMC.RData"))
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

load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space6/DI/whole_df_MLE_DI.RData")
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


whole_df_all <- rbind(whole_df_ABC[,c(1:5,10,12,14:24)])


lac_names <- c(
  `0.4` = 'lambda[c]~"="~0.4',
  `0.7` = 'lambda[c]~"="~0.7'
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
  `0.1` = 'lambda[a]~"="~0.1',
  `1` = 'lambda[a]~"="~1.0'
)


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
p_netdiv_all <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dnet_div,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA,width = 0.2)+ #outlier.shape = NA
  # geom_density_ridges(stat = "binline", bins = 20, scale = 2, draw_baseline = TRUE)+
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1.5,1.5)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","pink2","#8CC269","#4393C3"))+
  # ggplot2::scale_fill_manual("Method",values = c("red3","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~Net~diversification))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/drate_net_div2.tiff"),
     units="px", width=4000, height=2500,res = 300,compression="lzw")
print(p_netdiv_all)
while (!is.null(dev.list()))  dev.off()


p_lac<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlac,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","pink2","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~lambda[c]))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/drate_lac.tiff"),
     units="px", width=4000, height=2500,res = 300,compression="lzw")
print(p_lac)
while (!is.null(dev.list()))  dev.off()

p_mu<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dmu,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.5,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","pink2","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~mu))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/drate_mu.tiff"),
     units="px", width=4000, height=2500,res = 300,compression="lzw")
print(p_mu)
while (!is.null(dev.list()))  dev.off()

p_gam<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dgam,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.01,0.02)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","pink2","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~gamma))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/drate_gam.tiff"),
     units="px", width=4000, height=2500,res = 300,compression="lzw")
print(p_gam)
while (!is.null(dev.list()))  dev.off()

p_laa<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlaa,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.5,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","pink2","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~lambda[a]))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/drate_laa.tiff"),
     units="px", width=4000, height=2500,res = 300,compression="lzw")
print(p_laa)
while (!is.null(dev.list()))  dev.off()


#####
## 81 into 1 net div (each rep)
iqr = function(z, lower = 0.025, upper = 0.975) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

p_netdiv_all <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dnet_div,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1.5,1.5)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","pink2","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~Net~diversification))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(rep~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/drate_net_div_each_rep.tiff"),
     units="px", width=8000, height=4000,res = 350,compression="lzw")
print(p_netdiv_all)
while (!is.null(dev.list()))  dev.off()


p_lac<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlac,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1,1.7)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","pink2","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~lambda[c]))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(rep~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/drate_lac_each_rep.tiff"),
     units="px", width=8000, height=4000,res = 350,compression="lzw")
print(p_lac)
while (!is.null(dev.list()))  dev.off()

p_mu<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dmu,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.5,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","pink2","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~mu))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(rep~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/drate_mu_each_rep.tiff"),
     units="px", width=8000, height=4000,res = 350,compression="lzw")
print(p_mu)
while (!is.null(dev.list()))  dev.off()

p_gam<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dgam,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.01,0.02)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","pink2","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~gamma))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(rep~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/drate_gam_each_rep.tiff"),
     units="px", width=8000, height=4000,res = 350,compression="lzw")
print(p_gam)
while (!is.null(dev.list()))  dev.off()

p_laa<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlaa,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1.5) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005,fill = "white") +
  # ggplot2::geom_boxplot(outlier.shape=NA)+
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.5,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","pink2","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~lambda[a]))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(rep~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/drate_laa_each_rep.tiff"),
     units="px", width=8000, height=4000,res = 350,compression="lzw")
print(p_laa)
while (!is.null(dev.list()))  dev.off()


library(ggplot2)
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space3/DI/DAISIE_ABC_DI"
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_ABC_DI.csv")
for(n in c(2)){
  ABC_df<-c()
  generation <-c()
  set_val <- c()
  Rep <- c()
  for(set in 1:160){
    message("set", set)
    true_rates <- param_data[set,]
    file_to_load <- grep(paste0("DAISIE_ABC_DI_param_set_", set,"_ss_",n,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)
    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))

      n_gene <- length(output$ABC)
      if(nrow(output$ABC[[n_gene]]) < 400){ #500
        n_gene <- n_gene - 1
      }
      for(i in 1:n_gene){
        ABC_df <- rbind(ABC_df,output$ABC[[i]])
      }

      # colnames(ss_dist) <- c("MPD","MNTD","SDPD","SDNTD",
      #                        "D","Total","Ratio","NLTT")
      ABC_df <- as.data.frame(ABC_df)
      generation <- c(generation, rep(1:n_gene, each = 400))
      set_val <- c(set_val, rep(set,n_gene*400))

    } else {
      ABC_df <-rbind(ABC_df,rep(NA,4))
      generation <- c(generation, 1)
      set_val <- c(set_val, set)
    }
  }
  colnames(ABC_df) <- c("lac_abc","mu_abc","gam_abc","laa_abc")#"lac","mu","gam","laa","K",
  rownames(ABC_df) <- 1:nrow(ABC_df)

  rep <- set_val %% 10
  for(i in 1:length(rep)){
    if (rep[i] == 0){
      rep[i] = 10
    }
  }


  ABC_df_all <- data.frame(param_data[set_val,],ABC_df,generation,set_val,rep)

  ABC_df_all$net_div <- (ABC_df_all$lac-ABC_df_all$mu)
  ABC_df_all$net_div_ABC <- (ABC_df_all$lac_abc-ABC_df_all$mu_abc)
  ABC_df_all$ext_frac <- (ABC_df_all$mu)/(ABC_df_all$lac)
  ABC_df_all$ext_frac_ABC <- (ABC_df_all$mu_abc)/(ABC_df_all$lac_abc)
  save(ABC_df_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space3/DI/rates_all_generations",n,".RData"))
}




load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/with nltt2/rates_all_generations",2,".RData"))
ABC_df_all$ss = "ABC tips new"
ABC_df_all_0 = ABC_df_all



load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space3/DI/rates_all_generations",2,".RData"))
ABC_df_all$ss = "ABC tips old"
ABC_df_all_1 = ABC_df_all


ABC_df_all <- rbind(ABC_df_all_0,
                    ABC_df_all_1) #whole_df_ABC_20

ABC_df_all$dlac <- ABC_df_all$lac_abc - ABC_df_all$lac
ABC_df_all$dmu <- ABC_df_all$mu_abc - ABC_df_all$mu
ABC_df_all$dgam <- ABC_df_all$gam_abc - ABC_df_all$gam
ABC_df_all$dlaa <- ABC_df_all$laa_abc - ABC_df_all$laa
ABC_df_all$dnet_div <- ABC_df_all$net_div_ABC - ABC_df_all$net_div
ABC_df_all$dext_frac <- ABC_df_all$ext_frac_ABC - ABC_df_all$ext_frac
# ABC_df_all$total <- rep(rep(pars_ss$total, each = 400), 1) # 500,5
save(ABC_df_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/all_generations_all_ss.RData"))

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/all_generations_all_ss.RData"))


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

library(ggplot2)
iqr = function(z, lower = 0.025, upper = 0.975) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}
library(ggplot2)
ABC_df_all_4gene<- ABC_df_all[which(ABC_df_all$generation <15),]
p_netdiv_all <-ggplot2::ggplot(data = ABC_df_all_4gene,mapping = aes(x = dnet_div,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-2,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~Net~diversification))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(generation~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                            mu = as_labeller(mu_names, label_parsed),
                                                            gam = as_labeller(gam_names, label_parsed),
                                                            laa = as_labeller(laa_names, label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/all_ss_drate_each_gene_netdiv.tiff"),
     units="px", width=6000, height=3000,res = 350,compression="lzw")
print(p_netdiv_all)
while (!is.null(dev.list()))  dev.off()



p_lac <-ggplot2::ggplot(data = ABC_df_all_4gene,mapping = aes(x = dlac,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1.2,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~lambda^c))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(generation~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                            mu = as_labeller(mu_names, label_parsed),
                                                            gam = as_labeller(gam_names, label_parsed),
                                                            laa = as_labeller(laa_names, label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/all_ss_drate_each_gene_lac.tiff"),
     units="px", width=6000, height=3000,res = 350,compression="lzw")
print(p_lac)
while (!is.null(dev.list()))  dev.off()

p_mu<-ggplot2::ggplot(data = ABC_df_all_4gene,mapping = aes(x = dmu,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~mu))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(generation~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                            mu = as_labeller(mu_names, label_parsed),
                                                            gam = as_labeller(gam_names, label_parsed),
                                                            laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/all_ss_drate_each_gene_mu.tiff"),
     units="px", width=6000, height=3000,res = 350,compression="lzw")
print(p_mu)
while (!is.null(dev.list()))  dev.off()

p_gam<-ggplot2::ggplot(data = ABC_df_all_4gene,mapping = aes(x = dgam,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.01,0.02)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~gamma))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(generation~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                            mu = as_labeller(mu_names, label_parsed),
                                                            gam = as_labeller(gam_names, label_parsed),
                                                            laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/all_ss_drate_each_gene_gam.tiff"),
     units="px", width=6000, height=3000,res = 350,compression="lzw")
print(p_gam)
while (!is.null(dev.list()))  dev.off()

p_laa<-ggplot2::ggplot(data = ABC_df_all_4gene,mapping = aes(x = dlaa,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~lambda^a))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(generation~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                            mu = as_labeller(mu_names, label_parsed),
                                                            gam = as_labeller(gam_names, label_parsed),
                                                            laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space5/compare_tips/all_ss_drate_each_gene_laa.tiff"),
     units="px", width=6000, height=3000,res = 350,compression="lzw")
print(p_laa)
while (!is.null(dev.list()))  dev.off()

