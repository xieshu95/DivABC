## analyse DAISIE results (DI version comparison) for 'daisie_low_prior'
#####
library(ggplot2)
# formate results
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/obs_ss_long_with_pars_DI.RData"))
## ABC results
folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/DAISIE_ABC_short_DI")
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_ABC_short_DI.csv")
param_data2<-param_data[rep(seq_len(nrow(param_data)), each=500),] #500

# 1. formate ABC results
for(n in c(2,3)){ # 1,2,6,7,20
  lac_abc <- c()
  mu_abc <- c()
  gam_abc <- c()
  laa_abc <- c()
  n_iter <-c()
  n_iteration <- c()
  for(i in 1:160){
    file_to_load <- grep(paste0("DAISIE_ABC_short_DI_param_set_", i,"_ss_",n,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      n_iteration <- c(n_iteration,rep(num_iter,500))
      if(output$n_iter <= 2){
        lac_abc <- c(lac_abc, rep(NA,500))
        mu_abc <- c(mu_abc, rep(NA,500))
        gam_abc <- c(gam_abc, rep(NA,500))
        laa_abc <- c(laa_abc, rep(NA,500))
      } else if (nrow(output$ABC[[output$n_iter]]) == 500) {
        lac_abc <- c(lac_abc, output$ABC[[num_iter]][,1])
        mu_abc <- c(mu_abc, output$ABC[[num_iter]][,2])
        gam_abc <- c(gam_abc, output$ABC[[num_iter]][,3])
        laa_abc <- c(laa_abc, output$ABC[[num_iter]][,4])
      } else {
        lac_abc <- c(lac_abc, output$ABC[[num_iter-1]][,1])
        mu_abc <- c(mu_abc, output$ABC[[num_iter-1]][,2])
        gam_abc <- c(gam_abc, output$ABC[[num_iter-1]][,3])
        laa_abc <- c(laa_abc, output$ABC[[num_iter-1]][,4])
      }
    } else {
      lac_abc <- c(lac_abc, rep(NA,500))
      mu_abc <- c(mu_abc, rep(NA,500))
      gam_abc <- c(gam_abc, rep(NA,500))
      laa_abc <- c(laa_abc, rep(NA,500))
    }
  }
  whole_df_ABC <- data.frame(param_data2,
                             # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                             lac_abc,mu_abc,gam_abc,laa_abc)
  save(whole_df_ABC,
       file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/whole_df_ABC_ss_set",n,".RData"))

  whole_df_ABC$net_div <- (whole_df_ABC$lac-whole_df_ABC$mu)
  whole_df_ABC$net_div_ABC <- (whole_df_ABC$lac_abc-whole_df_ABC$mu_abc)

  whole_df_ABC$ext_frac <- (whole_df_ABC$mu)/(whole_df_ABC$lac)
  whole_df_ABC$ext_frac_ABC <- (whole_df_ABC$mu_abc)/(whole_df_ABC$lac_abc)
  save(whole_df_ABC,file =
         paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/delta_whole_df_ABC_ss_set",n,".RData"))

}

# 2. formate MCMC results (only plot the etimation points with ABC results)
# skip
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/DAISIE_MCMC_short_DI"
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_MCMC_short_DI.csv")
param_data <- param_data[1:160,]
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=2501),] #2001/400

lac_mcmc <- c()
mu_mcmc <- c()
gam_mcmc <- c()
laa_mcmc <- c()

seq <- seq(1,5001,2)
for(i in 1:160){
  file_to_load <- grep(paste0("DAISIE_MCMC_short_DI_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)



  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lac_mcmc <- c(lac_mcmc, output[seq,1]) # output[4002:2501,1]
    mu_mcmc <- c(mu_mcmc, output[seq,2])
    gam_mcmc <- c(gam_mcmc, output[seq,3])
    laa_mcmc <- c(laa_mcmc, output[seq,4])
  } else {
    lac_mcmc <- c(lac_mcmc, rep(NA,2501)) #rep(NA,400)
    mu_mcmc <- c(mu_mcmc, rep(NA,2501))
    gam_mcmc <- c(gam_mcmc, rep(NA,2501))
    laa_mcmc <- c(laa_mcmc, rep(NA,2501))
  }
}

whole_df_MCMC <- data.frame(param_data3,
                            lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc)

save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/whole_df_MCMC.RData"))

whole_df_MCMC$net_div <- (whole_df_MCMC$lac-whole_df_MCMC$mu)
whole_df_MCMC$net_div_mcmc <- (whole_df_MCMC$lac_mcmc - whole_df_MCMC$mu_mcmc)

whole_df_MCMC$ext_frac <- (whole_df_MCMC$mu)/(whole_df_MCMC$lac)
whole_df_MCMC$ext_frac_MCMC <- (whole_df_MCMC$mu_mcmc)/(whole_df_MCMC$lac_mcmc)

save(whole_df_MCMC,
     file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/delta_whole_df_MCMC.RData"))
#

# ######
# 3. MLE
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/whole_df_MLE.RData"))

# MLE_DI directly load MLE results from cluster
param_data <- readr::read_csv2("data/DAISIE_ABC_short_DI.csv")
load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/MLE_DI.RData")
whole_df_MLE <- data.frame(param_data,MLE_all[1:4])

whole_df_MLE$net_div <- (whole_df_MLE$lac-whole_df_MLE$mu)
whole_df_MLE$net_div_MLE <- (whole_df_MLE$lac_MLE-whole_df_MLE$mu_MLE)

whole_df_MLE$ext_frac <- (whole_df_MLE$mu)/(whole_df_MLE$lac)
whole_df_MLE$ext_frac_MLE <- (whole_df_MLE$mu_MLE)/(whole_df_MLE$lac_MLE)
save(whole_df_MLE,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/whole_df_MLE_DI.RData")

#####
# plot MCMC trace
#skip
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/DAISIE_MCMC_short_DI"
files <- list.files(folder_path)
for(i in 1:160){
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("DAISIE_MCMC_short_DI_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)

  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/MCMC_trace_short/set_",i,".tiff"),
         units="px", width=2000, height=4000,res = 400,compression="lzw")
    b_mcmc <- coda::as.mcmc(output[,1:4])
    plot_mcmc <- plot(b_mcmc)
    print(plot_mcmc)
    while (!is.null(dev.list()))  dev.off()
  }
}




# 81 all particles comparsion
## plot all particles (ABC-new vs ABC-old vs MCMC VS MLE)
library(ggplot2)
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/obs_ss_long_with_pars_DI.RData"))


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/delta_whole_df_ABC_ss_set",2,".RData"))
whole_df_ABC$Method = "ABC Diversity"
whole_df_ABC_s2 = whole_df_ABC
whole_df_ABC_s2$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s2$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s2$set <- rep(1:16, each = 5000)
whole_df_ABC_s2$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s2$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/delta_whole_df_ABC_ss_set",3,".RData"))
whole_df_ABC$Method = "ABC NLTT"
whole_df_ABC_s3 = whole_df_ABC
whole_df_ABC_s3$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s3$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s3$set <- rep(1:16, each = 5000)
whole_df_ABC_s3$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s3$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)


whole_df_ABC <- rbind(whole_df_ABC_s2,
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
ABC_median$Method <- rep(c("ABC Diversity","ABC NLTT"),each = 160) #


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/delta_whole_df_MCMC.RData"))
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

iqr = function(z, lower = 0.025, upper = 0.975) {
  data.frame(
    y = mean(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
p_netdiv_all <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dnet_div,y  = Method,color  = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_violin()+ #outlier.shape = NA
  # geom_density_ridges(stat = "binline", bins = 20, scale = 2, draw_baseline = TRUE)+
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1.5,1.5)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("orange","red2","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
                 text = ggplot2::element_text(size = 14,colour = "black"),
                 strip.text = element_text(size = 14,colour = "black")) +
  ggplot2::xlab(expression(Delta~Net~diversification))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/paper/drate_net_div.tiff"),
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
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("orange","red2","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 20,colour = "black"),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 11,colour = "black"),
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
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("orange","red2","#8CC269","#4393C3"))+
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
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("orange","red2","#8CC269","#4393C3"))+
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
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("orange","red2","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 20,colour = "black"),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 11,colour = "black"),
                 strip.text = element_text(size = 15,colour = "black")) +
  ggplot2::xlab(expression(Delta~lambda^a))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_low_prior/DI/paper/drate_combine2_2.tiff"),
     units="px", width=5600, height=4500,res = 310,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_lac+ggplot2::theme(legend.position = "none"),
  p_mu+ggplot2::theme(legend.position = "none"),
  p_gam+ggplot2::theme(legend.position = "none"),
  p_laa+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 2
)
legend <- cowplot::get_legend(
  p_gam + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_est_final <- cowplot::plot_grid(param_estimates,legend,rel_widths = c(3, 0.3))
print(param_est_final)

while (!is.null(dev.list()))  dev.off()