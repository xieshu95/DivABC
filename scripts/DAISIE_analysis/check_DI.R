## analyse DAISIE results (DI version comparison) for 'daisie_ss_check2'
#####
library(ggplot2)
# formate results
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/obs_ss_long_with_pars_DI.RData"))
## ABC results
folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/DAISIE_ABC_short_DI")
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_ABC_short_DI.csv")
param_data2<-param_data[rep(seq_len(nrow(param_data)), each=400),] #500

# 1. formate ABC results
for(n in c(0,1,2)){ # 1,2,6,7,20
  lac_abc <- c()
  mu_abc <- c()
  gam_abc <- c()
  laa_abc <- c()
  n_iter <-c()
  n_iteration <- c()
  for(i in 1:81){
    file_to_load <- grep(paste0("DAISIE_ABC_short_DI_param_set_", i,"_ss_",n,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      n_iteration[i] <- num_iter
      if(output$n_iter <= 2){
        lac_abc <- c(lac_abc, rep(NA,400))
        mu_abc <- c(mu_abc, rep(NA,400))
        gam_abc <- c(gam_abc, rep(NA,400))
        laa_abc <- c(laa_abc, rep(NA,400))
      } else if (nrow(output$ABC[[output$n_iter]]) == 400) {
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
      lac_abc <- c(lac_abc, rep(NA,400))
      mu_abc <- c(mu_abc, rep(NA,400))
      gam_abc <- c(gam_abc, rep(NA,400))
      laa_abc <- c(laa_abc, rep(NA,400))
    }
  }
  whole_df_ABC <- data.frame(param_data2,
                             # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                             lac_abc,mu_abc,gam_abc,laa_abc)
  save(whole_df_ABC,
       file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/whole_df_ABC_ss_set",n,".RData"))

  whole_df_ABC$net_div <- (whole_df_ABC$lac-whole_df_ABC$mu)
  whole_df_ABC$net_div_ABC <- (whole_df_ABC$lac_abc-whole_df_ABC$mu_abc)

  whole_df_ABC$ext_frac <- (whole_df_ABC$mu)/(whole_df_ABC$lac)
  whole_df_ABC$ext_frac_ABC <- (whole_df_ABC$mu_abc)/(whole_df_ABC$lac_abc)
  save(whole_df_ABC,file =
         paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/delta_whole_df_ABC_ss_set",n,".RData"))

}

# 2. formate MCMC results (only plot the etimation points with ABC results)
# skip
# folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/DAISIE_MCMC_short_DI"
# files <- list.files(folder_path)
# param_data <- readr::read_csv2("data/DAISIE_ABC_short_DI.csv")
# param_data <- param_data[1:81,]
# param_data3<-param_data[rep(seq_len(nrow(param_data)), each=400),] #2001/400
#
# lac_mcmc <- c()
# mu_mcmc <- c()
# gam_mcmc <- c()
# laa_mcmc <- c()
#
# for(i in 1:81){
#   file_to_load <- grep(paste0("DAISIE_MCMC_short_DI_param_set_", i,"_ss_1.RData"), #"_rep",rep,
#                        files,
#                        value = TRUE,
#                        fixed = TRUE)
#
#
#
#   if (!identical(file_to_load, character())) {
#     load(file.path(folder_path, file_to_load))
#     lac_mcmc <- c(lac_mcmc, output[1602:2001,1]) # output[602:1001,1]
#     mu_mcmc <- c(mu_mcmc, output[1602:2001,2])
#     gam_mcmc <- c(gam_mcmc, output[1602:2001,3])
#     laa_mcmc <- c(laa_mcmc, output[1602:2001,4])
#   } else {
#     lac_mcmc <- c(lac_mcmc, rep(NA,400)) #rep(NA,400)
#     mu_mcmc <- c(mu_mcmc, rep(NA,400))
#     gam_mcmc <- c(gam_mcmc, rep(NA,400))
#     laa_mcmc <- c(laa_mcmc, rep(NA,400))
#   }
# }
#
# whole_df_MCMC <- data.frame(param_data3,
#                             lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc)
#
# save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/whole_df_MCMC.RData"))
#
# whole_df_MCMC$net_div <- (whole_df_MCMC$lac-whole_df_MCMC$mu)
# whole_df_MCMC$net_div_mcmc <- (whole_df_MCMC$lac_mcmc - whole_df_MCMC$mu_mcmc)
#
# whole_df_MCMC$ext_frac <- (whole_df_MCMC$mu)/(whole_df_MCMC$lac)
# whole_df_MCMC$ext_frac_MCMC <- (whole_df_MCMC$mu_mcmc)/(whole_df_MCMC$lac_mcmc)
#
# save(whole_df_MCMC,
#      file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/delta_whole_df_MCMC.RData"))
#

# 81 all particles comparsion
## plot all particles (ABC-new vs ABC-old vs MCMC VS MLE)
library(ggplot2)
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/obs_ss_long_with_pars_DI.RData"))

# ss = "ABC-old"
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_unif_DI_7ss/delta_whole_df_ABC_ss_set",0,".RData"))
# whole_df_ABC$ss = "ABC-old"
# whole_df_ABC_old = whole_df_ABC[,-6]
# whole_df_ABC_old$total <- rep(rep(pars_ss$total, each = 500), 1)
#
#
# ss = "ABC-new1"
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DI_nltt/delta_whole_df_ABC_ss_set",0,".RData"))
# whole_df_ABC$ss = "ABC-new1"
# whole_df_ABC_new = whole_df_ABC[,-6]
# whole_df_ABC_new$total <- rep(rep(pars_ss$total, each = 300), 1)



load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/delta_whole_df_ABC_ss_set",0,".RData"))
whole_df_ABC$ss = "ABC-all"
whole_df_ABC_s0 = whole_df_ABC
whole_df_ABC_s0$total <- rep(rep(pars_ss$total, each = 400), 1)


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/delta_whole_df_ABC_ss_set",1,".RData"))
whole_df_ABC$ss = "ABC-nltt"
whole_df_ABC_s1 = whole_df_ABC
whole_df_ABC_s1$total <- rep(rep(pars_ss$total, each = 400), 1)


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/delta_whole_df_ABC_ss_set",2,".RData"))
whole_df_ABC$ss = "ABC-tips"
whole_df_ABC_s2 = whole_df_ABC
whole_df_ABC_s2$total <- rep(rep(pars_ss$total, each = 400), 1)

whole_df_ABC <- rbind(whole_df_ABC_s0,
                      whole_df_ABC_s1,
                      whole_df_ABC_s2) #whole_df_ABC_20

whole_df_ABC$dlac <- whole_df_ABC$lac_abc - whole_df_ABC$lac
whole_df_ABC$dmu <- whole_df_ABC$mu_abc - whole_df_ABC$mu
whole_df_ABC$dgam <- whole_df_ABC$gam_abc - whole_df_ABC$gam
whole_df_ABC$dlaa <- whole_df_ABC$laa_abc - whole_df_ABC$laa
whole_df_ABC$dnet_div <- whole_df_ABC$net_div_ABC - whole_df_ABC$net_div
whole_df_ABC$dext_frac <- whole_df_ABC$ext_frac_ABC - whole_df_ABC$ext_frac
# whole_df_ABC$total <- rep(rep(pars_ss$total, each = 400), 1) # 500,5


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/delta_whole_df_MCMC.RData"))
whole_df_MCMC$ss = "MCMC"
whole_df_MCMC$total <- rep(rep(pars_ss$total, each = 400), 1)
whole_df_MCMC$dlac <- whole_df_MCMC$lac_mcmc - whole_df_MCMC$lac
whole_df_MCMC$dmu <- whole_df_MCMC$mu_mcmc - whole_df_MCMC$mu
whole_df_MCMC$dgam <- whole_df_MCMC$gam_mcmc - whole_df_MCMC$gam
whole_df_MCMC$dlaa <- whole_df_MCMC$laa_mcmc - whole_df_MCMC$laa
whole_df_MCMC$dnet_div <- whole_df_MCMC$net_div_mcmc - whole_df_MCMC$net_div
whole_df_MCMC$dext_frac <- whole_df_MCMC$ext_frac_MCMC - whole_df_MCMC$ext_frac

load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/whole_df_MLE_DI.RData")
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

iqr = function(z, lower = 0.01, upper = 0.99) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

lac_names <- c(
  `0.3` = 'lambda[c]~"="~0.3',
  `0.4` = 'lambda[c]~"="~0.4',
  `0.5` = 'lambda[c]~"="~0.5'
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


# ## 1.only compare ABC-NEW AND ABC-OLD plot delta-rate for all the particles
# p_netdiv_lac <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color = ss)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#   ggplot2::scale_colour_manual(values = c("red4","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Tree size") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(lac~ mu,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
#                                          mu = as_labeller(mu_names, label_parsed)))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_ABC_netdiv_lac.tiff"),
#      units="px", width=5000, height=3000,res = 350,compression="lzw")
# print(p_netdiv_lac)
# while (!is.null(dev.list()))  dev.off()
#
# p_netdiv_mu <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color = ss)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#   ggplot2::scale_colour_manual(values = c("red4","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Tree size") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(. ~ mu, scales="free_y",
#              labeller = label_bquote(.(as.expression(
#                eval(parse(text = paste0('mu_names', '$`', mu, '`')))
#              ))))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_ABC_netdiv_mu.tiff"),
#      units="px", width=5000, height=3000,res = 350,compression="lzw")
# print(p_netdiv_mu)
# while (!is.null(dev.list()))  dev.off()
#
# p_netdiv_gam <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color = ss)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#   ggplot2::scale_colour_manual(values = c("red4","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Tree size") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_wrap(~ gam)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_ABC_netdiv_gam.tiff"),
#      units="px", width=5000, height=3000,res = 350,compression="lzw")
# print(p_netdiv_gam)
# while (!is.null(dev.list()))  dev.off()
#
#
# p_netdiv_laa <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color = ss)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#   ggplot2::scale_colour_manual(values = c("red4","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Tree size") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_wrap(~ laa)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_ABC_netdiv_laa.tiff"),
#      units="px", width=5000, height=3000,res = 350,compression="lzw")
# print(p_netdiv_laa)
# while (!is.null(dev.list()))  dev.off()


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
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~Net~diversification))+
  ggplot2::ylab("Methods") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_net_div.tiff"),
     units="px", width=5000, height=3000,res = 350,compression="lzw")
print(p_netdiv_all)
while (!is.null(dev.list()))  dev.off()


p_lac<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlac,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~lambda[c]))+
  ggplot2::ylab("Methods") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_81_lac.tiff"),
     units="px", width=5000, height=3000,res = 350,compression="lzw")
print(p_lac)
while (!is.null(dev.list()))  dev.off()

p_mu<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dmu,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~mu))+
  ggplot2::ylab("Methods") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_81_mu.tiff"),
     units="px", width=5000, height=3000,res = 350,compression="lzw")
print(p_mu)
while (!is.null(dev.list()))  dev.off()

p_gam<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dgam,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.03,0.03)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~gamma))+
  ggplot2::ylab("Methods") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_81_gam.tiff"),
     units="px", width=5000, height=3000,res = 350,compression="lzw")
print(p_gam)
while (!is.null(dev.list()))  dev.off()

p_laa<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlaa,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta~lambda[a]))+
  ggplot2::ylab("Methods") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_81_laa.tiff"),
     units="px", width=5000, height=3000,res = 350,compression="lzw")
print(p_laa)
while (!is.null(dev.list()))  dev.off()





## plot use all the particles rather than median values
# library(ggplot2)
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/obs_ss_long_with_pars.RData"))
#
# ss = 0
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 0
# whole_df_ABC_0 = whole_df_ABC
#
# ss = 1
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 1
# whole_df_ABC_1 = whole_df_ABC
#
# ss = 2
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 2
# whole_df_ABC_2 = whole_df_ABC

# ss = 6
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 6
# whole_df_ABC_6 = whole_df_ABC
#
# ss = 7
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 7
# whole_df_ABC_7 = whole_df_ABC
#
# ss = 20
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 20
# whole_df_ABC_20 = whole_df_ABC

# whole_df_ABC <- rbind(whole_df_ABC_0,whole_df_ABC_1,
#                       whole_df_ABC_2) #whole_df_ABC_20
#
# whole_df_ABC$dlac_abc <- whole_df_ABC$lac_abc - whole_df_ABC$lac
# whole_df_ABC$dmu_abc <- whole_df_ABC$mu_abc - whole_df_ABC$mu
# whole_df_ABC$dgam_abc <- whole_df_ABC$gam_abc - whole_df_ABC$gam
# whole_df_ABC$dlaa_abc <- whole_df_ABC$laa_abc - whole_df_ABC$laa
# whole_df_ABC$dnet_div_abc <- whole_df_ABC$net_div_ABC - whole_df_ABC$net_div
# whole_df_ABC$total <- rep(rep(pars_ss$total, each = 400), 1) # 500,5
#
# color_values <-c("ABC" = "red3","MCMC" = "green2", "MLE" = "yellow2")
iqr = function(z, lower = 0.1, upper = 0.9) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

## 1. total VS drates (combine all 81 sets with all particles)
p_netdiv <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dnet_div,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# facet_grid(~ lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed)))
# facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
#                                                mu = as_labeller(mu_names, label_parsed),
#                                                gam = as_labeller(gam_names, label_parsed),
#                                                laa = as_labeller(laa_names, label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_netdiv.tiff"),
     units="px", width=2500, height=1500,res = 350,compression="lzw")
print(p_netdiv)
while (!is.null(dev.list()))  dev.off()

p_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dlac, color = ss)) + ##,color = as.factor(gam)
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+ #,color = "red3"
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~lambda[c]))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_lac.tiff"),
     units="px", width=2500, height=1500,res = 350,compression="lzw")
print(p_lac)
while (!is.null(dev.list()))  dev.off()

p_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dmu, color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~mu))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_mu.tiff"),
     units="px", width=2500, height=1500,res = 350,compression="lzw")
print(p_mu)
while (!is.null(dev.list()))  dev.off()

p_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dgam, color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.03,0.03)+
  ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~gamma))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_gam.tiff"),
     units="px", width=2500, height=1500,res = 350,compression="lzw")
print(p_gam)
while (!is.null(dev.list()))  dev.off()

p_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dlaa, color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~lambda[a]))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_laa.tiff"),
     units="px", width=2500, height=1500,res = 350,compression="lzw")
print(p_laa)
while (!is.null(dev.list()))  dev.off()


## linear fitting
ggplot(whole_df_ABC, aes(x = total, y = dlac_abc, color = as.factor(ss)) ) +
  ggplot2::ylim(-0.05,0.05)+
  geom_smooth(method = "lm", alpha = .15, aes(fill = as.factor(ss)))

# ## 2.plot delta-rate for all the particles based on ss and generating values
# p_lac <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dlac,color = as.factor(lac))) +
#   ggplot2::stat_summary(fun.data = iqr) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#   ggplot2::scale_colour_manual(values = c("#FADC8D", "#F68221","red4"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Tree size") +
#   ggplot2::ylab(expression(Delta~lambda[c]))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_wrap(~ ss)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_all_particles_fit_lac.tiff"),
#      units="px", width=5000, height=3000,res = 350,compression="lzw")
# print(p_lac)
# while (!is.null(dev.list()))  dev.off()
#
# p_mu <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dmu,color = as.factor(mu))) +
#   ggplot2::stat_summary(fun.data = iqr) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#   ggplot2::scale_colour_manual(values = c("#FADC8D", "#F68221","red4"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Tree size") +
#   ggplot2::ylab(expression(Delta~mu))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_wrap(~ ss)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_all_particles_fit_mu.tiff"),
#      units="px", width=5000, height=3000,res = 350,compression="lzw")
# print(p_mu)
# while (!is.null(dev.list()))  dev.off()
#
# p_gam <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dgam,color = as.factor(gam))) +
#   ggplot2::stat_summary(fun.data = iqr) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.03,0.03)+
#   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#   ggplot2::scale_colour_manual(values = c("#FADC8D", "#F68221","red4"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Tree size") +
#   ggplot2::ylab(expression(Delta~gamma))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_wrap(~ ss)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_all_particles_fit_gam.tiff"),
#      units="px", width=5000, height=3000,res = 350,compression="lzw")
# print(p_gam)
# while (!is.null(dev.list()))  dev.off()
#
# p_laa <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dlaa,color = as.factor(laa))) +
#   ggplot2::stat_summary(fun.data = iqr) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#   ggplot2::scale_colour_manual(values = c("#FADC8D", "#F68221","red4"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Tree size") +
#   ggplot2::ylab(expression(Delta~lambda[a]))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_wrap(~ ss)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_all_particles_fit_laa.tiff"),
#      units="px", width=5000, height=3000,res = 350,compression="lzw")
# print(p_laa)
# while (!is.null(dev.list()))  dev.off()

## plot num_total VS drates_netdiv (all particles) only in ABC, facet with each rate
p_netdiv_lac <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color = ss)) + #whole_df_all
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_ABC_netdiv_lac.tiff"),
     units="px", width=4500, height=1500,res = 350,compression="lzw")
print(p_netdiv_lac)
while (!is.null(dev.list()))  dev.off()

p_netdiv_mu <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ mu,labeller = labeller(mu  = as_labeller(mu_names,  label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_ABC_netdiv_mu.tiff"),
     units="px", width=4500, height=1500,res = 350,compression="lzw")
print(p_netdiv_mu)
while (!is.null(dev.list()))  dev.off()

p_netdiv_gam <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ gam,labeller = labeller(gam  = as_labeller(gam_names,  label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_ABC_netdiv_gam.tiff"),
     units="px", width=4500, height=1500,res = 350,compression="lzw")
print(p_netdiv_gam)
while (!is.null(dev.list()))  dev.off()


p_netdiv_laa <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ laa,labeller = labeller(laa  = as_labeller(laa_names,  label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_ABC_netdiv_laa.tiff"),
     units="px", width=4500, height=1500,res = 350,compression="lzw")
print(p_netdiv_laa)
while (!is.null(dev.list()))  dev.off()


# plot ABC-nltt VS MLE VS MCMC
# p_netdiv_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dnet_div,color = ss)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
#   ggplot2::scale_colour_manual(values = c("orange","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Tree size") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(~ lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed)))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_nltt_netdiv_lac.tiff"),
#      units="px", width=4500, height=1500,res = 350,compression="lzw")
# print(p_netdiv_lac)
# while (!is.null(dev.list()))  dev.off()
#
# p_netdiv_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dnet_div,color = ss)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
#   ggplot2::scale_colour_manual(values = c("orange","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Tree size") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(~ mu,labeller = labeller(mu  = as_labeller(mu_names,  label_parsed)))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_nltt_netdiv_mu.tiff"),
#      units="px", width=4500, height=1500,res = 350,compression="lzw")
# print(p_netdiv_mu)
# while (!is.null(dev.list()))  dev.off()
#
# p_netdiv_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dnet_div,color = ss)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
#   ggplot2::scale_colour_manual(values = c("orange","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Tree size") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(~ gam,labeller = labeller(gam  = as_labeller(gam_names,  label_parsed)))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_nltt_netdiv_gam.tiff"),
#      units="px", width=4500, height=1500,res = 350,compression="lzw")
# print(p_netdiv_gam)
# while (!is.null(dev.list()))  dev.off()
#
#
# p_netdiv_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dnet_div,color = ss)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
#   ggplot2::scale_colour_manual(values = c("orange","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Tree size") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(~ laa,labeller = labeller(laa  = as_labeller(laa_names,  label_parsed)))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_nltt_netdiv_laa.tiff"),
#      units="px", width=4500, height=1500,res = 350,compression="lzw")
# print(p_netdiv_laa)
# while (!is.null(dev.list()))  dev.off()



#####
# combine all generations into one
library(ggplot2)
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/DAISIE_ABC_short_DI"
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_ABC_short_DI.csv")
for(n in c(0,1,2)){
  ABC_df<-c()
  generation <-c()
  set_val <- c()
  for(set in 1:81){
    message("set", set)
    true_rates <- param_data[set,]
    file_to_load <- grep(paste0("DAISIE_ABC_short_DI_param_set_", set,"_ss_",n,".RData"),  #,"_rep",rep
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
  ABC_df_all <- data.frame(param_data[set_val,],ABC_df,generation,set_val)

  ABC_df_all$net_div <- (ABC_df_all$lac-ABC_df_all$mu)
  ABC_df_all$net_div_ABC <- (ABC_df_all$lac_abc-ABC_df_all$mu_abc)
  ABC_df_all$ext_frac <- (ABC_df_all$mu)/(ABC_df_all$lac)
  ABC_df_all$ext_frac_ABC <- (ABC_df_all$mu_abc)/(ABC_df_all$lac_abc)
  save(ABC_df_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/rates_all_generations",n,".RData"))
}

library(ggplot2)
for(ss in c(0,1,2)) {
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/rates_all_generations",ss,".RData"))
  iqr = function(z, lower = 0.1, upper = 0.9) {
    data.frame(
      y = median(z),
      ymin = quantile(z, lower),
      ymax = quantile(z, upper)
    )
  }

  lac_names <- c(
    `0.3` = 'lambda[c]~"="~0.3',
    `0.4` = 'lambda[c]~"="~0.4',
    `0.5` = 'lambda[c]~"="~0.5'
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


  p_netdiv_all <-ggplot2::ggplot(data = ABC_df_all, aes(x = as.factor(generation), y = net_div_ABC)) +

    # ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::geom_boxplot()+
    ggplot2::ylim(-1,1)+
    # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
    # ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab(expression(Generation))+
    ggplot2::ylab("Net diverisifcation") +
    ggplot2::geom_hline(aes(yintercept = net_div), linetype = "dashed", size = 0.5)+
    facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                   mu = as_labeller(mu_names, label_parsed),
                                                   gam = as_labeller(gam_names, label_parsed),
                                                   laa = as_labeller(laa_names, label_parsed)))
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_each_gene_netdiv",ss,".tiff"),
       units="px", width=5000, height=3000,res = 350,compression="lzw")
  print(p_netdiv_all)
  while (!is.null(dev.list()))  dev.off()

  p_lac <-ggplot2::ggplot(data = ABC_df_all, aes(x = as.factor(generation), y = lac_abc)) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::geom_boxplot()+
    ggplot2::ylim(0,1)+
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab(expression(Generation))+
    ggplot2::ylab(expression(lambda[c])) +
    ggplot2::geom_hline(aes(yintercept = lac), linetype = "dashed", size = 0.5)+
    facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                   mu = as_labeller(mu_names, label_parsed),
                                                   gam = as_labeller(gam_names, label_parsed),
                                                   laa = as_labeller(laa_names, label_parsed)))
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_each_gene_lac",ss,".tiff"),
       units="px", width=5000, height=3000,res = 350,compression="lzw")
  print(p_lac)
  while (!is.null(dev.list()))  dev.off()

  p_mu <-ggplot2::ggplot(data = ABC_df_all, aes(x = as.factor(generation), y = mu_abc)) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::geom_boxplot()+
    ggplot2::ylim(0,0.6)+
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab(expression(Generation))+
    ggplot2::ylab(expression(mu)) +
    ggplot2::geom_hline(aes(yintercept = mu), linetype = "dashed", size = 0.5)+
    facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                   mu = as_labeller(mu_names, label_parsed),
                                                   gam = as_labeller(gam_names, label_parsed),
                                                   laa = as_labeller(laa_names, label_parsed)))
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_each_gene_mu",ss,".tiff"),
       units="px", width=5000, height=3000,res = 350,compression="lzw")
  print(p_mu)
  while (!is.null(dev.list()))  dev.off()

  p_gam <-ggplot2::ggplot(data = ABC_df_all, aes(x = as.factor(generation), y = gam_abc)) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::geom_boxplot()+
    ggplot2::ylim(0,0.03)+
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab(expression(Generation))+
    ggplot2::ylab(expression(gamma)) +
    ggplot2::geom_hline(aes(yintercept = gam), linetype = "dashed", size = 0.5)+
    facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                   mu = as_labeller(mu_names, label_parsed),
                                                   gam = as_labeller(gam_names, label_parsed),
                                                   laa = as_labeller(laa_names, label_parsed)))
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_each_gene_gam",ss,".tiff"),
       units="px", width=5000, height=3000,res = 350,compression="lzw")
  print(p_gam)
  while (!is.null(dev.list()))  dev.off()

  p_laa <-ggplot2::ggplot(data = ABC_df_all, aes(x = as.factor(generation), y = laa_abc)) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::geom_boxplot()+
    ggplot2::ylim(0,1)+
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab(expression(Generation))+
    ggplot2::ylab(expression(lambda[a])) +
    ggplot2::geom_hline(aes(yintercept = laa), linetype = "dashed", size = 0.5)+
    facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                   mu = as_labeller(mu_names, label_parsed),
                                                   gam = as_labeller(gam_names, label_parsed),
                                                   laa = as_labeller(laa_names, label_parsed)))
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/drate_each_gene_laa",ss,".tiff"),
       units="px", width=5000, height=3000,res = 350,compression="lzw")
  print(p_laa)
  while (!is.null(dev.list()))  dev.off()

}

#####
# heatmap all paticles (drate vs epsilon)
for(n in c(0)){ # 1,2,6,7,20
  lac_abc <- c()
  mu_abc <- c()
  gam_abc <- c()
  laa_abc <- c()
  n_iter <-c()
  n_iteration <- c()
  for(i in 1:81){
    file_to_load <- grep(paste0("DAISIE_ABC_short_DI_param_set_", i,"_ss_",n,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      n_iteration[i] <- num_iter
      if(output$n_iter <= 2){
        lac_abc <- c(lac_abc, rep(NA,400))
        mu_abc <- c(mu_abc, rep(NA,400))
        gam_abc <- c(gam_abc, rep(NA,400))
        laa_abc <- c(laa_abc, rep(NA,400))
      } else if (nrow(output$ABC[[output$n_iter]]) == 400) {
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
      lac_abc <- c(lac_abc, rep(NA,400))
      mu_abc <- c(mu_abc, rep(NA,400))
      gam_abc <- c(gam_abc, rep(NA,400))
      laa_abc <- c(laa_abc, rep(NA,400))
    }
  }
  whole_df_ABC <- data.frame(param_data2,
                             # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                             lac_abc,mu_abc,gam_abc,laa_abc)

}




## RDATA TO excel

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/obs_ss_long_with_pars_DI.RData"))
write.csv2(pars_ss,paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DI/obs_ss_long_with_pars_DI.csv"))

