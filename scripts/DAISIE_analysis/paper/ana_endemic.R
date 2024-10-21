## analyse DAISIE results (DI version comparison) for 'daisie_endemic'
#####
library(ggplot2)
# formate results
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/obs_ss_long_with_pars_DI.RData"))
## ABC results
folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/DAISIE_ABC_DI")
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_ABC_DI.csv")
param_data2<-param_data[rep(seq_len(nrow(param_data)), each=500),] #500

# 1. formate ABC results
for(n in c(0,1,2,3)){ # 1,2,6,7,20
  lac_abc <- c()
  mu_abc <- c()
  gam_abc <- c()
  laa_abc <- c()
  n_iter <-c()
  n_iteration <- c()
  for(i in 1:160){
    file_to_load <- grep(paste0("DAISIE_ABC_DI_param_set_", i,"_ss_",n,".RData"),  #,"_rep",rep
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
       file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/whole_df_ABC_ss_set",n,".RData"))

  whole_df_ABC$net_div <- (whole_df_ABC$lac-whole_df_ABC$mu)
  whole_df_ABC$net_div_ABC <- (whole_df_ABC$lac_abc-whole_df_ABC$mu_abc)

  whole_df_ABC$ext_frac <- (whole_df_ABC$mu)/(whole_df_ABC$lac)
  whole_df_ABC$ext_frac_ABC <- (whole_df_ABC$mu_abc)/(whole_df_ABC$lac_abc)
  save(whole_df_ABC,file =
         paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/delta_whole_df_ABC_ss_set",n,".RData"))

}

# 2. formate MCMC results (only plot the etimation points with ABC results)
# skip
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/DAISIE_MCMC_short_DI"
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

save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/whole_df_MCMC.RData"))

whole_df_MCMC$net_div <- (whole_df_MCMC$lac-whole_df_MCMC$mu)
whole_df_MCMC$net_div_mcmc <- (whole_df_MCMC$lac_mcmc - whole_df_MCMC$mu_mcmc)

whole_df_MCMC$ext_frac <- (whole_df_MCMC$mu)/(whole_df_MCMC$lac)
whole_df_MCMC$ext_frac_MCMC <- (whole_df_MCMC$mu_mcmc)/(whole_df_MCMC$lac_mcmc)

save(whole_df_MCMC,
     file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/delta_whole_df_MCMC.RData"))
#

# ######
# 3. MLE
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/whole_df_MLE.RData"))

# MLE_DI directly load MLE results from cluster
param_data <- readr::read_csv2("data/DAISIE_ABC_DI.csv")
load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/MLE_DI.RData")
whole_df_MLE <- data.frame(param_data,MLE_all[1:4])

whole_df_MLE$net_div <- (whole_df_MLE$lac-whole_df_MLE$mu)
whole_df_MLE$net_div_MLE <- (whole_df_MLE$lac_MLE-whole_df_MLE$mu_MLE)

whole_df_MLE$ext_frac <- (whole_df_MLE$mu)/(whole_df_MLE$lac)
whole_df_MLE$ext_frac_MLE <- (whole_df_MLE$mu_MLE)/(whole_df_MLE$lac_MLE)
save(whole_df_MLE,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/whole_df_MLE_DI.RData")

#####
# plot MCMC trace
#skip
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/DAISIE_MCMC_short_DI"
files <- list.files(folder_path)
for(i in 1:160){
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("DAISIE_MCMC_short_DI_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)

  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/MCMC_trace_short/set_",i,".tiff"),
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


# ## 1.only compare ABC-NEW AND ABC-OLD plot delta-rate for all the particles
# p_netdiv_lac <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color  = Method)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#   ggplot2::scale_colour_manual("Method",values = c("red4","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Species richness") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(lac~ mu,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
#                                          mu = as_labeller(mu_names, label_parsed)))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_ABC_netdiv_lac.tiff"),
#      units="px", width=4000, height=2500,res = 300,compression="lzw")
# print(p_netdiv_lac)
# while (!is.null(dev.list()))  dev.off()
#
# p_netdiv_mu <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color  = Method)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#   ggplot2::scale_colour_manual("Method",values = c("red4","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Species richness") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(. ~ mu, scales="free_y",
#              labeller = label_bquote(.(as.expression(
#                eval(parse(text = paste0('mu_names', '$`', mu, '`')))
#              ))))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_ABC_netdiv_mu.tiff"),
#      units="px", width=4000, height=2500,res = 300,compression="lzw")
# print(p_netdiv_mu)
# while (!is.null(dev.list()))  dev.off()
#
# p_netdiv_gam <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color  = Method)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#   ggplot2::scale_colour_manual("Method",values = c("red4","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Species richness") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_wrap(~ gam)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_ABC_netdiv_gam.tiff"),
#      units="px", width=4000, height=2500,res = 300,compression="lzw")
# print(p_netdiv_gam)
# while (!is.null(dev.list()))  dev.off()
#
#
# p_netdiv_laa <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color  = Method)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
#   ggplot2::scale_colour_manual("Method",values = c("red4","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Species richness") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_wrap(~ laa)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_ABC_netdiv_laa.tiff"),
#      units="px", width=4000, height=2500,res = 300,compression="lzw")
# print(p_netdiv_laa)
# while (!is.null(dev.list()))  dev.off()


## 81 into 1 net div (combine 10 reps)
iqr = function(z, lower = 0.05, upper = 0.95) {
  data.frame(
    y = mean(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}
library(ggridges)
library(ggplot2)
library(viridis)
# install.packages("hrbrthemes")
library(hrbrthemes)
p_netdiv_all <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dnet_div,y  = Method,color  = Method)) +
  # ggplot2:: geom_jitter(position = position_jitter(height = 0.15, width = 0), alpha = .01,size  = 0.1) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_violin()+ #outlier.shape = NA
  # geom_density_ridges(stat = "binline", bins = 20, scale = 2, draw_baseline = TRUE)+
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1.5,1.5)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
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
  ggplot2::xlim(-1,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
                 text = ggplot2::element_text(size = 14,colour = "black"),
                 strip.text = element_text(size = 14,colour = "black")) +
  ggplot2::xlab(expression(Delta~lambda^c))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_lac.tiff"),
     units="px", width=4000, height=2500,res = 300,compression="lzw")
print(p_lac)
while (!is.null(dev.list()))  dev.off()

p_mu<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dmu,y  = Method,color  = Method)) +
  # ggplot2:: geom_jitter(position = position_jitter(height = 0.15, width = 0), alpha = .012) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.5,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
                 text = ggplot2::element_text(size = 14,colour = "black"),
                 strip.text = element_text(size = 14,colour = "black")) +
  ggplot2::xlab(expression(Delta~mu))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_mu.tiff"),
     units="px", width=4000, height=2500,res = 300,compression="lzw")
print(p_mu)
while (!is.null(dev.list()))  dev.off()

p_gam<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dgam,y  = Method,color  = Method)) +
  # ggplot2:: geom_jitter(position = position_jitter(height = 0.15, width = 0), alpha = .012) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.01,0.018)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
                 text = ggplot2::element_text(size = 14,colour = "black"),
                 strip.text = element_text(size = 14,colour = "black")) +
  ggplot2::xlab(expression(Delta~gamma))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_gam.tiff"),
     units="px", width=4000, height=2500,res = 300,compression="lzw")
print(p_gam)
while (!is.null(dev.list()))  dev.off()

p_laa<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlaa,y  = Method,color  = Method)) +
  # ggplot2:: geom_jitter(position = position_jitter(height = 0.15, width = 0), alpha = .012) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.5,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 14,colour = "black"),
                 text = ggplot2::element_text(size = 14,colour = "black"),
                 strip.text = element_text(size = 14,colour = "black")) +
  ggplot2::xlab(expression(Delta~lambda^a))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_laa.tiff"),
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

p_netdiv_all <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dnet_div,y  = Method,color  = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1.51,1.51)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 16,colour = "black"),
                 text = ggplot2::element_text(size = 14,colour = "black"),
                 strip.text = element_text(size = 14,colour = "black")) +
  ggplot2::xlab(expression(Delta~Net~diversification))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(rep~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_net_div_each_rep2.tiff"),
     units="px", width=6200, height=3500,res = 320,compression="lzw")
print(p_netdiv_all)
while (!is.null(dev.list()))  dev.off()


p_lac<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlac,y  = Method,color  = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1,1.7)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 16,colour = "black"),
                 text = ggplot2::element_text(size = 14,colour = "black"),
                 strip.text = element_text(size = 14,colour = "black")) +
  ggplot2::xlab(expression(Delta~lambda^c))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(rep~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_lac_each_rep.tiff"),
     units="px", width=6200, height=3500,res = 320,compression="lzw")
print(p_lac)
while (!is.null(dev.list()))  dev.off()

p_mu<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dmu,y  = Method,color  = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.6,1.8)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 16,colour = "black"),
                 text = ggplot2::element_text(size = 14,colour = "black"),
                 strip.text = element_text(size = 14,colour = "black")) +
  ggplot2::xlab(expression(Delta~mu))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(rep~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_mu_each_rep.tiff"),
     units="px", width=6200, height=3500,res = 320,compression="lzw")
print(p_mu)
while (!is.null(dev.list()))  dev.off()

p_gam<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dgam,y  = Method,color  = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.008,0.019)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 18,colour = "black"),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank(),
                 strip.text = element_text(size = 14,colour = "black")) +
  ggplot2::xlab(expression(Delta~gamma))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(rep~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_gam_each_rep.tiff"),
     units="px", width=6200, height=3500,res = 320,compression="lzw")
print(p_gam)
while (!is.null(dev.list()))  dev.off()

p_laa<-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = dlaa,y  = Method,color  = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005,fill = "white") +
  # ggplot2::geom_boxplot(outlier.shape=NA)+
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.6,1.9)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 18,colour = "black"),
                 axis.text.x = ggplot2::element_text(size = 14,colour = "black"),
                 axis.text.y = ggplot2::element_blank(),
                 strip.text = element_text(size = 14,colour = "black")) +
  ggplot2::xlab(expression(Delta~lambda^a))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(rep~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_laa_each_rep.tiff"),
     units="px", width=6200, height=3500,res = 320,compression="lzw")
print(p_laa)
while (!is.null(dev.list()))  dev.off()

## plot use all the particles rather than median values
# library(ggplot2)
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/obs_ss_long_with_pars.RData"))
#
# ss = 0
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 0
# whole_df_ABC_0 = whole_df_ABC
#
# ss = 1
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 1
# whole_df_ABC_1 = whole_df_ABC
#
# ss = 2
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 2
# whole_df_ABC_2 = whole_df_ABC

# ss = 6
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 6
# whole_df_ABC_6 = whole_df_ABC
#
# ss = 7
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 7
# whole_df_ABC_7 = whole_df_ABC
#
# ss = 20
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/delta_whole_df_ABC_ss_set",ss,".RData"))
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
# color_values <-c("ABC" = "red2","MCMC" = "green2", "MLE" = "yellow2")
iqr = function(z, lower = 0.1, upper = 0.9) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

## 1. total VS drates (combine all 81 sets with all particles)
p_netdiv <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dnet_div,color  = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.7) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-2,1.5)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Species richness") +
  # ggplot2::xlab(expression(log[10]~(Tree~size))) +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# facet_grid(~ lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed)))
# facet_grid(lac+mu~ gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
#                                                mu = as_labeller(mu_names, label_parsed),
#                                                gam = as_labeller(gam_names, label_parsed),
#                                                laa = as_labeller(laa_names, label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_netdiv_median.tiff"),
     units="px", width=4000, height=2000,res = 350,compression="lzw")
print(p_netdiv)
while (!is.null(dev.list()))  dev.off()

p_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dlac, color  = Method)) + ##,color = as.factor(gam)
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.8,2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+ #,color = "red2"
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~lambda^c))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_lac.tiff"),
#      units="px", width=2500, height=1500,res = 350,compression="lzw")
# print(p_lac)
# while (!is.null(dev.list()))  dev.off()

p_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dmu, color  = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~mu))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_mu.tiff"),
#      units="px", width=2500, height=1500,res = 350,compression="lzw")
# print(p_mu)
# while (!is.null(dev.list()))  dev.off()

p_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dgam, color  = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.01,0.02)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~gamma))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_gam.tiff"),
#      units="px", width=2500, height=1500,res = 350,compression="lzw")
# print(p_gam)
# while (!is.null(dev.list()))  dev.off()

p_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dlaa, color  = Method)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-2,2)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~lambda^a))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_all_rates_median.tiff"),
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

##!!!link to drate_num_clade file!!!!



# # tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_laa.tiff"),
# #      units="px", width=2500, height=1500,res = 350,compression="lzw")
# # print(p_laa)
# # while (!is.null(dev.list()))  dev.off()
#
# ## log(Species richness) vs drates, facet
# p_netdiv <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = log10(total),y = dnet_div,color  = Method)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.3) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.8,0.8)+
#   ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
#   ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab(expression(log[10]~(Tree~size))) +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(mu+gam+laa~lac ,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
#                                                  mu = as_labeller(mu_names, label_parsed),
#                                                  gam = as_labeller(gam_names, label_parsed),
#                                                  laa = as_labeller(laa_names, label_parsed)))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_net_div_facet.tiff"),
#      units="px", width=3500, height=2500,res = 300,compression="lzw")
# print(p_netdiv)
# while (!is.null(dev.list()))  dev.off()
#
#
# p_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = log10(total),y = dlac, color  = Method)) + ##,color = as.factor(gam)
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-1.2,1.2)+
#   ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+ #,color = "red2"
#   ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab(expression(log[10]~(Tree~size))) +
#   ggplot2::ylab(expression(Delta~lambda^c))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
#   facet_grid(mu+gam+laa~lac ,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
#                                                  mu = as_labeller(mu_names, label_parsed),
#                                                  gam = as_labeller(gam_names, label_parsed),
#                                                  laa = as_labeller(laa_names, label_parsed)))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_lac_facet.tiff"),
#      units="px", width=3500, height=2500,res = 300,compression="lzw")
# print(p_lac)
# while (!is.null(dev.list()))  dev.off()
#
# p_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = log10(total),y = dmu, color  = Method)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
#   ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab(expression(log[10]~(Tree~size))) +
#   ggplot2::ylab(expression(Delta~mu))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(mu+gam+laa~lac ,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
#                                                  mu = as_labeller(mu_names, label_parsed),
#                                                  gam = as_labeller(gam_names, label_parsed),
#                                                  laa = as_labeller(laa_names, label_parsed)))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_mu_facet.tiff"),
#      units="px", width=3500, height=2500,res = 300,compression="lzw")
# print(p_mu)
# while (!is.null(dev.list()))  dev.off()
#
# p_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = log10(total),y = dgam, color  = Method)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.02,0.02)+
#   ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
#   ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab(expression(log[10]~(Tree~size))) +
#   ggplot2::ylab(expression(Delta~gamma))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(mu+gam+laa~lac ,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
#                                                  mu = as_labeller(mu_names, label_parsed),
#                                                  gam = as_labeller(gam_names, label_parsed),
#                                                  laa = as_labeller(laa_names, label_parsed)))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_gam_facet.tiff"),
#      units="px", width=3500, height=2500,res = 300,compression="lzw")
# print(p_gam)
# while (!is.null(dev.list()))  dev.off()
#
# p_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = log10(total),y = dlaa, color  = Method)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-1.5,1.5)+
#   ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
#   ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab(expression(log[10]~(Tree~size))) +
#   ggplot2::ylab(expression(Delta~lambda^a))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(mu+gam+laa~lac ,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
#                                                  mu = as_labeller(mu_names, label_parsed),
#                                                  gam = as_labeller(gam_names, label_parsed),
#                                                  laa = as_labeller(laa_names, label_parsed)))
#
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_laa_facet.tiff"),
#      units="px", width=3500, height=2500,res = 300,compression="lzw")
# print(p_laa)
# while (!is.null(dev.list()))  dev.off()
#
#
#
# ## linear fitting
# ggplot(whole_df_ABC, aes(x = total, y = dlac_abc, color = as.factor(ss)) ) +
#   ggplot2::ylim(-0.05,0.05)+
#   geom_smooth(method = "lm", alpha = .15, aes(fill = as.factor(ss)))
#
# # ## 2.plot delta-rate for all the particles based on ss and generating values
# # p_lac <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dlac,color = as.factor(lac))) +
# #   ggplot2::stat_summary(fun.data = iqr) +
# #   ggplot2::theme_bw() +
# #   ggplot2::theme_classic() +
# #   ggplot2::ylim(-0.6,0.6)+
# #   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
# #   ggplot2::scale_colour_manual("Method",values = c("#FADC8D", "#F68221","red4"))+
# #   ggplot2::theme(title = ggplot2::element_text(size = 12),
# #                  text = ggplot2::element_text(size = 12)) +
# #   ggplot2::xlab("Species richness") +
# #   ggplot2::ylab(expression(Delta~lambda^c))+
# #   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
# #   facet_wrap(~ ss)
# # tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_all_particles_fit_lac.tiff"),
# #      units="px", width=4000, height=2500,res = 300,compression="lzw")
# # print(p_lac)
# # while (!is.null(dev.list()))  dev.off()
# #
# # p_mu <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dmu,color = as.factor(mu))) +
# #   ggplot2::stat_summary(fun.data = iqr) +
# #   ggplot2::theme_bw() +
# #   ggplot2::theme_classic() +
# #   ggplot2::ylim(-0.6,0.6)+
# #   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
# #   ggplot2::scale_colour_manual("Method",values = c("#FADC8D", "#F68221","red4"))+
# #   ggplot2::theme(title = ggplot2::element_text(size = 12),
# #                  text = ggplot2::element_text(size = 12)) +
# #   ggplot2::xlab("Species richness") +
# #   ggplot2::ylab(expression(Delta~mu))+
# #   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
# #   facet_wrap(~ ss)
# # tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_all_particles_fit_mu.tiff"),
# #      units="px", width=4000, height=2500,res = 300,compression="lzw")
# # print(p_mu)
# # while (!is.null(dev.list()))  dev.off()
# #
# # p_gam <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dgam,color = as.factor(gam))) +
# #   ggplot2::stat_summary(fun.data = iqr) +
# #   ggplot2::theme_bw() +
# #   ggplot2::theme_classic() +
# #   ggplot2::ylim(-0.03,0.03)+
# #   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
# #   ggplot2::scale_colour_manual("Method",values = c("#FADC8D", "#F68221","red4"))+
# #   ggplot2::theme(title = ggplot2::element_text(size = 12),
# #                  text = ggplot2::element_text(size = 12)) +
# #   ggplot2::xlab("Species richness") +
# #   ggplot2::ylab(expression(Delta~gamma))+
# #   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
# #   facet_wrap(~ ss)
# # tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_all_particles_fit_gam.tiff"),
# #      units="px", width=4000, height=2500,res = 300,compression="lzw")
# # print(p_gam)
# # while (!is.null(dev.list()))  dev.off()
# #
# # p_laa <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dlaa,color = as.factor(laa))) +
# #   ggplot2::stat_summary(fun.data = iqr) +
# #   ggplot2::theme_bw() +
# #   ggplot2::theme_classic() +
# #   ggplot2::ylim(-0.6,0.6)+
# #   ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
# #   ggplot2::scale_colour_manual("Method",values = c("#FADC8D", "#F68221","red4"))+
# #   ggplot2::theme(title = ggplot2::element_text(size = 12),
# #                  text = ggplot2::element_text(size = 12)) +
# #   ggplot2::xlab("Species richness") +
# #   ggplot2::ylab(expression(Delta~lambda^a))+
# #   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
# #   facet_wrap(~ ss)
# # tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_all_particles_fit_laa.tiff"),
# #      units="px", width=4000, height=2500,res = 300,compression="lzw")
# # print(p_laa)
# # while (!is.null(dev.list()))  dev.off()
#
# ## plot num_total VS drates_netdiv (all particles) only in ABC, facet with each rate
# p_netdiv_lac <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color  = Method)) + #whole_df_all
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.8,0.8)+
#   ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Species richness") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(~ lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed)))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_ABC_netdiv_lac.tiff"),
#      units="px", width=4500, height=1500,res = 350,compression="lzw")
# print(p_netdiv_lac)
# while (!is.null(dev.list()))  dev.off()
#
# p_netdiv_mu <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color  = Method)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Species richness") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(~ mu,labeller = labeller(mu  = as_labeller(mu_names,  label_parsed)))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_ABC_netdiv_mu.tiff"),
#      units="px", width=4500, height=1500,res = 350,compression="lzw")
# print(p_netdiv_mu)
# while (!is.null(dev.list()))  dev.off()
#
# p_netdiv_gam <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color  = Method)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Species richness") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(~ gam,labeller = labeller(gam  = as_labeller(gam_names,  label_parsed)))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_ABC_netdiv_gam.tiff"),
#      units="px", width=4500, height=1500,res = 350,compression="lzw")
# print(p_netdiv_gam)
# while (!is.null(dev.list()))  dev.off()
#
#
# p_netdiv_laa <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div,color  = Method)) +
#   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
#   ggplot2::theme_bw() +
#   ggplot2::theme_classic() +
#   ggplot2::ylim(-0.6,0.6)+
#   ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab("Species richness") +
#   ggplot2::ylab(expression(Delta~Net~diversification))+
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
#   facet_grid(~ laa,labeller = labeller(laa  = as_labeller(laa_names,  label_parsed)))
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/drate_ABC_netdiv_laa.tiff"),
#      units="px", width=4500, height=1500,res = 350,compression="lzw")
# print(p_netdiv_laa)
# while (!is.null(dev.list()))  dev.off()
#
#
# # plot ABC-nltt VS MLE VS MCMC
# # p_netdiv_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dnet_div,color  = Method)) +
# #   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
# #   ggplot2::theme_bw() +
# #   ggplot2::theme_classic() +
# #   ggplot2::ylim(-0.6,0.6)+
# #   ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
# #   ggplot2::scale_colour_manual("Method",values = c("orange","#8CC269","#4393C3"))+
# #   ggplot2::theme(title = ggplot2::element_text(size = 12),
# #                  text = ggplot2::element_text(size = 12)) +
# #   ggplot2::xlab("Species richness") +
# #   ggplot2::ylab(expression(Delta~Net~diversification))+
# #   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
# #   facet_grid(~ lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed)))
# # tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_nltt_netdiv_lac.tiff"),
# #      units="px", width=4500, height=1500,res = 350,compression="lzw")
# # print(p_netdiv_lac)
# # while (!is.null(dev.list()))  dev.off()
# #
# # p_netdiv_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dnet_div,color  = Method)) +
# #   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
# #   ggplot2::theme_bw() +
# #   ggplot2::theme_classic() +
# #   ggplot2::ylim(-0.6,0.6)+
# #   ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
# #   ggplot2::scale_colour_manual("Method",values = c("orange","#8CC269","#4393C3"))+
# #   ggplot2::theme(title = ggplot2::element_text(size = 12),
# #                  text = ggplot2::element_text(size = 12)) +
# #   ggplot2::xlab("Species richness") +
# #   ggplot2::ylab(expression(Delta~Net~diversification))+
# #   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
# #   facet_grid(~ mu,labeller = labeller(mu  = as_labeller(mu_names,  label_parsed)))
# # tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_nltt_netdiv_mu.tiff"),
# #      units="px", width=4500, height=1500,res = 350,compression="lzw")
# # print(p_netdiv_mu)
# # while (!is.null(dev.list()))  dev.off()
# #
# # p_netdiv_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dnet_div,color  = Method)) +
# #   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
# #   ggplot2::theme_bw() +
# #   ggplot2::theme_classic() +
# #   ggplot2::ylim(-0.6,0.6)+
# #   ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
# #   ggplot2::scale_colour_manual("Method",values = c("orange","#8CC269","#4393C3"))+
# #   ggplot2::theme(title = ggplot2::element_text(size = 12),
# #                  text = ggplot2::element_text(size = 12)) +
# #   ggplot2::xlab("Species richness") +
# #   ggplot2::ylab(expression(Delta~Net~diversification))+
# #   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
# #   facet_grid(~ gam,labeller = labeller(gam  = as_labeller(gam_names,  label_parsed)))
# # tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_nltt_netdiv_gam.tiff"),
# #      units="px", width=4500, height=1500,res = 350,compression="lzw")
# # print(p_netdiv_gam)
# # while (!is.null(dev.list()))  dev.off()
# #
# #
# # p_netdiv_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dnet_div,color  = Method)) +
# #   ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
# #   ggplot2::theme_bw() +
# #   ggplot2::theme_classic() +
# #   ggplot2::ylim(-0.6,0.6)+
# #   ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
# #   ggplot2::scale_colour_manual("Method",values = c("orange","#8CC269","#4393C3"))+
# #   ggplot2::theme(title = ggplot2::element_text(size = 12),
# #                  text = ggplot2::element_text(size = 12)) +
# #   ggplot2::xlab("Species richness") +
# #   ggplot2::ylab(expression(Delta~Net~diversification))+
# #   ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
# #   facet_grid(~ laa,labeller = labeller(laa  = as_labeller(laa_names,  label_parsed)))
# # tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/drate_nltt_netdiv_laa.tiff"),
# #      units="px", width=4500, height=1500,res = 350,compression="lzw")
# # print(p_netdiv_laa)
# # while (!is.null(dev.list()))  dev.off()
#


#####
# combine all generations into one
library(ggplot2)
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/DAISIE_ABC_DI"
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_ABC_DI.csv")
for(n in c(0,1,2,3)){
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
      if(nrow(output$ABC[[n_gene]]) < 500){ #500
        n_gene <- n_gene - 1
      }
      for(i in 1:n_gene){
        ABC_df <- rbind(ABC_df,output$ABC[[i]])
      }

      # colnames(ss_dist) <- c("MPD","MNTD","SDPD","SDNTD",
      #                        "D","Total","Ratio","NLTT")
      ABC_df <- as.data.frame(ABC_df)
      generation <- c(generation, rep(1:n_gene, each = 500))
      set_val <- c(set_val, rep(set,n_gene*500))

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
  save(ABC_df_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/rates_all_generations",n,".RData"))
}

library(ggplot2)
for(ss in c(0,1,2)) {
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/rates_all_generations",ss,".RData"))
  iqr = function(z, lower = 0.05, upper = 0.95) {
    data.frame(
      y = median(z),
      ymin = quantile(z, lower),
      ymax = quantile(z, upper)
    )
  }

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


  p_netdiv_all <-ggplot2::ggplot(data = ABC_df_all, aes(x = as.factor(generation), y = net_div_ABC)) +

    # ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::geom_boxplot(outlier.shape=NA)+ #
    ggplot2::ylim(-1,1)+
    # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
    # ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab(expression(Generation))+
    ggplot2::ylab("Net diverisifcation") +
    ggplot2::geom_hline(aes(yintercept = net_div), linetype = "dashed", size = 0.5)+
    facet_grid(rep~laa+gam+mu+lac ,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                       mu = as_labeller(mu_names, label_parsed),
                                                       gam = as_labeller(gam_names, label_parsed),
                                                       laa = as_labeller(laa_names, label_parsed)))
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/drate_each_gene_netdiv",ss,".tiff"),
       units="px", width=6000, height=3000,res = 350,compression="lzw")
  print(p_netdiv_all)
  while (!is.null(dev.list()))  dev.off()

  p_lac <-ggplot2::ggplot(data = ABC_df_all, aes(x = as.factor(generation), y = lac_abc)) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::geom_boxplot(outlier.shape=NA)+
    ggplot2::ylim(0,1)+
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab(expression(Generation))+
    ggplot2::ylab(expression(lambda^c)) +
    ggplot2::geom_hline(aes(yintercept = lac), linetype = "dashed", size = 0.5)+
    facet_grid(rep~laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                      mu = as_labeller(mu_names, label_parsed),
                                                      gam = as_labeller(gam_names, label_parsed),
                                                      laa = as_labeller(laa_names, label_parsed)))
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/drate_each_gene_lac",ss,".tiff"),
       units="px", width=6000, height=3000,res = 350,compression="lzw")
  print(p_lac)
  while (!is.null(dev.list()))  dev.off()

  p_mu <-ggplot2::ggplot(data = ABC_df_all, aes(x = as.factor(generation), y = mu_abc)) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::geom_boxplot(outlier.shape=NA)+
    ggplot2::ylim(0,0.6)+
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab(expression(Generation))+
    ggplot2::ylab(expression(mu)) +
    ggplot2::geom_hline(aes(yintercept = mu), linetype = "dashed", size = 0.5)+
    facet_grid(rep~laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                      mu = as_labeller(mu_names, label_parsed),
                                                      gam = as_labeller(gam_names, label_parsed),
                                                      laa = as_labeller(laa_names, label_parsed)))
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/drate_each_gene_mu",ss,".tiff"),
       units="px", width=6000, height=3000,res = 350,compression="lzw")
  print(p_mu)
  while (!is.null(dev.list()))  dev.off()

  p_gam <-ggplot2::ggplot(data = ABC_df_all, aes(x = as.factor(generation), y = gam_abc)) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::geom_boxplot(outlier.shape=NA)+
    ggplot2::ylim(0,0.03)+
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab(expression(Generation))+
    ggplot2::ylab(expression(gamma)) +
    ggplot2::geom_hline(aes(yintercept = gam), linetype = "dashed", size = 0.5)+
    facet_grid(rep~laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                      mu = as_labeller(mu_names, label_parsed),
                                                      gam = as_labeller(gam_names, label_parsed),
                                                      laa = as_labeller(laa_names, label_parsed)))
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/drate_each_gene_gam",ss,".tiff"),
       units="px", width=6000, height=3000,res = 350,compression="lzw")
  print(p_gam)
  while (!is.null(dev.list()))  dev.off()

  p_laa <-ggplot2::ggplot(data = ABC_df_all, aes(x = as.factor(generation), y = laa_abc)) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::geom_boxplot(outlier.shape=NA)+
    ggplot2::ylim(0,1.7)+
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab(expression(Generation))+
    ggplot2::ylab(expression(lambda^a)) +
    ggplot2::geom_hline(aes(yintercept = laa), linetype = "dashed", size = 0.5)+
    facet_grid(rep~laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                      mu = as_labeller(mu_names, label_parsed),
                                                      gam = as_labeller(gam_names, label_parsed),
                                                      laa = as_labeller(laa_names, label_parsed)))
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/drate_each_gene_laa",ss,".tiff"),
       units="px", width=6000, height=3000,res = 350,compression="lzw")
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
    file_to_load <- grep(paste0("DAISIE_ABC_DI_param_set_", i,"_ss_",n,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      n_iteration[i] <- num_iter
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

}


#####
#  density plots - ABC comparison(all VS tips VS nltt) for each parameter set
library(ggplot2)
ss = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/delta_whole_df_ABC_ss_set",ss,".RData"))
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/delta_whole_df_MCMC.RData"))
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/whole_df_MLE_DI.RData"))
## get legend first
param_abc <- whole_df_ABC[1:10,]
param_mcmc <- whole_df_MCMC[1:10,]
param_mle <- whole_df_MLE[1:10,]

# ,"#FADC8D","orange",,"#4393C3"
color_values <-c("ABC" = "orange", "MCMC" = "#8CC269", "MLE" = "#4393C3")
p_legend <-ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  xlim(0,1)+
  ggplot2::geom_density(data = param_mcmc,
                        ggplot2::aes(x = lac_mcmc,fill = "MCMC"),colour = "red4",
                        alpha = 0.5) +
  ggplot2::geom_density(ggplot2::aes(x = lac_abc,fill = "ABC"),colour = "blue3",
                        alpha = 0.7) +
  ggplot2::geom_density(data = param_mle,
                        ggplot2::aes(x = lac_MLE,fill = "MLE"),colour = "green4",
                        alpha = 0.5) +
  ggplot2::theme_classic() +
  ggplot2::ylab("Density") +
  ggplot2::scale_fill_manual(name = "Method",
                             values = c("ABC" = "orange", "MCMC" = "#8CC269", "MLE" = "#4393C3"),
                             labels = c("ABC","MCMC","MLE"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 15)) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 15)) +
  ggplot2::xlab(expression(lambda^c))+
  ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)


legend_all <- cowplot::get_legend(
  p_legend + theme(legend.box.margin = margin(0, 0, 0, 6))
)



for(i in 1:160){
  param_abc <- whole_df_ABC[((i*500-499)):(i*500),]
  param_mcmc <- whole_df_MCMC[((i*2000-499)):(i*2000),]
  param_mle <- whole_df_MLE[i,]

  # if(!is.na(param_abc[,7])){
  p_lac <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.03,1.2)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = lac_mcmc,fill = "MCMC"),
                            alpha = 1,bins = 50) +
    ggplot2::geom_histogram(ggplot2::aes(x = lac_abc,
                                         fill = "ABC"),
                            alpha = 0.8,bins = 50) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = lac_MLE),color = "#4393C3",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab(expression(lambda^c))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("ABC","MCMC","MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)
  # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
  #                     linetype = "dashed", size = 0.5,color = "red")



  p_mu <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.02,0.7)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = mu_mcmc,fill = "MCMC"),
                            alpha = 1,bins = 50) +
    ggplot2::geom_histogram(ggplot2::aes(x = mu_abc,
                                         fill = "ABC"),
                            alpha = 0.8,bins = 50) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = mu_MLE),color = "#4393C3",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab(expression(mu))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("ABC","MCMC","MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", size = 0.5)

  p_gam <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.002,0.03)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = gam_mcmc,fill = "MCMC"),
                            alpha = 1,bins = 50) +
    ggplot2::geom_histogram(ggplot2::aes(x = gam_abc,
                                         fill = "ABC"),
                            alpha = 0.8,bins = 50) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = gam_MLE),color = "#4393C3",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab(expression(gamma))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("ABC","MCMC","MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed", size = 0.5)

  p_laa <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.03,1.8)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = laa_mcmc,fill = "MCMC"),
                            alpha = 1,bins = 50) +
    ggplot2::geom_histogram(ggplot2::aes(x = laa_abc,
                                         fill = "ABC"),
                            alpha = 0.8,bins = 50) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = laa_MLE),color = "#4393C3",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab(expression(lambda^a))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("ABC","MCMC","MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.5)

  p_net_div <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.03,1.2)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = net_div_mcmc,fill = "MCMC"),
                            alpha = 1,bins = 50) +
    ggplot2::geom_histogram(ggplot2::aes(x = net_div_ABC,fill = "ABC"),
                            alpha = 0.8,bins = 50) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = net_div_MLE),color = "#4393C3",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab("Net diversification")+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("ABC","MCMC","MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = net_div), linetype = "dashed", size = 0.5)

  # p_ext_frac <-ggplot2::ggplot(data = param_abc) +
  #   ggplot2::theme_bw() +
  #   # xlim(0,1.0)+
  #   ggplot2::geom_histogram(data = param_mcmc,
  #                           ggplot2::aes(x = ext_frac_MCMC,fill = "MCMC"),
  #                           alpha = 0.7) +
  #   ggplot2::geom_histogram(ggplot2::aes(x = ext_frac_ABC,fill = "ABC"),
  #                           alpha = 0.7) +
  #   ggplot2::geom_vline(data= param_mle,
  #                       aes(xintercept = ext_frac_MLE),color = "#59A95A",
  #                       linetype = "solid", size = 1)+
  #   ggplot2::theme_classic() +
  #   ggplot2::theme(title = ggplot2::element_text(size = 13),
  #                  text = ggplot2::element_text(size = 13)) +
  #   ggplot2::ylab("Frequency") +
  #   ggplot2::xlab("Extinction fraction")+
  #   ggplot2::scale_fill_manual(name = "Method",
  #                              values = color_values,
  #                              labels = c("MCMC", "ABC", "MLE"))+
  #   ggplot2::theme(legend.position = "none") +
  #   ggplot2::geom_vline(data= param_abc, aes(xintercept = ext_frac), linetype = "dashed", size = 0.5)



  p_emp <- ggplot() + theme_void()

  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/cowplot_AMM/ss_",ss,"_AMM_hist_set_",i,".tiff"),
       units="px", width=4000, height=2000,res = 400,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_lac,p_mu,p_net_div,p_gam,p_laa,p_emp,
    align = "hv", nrow = 2, ncol = 3
  )
  param_est_final <- cowplot::plot_grid(param_estimates,legend_all,rel_widths = c(3, .4))
  print(param_est_final)
  while (!is.null(dev.list()))  dev.off()
  # }
}






## RDATA TO excel

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/obs_ss_long_with_pars_DI.RData"))
write.csv2(pars_ss,paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/obs_ss_long_with_pars_DI.csv"))


###
load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/obs_ss_long_with_pars_DI.RData")
df <- pars_ss
n <- 10
ss_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
ss_mean <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), mean)[-1]
ss_max <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), max)[-1]
ss_min <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), min)[-1]
ss_sd <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), sd)[-1]



write.csv2(round(ss_median,0),paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/ss_median.csv"))
write.csv2(round(ss_mean,0),paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/ss_mean.csv"))
write.csv2(round(ss_max,0),paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/ss_max.csv"))
write.csv2(round(ss_min,0),paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/ss_min.csv"))
write.csv2(round(ss_sd,0),paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/ss_sd.csv"))

###
library(coda)
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/new MCMC/DAISIE_MCMC_short_DI"
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_MCMC_short_DI.csv")
param_data <- param_data[1:160,]


lac_cor <- c()
mu_cor <- c()
gam_cor <- c()
laa_cor <- c()

seq <- seq(1,5001,2)
for(i in 1:160){
  file_to_load <- grep(paste0("DAISIE_MCMC_short_DI_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)



  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lac_cor <- c(lac_cor, autocorr(coda::as.mcmc(output[seq,1]), lags = c(2), relative=TRUE)) # output[4002:2501,1]
    mu_cor <- c(mu_cor, autocorr(coda::as.mcmc(output[seq,2]), lags = c(2), relative=TRUE))
    gam_cor <- c(gam_cor, autocorr(coda::as.mcmc(output[seq,3]), lags = c(2), relative=TRUE))
    laa_cor <- c(laa_cor, autocorr(coda::as.mcmc(output[seq,4]), lags = c(2), relative=TRUE))
  } else {
    lac_cor <- c(lac_cor, rep(NA,1)) #rep(NA,400)
    mu_cor <- c(mu_cor, rep(NA,1))
    gam_cor <- c(gam_cor, rep(NA,1))
    laa_cor <- c(laa_cor, rep(NA,1))
  }
}

whole_df_cor <- data.frame(param_data,
                           lac_cor,mu_cor,gam_cor,laa_cor)

save(whole_df_cor,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/whole_df_cor_lag1.RData"))

plot(density(whole_df_cor[,6],na.rm = T))
median(whole_df_cor[,6],na.rm = T)
median(whole_df_cor[,7],na.rm = T)
median(whole_df_cor[,8],na.rm = T)
median(whole_df_cor[,9],na.rm = T)

sd(whole_df_cor[,6],na.rm = T)
sd(whole_df_cor[,7],na.rm = T)
sd(whole_df_cor[,8],na.rm = T)
sd(whole_df_cor[,9],na.rm = T)

#
# autocorr(output[,1], lags = c(1), relative=TRUE)
