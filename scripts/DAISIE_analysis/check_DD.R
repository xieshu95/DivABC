## analyse DAISIE results (DD version comparison) for 'daisie_ss_check2'
#####
library(ggplot2)
# formate results
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/obs_ss_long_with_pars.RData"))
## ABC results
folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/DAISIE_ABC_short")
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_ABC_short.csv")
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
    file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", i,"_ss_",n,".RData"),  #,"_rep",rep
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
  whole_df_ABC <- data.frame(param_data2,n_iteration,
                             # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                             lac_abc,mu_abc,gam_abc,laa_abc)
  save(whole_df_ABC,
       file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/whole_df_ABC_ss_set",n,".RData"))

  whole_df_ABC$net_div <- (whole_df_ABC$lac-whole_df_ABC$mu)
  whole_df_ABC$net_div_ABC <- (whole_df_ABC$lac_abc-whole_df_ABC$mu_abc)

  whole_df_ABC$ext_frac <- (whole_df_ABC$mu)/(whole_df_ABC$lac)
  whole_df_ABC$ext_frac_ABC <- (whole_df_ABC$mu_abc)/(whole_df_ABC$lac_abc)
  save(whole_df_ABC,file =
         paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_ABC_ss_set",n,".RData"))

}

# ######
# 2. formate MCMC results (only plot the etimation points with ABC results)
# skip
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/DAISIE_MCMC_short"
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_ABC_short.csv")
param_data <- param_data[1:81,]
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=1001),] #2001/400

lac_mcmc <- c()
mu_mcmc <- c()
gam_mcmc <- c()
laa_mcmc <- c()

for(i in 1:81){
  file_to_load <- grep(paste0("DAISIE_MCMC_short_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)



  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lac_mcmc <- c(lac_mcmc, output[,1]) # output[602:1001,1]
    mu_mcmc <- c(mu_mcmc, output[,2])
    gam_mcmc <- c(gam_mcmc, output[,3])
    laa_mcmc <- c(laa_mcmc, output[,4])
  } else {
    lac_mcmc <- c(lac_mcmc, rep(NA,1001)) #rep(NA,400)
    mu_mcmc <- c(mu_mcmc, rep(NA,1001))
    gam_mcmc <- c(gam_mcmc, rep(NA,1001))
    laa_mcmc <- c(laa_mcmc, rep(NA,1001))
  }
}

whole_df_MCMC <- data.frame(param_data3,
                            lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc)

save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/whole_df_MCMC.RData"))

whole_df_MCMC$net_div <- (whole_df_MCMC$lac-whole_df_MCMC$mu)
whole_df_MCMC$net_div_mcmc <- (whole_df_MCMC$lac_mcmc - whole_df_MCMC$mu_mcmc)

whole_df_MCMC$ext_frac <- (whole_df_MCMC$mu)/(whole_df_MCMC$lac)
whole_df_MCMC$ext_frac_MCMC <- (whole_df_MCMC$mu_mcmc)/(whole_df_MCMC$lac_mcmc)

save(whole_df_MCMC,
     file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_MCMC.RData"))

# ######
# # MLE
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/whole_df_MLE.RData"))
#
# # # MLE_DI directly load MLE results from cluster
# # param_data <- readr::read_csv2("data/DAISIE_ABC_short_DI.csv")
# # load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/MLE_DI.RData")
# # whole_df_MLE <- data.frame(param_data,MLE_all[1:4])
# #
# # whole_df_MLE$net_div <- (whole_df_MLE$lac-whole_df_MLE$mu)
# # whole_df_MLE$net_div_MLE <- (whole_df_MLE$lac_MLE-whole_df_MLE$mu_MLE)
# #
# # whole_df_MLE$ext_frac <- (whole_df_MLE$mu)/(whole_df_MLE$lac)
# # whole_df_MLE$ext_frac_MLE <- (whole_df_MLE$mu_MLE)/(whole_df_MLE$lac_MLE)
# # save(whole_df_MLE,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/whole_df_MLE_DI.RData")
#
# #####
# # plot MCMC trace
# #skip
# folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/DAISIE_MCMC_short"
# files <- list.files(folder_path)
# for(i in 1:81){
#   # param_set = (param_num-1)*5 + i
#   file_to_load <- grep(paste0("DAISIE_MCMC_short_param_set_", i,"_ss_1.RData"), #"_rep",rep,
#                        files,
#                        value = TRUE,
#                        fixed = TRUE)
#
#   if (!identical(file_to_load, character())) {
#     load(file.path(folder_path, file_to_load))
#     tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/MCMC_trace_short/set_",i,".tiff"),
#          units="px", width=2000, height=4000,res = 400,compression="lzw")
#     b_mcmc <- coda::as.mcmc(output[,1:4])
#     plot_mcmc <- plot(b_mcmc)
#     print(plot_mcmc)
#     while (!is.null(dev.list()))  dev.off()
#   }
# }
#
#
# ######
# ## combine ABC, MCMC, MLE for each parameter set(use median value)
# #skip
# for(ss in c(0,1,2,6,7,20)){
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_ABC_ss_set",ss,".RData"))
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_MCMC.RData"))
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/whole_df_MLE.RData"))
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/obs_ss_long_with_pars.RData"))
#   ## get number of iterations and mean values
#   df <- whole_df_ABC
#   n <- 500
#   ABC_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
#
#   df<-whole_df_MCMC
#   n <- 1001
#   MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
#
#   MLE_median <- whole_df_MLE
#
#
#   ## combine ABC MCMC MLE as "AMM"
#   AMM_all_df <- cbind(ABC_median[1:14],
#                       MCMC_median[,c(6,7,8,9,11,13)],
#                       MLE_median[,c(6,7,8,9,11,13)],
#                       pars_ss[,c(8,9,10,11,14)])
#   save(AMM_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/AMM_per_set_ss",ss,".RData"))
#
#   AMM_all_df$dlac_abc <- AMM_all_df$lac_abc - AMM_all_df$lac
#   AMM_all_df$dmu_abc <- AMM_all_df$mu_abc - AMM_all_df$mu
#   AMM_all_df$dgam_abc <- AMM_all_df$gam_abc - AMM_all_df$gam
#   AMM_all_df$dlaa_abc <- AMM_all_df$laa_abc - AMM_all_df$laa
#
#   AMM_all_df$dlac_mcmc <- AMM_all_df$lac_mcmc - AMM_all_df$lac
#   AMM_all_df$dmu_mcmc <- AMM_all_df$mu_mcmc - AMM_all_df$mu
#   AMM_all_df$dgam_mcmc <- AMM_all_df$gam_mcmc - AMM_all_df$gam
#   AMM_all_df$dlaa_mcmc <- AMM_all_df$laa_mcmc - AMM_all_df$laa
#
#
#   AMM_all_df$dlac_MLE <- AMM_all_df$lac_MLE - AMM_all_df$lac
#   AMM_all_df$dmu_MLE <- AMM_all_df$mu_MLE - AMM_all_df$mu
#   AMM_all_df$dgam_MLE <- AMM_all_df$gam_MLE - AMM_all_df$gam
#   AMM_all_df$dlaa_MLE <- AMM_all_df$laa_MLE - AMM_all_df$laa
#
#   AMM_all_df$dnet_div_abc <- AMM_all_df$net_div_ABC - AMM_all_df$net_div
#   AMM_all_df$dnet_div_mcmc <- AMM_all_df$net_div_mcmc - AMM_all_df$net_div
#   AMM_all_df$dnet_div_MLE <- AMM_all_df$net_div_MLE - AMM_all_df$net_div
#   save(AMM_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/AMM_per_set_drate_ss",ss,".RData"))
# }
#
# #####
# library(ggplot2)
# ## 6. plot observed treesize /tip ratio vs estimation error
# for(ss in c(0,1,2,6,7,20)){
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/AMM_per_set_ss",ss,".RData"))
#   color_values <-c("ABC" = "red3","MCMC" = "green2", "MLE" = "yellow2")
#   p_lac <-ggplot2::ggplot(data = AMM_all_df) +
#     ggplot2::theme_bw() +
#     ggplot2::ylim(0,1.0)+
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(lac_MLE),color = "MLE")) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(lac_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(lac_abc),color = "ABC"),shape = 18) +
#     ggplot2::theme_classic() +
#     ggplot2::theme(title = ggplot2::element_text(size = 12),
#                    text = ggplot2::element_text(size = 12)) +
#     ggplot2::xlab("Tree size") +
#     ggplot2::ylab(expression(lambda[c]))+
#     ggplot2::geom_hline(yintercept = AMM_all_df$lac, linetype = "dashed", size = 0.5) +
#     ggplot2::scale_color_manual(name = "Method",
#                                 values = color_values,
#                                 labels = c("ABC", "MCMC", "MLE"))
#
#   p_mu <-ggplot2::ggplot(data = AMM_all_df) +
#     ggplot2::theme_bw() +
#     ggplot2::ylim(0,0.5)+
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(mu_MLE),color = "MLE")) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(mu_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(mu_abc),color = "ABC"),shape = 18) +
#     ggplot2::theme_classic() +
#     ggplot2::theme(title = ggplot2::element_text(size = 12),
#                    text = ggplot2::element_text(size = 12)) +
#     ggplot2::xlab("Tree size") +
#     ggplot2::ylab(expression(mu))+
#     ggplot2::geom_hline(yintercept = AMM_all_df$mu, linetype = "dashed", size = 0.5) +
#     ggplot2::scale_color_manual(name = "Method",
#                                 values = color_values,
#                                 labels = c("ABC", "MCMC", "MLE"))
#
#   p_gam <-ggplot2::ggplot(data = AMM_all_df) +
#     ggplot2::theme_bw() +
#     ggplot2::ylim(0,0.05)+
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(gam_MLE),color = "MLE")) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(gam_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(gam_abc),color = "ABC"),shape = 18) +
#     ggplot2::theme_classic() +
#     ggplot2::theme(title = ggplot2::element_text(size = 12),
#                    text = ggplot2::element_text(size = 12)) +
#     ggplot2::xlab("Tree size") +
#     ggplot2::ylab(expression(gamma))+
#     ggplot2::geom_hline(yintercept = AMM_all_df$gam, linetype = "dashed", size = 0.5) +
#     ggplot2::scale_color_manual(name = "Method",
#                                 values = color_values,
#                                 labels = c("ABC", "MCMC", "MLE"))
#
#   p_laa <-ggplot2::ggplot(data = AMM_all_df) +
#     ggplot2::theme_bw() +
#     ggplot2::ylim(0,0.5)+
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(laa_MLE),color = "MLE")) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(laa_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(laa_abc),color = "ABC"),shape = 18) +
#     ggplot2::theme_classic() +
#     ggplot2::theme(title = ggplot2::element_text(size = 12),
#                    text = ggplot2::element_text(size = 12)) +
#     ggplot2::xlab("Tree size") +
#     ggplot2::ylab(expression(lambda[a]))+
#     ggplot2::geom_hline(yintercept = AMM_all_df$laa, linetype = "dashed", size = 0.5) +
#     ggplot2::scale_color_manual(name = "Method",
#                                 values = color_values,
#                                 labels = c("ABC", "MCMC", "MLE"))
#   p_div <-ggplot2::ggplot(data = AMM_all_df) +
#     ggplot2::theme_bw() +
#     ggplot2::ylim(-0.1,1)+
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(net_div_MLE),color = "MLE")) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(net_div_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = abs(net_div_ABC),color = "ABC"),shape = 18) +
#     ggplot2::theme_classic() +
#     ggplot2::theme(title = ggplot2::element_text(size = 12),
#                    text = ggplot2::element_text(size = 12)) +
#     ggplot2::ylab(expression("Net Diversification")) +
#     ggplot2::xlab("Tree size")+
#     ggplot2::scale_color_manual(name = "Method",
#                                 values = color_values,
#                                 labels = c("ABC", "MCMC", "MLE"))+
#     ggplot2::geom_hline(yintercept = AMM_all_df$net_div, linetype = "dashed", size = 0.5)
#
#   tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/exact_rate_ss",ss,".tiff"),
#        units="px", width=2500, height=2000,res = 400,compression="lzw")
#   params <- cowplot::plot_grid(
#     p_lac+ggplot2::theme(legend.position = "none"),
#     p_mu+ggplot2::theme(legend.position = "none"),
#     p_gam+ggplot2::theme(legend.position = "none"),
#     p_laa+ggplot2::theme(legend.position = "none"),
#     p_div+ggplot2::theme(legend.position = "none"),
#     align = "hv", nrow = 2, ncol = 3
#   )
#   legend <- cowplot::get_legend(
#     p_lac + theme(legend.box.margin = margin(0, 0, 0, 6))
#   )
#   param_estimates <- cowplot::plot_grid(params,legend,
#                                         rel_widths = c(3,0.4)
#   )
#   print(param_estimates)
#   while (!is.null(dev.list()))  dev.off()
# }
#
#
# ## plot drate
# library(ggplot2)
# ## 6. plot observed treesize /tip ratio vs estimation error
# for(ss in c(0,1,2,6,7,20)){  #0,1,2,6,7,20
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/AMM_per_set_drate_ss",ss,".RData"))
#   color_values <-c("ABC" = "red3","MCMC" = "green2", "MLE" = "yellow2")
#   p_lac <-ggplot2::ggplot(data = AMM_all_df) +
#     ggplot2::theme_bw() +
#     ggplot2::ylim(-1.0,1.0)+
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (dlac_MLE),color = "MLE")) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (dlac_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (dlac_abc),color = "ABC"),shape = 18) +
#     ggplot2::theme_classic() +
#     ggplot2::theme(title = ggplot2::element_text(size = 12),
#                    text = ggplot2::element_text(size = 12)) +
#     ggplot2::xlab("Tree size") +
#     ggplot2::ylab(expression(Delta~lambda[c]))+
#     ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
#     ggplot2::scale_color_manual(name = "Method",
#                                 values = color_values,
#                                 labels = c("ABC", "MCMC", "MLE"))
#
#   p_mu <-ggplot2::ggplot(data = AMM_all_df) +
#     ggplot2::theme_bw() +
#     ggplot2::ylim(-0.5,0.5)+
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (dmu_MLE),color = "MLE")) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (dmu_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (dmu_abc),color = "ABC"),shape = 18) +
#     ggplot2::theme_classic() +
#     ggplot2::theme(title = ggplot2::element_text(size = 12),
#                    text = ggplot2::element_text(size = 12)) +
#     ggplot2::xlab("Tree size") +
#     ggplot2::ylab(expression(Delta~mu))+
#     ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
#     ggplot2::scale_color_manual(name = "Method",
#                                 values = color_values,
#                                 labels = c("ABC", "MCMC", "MLE"))
#
#   p_gam <-ggplot2::ggplot(data = AMM_all_df) +
#     ggplot2::theme_bw() +
#     ggplot2::ylim(-0.05,0.05)+
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (dgam_MLE),color = "MLE")) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (dgam_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (dgam_abc),color = "ABC"),shape = 18) +
#     ggplot2::theme_classic() +
#     ggplot2::theme(title = ggplot2::element_text(size = 12),
#                    text = ggplot2::element_text(size = 12)) +
#     ggplot2::xlab("Tree size") +
#     ggplot2::ylab(expression(Delta~gamma))+
#     ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
#     ggplot2::scale_color_manual(name = "Method",
#                                 values = color_values,
#                                 labels = c("ABC", "MCMC", "MLE"))
#
#   p_laa <-ggplot2::ggplot(data = AMM_all_df) +
#     ggplot2::theme_bw() +
#     ggplot2::ylim(-0.5,0.5)+
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (laa_MLE),color = "MLE")) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (laa_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (laa_abc),color = "ABC"),shape = 18) +
#     ggplot2::theme_classic() +
#     ggplot2::theme(title = ggplot2::element_text(size = 12),
#                    text = ggplot2::element_text(size = 12)) +
#     ggplot2::xlab("Tree size") +
#     ggplot2::ylab(expression(Delta~lambda[a]))+
#     ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
#     ggplot2::scale_color_manual(name = "Method",
#                                 values = color_values,
#                                 labels = c("ABC", "MCMC", "MLE"))
#   p_div <-ggplot2::ggplot(data = AMM_all_df) +
#     ggplot2::theme_bw() +
#     ggplot2::ylim(-1,1)+
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (dnet_div_MLE),color = "MLE")) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (dnet_div_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
#     ggplot2::geom_point(ggplot2::aes(x = total,y = (dnet_div_abc),color = "ABC"),shape = 18) +
#     ggplot2::theme_classic() +
#     ggplot2::theme(title = ggplot2::element_text(size = 12),
#                    text = ggplot2::element_text(size = 12)) +
#     ggplot2::ylab(expression(Delta~"Net Diversification")) +
#     ggplot2::xlab("Tree size")+
#     ggplot2::scale_color_manual(name = "Method",
#                                 values = color_values,
#                                 labels = c("ABC", "MCMC", "MLE"))+
#     ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)
#
#   tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_ss",ss,".tiff"),
#        units="px", width=3000, height=2500,res = 400,compression="lzw")
#   params <- cowplot::plot_grid(
#     p_lac+ggplot2::theme(legend.position = "none"),
#     p_mu+ggplot2::theme(legend.position = "none"),
#     p_gam+ggplot2::theme(legend.position = "none"),
#     p_laa+ggplot2::theme(legend.position = "none"),
#     p_div+ggplot2::theme(legend.position = "none"),
#     align = "hv", nrow = 2, ncol = 3
#   )
#   legend <- cowplot::get_legend(
#     p_lac + theme(legend.box.margin = margin(0, 0, 0, 6))
#   )
#   param_estimates <- cowplot::plot_grid(params,legend,
#                                         rel_widths = c(3,0.4)
#   )
#   print(param_estimates)
#   while (!is.null(dev.list()))  dev.off()
# }
#
# #####
# # 7. plot AMM distribution
# # AMM + net_diversification
# library(ggplot2)
# for(ss in c(0)){ #c(0,1,2,6,7,20)
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_ABC_ss_set",ss,".RData"))
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_MCMC.RData"))
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/whole_df_MLE.RData"))
#   ## get legend first
#   param_abc <- whole_df_ABC[1:10,]
#   param_mcmc <- whole_df_MCMC[1:10,]
#   param_mle <- whole_df_MLE[1:10,]
#
#   p_legend <-ggplot2::ggplot(data = param_abc) +
#     ggplot2::theme_bw() +
#     xlim(0,1)+
#     ggplot2::geom_density(data = param_mcmc,
#                           ggplot2::aes(x = lac_mcmc,fill = "MCMC"),colour = "red4",
#                           alpha = 0.7) +
#     ggplot2::geom_density(ggplot2::aes(x = lac_abc),
#                           fill = "royalblue",colour = "blue3",
#                           alpha = 0.7) +
#     ggplot2::geom_density(data = param_mle,
#                           ggplot2::aes(x = lac_MLE,fill = "MLE"),colour = "green4",
#                           alpha = 0.5) +
#     ggplot2::theme_classic() +
#     ggplot2::ylab("Density") +
#     ggplot2::scale_fill_manual(name = "Method",
#                                values = c( "MCMC" = "#F7903D", "ABC" = "#4D85BD", "MLE" = "#59A95A"),
#                                labels = c("MCMC", "ABC", "MLE"))+
#     ggplot2::theme(legend.text = ggplot2::element_text(size = 15)) +
#     ggplot2::theme(legend.title = ggplot2::element_text(size = 15)) +
#     ggplot2::xlab(expression(lambda^c))+
#     ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)
#
#
#   legend_all <- cowplot::get_legend(
#     p_legend + theme(legend.box.margin = margin(0, 0, 0, 6))
#   )
#   color_values <-c("MCMC" = "#F7903D", "ABC" = "#4D85BD", "MLE" = "#59A95A")
#
#
#   for(i in 1:10){
#     param_abc <- whole_df_ABC[((i*500-499)):(i*500),]
#     param_mcmc <- whole_df_MCMC[((i*1001-499)):(i*1001),]
#     param_mle <- whole_df_MLE[i,]
#
#     # if(!is.na(param_abc[,7])){
#     p_lac <-ggplot2::ggplot(data = param_abc) +
#       ggplot2::theme_bw() +
#       xlim(-0.03,1)+
#       ggplot2::geom_histogram(data = param_mcmc,
#                               ggplot2::aes(x = lac_mcmc,fill = "MCMC"),
#                               alpha = 0.7,bins = 50) +
#       ggplot2::geom_histogram(ggplot2::aes(x = lac_abc,
#                                            fill = "ABC"),
#                               alpha = 0.7,bins = 50) +
#       ggplot2::geom_vline(data= param_mle,
#                           aes(xintercept = lac_MLE),color = "#59A95A",
#                           linetype = "solid", size = 1)+
#       ggplot2::theme_classic() +
#       ggplot2::theme(title = ggplot2::element_text(size = 15),
#                      text = ggplot2::element_text(size = 15)) +
#       ggplot2::ylab("Frequency") +
#       ggplot2::xlab(expression(lambda^c))+
#       ggplot2::scale_fill_manual(name = "Method",
#                                  values = color_values,
#                                  labels = c("MCMC", "ABC", "MLE"))+
#       # ggplot2::theme(legend.position = "none") +
#       ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)
#     # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
#     #                     linetype = "dashed", size = 0.5,color = "red")
#
#
#
#     p_mu <-ggplot2::ggplot(data = param_abc) +
#       ggplot2::theme_bw() +
#       xlim(-0.02,0.5)+
#       ggplot2::geom_histogram(data = param_mcmc,
#                               ggplot2::aes(x = mu_mcmc,fill = "MCMC"),
#                               alpha = 0.7,bins = 50) +
#       ggplot2::geom_histogram(ggplot2::aes(x = mu_abc,
#                                            fill = "ABC"),
#                               alpha = 0.7,bins = 50) +
#       ggplot2::geom_vline(data= param_mle,
#                           aes(xintercept = mu_MLE),color = "#59A95A",
#                           linetype = "solid", size = 1)+
#       ggplot2::theme_classic() +
#       ggplot2::theme(title = ggplot2::element_text(size = 15),
#                      text = ggplot2::element_text(size = 15)) +
#       ggplot2::ylab("Frequency") +
#       ggplot2::xlab(expression(mu))+
#       ggplot2::scale_fill_manual(name = "Method",
#                                  values = color_values,
#                                  labels = c("MCMC", "ABC", "MLE"))+
#       ggplot2::theme(legend.position = "none") +
#       ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", size = 0.5)
#
#     p_gam <-ggplot2::ggplot(data = param_abc) +
#       ggplot2::theme_bw() +
#       xlim(-0.002,0.05)+
#       ggplot2::geom_histogram(data = param_mcmc,
#                               ggplot2::aes(x = gam_mcmc,fill = "MCMC"),
#                               alpha = 0.7,bins = 50) +
#       ggplot2::geom_histogram(ggplot2::aes(x = gam_abc,
#                                            fill = "ABC"),
#                               alpha = 0.7,bins = 50) +
#       ggplot2::geom_vline(data= param_mle,
#                           aes(xintercept = gam_MLE),color = "#59A95A",
#                           linetype = "solid", size = 1)+
#       ggplot2::theme_classic() +
#       ggplot2::theme(title = ggplot2::element_text(size = 15),
#                      text = ggplot2::element_text(size = 15)) +
#       ggplot2::ylab("Frequency") +
#       ggplot2::xlab(expression(gamma))+
#       ggplot2::scale_fill_manual(name = "Method",
#                                  values = color_values,
#                                  labels = c("MCMC", "ABC", "MLE"))+
#       ggplot2::theme(legend.position = "none") +
#       ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed", size = 0.5)
#
#     p_laa <-ggplot2::ggplot(data = param_abc) +
#       ggplot2::theme_bw() +
#       xlim(-0.03,1.0)+
#       ggplot2::geom_histogram(data = param_mcmc,
#                               ggplot2::aes(x = laa_mcmc,fill = "MCMC"),
#                               alpha = 0.7,bins = 50) +
#       ggplot2::geom_histogram(ggplot2::aes(x = laa_abc,
#                                            fill = "ABC"),
#                               alpha = 0.7,bins = 50) +
#       ggplot2::geom_vline(data= param_mle,
#                           aes(xintercept = laa_MLE),color = "#59A95A",
#                           linetype = "solid", size = 1)+
#       ggplot2::theme_classic() +
#       ggplot2::theme(title = ggplot2::element_text(size = 15),
#                      text = ggplot2::element_text(size = 15)) +
#       ggplot2::ylab("Frequency") +
#       ggplot2::xlab(expression(lambda^a))+
#       ggplot2::scale_fill_manual(name = "Method",
#                                  values = color_values,
#                                  labels = c("MCMC", "ABC", "MLE"))+
#       ggplot2::theme(legend.position = "none") +
#       ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.5)
#
#     p_net_div <-ggplot2::ggplot(data = param_abc) +
#       ggplot2::theme_bw() +
#       xlim(-0.03,1.0)+
#       ggplot2::geom_histogram(data = param_mcmc,
#                               ggplot2::aes(x = net_div_mcmc,fill = "MCMC"),
#                               alpha = 0.7,bins = 50) +
#       ggplot2::geom_histogram(ggplot2::aes(x = net_div_ABC,fill = "ABC"),
#                               alpha = 0.7,bins = 50) +
#       ggplot2::geom_vline(data= param_mle,
#                           aes(xintercept = net_div_MLE),color = "#59A95A",
#                           linetype = "solid", size = 1)+
#       ggplot2::theme_classic() +
#       ggplot2::theme(title = ggplot2::element_text(size = 12),
#                      text = ggplot2::element_text(size = 12)) +
#       ggplot2::ylab("Frequency") +
#       ggplot2::xlab("Net diversification")+
#       ggplot2::scale_fill_manual(name = "Method",
#                                  values = color_values,
#                                  labels = c("MCMC", "ABC", "MLE"))+
#       ggplot2::theme(legend.position = "none") +
#       ggplot2::geom_vline(data= param_abc, aes(xintercept = net_div), linetype = "dashed", size = 0.5)
#
#     # p_ext_frac <-ggplot2::ggplot(data = param_abc) +
#     #   ggplot2::theme_bw() +
#     #   # xlim(0,1.0)+
#     #   ggplot2::geom_histogram(data = param_mcmc,
#     #                           ggplot2::aes(x = ext_frac_MCMC,fill = "MCMC"),
#     #                           alpha = 0.7) +
#     #   ggplot2::geom_histogram(ggplot2::aes(x = ext_frac_ABC,fill = "ABC"),
#     #                           alpha = 0.7) +
#     #   ggplot2::geom_vline(data= param_mle,
#     #                       aes(xintercept = ext_frac_MLE),color = "#59A95A",
#     #                       linetype = "solid", size = 1)+
#     #   ggplot2::theme_classic() +
#     #   ggplot2::theme(title = ggplot2::element_text(size = 13),
#     #                  text = ggplot2::element_text(size = 13)) +
#     #   ggplot2::ylab("Frequency") +
#     #   ggplot2::xlab("Extinction fraction")+
#     #   ggplot2::scale_fill_manual(name = "Method",
#     #                              values = color_values,
#     #                              labels = c("MCMC", "ABC", "MLE"))+
#     #   ggplot2::theme(legend.position = "none") +
#     #   ggplot2::geom_vline(data= param_abc, aes(xintercept = ext_frac), linetype = "dashed", size = 0.5)
#
#
#
#     p_emp <- ggplot() + theme_void()
#
#     tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/cowplot_AMM/ss_",ss,"AMM_hist_set_",i,".tiff"),
#          units="px", width=3500, height=2500,res = 400,compression="lzw")
#     param_estimates <- cowplot::plot_grid(
#       p_lac,p_mu,p_net_div,p_gam,p_laa,p_emp,
#       align = "hv", nrow = 2, ncol = 3
#     )
#     param_est_final <- cowplot::plot_grid(param_estimates,legend_all,rel_widths = c(3, .4))
#     print(param_est_final)
#     while (!is.null(dev.list()))  dev.off()
#     # }
#   }
# }
#
#
#
# #####
# # 8. plot the epsilon through generation
# library(ggplot2)
# param_data <- readr::read_csv2("data/DAISIE_ABC_short.csv")
#
# folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/DAISIE_ABC_short1"
# files <- list.files(folder_path)
# for(n in c(0,1,2,5,6,7,20)){
#   for(set in 1:81){
#     message("set", set)
#     file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", set,"_ss_",n,".RData"),  #,"_rep",rep
#                          files,
#                          value = TRUE,
#                          fixed = TRUE)
#
#     # abc <- NULL; rm(abc) # nolint ; hack around global var
#     if (!identical(file_to_load, character())) {
#       load(file.path(folder_path, file_to_load))
#
#       ss_dist<-c()
#       n_gene <- length(output$ss_diff_list)
#       if(nrow(output$ss_diff_list[[n_gene]]) < 500){
#         n_gene <- n_gene - 1
#       }
#       for(i in 1:n_gene){
#         ss_dist <- rbind(ss_dist,output$ss_diff_list[[i]])
#       }
#
#       colnames(ss_dist) <- c("Nltt","Cltt","Anagenesis","Cladogenesis","Nonendemic",
#                              "SCSD","CTSD")
#       # "pw_nltt","pw_cs","pw_nltt_sd","pw_cs_sd")
#       rownames(ss_dist) <- 1:nrow(ss_dist)
#       ss_dist <- as.data.frame(ss_dist)
#       ss_dist$generation <- as.factor(rep(1:n_gene, each = 500))
#
#       g1 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Nltt)) +
#         ggplot2::theme_bw() +
#         ggplot2::geom_boxplot()
#       # print(g1)
#
#       g2 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Cltt)) +
#         ggplot2::theme_bw() +
#         ggplot2::geom_boxplot()
#       # print(g2)
#
#       g3 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Anagenesis)) +
#         ggplot2::theme_bw() +
#         ggplot2::geom_boxplot()
#       # print(g3)
#
#       g4 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Cladogenesis)) +
#         ggplot2::theme_bw() +
#         ggplot2::geom_boxplot()
#       # print(g4)
#
#       g5 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Nonendemic)) +
#         ggplot2::theme_bw() +
#         ggplot2::geom_boxplot()
#       # print(g5)
#
#       g6 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = SCSD)) +
#         ggplot2::theme_bw() +
#         ggplot2::geom_boxplot()
#       # print(g6)
#
#       g7 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = CTSD)) +
#         ggplot2::theme_bw() +
#         ggplot2::geom_boxplot()
#       # print(g7)
#
#       tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/dss/ss_",n,"_param_",set,".tiff"),
#            units="px", width=3000, height=3000,res = 400,compression="lzw")
#       dss <- cowplot::plot_grid(
#         g1,g2,g3,g4,g5,g6,g7,g8,g9,
#         align = "hv", nrow = 3, ncol = 3
#       )
#       print(dss)
#       while (!is.null(dev.list()))  dev.off()
#     }
#   }
# }
#
#
# #####
# # 9. plot rate estimations through generation
# library(ggplot2)
# folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/DAISIE_ABC_short1"
# files <- list.files(folder_path)
# param_data <- readr::read_csv2("data/DAISIE_ABC_short.csv")
# for(n in c(0,1,2,6,7,20)){
#   for(set in 1:81){
#     message("set", set)
#     true_rates <- param_data[set,]
#     file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", set,"_ss_",n,".RData"),  #,"_rep",rep
#                          files,
#                          value = TRUE,
#                          fixed = TRUE)
#
#     # abc <- NULL; rm(abc) # nolint ; hack around global var
#     if (!identical(file_to_load, character())) {
#       load(file.path(folder_path, file_to_load))
#       ABC_df<-c()
#       n_gene <- length(output$ABC)
#       if(nrow(output$ABC[[n_gene]]) < 500){ #500
#         n_gene <- n_gene - 1
#       }
#       for(i in 1:n_gene){
#         ABC_df <- rbind(ABC_df,output$ABC[[i]])
#       }
#
#       # colnames(ss_dist) <- c("MPD","MNTD","SDPD","SDNTD",
#       #                        "D","Total","Ratio","NLTT")
#       colnames(ABC_df) <- c("lac","mu","gam","laa")
#       rownames(ABC_df) <- 1:nrow(ABC_df)
#       ABC_df <- as.data.frame(ABC_df)
#       ABC_df$generation <- as.factor(rep(1:n_gene, each = 500))
#
#       g1 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = lac)) +
#         ggplot2::theme_bw() +
#         ggplot2::geom_boxplot()+
#         ggplot2::geom_hline(data= true_rates, aes(yintercept = lac), linetype = "dashed", size = 0.5)
#       # print(g1)
#       g2 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = mu)) +
#         ggplot2::theme_bw() +
#         ggplot2::geom_boxplot()+
#         ggplot2::geom_hline(data= true_rates, aes(yintercept = mu), linetype = "dashed", size = 0.5)
#
#       g3 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = gam)) +
#         ggplot2::theme_bw() +
#         ggplot2::geom_boxplot()+
#         ggplot2::geom_hline(data= true_rates, aes(yintercept = gam), linetype = "dashed", size = 0.5)
#
#
#       g4 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = laa)) +
#         ggplot2::theme_bw() +
#         ggplot2::geom_boxplot()+
#         ggplot2::geom_hline(data= true_rates, aes(yintercept = laa), linetype = "dashed", size = 0.5)
#
#       tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/rate_each_gene/ss_",n,"_param_",set,".tiff"),
#            units="px", width=2000, height=2000,res = 400,compression="lzw")
#       dss <- cowplot::plot_grid(
#         g1,g2,g3,g4,
#         align = "hv", nrow = 2, ncol = 2
#       )
#       print(dss)
#       while (!is.null(dev.list()))  dev.off()
#     }
#   }
# }
#
# #####
# #cowplot only ABC (histgram)
# library(ggplot2)
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/whole_df_MLE.RData"))
# for(n in c(0)){
#   load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_ABC_ss_set",n,".RData"))
#   for(i in 1:81){
#     param_abc <- whole_df_ABC[((i*500-499)):(i*500),]
#
#     if(!is.na(param_abc[1,6])){
#       p_lac <-ggplot2::ggplot(data = param_abc) +
#         ggplot2::theme_bw() +
#         xlim(-0.02,1)+
#         ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
#                                 fill = "royalblue",
#                                 alpha = 0.9, bins = 30) +
#         ggplot2::theme_classic() +
#         ggplot2::theme(title = ggplot2::element_text(size = 12),
#                        text = ggplot2::element_text(size = 12)) +
#         ggplot2::ylab("Density") +
#         ggplot2::xlab(expression(lambda^c))+
#         ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)+
#         ggplot2::geom_vline(data= whole_df_MLE[i,], aes(xintercept = lac_MLE),
#                             linetype = "dashed", size = 0.5,color = "red")
#
#       p_mu <-ggplot2::ggplot(data = param_abc) +
#         ggplot2::theme_bw() +
#         xlim(-0.01,0.5)+
#         ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_abc),
#                                 fill = "royalblue",
#                                 alpha = 0.9, bins = 30) +
#         ggplot2::theme_classic() +
#         ggplot2::theme(title = ggplot2::element_text(size = 12),
#                        text = ggplot2::element_text(size = 12)) +
#         ggplot2::ylab("Density") +
#         ggplot2::xlab(expression(mu))+
#         ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", size = 0.5)+
#         ggplot2::geom_vline(data= whole_df_MLE[i,], aes(xintercept = mu_MLE),
#                             linetype = "dashed", size = 0.5,color = "red")
#
#       p_gam <-ggplot2::ggplot(data = param_abc) +
#         ggplot2::theme_bw() +
#         xlim(-0.001,0.03)+
#         ggplot2::geom_histogram(mapping = ggplot2::aes(x = gam_abc),
#                                 fill = "royalblue",
#                                 alpha = 0.9, bins = 30) +
#         ggplot2::theme_classic() +
#         ggplot2::theme(title = ggplot2::element_text(size = 12),
#                        text = ggplot2::element_text(size = 12)) +
#         ggplot2::ylab("Density") +
#         ggplot2::xlab(expression(gamma))+
#         ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed", size = 0.5)+
#         ggplot2::geom_vline(data= whole_df_MLE[i,], aes(xintercept = gam_MLE),
#                             linetype = "dashed", size = 0.5,color = "red")
#
#
#       p_laa <-ggplot2::ggplot(data = param_abc) +
#         ggplot2::theme_bw() +
#         xlim(-0.01,0.75)+
#         ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_abc),
#                                 fill = "royalblue",
#                                 alpha = 0.9, bins = 30) +
#         ggplot2::theme_classic() +
#         ggplot2::theme(title = ggplot2::element_text(size = 12),
#                        text = ggplot2::element_text(size = 12)) +
#         ggplot2::ylab("Density") +
#         ggplot2::xlab(expression(lambda^a))+
#         ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.5)+
#         ggplot2::geom_vline(data= whole_df_MLE[i,], aes(xintercept = laa_MLE),
#                             linetype = "dashed", size = 0.5,color = "red")
#
#       mu_vs_lac <- ggplot2::ggplot(data = param_abc) +
#         ggplot2::theme_bw() +
#         xlim(0,1)+
#         ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = mu_abc),
#                             colour = "royalblue",shape = 16,alpha = 0.2) +
#         ggplot2::theme_classic() +
#         ggplot2::theme(title = ggplot2::element_text(size = 12),
#                        text = ggplot2::element_text(size = 12)) +
#         ggplot2::ylab(expression(mu)) +
#         ggplot2::xlab(expression(lambda^c)) +
#         ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = mu),
#                             colour = "black",shape = 16,size = 2.5)
#       # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
#       # ggplot2::geom_hline(data= param_abc, aes(yintercept = mu), colour = "grey50")
#
#       gam_vs_lac <- ggplot2::ggplot(data = param_abc) +
#         ggplot2::theme_bw() +
#         xlim(0,1)+
#         ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = gam_abc),
#                             colour = "royalblue",shape = 16,alpha = 0.2) +
#         ggplot2::theme_classic() +
#         ggplot2::theme(title = ggplot2::element_text(size = 12),
#                        text = ggplot2::element_text(size = 12)) +
#         ggplot2::ylab(expression(gamma)) +
#         ggplot2::xlab(expression(lambda^c)) +
#         ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = gam),
#                             colour = "black",shape = 16,size = 2.5)
#       # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
#       # ggplot2::geom_hline(data= param_abc, aes(yintercept = gam), colour = "grey50")
#
#       laa_vs_lac <- ggplot2::ggplot(data = param_abc) +
#         ggplot2::theme_bw() +
#         xlim(0,1)+
#         ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = laa_abc),
#                             colour = "royalblue",shape = 16,alpha = 0.2) +
#         ggplot2::theme_classic() +
#         ggplot2::theme(title = ggplot2::element_text(size = 12),
#                        text = ggplot2::element_text(size = 12)) +
#         ggplot2::ylab(expression(lambda^a)) +
#         ggplot2::xlab(expression(lambda^c)) +
#         ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = laa),
#                             colour = "black",shape = 16,size = 2.5)
#       # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
#       # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")
#
#       gam_vs_mu <- ggplot2::ggplot(data = param_abc) +
#         ggplot2::theme_bw() +
#         xlim(0,0.5)+
#         ggplot2::geom_point(mapping = ggplot2::aes(x = mu_abc,y = gam_abc),
#                             colour = "royalblue",shape = 16,alpha = 0.2) +
#         ggplot2::theme_classic() +
#         ggplot2::theme(title = ggplot2::element_text(size = 12),
#                        text = ggplot2::element_text(size = 12)) +
#         ggplot2::ylab(expression(gamma)) +
#         ggplot2::xlab(expression(mu)) +
#         ggplot2::geom_point(mapping = ggplot2::aes(x = mu,y = gam),
#                             colour = "black",shape = 16,size = 2.5)
#       # ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), colour = "grey50") +
#       # ggplot2::geom_hline(data= param_abc, aes(yintercept = gam), colour = "grey50")
#
#       laa_vs_mu <- ggplot2::ggplot(data = param_abc) +
#         ggplot2::theme_bw() +
#         xlim(0,0.5)+
#         ggplot2::geom_point(mapping = ggplot2::aes(x = mu_abc,y = laa_abc),
#                             colour = "royalblue",shape = 16,alpha = 0.2) +
#         ggplot2::theme_classic() +
#         ggplot2::theme(title = ggplot2::element_text(size = 12),
#                        text = ggplot2::element_text(size = 12)) +
#         ggplot2::ylab(expression(lambda^a)) +
#         ggplot2::xlab(expression(mu)) +
#         ggplot2::geom_point(mapping = ggplot2::aes(x = mu,y = laa),
#                             colour = "black",shape = 16,size = 2.5)
#       # ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), colour = "grey50") +
#       # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")
#
#       laa_vs_gam <- ggplot2::ggplot(data = param_abc) +
#         ggplot2::theme_bw() +
#         xlim(0,0.03)+
#         ggplot2::geom_point(mapping = ggplot2::aes(x = gam_abc,y = laa_abc),
#                             colour = "royalblue",shape = 16,alpha = 0.2) +
#         ggplot2::theme_classic() +
#         ggplot2::theme(title = ggplot2::element_text(size = 12),
#                        text = ggplot2::element_text(size = 12)) +
#         ggplot2::ylab(expression(lambda^a)) +
#         ggplot2::xlab(expression(gamma)) +
#         ggplot2::geom_point(mapping = ggplot2::aes(x = gam,y = laa),
#                             colour = "black",shape = 16,size = 2.5)
#       # ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), colour = "grey50") +
#       # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")
#
#       p_emp <- ggplot() + theme_void()
#
#       tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/cowplots_hist/ss",n,"_param_",i,".tiff"),
#            units="px", width=3000, height=2000,res = 400,compression="lzw")
#       param_estimates <- cowplot::plot_grid(
#         p_lac,p_emp,p_emp,p_emp,
#         mu_vs_lac,p_mu,p_emp,p_emp,
#         gam_vs_lac,gam_vs_mu,p_gam,p_emp,
#         laa_vs_lac,laa_vs_mu,laa_vs_gam,p_laa,
#         align = "hv", nrow = 4, ncol = 4
#       )
#       print(param_estimates)
#       while (!is.null(dev.list()))  dev.off()
#     }
#   }
# }

# 81 all particles comparsion
## plot all particles (ABC-new vs ABC-old vs MCMC VS MLE)
library(ggplot2)
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/obs_ss_long_with_pars.RData"))

# ss = "ABC-old"
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_unif_DD_7ss/delta_whole_df_ABC_ss_set",0,".RData"))
# whole_df_ABC$ss = "ABC-old"
# whole_df_ABC_old = whole_df_ABC[,-6]
# whole_df_ABC_old$total <- rep(rep(pars_ss$total, each = 500), 1)
#
#
# ss = "ABC-new1"
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DD_nltt/delta_whole_df_ABC_ss_set",0,".RData"))
# whole_df_ABC$ss = "ABC-new1"
# whole_df_ABC_new = whole_df_ABC[,-6]
# whole_df_ABC_new$total <- rep(rep(pars_ss$total, each = 300), 1)



load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_ABC_ss_set",0,".RData"))
whole_df_ABC$ss = "ABC-all"
whole_df_ABC_s0 = whole_df_ABC[,-6]
whole_df_ABC_s0$total <- rep(rep(pars_ss$total, each = 400), 1)


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_ABC_ss_set",1,".RData"))
whole_df_ABC$ss = "ABC-nltt"
whole_df_ABC_s1 = whole_df_ABC[,-6]
whole_df_ABC_s1$total <- rep(rep(pars_ss$total, each = 400), 1)


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_ABC_ss_set",2,".RData"))
whole_df_ABC$ss = "ABC-tips"
whole_df_ABC_s2 = whole_df_ABC[,-6]
whole_df_ABC_s2$total <- rep(rep(pars_ss$total, each = 400), 1)

whole_df_ABC <- rbind(whole_df_ABC_s0,
                      whole_df_ABC_s1,
                      whole_df_ABC_s2) #whole_df_ABC_20

# whole_df_ABC <- rbind(whole_df_ABC_s1)
whole_df_ABC$dlac <- whole_df_ABC$lac_abc - whole_df_ABC$lac
whole_df_ABC$dmu <- whole_df_ABC$mu_abc - whole_df_ABC$mu
whole_df_ABC$dgam <- whole_df_ABC$gam_abc - whole_df_ABC$gam
whole_df_ABC$dlaa <- whole_df_ABC$laa_abc - whole_df_ABC$laa
whole_df_ABC$dnet_div <- whole_df_ABC$net_div_ABC - whole_df_ABC$net_div
whole_df_ABC$dext_frac <- whole_df_ABC$ext_frac_ABC - whole_df_ABC$ext_frac
# whole_df_ABC$total <- rep(rep(pars_ss$total, each = 400), 1) # 500,5


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_MCMC.RData"))

whole_df_MCMC$ss = "MCMC"
whole_df_MCMC$total <- rep(rep(pars_ss$total, each = 400), 1)
whole_df_MCMC$dlac <- whole_df_MCMC$lac_mcmc - whole_df_MCMC$lac
whole_df_MCMC$dmu <- whole_df_MCMC$mu_mcmc - whole_df_MCMC$mu
whole_df_MCMC$dgam <- whole_df_MCMC$gam_mcmc - whole_df_MCMC$gam
whole_df_MCMC$dlaa <- whole_df_MCMC$laa_mcmc - whole_df_MCMC$laa
whole_df_MCMC$dnet_div <- whole_df_MCMC$net_div_mcmc - whole_df_MCMC$net_div
whole_df_MCMC$dext_frac <- whole_df_MCMC$ext_frac_MCMC - whole_df_MCMC$ext_frac

load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/whole_df_MLE.RData")
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


## 81 into 1 net div (facet plots)
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
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_81_netdiv.tiff"),
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
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_81_lac.tiff"),
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
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_81_mu.tiff"),
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
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_81_gam.tiff"),
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
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_81_laa.tiff"),
     units="px", width=5000, height=3000,res = 350,compression="lzw")
print(p_laa)
while (!is.null(dev.list()))  dev.off()





## plot use all the particles rather than median values
# ss = 0
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 0
# whole_df_ABC_0 = whole_df_ABC
#
# ss = 1
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 1
# whole_df_ABC_1 = whole_df_ABC
#
# ss = 2
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 2
# whole_df_ABC_2 = whole_df_ABC

# ss = 6
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 6
# whole_df_ABC_6 = whole_df_ABC
#
# ss = 7
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 7
# whole_df_ABC_7 = whole_df_ABC
#
# ss = 20
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/delta_whole_df_ABC_ss_set",ss,".RData"))
# whole_df_ABC$ss = 20
# whole_df_ABC_20 = whole_df_ABC


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

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_netdiv.tiff"),
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
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_lac.tiff"),
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
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_mu.tiff"),
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
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_gam.tiff"),
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
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_laa.tiff"),
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
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_all_particles_fit_lac.tiff"),
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
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_all_particles_fit_mu.tiff"),
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
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_all_particles_fit_gam.tiff"),
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
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_all_particles_fit_laa.tiff"),
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
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_ABC_netdiv_lac.tiff"),
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
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_ABC_netdiv_mu.tiff"),
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
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_ABC_netdiv_gam.tiff"),
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
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_ABC_netdiv_laa.tiff"),
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
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_nltt_netdiv_lac.tiff"),
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
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_nltt_netdiv_mu.tiff"),
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
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_nltt_netdiv_gam.tiff"),
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
# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_nltt_netdiv_laa.tiff"),
#      units="px", width=4500, height=1500,res = 350,compression="lzw")
# print(p_netdiv_laa)
# while (!is.null(dev.list()))  dev.off()


library(ggplot2)
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/DAISIE_ABC_short"
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_ABC_short.csv")
for(n in c(0,1,2)){
  ABC_df<-c()
  generation <-c()
  set_val <- c()
  for(set in 1:81){
    message("set", set)
    true_rates <- param_data[set,]
    file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", set,"_ss_",n,".RData"),  #,"_rep",rep
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

    } else{
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
  save(ABC_df_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/rates_all_generations",n,".RData"))
}

library(ggplot2)
for(ss in c(0,1,2)) {
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/rates_all_generations",ss,".RData"))
  iqr = function(z, lower = 0.1, upper = 0.9) {
    data.frame(
      y = median(z),
      ymin = quantile(z, lower),
      ymax = quantile(z, upper)
    )
  }

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
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_each_gene_netdiv",ss,".tiff"),
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
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_each_gene_lac",ss,".tiff"),
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
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_each_gene_mu",ss,".tiff"),
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
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_each_gene_gam",ss,".tiff"),
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
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_ss_check2/DD/drate_each_gene_laa",ss,".tiff"),
       units="px", width=5000, height=3000,res = 350,compression="lzw")
  print(p_laa)
  while (!is.null(dev.list()))  dev.off()

}



