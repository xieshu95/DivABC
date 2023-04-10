## analyse DAISIE results
#####
library(ggplot2)
# formate results
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/obs_ss_long_with_pars.RData"))
## ABC results
folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/DAISIE_ABC_short1")
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_ABC_short.csv")
param_data2<-param_data[rep(seq_len(nrow(param_data)), each=500),] #500

# 1. formate ABC results
for(n in c(0,1,2,6,7,20)){
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
      if(output$n_iter <= 4){
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
  whole_df_ABC <- data.frame(param_data2,n_iteration,
                             # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                             lac_abc,mu_abc,gam_abc,laa_abc)
  save(whole_df_ABC,
       file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/whole_df_ABC_ss_set",n,".RData"))

  whole_df_ABC$net_div <- (whole_df_ABC$lac-whole_df_ABC$mu)
  whole_df_ABC$net_div_ABC <- (whole_df_ABC$lac_abc-whole_df_ABC$mu_abc)

  whole_df_ABC$ext_frac <- (whole_df_ABC$mu)/(whole_df_ABC$lac)
  whole_df_ABC$ext_frac_ABC <- (whole_df_ABC$mu_abc)/(whole_df_ABC$lac_abc)
  save(whole_df_ABC,file =
         paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/delta_whole_df_ABC_ss_set",n,".RData"))

}

######
# 2. formate MCMC results (only plot the etimation points with ABC results)
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/DAISIE_MCMC_short"
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_ABC_short.csv")
param_data <- param_data[1:81,]
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=1001),] #5001

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
    lac_mcmc <- c(lac_mcmc, output[,1])
    mu_mcmc <- c(mu_mcmc, output[,2])
    gam_mcmc <- c(gam_mcmc, output[,3])
    laa_mcmc <- c(laa_mcmc, output[,4])
  } else {
    lac_mcmc <- c(lac_mcmc, rep(NA,1001))
    mu_mcmc <- c(mu_mcmc, rep(NA,1001))
    gam_mcmc <- c(gam_mcmc, rep(NA,1001))
    laa_mcmc <- c(laa_mcmc, rep(NA,1001))
  }
}

whole_df_MCMC <- data.frame(param_data3,
                            lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc)

save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/whole_df_MCMC.RData"))

whole_df_MCMC$net_div <- (whole_df_MCMC$lac-whole_df_MCMC$mu)
whole_df_MCMC$net_div_mcmc <- (whole_df_MCMC$lac_mcmc - whole_df_MCMC$mu_mcmc)

whole_df_MCMC$ext_frac <- (whole_df_MCMC$mu)/(whole_df_MCMC$lac)
whole_df_MCMC$ext_frac_MCMC <- (whole_df_MCMC$mu_mcmc)/(whole_df_MCMC$lac_mcmc)

save(whole_df_MCMC,
     file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/delta_whole_df_MCMC.RData"))

######
# MLE
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/whole_df_MLE.RData"))

#####
# plot MCMC trace
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/DAISIE_MCMC_short"
files <- list.files(folder_path)
for(i in 1:81){
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("DAISIE_MCMC_short_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)

  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/MCMC_trace_short/set_",i,".tiff"),
         units="px", width=2000, height=4000,res = 400,compression="lzw")
    b_mcmc <- coda::as.mcmc(output[,1:4])
    plot_mcmc <- plot(b_mcmc)
    print(plot_mcmc)
    while (!is.null(dev.list()))  dev.off()
  }
}


######
## combine ABC, MCMC, MLE for each parameter set(use median value)
for(ss in c(0,1,2,6,7,20)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/delta_whole_df_ABC_ss_set",ss,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/delta_whole_df_MCMC.RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/whole_df_MLE.RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/obs_ss_long_with_pars.RData"))
  ## get number of iterations and mean values
  df <- whole_df_ABC
  n <- 500
  ABC_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

  df<-whole_df_MCMC
  n <- 1001
  MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

  MLE_median <- whole_df_MLE


  ## combine ABC MCMC MLE as "AMM"
  AMM_all_df <- cbind(ABC_median[1:14],
                      MCMC_median[,c(6,7,8,9,11,13)],
                      MLE_median[,c(6,7,8,9,11,13)],
                      pars_ss[,c(8,9,10,11,14)])
  save(AMM_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/AMM_per_set_ss",ss,".RData"))

  AMM_all_df$dlac_abc <- AMM_all_df$lac_abc - AMM_all_df$lac
  AMM_all_df$dmu_abc <- AMM_all_df$mu_abc - AMM_all_df$mu
  AMM_all_df$dgam_abc <- AMM_all_df$gam_abc - AMM_all_df$gam
  AMM_all_df$dlaa_abc <- AMM_all_df$laa_abc - AMM_all_df$laa

  AMM_all_df$dlac_mcmc <- AMM_all_df$lac_mcmc - AMM_all_df$lac
  AMM_all_df$dmu_mcmc <- AMM_all_df$mu_mcmc - AMM_all_df$mu
  AMM_all_df$dgam_mcmc <- AMM_all_df$gam_mcmc - AMM_all_df$gam
  AMM_all_df$dlaa_mcmc <- AMM_all_df$laa_mcmc - AMM_all_df$laa


  AMM_all_df$dlac_MLE <- AMM_all_df$lac_MLE - AMM_all_df$lac
  AMM_all_df$dmu_MLE <- AMM_all_df$mu_MLE - AMM_all_df$mu
  AMM_all_df$dgam_MLE <- AMM_all_df$gam_MLE - AMM_all_df$gam
  AMM_all_df$dlaa_MLE <- AMM_all_df$laa_MLE - AMM_all_df$laa

  AMM_all_df$dnet_div_abc <- AMM_all_df$net_div_ABC - AMM_all_df$net_div
  AMM_all_df$dnet_div_mcmc <- AMM_all_df$net_div_mcmc - AMM_all_df$net_div
  AMM_all_df$dnet_div_MLE <- AMM_all_df$net_div_MLE - AMM_all_df$net_div
  save(AMM_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/AMM_per_set_drate_ss",ss,".RData"))
}

#####
library(ggplot2)
## 6. plot observed treesize /tip ratio vs estimation error
for(ss in c(0,1,2,6,7,20)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/AMM_per_set_ss",ss,".RData"))
  color_values <-c("ABC" = "red3","MCMC" = "green2", "MLE" = "yellow2")
  p_lac <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(lac_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(lac_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(lac_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(lambda[c]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$lac, linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_mu <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,0.5)+
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(mu_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(mu_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(mu_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(mu))+
    ggplot2::geom_hline(yintercept = AMM_all_df$mu, linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_gam <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,0.05)+
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(gam_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(gam_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(gam_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(gamma))+
    ggplot2::geom_hline(yintercept = AMM_all_df$gam, linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_laa <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,0.5)+
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(laa_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(laa_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(laa_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(lambda[a]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$laa, linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))
  p_div <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.1,1)+
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(net_div_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(net_div_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = abs(net_div_ABC),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression("Net Diversification")) +
    ggplot2::xlab("Tree size")+
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))+
    ggplot2::geom_hline(yintercept = AMM_all_df$net_div, linetype = "dashed", size = 0.5)

  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/exact_rate_ss",ss,".tiff"),
       units="px", width=2500, height=2000,res = 400,compression="lzw")
  params <- cowplot::plot_grid(
    p_lac+ggplot2::theme(legend.position = "none"),
    p_mu+ggplot2::theme(legend.position = "none"),
    p_gam+ggplot2::theme(legend.position = "none"),
    p_laa+ggplot2::theme(legend.position = "none"),
    p_div+ggplot2::theme(legend.position = "none"),
    align = "hv", nrow = 2, ncol = 3
  )
  legend <- cowplot::get_legend(
    p_lac + theme(legend.box.margin = margin(0, 0, 0, 6))
  )
  param_estimates <- cowplot::plot_grid(params,legend,
                                        rel_widths = c(3,0.4)
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()
}


## plot drate
library(ggplot2)
## 6. plot observed treesize /tip ratio vs estimation error
for(ss in c(0,1,2,6,7,20)){  #0,1,2,6,7,20
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/AMM_per_set_drate_ss",ss,".RData"))
  color_values <-c("ABC" = "red3","MCMC" = "green2", "MLE" = "yellow2")
  p_lac <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-1.0,1.0)+
    ggplot2::geom_point(ggplot2::aes(x = total,y = (dlac_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = (dlac_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = (dlac_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~lambda[c]))+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_mu <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.5,0.5)+
    ggplot2::geom_point(ggplot2::aes(x = total,y = (dmu_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = (dmu_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = (dmu_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~mu))+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_gam <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.05,0.05)+
    ggplot2::geom_point(ggplot2::aes(x = total,y = (dgam_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = (dgam_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = (dgam_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~gamma))+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_laa <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.5,0.5)+
    ggplot2::geom_point(ggplot2::aes(x = total,y = (laa_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = (laa_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = (laa_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~lambda[a]))+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))
  p_div <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-1,1)+
    ggplot2::geom_point(ggplot2::aes(x = total,y = (dnet_div_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = (dnet_div_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = total,y = (dnet_div_abc),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab(expression(Delta~"Net Diversification")) +
    ggplot2::xlab("Tree size")+
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)

  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/drate_ss",ss,".tiff"),
       units="px", width=3000, height=2500,res = 400,compression="lzw")
  params <- cowplot::plot_grid(
    p_lac+ggplot2::theme(legend.position = "none"),
    p_mu+ggplot2::theme(legend.position = "none"),
    p_gam+ggplot2::theme(legend.position = "none"),
    p_laa+ggplot2::theme(legend.position = "none"),
    p_div+ggplot2::theme(legend.position = "none"),
    align = "hv", nrow = 2, ncol = 3
  )
  legend <- cowplot::get_legend(
    p_lac + theme(legend.box.margin = margin(0, 0, 0, 6))
  )
  param_estimates <- cowplot::plot_grid(params,legend,
                                        rel_widths = c(3,0.4)
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()
}

#####
# 7. plot AMM distribution
# AMM + net_diversification
library(ggplot2)
for(ss in c(0)){ #c(0,1,2,6,7,20)
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/delta_whole_df_ABC_ss_set",ss,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/delta_whole_df_MCMC.RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/whole_df_MLE.RData"))
  ## get legend first
  param_abc <- whole_df_ABC[1:10,]
  param_mcmc <- whole_df_MCMC[1:10,]
  param_mle <- whole_df_MLE[1:10,]

  p_legend <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,1)+
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = lac_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.7) +
    ggplot2::geom_density(ggplot2::aes(x = lac_abc),
                          fill = "royalblue",colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_density(data = param_mle,
                          ggplot2::aes(x = lac_MLE,fill = "MLE"),colour = "green4",
                          alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::ylab("Density") +
    ggplot2::scale_fill_manual(name = "Method",
                               values = c( "MCMC" = "#F7903D", "ABC" = "#4D85BD", "MLE" = "#59A95A"),
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 15)) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 15)) +
    ggplot2::xlab(expression(lambda^c))+
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)


  legend_all <- cowplot::get_legend(
    p_legend + theme(legend.box.margin = margin(0, 0, 0, 6))
  )
  color_values <-c("MCMC" = "#F7903D", "ABC" = "#4D85BD", "MLE" = "#59A95A")


  for(i in 1:10){
    param_abc <- whole_df_ABC[((i*500-499)):(i*500),]
    param_mcmc <- whole_df_MCMC[((i*1001-499)):(i*1001),]
    param_mle <- whole_df_MLE[i,]

    # if(!is.na(param_abc[,7])){
    p_lac <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(-0.03,1)+
      ggplot2::geom_histogram(data = param_mcmc,
                              ggplot2::aes(x = lac_mcmc,fill = "MCMC"),
                              alpha = 0.7,bins = 50) +
      ggplot2::geom_histogram(ggplot2::aes(x = lac_abc,
                                           fill = "ABC"),
                              alpha = 0.7,bins = 50) +
      ggplot2::geom_vline(data= param_mle,
                          aes(xintercept = lac_MLE),color = "#59A95A",
                          linetype = "solid", size = 1)+
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(lambda^c))+
      ggplot2::scale_fill_manual(name = "Method",
                                 values = color_values,
                                 labels = c("MCMC", "ABC", "MLE"))+
      # ggplot2::theme(legend.position = "none") +
      ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)
    # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
    #                     linetype = "dashed", size = 0.5,color = "red")



    p_mu <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(-0.02,0.5)+
      ggplot2::geom_histogram(data = param_mcmc,
                              ggplot2::aes(x = mu_mcmc,fill = "MCMC"),
                              alpha = 0.7,bins = 50) +
      ggplot2::geom_histogram(ggplot2::aes(x = mu_abc,
                                           fill = "ABC"),
                              alpha = 0.7,bins = 50) +
      ggplot2::geom_vline(data= param_mle,
                          aes(xintercept = mu_MLE),color = "#59A95A",
                          linetype = "solid", size = 1)+
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(mu))+
      ggplot2::scale_fill_manual(name = "Method",
                                 values = color_values,
                                 labels = c("MCMC", "ABC", "MLE"))+
      ggplot2::theme(legend.position = "none") +
      ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", size = 0.5)

    p_gam <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(-0.002,0.05)+
      ggplot2::geom_histogram(data = param_mcmc,
                              ggplot2::aes(x = gam_mcmc,fill = "MCMC"),
                              alpha = 0.7,bins = 50) +
      ggplot2::geom_histogram(ggplot2::aes(x = gam_abc,
                                           fill = "ABC"),
                              alpha = 0.7,bins = 50) +
      ggplot2::geom_vline(data= param_mle,
                          aes(xintercept = gam_MLE),color = "#59A95A",
                          linetype = "solid", size = 1)+
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(gamma))+
      ggplot2::scale_fill_manual(name = "Method",
                                 values = color_values,
                                 labels = c("MCMC", "ABC", "MLE"))+
      ggplot2::theme(legend.position = "none") +
      ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed", size = 0.5)

    p_laa <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(-0.03,1.0)+
      ggplot2::geom_histogram(data = param_mcmc,
                              ggplot2::aes(x = laa_mcmc,fill = "MCMC"),
                              alpha = 0.7,bins = 50) +
      ggplot2::geom_histogram(ggplot2::aes(x = laa_abc,
                                           fill = "ABC"),
                              alpha = 0.7,bins = 50) +
      ggplot2::geom_vline(data= param_mle,
                          aes(xintercept = laa_MLE),color = "#59A95A",
                          linetype = "solid", size = 1)+
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(lambda^a))+
      ggplot2::scale_fill_manual(name = "Method",
                                 values = color_values,
                                 labels = c("MCMC", "ABC", "MLE"))+
      ggplot2::theme(legend.position = "none") +
      ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.5)

    p_net_div <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(-0.03,1.0)+
      ggplot2::geom_histogram(data = param_mcmc,
                              ggplot2::aes(x = net_div_mcmc,fill = "MCMC"),
                              alpha = 0.7,bins = 50) +
      ggplot2::geom_histogram(ggplot2::aes(x = net_div_ABC,fill = "ABC"),
                              alpha = 0.7,bins = 50) +
      ggplot2::geom_vline(data= param_mle,
                          aes(xintercept = net_div_MLE),color = "#59A95A",
                          linetype = "solid", size = 1)+
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab("Net diversification")+
      ggplot2::scale_fill_manual(name = "Method",
                                 values = color_values,
                                 labels = c("MCMC", "ABC", "MLE"))+
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

    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/cowplot_AMM/ss_",ss,"AMM_hist_set_",i,".tiff"),
         units="px", width=3500, height=2500,res = 400,compression="lzw")
    param_estimates <- cowplot::plot_grid(
      p_lac,p_mu,p_net_div,p_gam,p_laa,p_emp,
      align = "hv", nrow = 2, ncol = 3
    )
    param_est_final <- cowplot::plot_grid(param_estimates,legend_all,rel_widths = c(3, .4))
    print(param_est_final)
    while (!is.null(dev.list()))  dev.off()
    # }
  }
}



#####
# 8. plot the epsilon through generation
library(ggplot2)
library(ggplot2)
param_data <- readr::read_csv2("data/DAISIE_ABC_short.csv")

folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/DAISIE_ABC_short1"
files <- list.files(folder_path)
for(n in c(0,1,2,56,7,20)){
  for(set in 1:81){
    message("set", set)
    file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", set,"_ss_",n,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))

      ss_dist<-c()
      n_gene <- length(output$ss_diff_list)
      if(nrow(output$ss_diff_list[[n_gene]]) < 500){
        n_gene <- n_gene - 1
      }
      for(i in 1:n_gene){
        ss_dist <- rbind(ss_dist,output$ss_diff_list[[i]])
      }

      colnames(ss_dist) <- c("Nltt","Cltt","Anagenesis","Cladogenesis","Nonendemic",
                             "SCSD","CTSD")
                             # "pw_nltt","pw_cs","pw_nltt_sd","pw_cs_sd")
      rownames(ss_dist) <- 1:nrow(ss_dist)
      ss_dist <- as.data.frame(ss_dist)
      ss_dist$generation <- as.factor(rep(1:n_gene, each = 500))

      g1 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Nltt)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()
      # print(g1)

      g2 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Cltt)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()
      # print(g2)

      g3 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Anagenesis)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()
      # print(g3)

      g4 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Cladogenesis)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()
      # print(g4)

      g5 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Nonendemic)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()
      # print(g5)

      g6 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = SCSD)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()
      # print(g6)

      g7 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = CTSD)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()
      # print(g7)

      tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/dss/ss_",n,"_param_",set,".tiff"),
           units="px", width=3000, height=3000,res = 400,compression="lzw")
      dss <- cowplot::plot_grid(
        g1,g2,g3,g4,g5,g6,g7,g8,g9,
        align = "hv", nrow = 3, ncol = 3
      )
      print(dss)
      while (!is.null(dev.list()))  dev.off()
    }
  }
}


#####
# 9. plot rate estimations through generation
library(ggplot2)
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/DAISIE_ABC_short1"
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_ABC_short.csv")
for(n in c(0,1,2,6,7,20)){
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
      ABC_df<-c()
      n_gene <- length(output$ABC)
      if(nrow(output$ABC[[n_gene]]) < 500){ #500
        n_gene <- n_gene - 1
      }
      for(i in 1:n_gene){
        ABC_df <- rbind(ABC_df,output$ABC[[i]])
      }

      # colnames(ss_dist) <- c("MPD","MNTD","SDPD","SDNTD",
      #                        "D","Total","Ratio","NLTT")
      colnames(ABC_df) <- c("lac","mu","gam","laa")
      rownames(ABC_df) <- 1:nrow(ABC_df)
      ABC_df <- as.data.frame(ABC_df)
      ABC_df$generation <- as.factor(rep(1:n_gene, each = 500))

      g1 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = lac)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()+
        ggplot2::geom_hline(data= true_rates, aes(yintercept = lac), linetype = "dashed", size = 0.5)
      # print(g1)
      g2 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = mu)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()+
        ggplot2::geom_hline(data= true_rates, aes(yintercept = mu), linetype = "dashed", size = 0.5)

      g3 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = gam)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()+
        ggplot2::geom_hline(data= true_rates, aes(yintercept = gam), linetype = "dashed", size = 0.5)


      g4 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = laa)) +
        ggplot2::theme_bw() +
        ggplot2::geom_boxplot()+
        ggplot2::geom_hline(data= true_rates, aes(yintercept = laa), linetype = "dashed", size = 0.5)

      tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/rate_each_gene/ss_",n,"_param_",set,".tiff"),
           units="px", width=2000, height=2000,res = 400,compression="lzw")
      dss <- cowplot::plot_grid(
        g1,g2,g3,g4,
        align = "hv", nrow = 2, ncol = 2
      )
      print(dss)
      while (!is.null(dev.list()))  dev.off()
    }
  }
}

#####
#cowplot only ABC (histgram)
library(ggplot2)
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/whole_df_MLE.RData"))
for(n in c(0)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/delta_whole_df_ABC_ss_set",n,".RData"))
  for(i in 1:81){
    param_abc <- whole_df_ABC[((i*500-499)):(i*500),]

    if(!is.na(param_abc[1,6])){
      p_lac <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(-0.02,1)+
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
                                fill = "royalblue",
                                alpha = 0.9, bins = 30) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(lambda^c))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)+
        ggplot2::geom_vline(data= whole_df_MLE[i,], aes(xintercept = lac_MLE),
                          linetype = "dashed", size = 0.5,color = "red")

      p_mu <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(-0.01,0.5)+
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_abc),
                                fill = "royalblue",
                                alpha = 0.9, bins = 30) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(mu))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", size = 0.5)+
        ggplot2::geom_vline(data= whole_df_MLE[i,], aes(xintercept = mu_MLE),
                            linetype = "dashed", size = 0.5,color = "red")

      p_gam <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(-0.001,0.03)+
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = gam_abc),
                                fill = "royalblue",
                                alpha = 0.9, bins = 30) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(gamma))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed", size = 0.5)+
        ggplot2::geom_vline(data= whole_df_MLE[i,], aes(xintercept = gam_MLE),
                            linetype = "dashed", size = 0.5,color = "red")


      p_laa <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(-0.01,0.75)+
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_abc),
                                fill = "royalblue",
                                alpha = 0.9, bins = 30) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(lambda^a))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.5)+
        ggplot2::geom_vline(data= whole_df_MLE[i,], aes(xintercept = laa_MLE),
                            linetype = "dashed", size = 0.5,color = "red")

      mu_vs_lac <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,1)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = mu_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(mu)) +
        ggplot2::xlab(expression(lambda^c)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = mu),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = mu), colour = "grey50")

      gam_vs_lac <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,1)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = gam_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(gamma)) +
        ggplot2::xlab(expression(lambda^c)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = gam),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = gam), colour = "grey50")

      laa_vs_lac <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,1)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = laa_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(lambda^a)) +
        ggplot2::xlab(expression(lambda^c)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = laa),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")

      gam_vs_mu <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.5)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = mu_abc,y = gam_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(gamma)) +
        ggplot2::xlab(expression(mu)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = mu,y = gam),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = gam), colour = "grey50")

      laa_vs_mu <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.5)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = mu_abc,y = laa_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(lambda^a)) +
        ggplot2::xlab(expression(mu)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = mu,y = laa),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")

      laa_vs_gam <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.03)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = gam_abc,y = laa_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(lambda^a)) +
        ggplot2::xlab(expression(gamma)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = gam,y = laa),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")

      p_emp <- ggplot() + theme_void()

      tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/cowplots_hist/ss",n,"_param_",i,".tiff"),
           units="px", width=3000, height=2000,res = 400,compression="lzw")
      param_estimates <- cowplot::plot_grid(
        p_lac,p_emp,p_emp,p_emp,
        mu_vs_lac,p_mu,p_emp,p_emp,
        gam_vs_lac,gam_vs_mu,p_gam,p_emp,
        laa_vs_lac,laa_vs_mu,laa_vs_gam,p_laa,
        align = "hv", nrow = 4, ncol = 4
      )
      print(param_estimates)
      while (!is.null(dev.list()))  dev.off()
    }
  }
}


## plot use all the particles rather than median values
library(ggplot2)
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/obs_ss_long_with_pars.RData"))

ss = 0
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/delta_whole_df_ABC_ss_set",ss,".RData"))
whole_df_ABC$ss = 0
whole_df_ABC_0 = whole_df_ABC

ss = 1
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/delta_whole_df_ABC_ss_set",ss,".RData"))
whole_df_ABC$ss = 1
whole_df_ABC_1 = whole_df_ABC

ss = 2
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/delta_whole_df_ABC_ss_set",ss,".RData"))
whole_df_ABC$ss = 2
whole_df_ABC_2 = whole_df_ABC

ss = 6
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/delta_whole_df_ABC_ss_set",ss,".RData"))
whole_df_ABC$ss = 6
whole_df_ABC_6 = whole_df_ABC

ss = 7
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/delta_whole_df_ABC_ss_set",ss,".RData"))
whole_df_ABC$ss = 7
whole_df_ABC_7 = whole_df_ABC

ss = 20
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/delta_whole_df_ABC_ss_set",ss,".RData"))
whole_df_ABC$ss = 20
whole_df_ABC_20 = whole_df_ABC

whole_df_ABC <- rbind(whole_df_ABC_0,whole_df_ABC_1,
                          whole_df_ABC_2,whole_df_ABC_6,
                          whole_df_ABC_7) #whole_df_ABC_20

whole_df_ABC$dlac_abc <- whole_df_ABC$lac_abc - whole_df_ABC$lac
whole_df_ABC$dmu_abc <- whole_df_ABC$mu_abc - whole_df_ABC$mu
whole_df_ABC$dgam_abc <- whole_df_ABC$gam_abc - whole_df_ABC$gam
whole_df_ABC$dlaa_abc <- whole_df_ABC$laa_abc - whole_df_ABC$laa
whole_df_ABC$dnet_div_abc <- whole_df_ABC$net_div_ABC - whole_df_ABC$net_div
whole_df_ABC$total <- rep(rep(pars_ss$total, each = 500), 5)

color_values <-c("ABC" = "red3","MCMC" = "green2", "MLE" = "yellow2")
iqr = function(z, lower = 0.1, upper = 0.9) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

## 1. only plot delta-rate for all the particles based on ss
p_lac <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dlac_abc)) + ##,color = as.factor(gam)
  ggplot2::stat_summary(fun.data = iqr) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 1,color = "red3")+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~lambda[c]))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ ss)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/drate_all_particles_lac.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_lac)
while (!is.null(dev.list()))  dev.off()

p_mu <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dmu_abc)) +
  ggplot2::stat_summary(fun.data = iqr) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 1,color = "red3")+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~mu))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ ss)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/drate_all_particles_mu.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_mu)
while (!is.null(dev.list()))  dev.off()

p_gam <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dgam_abc)) +
  ggplot2::stat_summary(fun.data = iqr) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.03,0.03)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 1,color = "red3")+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~gamma))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ ss)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/drate_all_particles_gam.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_gam)
while (!is.null(dev.list()))  dev.off()

p_laa <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dlaa_abc)) +
  ggplot2::stat_summary(fun.data = iqr) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 1,color = "red3")+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~lambda[a]))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ ss)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/drate_all_particles_laa.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_laa)
while (!is.null(dev.list()))  dev.off()

p_net_div <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div_abc)) +
  ggplot2::stat_summary(fun.data = iqr) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ ss)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/drate_all_particles_net_div.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_net_div)
while (!is.null(dev.list()))  dev.off()


## linear fitting
ggplot(whole_df_ABC, aes(x = total, y = dlac_abc, color = as.factor(ss)) ) +
  ggplot2::ylim(-0.05,0.05)+
  geom_smooth(method = "lm", alpha = .15, aes(fill = as.factor(ss)))

## 2.plot delta-rate for all the particles based on ss and generating values
p_lac <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dlac_abc,color = as.factor(lac))) +
  ggplot2::stat_summary(fun.data = iqr) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("#FADC8D", "#F68221","red4"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~lambda[c]))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ ss)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/drate_all_particles_fit_lac.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_lac)
while (!is.null(dev.list()))  dev.off()

p_mu <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dmu_abc,color = as.factor(mu))) +
  ggplot2::stat_summary(fun.data = iqr) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("#FADC8D", "#F68221","red4"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~mu))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ ss)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/drate_all_particles_fit_mu.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_mu)
while (!is.null(dev.list()))  dev.off()

p_gam <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dgam_abc,color = as.factor(gam))) +
  ggplot2::stat_summary(fun.data = iqr) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.03,0.03)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("#FADC8D", "#F68221","red4"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~gamma))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ ss)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/drate_all_particles_fit_gam.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_gam)
while (!is.null(dev.list()))  dev.off()

p_laa <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dlaa_abc,color = as.factor(laa))) +
  ggplot2::stat_summary(fun.data = iqr) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("#FADC8D", "#F68221","red4"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~lambda[a]))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ ss)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/drate_all_particles_fit_laa.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_laa)
while (!is.null(dev.list()))  dev.off()

## net_div related to lac,mu,gam,laa
p_net_div <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div_abc,color = as.factor(lac))) +
  ggplot2::stat_summary(fun.data = iqr) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("#FADC8D", "#F68221","red4"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ ss)
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/drate_net_div_lac.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_net_div)
while (!is.null(dev.list()))  dev.off()

p_net_div <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div_abc,color = as.factor(mu))) +
  ggplot2::stat_summary(fun.data = iqr) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("#FADC8D", "#F68221","red4"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ ss)

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/drate_net_div_mu.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_net_div)

while (!is.null(dev.list()))  dev.off()
p_net_div <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div_abc,color = as.factor(gam))) +
  ggplot2::stat_summary(fun.data = iqr) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("#FADC8D", "#F68221","red4"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ ss)

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/drate_net_div_gam.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_net_div)
while (!is.null(dev.list()))  dev.off()

p_net_div <-ggplot2::ggplot(data = whole_df_ABC,mapping = aes(x = total,y = dnet_div_abc,color = as.factor(laa))) +
  ggplot2::stat_summary(fun.data = iqr) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.6,0.6)+
  ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("#FADC8D", "#F68221","red4"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_wrap(~ ss)

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/drate_net_div_laa.tiff"),
     units="px", width=5000, height=3000,res = 300,compression="lzw")
print(p_net_div)
while (!is.null(dev.list()))  dev.off()




