### cowplot relationships between rates ---per scenario(10 reps per value)

# folder_path <- "G:/results/project 2/tip_info/round3/est_allpars/DAISIE_ABC"
# files <- list.files(folder_path)
# param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
# i = 1
# file_to_load <- grep(paste0("DAISIE_ABC_param_set_", i,".RData"),
#                      files,
#                      value = TRUE,
#                      fixed = TRUE)
# load(file.path(folder_path, file_to_load))
library(ggplot2)

load("G:/results/project 2/tip_info/round3/test_epsilon/whole_df_ABC.RData")
load("G:/results/project 2/tip_info/round3/dec_kernel_old/MCMC_allpars/whole_df_MCMC_short.RData")
load("G:/results/project 2/tip_info/round3/dec_kernel_old/MLE_allpars/MLE_all.RData")

# whole_df_ABC$dlac_abc <-whole_df_ABC$lac_abc -  whole_df_ABC$lac
# whole_df_ABC$dmu_abc <-whole_df_ABC$mu_abc -  whole_df_ABC$mu
# whole_df_ABC$dgam_abc <-whole_df_ABC$gam_abc -  whole_df_ABC$gam
# whole_df_ABC$dlaa_abc <-whole_df_ABC$laa_abc -  whole_df_ABC$laa
# save(whole_df_ABC,file = "G:/results/project 2/tip_info/round3/test_epsilon/whole_df_ABC.RData")
#
# whole_df_MCMC$dlac_mcmc <-whole_df_MCMC$lac_mcmc -  whole_df_MCMC$lac
# whole_df_MCMC$dmu_mcmc <-whole_df_MCMC$mu_mcmc -  whole_df_MCMC$mu
# whole_df_MCMC$dgam_mcmc <-whole_df_MCMC$gam_mcmc -  whole_df_MCMC$gam
# whole_df_MCMC$dlaa_mcmc <-whole_df_MCMC$laa_mcmc -  whole_df_MCMC$laa
# save(whole_df_MCMC,file = "G:/results/project 2/tip_info/round3/dec_kernel_old/MCMC_allpars/whole_df_MCMC_short.RData")
#
# MLE_all$dlac_MLE <-MLE_all$lac_MLE -  MLE_all$lac
# MLE_all$dmu_MLE <-MLE_all$mu_MLE -  MLE_all$mu
# MLE_all$dgam_MLE <-MLE_all$gam_MLE -  MLE_all$gam
# MLE_all$dlaa_MLE <-MLE_all$laa_MLE -  MLE_all$laa
# save(MLE_all,file = "G:/results/project 2/tip_info/round3/dec_kernel_old/MLE_allpars/MLE_all.RData")

param_abc <- whole_df_ABC[1:10,]
param_mcmc <- whole_df_MCMC[1:10,]
param_mle <- MLE_all[1:10,]
p_legend <-ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  xlim(0,2)+
  # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
  #                       fill = "#009E73",colour = "#009E73",
  #                       alpha = 0.3, binwidth = 0.01) +
  ggplot2::geom_density(data = param_mcmc,
                        ggplot2::aes(x = lac_mcmc,fill = "MCMC"),colour = "red4",
                        alpha = 0.9) +
  ggplot2::geom_density(ggplot2::aes(x = lac_abc,
                                     fill = "ABC"),colour = "blue3",
                        alpha = 0.7) +
  ggplot2::geom_density(data = param_mle,
                        ggplot2::aes(x = lac_MLE,fill = "MLE"),colour = "green4",
                        alpha = 0.5) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 7)) +
  ggplot2::ylab("Density") +
  ggplot2::xlab(expression(lambda^c))+
  ggplot2::scale_fill_manual(name = "Method",
                             values = c( "MCMC" = "#F7903D", "ABC" = "#4D85BD", "MLE" = "#59A95A"),
                             labels = c("MCMC", "ABC", "MLE"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10)) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10)) +
  ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)


legend_all <- cowplot::get_legend(
  p_legend + theme(legend.box.margin = margin(0, 0, 0, 6))
)
color_values <-c("MCMC" = "#F7903D", "ABC" = "#4D85BD", "MLE" = "#59A95A")
#
# fillin_colors <- c("MCMC"="#F7903D","ABC"="#4D85BD","MLE"= "#59A95A")
# colors <- c("MCMC"="red4","ABC"="blue3","MLE"= "gree4")
for(i in 1:16){
  param_abc <- whole_df_ABC[((i*1000-999)):(i*1000),]
  param_mcmc <- whole_df_MCMC[((i*10000-9999)):(i*10000),]
  param_mle <- MLE_all[((i*10-9)):(i*10),]
  p_lac <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
    #                       fill = "#009E73",colour = "#009E73",
    #                       alpha = 0.3, binwidth = 0.01) +
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = lac_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = lac_abc,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_density(data = param_mle,
                          ggplot2::aes(x = lac_MLE,fill = "MLE"),colour = "green4",
                          alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda^c))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.8)




  p_mu <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_abc),
    #                       fill = "#009E73",colour = "#009E73",
    #                       alpha = 0.3, binwidth = 0.01) +
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = mu_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = mu_abc,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_density(data = param_mle,
                          ggplot2::aes(x = mu_MLE,fill = "MLE"),colour = "green4",
                          alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(mu))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", size = 0.8)


  p_gam <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,0.07)+
    # ggplot2::geom_histogram(mapping = ggplot2::aes(x = gam_abc),
    #                       fill = "#009E73",colour = "#009E73",
    #                       alpha = 0.3, binwidth = 0.0005) +
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = gam_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = gam_abc,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_density(data = param_mle,
                          ggplot2::aes(x = gam_MLE,fill = "MLE"),colour = "green4",
                          alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(gamma))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed", size = 0.8)



  p_laa <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    # ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_abc),
    #                       fill = "#009E73",colour = "#009E73",
    #                       alpha = 0.3, binwidth = 0.01) +
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = laa_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = laa_abc,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_density(data = param_mle,
                          ggplot2::aes(x = laa_MLE,fill = "MLE"),colour = "green4",
                          alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda^a))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.8)


  mu_vs_lac <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    ylim(0,2)+
    ggplot2::geom_point(data = param_mcmc,mapping = ggplot2::aes(x = lac_mcmc,y = mu_mcmc),
                        colour = "#F7903D",shape = 16,alpha = 0.3) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = mu_abc),
                        colour = "#4D85BD",shape = 16,alpha = 0.5) +
    ggplot2::geom_point(data = param_mle,mapping = ggplot2::aes(x = lac_MLE,y = mu_MLE),
                        colour = "#54B345",shape = 16,alpha = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(mu)) +
    ggplot2::xlab(expression(lambda^c)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = mu),
                        colour = "black",shape = 16,size = 2.5)+
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", colour = "grey50",size = 0.8) +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = mu), linetype = "dashed", colour = "grey50",size = 0.8)

  gam_vs_lac <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    ylim(0,0.07)+
    ggplot2::geom_point(data = param_mcmc,mapping = ggplot2::aes(x = lac_mcmc,y = gam_mcmc),
                        colour = "#F7903D",shape = 16,alpha = 0.3) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = gam_abc),
                        colour = "#4D85BD",shape = 16,alpha = 0.5) +
    ggplot2::geom_point(data = param_mle,mapping = ggplot2::aes(x = lac_MLE,y = gam_MLE),
                        colour = "#54B345",shape = 16,alpha = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(gamma)) +
    ggplot2::xlab(expression(lambda^c)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = gam),
                        colour = "black",shape = 16,size = 2.5)+
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", colour = "grey50",size = 0.8) +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = gam), linetype = "dashed", colour = "grey50",size = 0.8)

  laa_vs_lac <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    ylim(0,2)+
    ggplot2::geom_point(data = param_mcmc,mapping = ggplot2::aes(x = lac_mcmc,y = laa_mcmc),
                        colour = "#F7903D",shape = 16,alpha = 0.3) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = laa_abc),
                        colour = "#4D85BD",shape = 16,alpha = 0.5) +
    ggplot2::geom_point(data = param_mle,mapping = ggplot2::aes(x = lac_MLE,y = laa_MLE),
                        colour = "#54B345",shape = 16,alpha = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(lambda^a)) +
    ggplot2::xlab(expression(lambda^c)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = laa),
                        colour = "black",shape = 16,size = 2.5)+
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", colour = "grey50",size = 0.8) +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), linetype = "dashed", colour = "grey50",size = 0.8)


  gam_vs_mu <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    ylim(0,0.07)+
    ggplot2::geom_point(data = param_mcmc,mapping = ggplot2::aes(x = mu_mcmc,y = gam_mcmc),
                        colour = "#F7903D",shape = 16,alpha = 0.3) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = mu_abc,y = gam_abc),
                        colour = "#4D85BD",shape = 16,alpha = 0.5) +
    ggplot2::geom_point(data = param_mle,mapping = ggplot2::aes(x = mu_MLE,y = gam_MLE),
                        colour = "#54B345",shape = 16,alpha = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(gamma)) +
    ggplot2::xlab(expression(mu)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = mu,y = gam),
                        colour = "black",shape = 16,size = 2.5)+
    ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", colour = "grey50",size = 0.8) +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = gam), linetype = "dashed", colour = "grey50",size = 0.8)

  laa_vs_mu <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    ylim(0,2)+
    ggplot2::geom_point(data = param_mcmc,mapping = ggplot2::aes(x = mu_mcmc,y = laa_mcmc),
                        colour = "#F7903D",shape = 16,alpha = 0.3) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = mu_abc,y = laa_abc),
                        colour = "#4D85BD",shape = 16,alpha = 0.5) +
    ggplot2::geom_point(data = param_mle,mapping = ggplot2::aes(x = mu_MLE,y = laa_MLE),
                        colour = "#54B345",shape = 16,alpha = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(lambda^a)) +
    ggplot2::xlab(expression(mu)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = mu,y = laa),
                        colour = "black",shape = 16,size = 2.5)+
    ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", colour = "grey50",size = 0.8) +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), linetype = "dashed", colour = "grey50",size = 0.8)

  laa_vs_gam <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,0.07)+
    ylim(0,2)+
    ggplot2::geom_point(data = param_mcmc,mapping = ggplot2::aes(x = gam_mcmc,y = laa_mcmc),
                        colour = "#F7903D",shape = 16,alpha = 0.3) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = gam_abc,y = laa_abc),
                        colour = "#4D85BD",shape = 16,alpha = 0.5) +
    ggplot2::geom_point(data = param_mle,mapping = ggplot2::aes(x = gam_MLE,y = laa_MLE),
                        colour = "#54B345",shape = 16,alpha = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(lambda^a)) +
    ggplot2::xlab(expression(gamma)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = gam,y = laa),
                        colour = "black",shape = 16,size = 2.5)+
    ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed", colour = "grey50",size = 0.8) +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), linetype = "dashed", colour = "grey50",size = 0.8)

  dmu_vs_dlac <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.5,1.5)+
    ylim(-0.5,1.5)+
    ggplot2::geom_point(data = param_mcmc,mapping = ggplot2::aes(y = dlac_mcmc,x = dmu_mcmc),
                        colour = "#F7903D",shape = 16,alpha = 0.3) +
    ggplot2::geom_point(mapping = ggplot2::aes(y = dlac_abc,x = dmu_abc),
                        colour = "#4D85BD",shape = 16,alpha = 0.5) +
    ggplot2::geom_point(data = param_mle,mapping = ggplot2::aes(y = dlac_MLE,x = dmu_MLE),
                        colour = "#54B345",shape = 16,alpha = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::xlab(expression(Delta~mu)) +
    ggplot2::ylab(expression(Delta~lambda^c)) +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = 0), linetype = "dashed", colour = "grey50",size = 0.8) +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = 0), linetype = "dashed", colour = "grey50",size = 0.8)


  dgam_vs_dlac <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.01,0.05)+
    ylim(-0.5,1.5)+
    ggplot2::geom_point(data = param_mcmc,mapping = ggplot2::aes(y = dlac_mcmc,x = dgam_mcmc),
                        colour = "#F7903D",shape = 16,alpha = 0.3) +
    ggplot2::geom_point(mapping = ggplot2::aes(y = dlac_abc,x = dgam_abc),
                        colour = "#4D85BD",shape = 16,alpha = 0.5) +
    ggplot2::geom_point(data = param_mle,mapping = ggplot2::aes(y = dlac_MLE,x = dgam_MLE),
                        colour = "#54B345",shape = 16,alpha = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::xlab(expression(Delta~gamma)) +
    ggplot2::ylab(expression(Delta~lambda^c)) +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = 0),
                        linetype = "dashed", colour = "grey50",size = 0.8) +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = 0),
                        linetype = "dashed", colour = "grey50",size = 0.8)

  dlaa_vs_dlac <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.5,1.5)+
    ylim(-0.5,1.5)+
    ggplot2::geom_point(data = param_mcmc,mapping = ggplot2::aes(y = dlac_mcmc,x = dlaa_mcmc),
                        colour = "#F7903D",shape = 16,alpha = 0.3) +
    ggplot2::geom_point(mapping = ggplot2::aes(y = dlac_abc,x = dlaa_abc),
                        colour = "#4D85BD",shape = 16,alpha = 0.5) +
    ggplot2::geom_point(data = param_mle,mapping = ggplot2::aes(y = dlac_MLE,x = dlaa_MLE),
                        colour = "#54B345",shape = 16,alpha = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::xlab(expression(Delta~lambda^a)) +
    ggplot2::ylab(expression(Delta~lambda^c)) +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = 0),
                        linetype = "dashed", colour = "grey50",size = 0.8) +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = 0),
                        linetype = "dashed", colour = "grey50",size = 0.8)

  dgam_vs_dmu <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.01,0.05)+
    ylim(-0.5,1.5)+
    ggplot2::geom_point(data = param_mcmc,mapping = ggplot2::aes(y = dmu_mcmc,x = dgam_mcmc),
                        colour = "#F7903D",shape = 16,alpha = 0.3) +
    ggplot2::geom_point(mapping = ggplot2::aes(y = dmu_abc,x = dgam_abc),
                        colour = "#4D85BD",shape = 16,alpha = 0.5) +
    ggplot2::geom_point(data = param_mle,mapping = ggplot2::aes(y = dmu_MLE,x = dgam_MLE),
                        colour = "#54B345",shape = 16,alpha = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::xlab(expression(Delta~gamma)) +
    ggplot2::ylab(expression(Delta~mu)) +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = 0),
                        linetype = "dashed", colour = "grey50",size = 0.8) +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = 0),
                        linetype = "dashed", colour = "grey50",size = 0.8)

  dlaa_vs_dmu <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.5,1.5)+
    ylim(-0.5,1.5)+
    ggplot2::geom_point(data = param_mcmc,mapping = ggplot2::aes(y = dmu_mcmc,x = dlaa_mcmc),
                        colour = "#F7903D",shape = 16,alpha = 0.3) +
    ggplot2::geom_point(mapping = ggplot2::aes(y = dmu_abc,x = dlaa_abc),
                        colour = "#4D85BD",shape = 16,alpha = 0.5) +
    ggplot2::geom_point(data = param_mle,mapping = ggplot2::aes(y = dmu_MLE,x = dlaa_MLE),
                        colour = "#54B345",shape = 16,alpha = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::xlab(expression(Delta~lambda^a)) +
    ggplot2::ylab(expression(Delta~mu)) +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = 0),
                        linetype = "dashed", colour = "grey50",size = 0.8) +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = 0),
                        linetype = "dashed", colour = "grey50",size = 0.8)

  dlaa_vs_dgam <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.5,1.5)+
    ylim(-0.01,0.05)+
    ggplot2::geom_point(data = param_mcmc,mapping = ggplot2::aes(y = dgam_mcmc,x = dlaa_mcmc),
                        colour = "#F7903D",shape = 16,alpha = 0.3) +
    ggplot2::geom_point(mapping = ggplot2::aes(y = dgam_abc,x = dlaa_abc),
                        colour = "#4D85BD",shape = 16,alpha = 0.5) +
    ggplot2::geom_point(data = param_mle,mapping = ggplot2::aes(y = dgam_MLE,x = dlaa_MLE),
                        colour = "#54B345",shape = 16,alpha = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::xlab(expression(Delta~lambda^a)) +
    ggplot2::ylab(expression(Delta~gamma)) +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = 0),
                        linetype = "dashed", colour = "grey50",size = 0.8) +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = 0),
                        linetype = "dashed", colour = "grey50",size = 0.8)

  # p_emp <- ggplot() + theme_void()

  tiff(paste0("G:/results/project 2/tip_info/round3/test_epsilon/cowplot_ABC_MCMC_MLE_with_drates/param_",i,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_lac,dmu_vs_dlac,dgam_vs_dlac,dlaa_vs_dlac,
    mu_vs_lac,p_mu,dgam_vs_dmu,dlaa_vs_dmu,
    gam_vs_lac,gam_vs_mu,p_gam,dlaa_vs_dgam,
    laa_vs_lac,laa_vs_mu,laa_vs_gam,p_laa,
    align = "hv", nrow = 4, ncol = 4
  )
  param_est_final <- cowplot::plot_grid(param_estimates,legend_all,rel_widths = c(3, .4))
  print(param_est_final)
  while (!is.null(dev.list()))  dev.off()
}


# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
#
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#
#   numPlots = length(plots)
#
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#
#   if (numPlots==1) {
#     print(plots[[1]])
#
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }
#
#







