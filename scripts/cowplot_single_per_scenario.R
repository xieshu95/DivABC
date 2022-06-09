### SINGLE_PAR cowplot relationships between rates ---per scenario(10 reps per value)

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

load("G:/results/project 2/tip_info/round3/dec_kernel/single_par_DI/whole_df_ABC.RData")
load("G:/results/project 2/tip_info/round3/dec_kernel/MCMC_single_par/whole_df_MCMC.RData")
load("G:/results/project 2/tip_info/round3/dec_kernel/MLE_single/whole_df_MLE.RData")

param_abc <- whole_df_ABC[1:10,]
param_mcmc <- whole_df_MCMC[1:10,]
param_mle <- whole_df_MLE[1:10,]
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
for(i in 1:4){
  param_abc <- whole_df_ABC[((i*2000-1999)):(i*2000),]
  param_mcmc <- whole_df_MCMC[((i*20000-19999)):(i*20000),]
  param_mle <- whole_df_MLE[((i*10-9)):(i*10),]
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
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)

  tiff(paste0("G:/results/project 2/tip_info/round3/dec_kernel/single_par_DI/cowplots/per_scenario/param_",i,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  print(p_lac)
  while (!is.null(dev.list()))  dev.off()
}


for(i in 5:8){
  param_abc <- whole_df_ABC[((i*2000-1999)):(i*2000),]
  param_mcmc <- whole_df_MCMC[((i*20000-19999)):(i*20000),]
  param_mle <- whole_df_MLE[((i*10-9)):(i*10),]

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
    ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", size = 0.5)



  tiff(paste0("G:/results/project 2/tip_info/round3/dec_kernel/single_par_DI/cowplots/per_scenario/param_",i,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  print(p_mu)
  while (!is.null(dev.list()))  dev.off()
}

for(i in 9:12){
  param_abc <- whole_df_ABC[((i*2000-1999)):(i*2000),]
  param_mcmc <- whole_df_MCMC[((i*20000-19999)):(i*20000),]
  param_mle <- whole_df_MLE[((i*10-9)):(i*10),]

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
    ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed", size = 0.5)

  tiff(paste0("G:/results/project 2/tip_info/round3/dec_kernel/single_par_DI/cowplots/per_scenario/param_",i,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  print(p_gam)
  while (!is.null(dev.list()))  dev.off()
}

for(i in 13:16){
  param_abc <- whole_df_ABC[((i*2000-1999)):(i*2000),]
  param_mcmc <- whole_df_MCMC[((i*20000-19999)):(i*20000),]
  param_mle <- whole_df_MLE[((i*10-9)):(i*10),]

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
    ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.5)

  tiff(paste0("G:/results/project 2/tip_info/round3/dec_kernel/single_par_DI/cowplots/per_scenario/param_",i,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  print(p_laa)
  while (!is.null(dev.list()))  dev.off()
}

#### all_in_one plot (after getting legend)
plot_list <- list()
for(i in 1:4){
  param_abc <- whole_df_ABC[((i*2000-1999)):(i*2000),]
  param_mcmc <- whole_df_MCMC[((i*20000-19999)):(i*20000),]
  param_mle <- whole_df_MLE[((i*10-9)):(i*10),]
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
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)

  plot_list[[i]] <- p_lac
}


for(i in 5:8){
  param_abc <- whole_df_ABC[((i*2000-1999)):(i*2000),]
  param_mcmc <- whole_df_MCMC[((i*20000-19999)):(i*20000),]
  param_mle <- whole_df_MLE[((i*10-9)):(i*10),]

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
    ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", size = 0.5)

  plot_list[[i]] <- p_mu
}

for(i in 9:12){
  param_abc <- whole_df_ABC[((i*2000-1999)):(i*2000),]
  param_mcmc <- whole_df_MCMC[((i*20000-19999)):(i*20000),]
  param_mle <- whole_df_MLE[((i*10-9)):(i*10),]

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
    ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed", size = 0.5)

  plot_list[[i]] <- p_gam
}

for(i in 13:16){
  param_abc <- whole_df_ABC[((i*2000-1999)):(i*2000),]
  param_mcmc <- whole_df_MCMC[((i*20000-19999)):(i*20000),]
  param_mle <- whole_df_MLE[((i*10-9)):(i*10),]

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
    ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.5)

  plot_list[[i]] <- p_laa
}

tiff(paste0("G:/results/project 2/tip_info/round3/dec_kernel/single_par_DI/cowplots/per_scenario/boxplot_all.tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],
  plot_list[[5]],plot_list[[6]],plot_list[[7]],plot_list[[8]],
  plot_list[[9]],plot_list[[10]],plot_list[[11]],plot_list[[12]],
  plot_list[[13]],plot_list[[14]],plot_list[[15]],plot_list[[16]],
  align = "hv", nrow = 4, ncol = 4
)
param_est_final <- cowplot::plot_grid(param_estimates,legend_all,rel_widths = c(3, .4))
print(param_est_final)
while (!is.null(dev.list()))  dev.off()
