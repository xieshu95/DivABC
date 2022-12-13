#####
# compare net-diversification
library(ggplot2)

load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_whole_df_ABC_ss_set0.RData"))
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_whole_df_MCMC_1001.RData")
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_MLE_secsse_ABC.RData")

# save(MLE_all,file = "G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_MLE_secsse_ABC.RData")


## get legend first
param_abc <- whole_df_ABC[1:10,]
param_mcmc <- whole_df_MCMC[1:10,]
param_mle <- MLE_all[1:10,]
p_legend <-ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  xlim(0,2)+
  ggplot2::geom_density(data = param_mcmc,
                        ggplot2::aes(x = lam1_mcmc,fill = "MCMC"),colour = "red4",
                        alpha = 0.9) +
  ggplot2::geom_density(ggplot2::aes(x = lam1_abc,
                                     fill = "ABC"),colour = "blue3",
                        alpha = 0.7) +
  ggplot2::geom_density(data = param_mle,
                        ggplot2::aes(x = lam1_MLE,fill = "MLE"),colour = "green4",
                        alpha = 0.5) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 7)) +
  ggplot2::ylab("Density") +
  ggplot2::xlab(expression(lambda[1]))+
  ggplot2::scale_fill_manual(name = "Method",
                             values = c( "MCMC" = "#F7903D", "ABC" = "#4D85BD", "MLE" = "#59A95A"),
                             labels = c("MCMC", "ABC", "MLE"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10)) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10)) +
  ggplot2::geom_vline(data= param_abc, aes(xintercept = lam1), linetype = "dashed", size = 0.5)


legend_all <- cowplot::get_legend(
  p_legend + theme(legend.box.margin = margin(0, 0, 0, 6))
)
color_values <-c("MCMC" = "#F7903D", "ABC" = "#4D85BD", "MLE" = "#59A95A")

#####
# histogram
for(i in 1:27){
  param_abc <- whole_df_ABC[((i*500-499)):(i*500),]
  param_mcmc <- whole_df_MCMC[((i*1001-499)):(i*1001),]
  param_mle <- MLE_all[i,]

  p_net_div1 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.032,0.8)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = net_div_MCMC1,fill = "MCMC"),
                            alpha = 0.9,binwidth = 0.032) +
    ggplot2::geom_histogram(ggplot2::aes(x = net_div_ABC1,fill = "ABC"),
                            alpha = 0.9,binwidth = 0.032) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = net_div_MLE1),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab("Net diversification state 1")+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = net_div1), linetype = "dashed", size = 0.5)
  # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
  #                     linetype = "dashed", size = 0.5,color = "red")

  p_net_div2 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.032,0.8)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = net_div_MCMC2,fill = "MCMC"),
                            alpha = 0.9,binwidth = 0.032) +
    ggplot2::geom_histogram(ggplot2::aes(x = net_div_ABC2,fill = "ABC"),
                            alpha = 0.9,binwidth = 0.032) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = net_div_MLE2),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab("Net diversification state 2")+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = net_div2), linetype = "dashed", size = 0.5)


  p_ext_frac1 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    # xlim(0,0.8)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = ext_frac_MCMC1,fill = "MCMC"),
                            alpha = 0.9) +
    ggplot2::geom_histogram(ggplot2::aes(x = ext_frac_ABC1,fill = "ABC"),
                            alpha = 0.9) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = ext_frac_MLE1),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab("Extinction fraction state 1")+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = ext_frac1), linetype = "dashed", size = 0.5)


  p_ext_frac2 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    # xlim(0,0.8)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = ext_frac_MCMC2,fill = "MCMC"),
                            alpha = 0.9) +
    ggplot2::geom_histogram(ggplot2::aes(x = ext_frac_ABC2,fill = "ABC"),
                            alpha = 0.9) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = ext_frac_MLE2),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab("Extinction fraction state 1")+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = ext_frac2), linetype = "dashed", size = 0.5)


  p_emp <- ggplot() + theme_void()

  tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/net_div/net_div_hist_set_",i,".tiff"),
       units="px", width=2200, height=2000,res = 300,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_net_div1,p_net_div2,p_ext_frac1,p_ext_frac2,
    align = "hv", nrow = 2, ncol = 2
  )
  param_est_final <- cowplot::plot_grid(param_estimates,legend_all,rel_widths = c(3, .4))
  print(param_est_final)
  while (!is.null(dev.list()))  dev.off()
  # }
}


# for(i in 1:70){
#   param_abc <- whole_df_ABC[((i*500-499)):(i*500),]
#   param_mcmc <- whole_df_MCMC[((i*5001-5000)):(i*5001),]
#   param_mle <- MLE_all[i,]
#
#   p_net_div1 <-ggplot2::ggplot(data = param_abc) +
#     ggplot2::theme_bw() +
#     xlim(0,0.8)+
#     ggplot2::geom_density(data = param_mcmc,
#                           ggplot2::aes(x = net_div_mcmc1,fill = "MCMC"),colour = "red4",
#                           alpha = 0.9) +
#     ggplot2::geom_density(ggplot2::aes(x = net_div_ABC1,
#                                        fill = "ABC"),colour = "blue3",
#                           alpha = 0.7) +
#     ggplot2::geom_vline(data= param_mle,
#                         aes(xintercept = net_div_MLE1),colour = "green4",
#                         linetype = "solid", size = 1)+
#     ggplot2::theme_classic() +
#     ggplot2::theme(title = ggplot2::element_text(size = 12),
#                    text = ggplot2::element_text(size = 12)) +
#     ggplot2::ylab("Density") +
#     ggplot2::xlab("Net diversification state 1")+
#     ggplot2::scale_fill_manual(name = "Method",
#                                values = color_values,
#                                labels = c("MCMC", "ABC", "MLE"))+
#     ggplot2::theme(legend.position = "none") +
#     ggplot2::geom_vline(data= param_abc, aes(xintercept = net_div1), linetype = "dashed", size = 0.5)
#   # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
#   #                     linetype = "dashed", size = 0.5,color = "red")
#
#   p_net_div2 <-ggplot2::ggplot(data = param_abc) +
#     ggplot2::theme_bw() +
#     xlim(0,0.8)+
#     ggplot2::geom_density(data = param_mcmc,
#                           ggplot2::aes(x = net_div_mcmc2,fill = "MCMC"),colour = "red4",
#                           alpha = 0.9) +
#     ggplot2::geom_density(ggplot2::aes(x = net_div_ABC2,
#                                        fill = "ABC"),colour = "blue3",
#                           alpha = 0.7) +
#     ggplot2::geom_vline(data= param_mle,
#                         aes(xintercept = net_div_MLE2),colour = "green4",
#                         linetype = "solid", size = 1)+
#     ggplot2::theme_classic() +
#     ggplot2::theme(title = ggplot2::element_text(size = 12),
#                    text = ggplot2::element_text(size = 12)) +
#     ggplot2::ylab("Density") +
#     ggplot2::xlab("Net diversification state 2")+
#     ggplot2::scale_fill_manual(name = "Method",
#                                values = color_values,
#                                labels = c("MCMC", "ABC", "MLE"))+
#     ggplot2::theme(legend.position = "none") +
#     ggplot2::geom_vline(data= param_abc, aes(xintercept = net_div2), linetype = "dashed", size = 0.5)
#
#   p_emp <- ggplot() + theme_void()
#
#   tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse/net_div/net_div_set_",i,".tiff"),
#        units="px", width=3000, height=1500,res = 300,compression="lzw")
#   param_estimates <- cowplot::plot_grid(
#     p_net_div1,p_net_div2,
#     align = "hv", nrow = 1, ncol = 2
#   )
#   param_est_final <- cowplot::plot_grid(param_estimates,legend_all,rel_widths = c(3, .4))
#   print(param_est_final)
#   while (!is.null(dev.list()))  dev.off()
#   # }
# }




######
# dlam dmu dq

for(i in 1:5){
  param_abc <- whole_df_ABC[((i*500-499)):(i*500),]
  param_mcmc <- whole_df_MCMC[((i*5001-499)):(i*5001),]
  param_mle <- MLE_all[i,]

  # if(!is.na(param_abc[,7])){

  p_dlam <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    # xlim(0,1)+
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = dlam_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = dlam_ABC,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = dlam_MLE),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = dlam), linetype = "dashed", size = 0.5)
  # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
  #                     linetype = "dashed", size = 0.5,color = "red")

  p_dmu <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    # xlim(0,1)+
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = dmu_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = dmu_ABC,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = dmu_MLE),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(mu))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = dmu), linetype = "dashed", size = 0.5)


  p_dq <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    # xlim(0,1)+
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = dq_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = dq_ABC,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = dq_MLE),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(q[12]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = dq), linetype = "dashed", size = 0.5)

  p_net_div1 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    # xlim(0,1)+
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = net_div_mcmc1,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = net_div_ABC1,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = net_div_MLE1),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda[1]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = net_div1), linetype = "dashed", size = 0.5)
  # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
  #                     linetype = "dashed", size = 0.5,color = "red")

  p_net_div2 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    # xlim(0,1)+
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = net_div_mcmc2,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = net_div_ABC2,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = net_div_MLE2),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda[1]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = net_div2), linetype = "dashed", size = 0.5)

  p_emp <- ggplot() + theme_void()

  tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/cowplot_AMM/MLE_set_",i,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
    align = "hv", nrow = 2, ncol = 3
  )
  param_est_final <- cowplot::plot_grid(param_estimates,legend_all,rel_widths = c(3, .4))
  print(param_est_final)
  while (!is.null(dev.list()))  dev.off()
  # }
}
