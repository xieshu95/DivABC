
## prepare data go to file formate_results.R
# load("G:/results/project 2/tip_info/round4/secsse_long_2/whole_df_ABC_long.RData")
library(ggplot2)

load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_whole_df_ABC_ss_set0.RData"))
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_whole_df_MCMC_1001.RData")
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_MLE_secsse_ABC.RData")


#####
# 1. cowplot with only ABC (density)
for(n in c(0,20,30)){ #c(0,1,2,5,8)
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/whole_df_ABC_ss_set",n,".RData"))
  for(i in 1:27){ #7/70
    # param_abc <- whole_df_ABC[((i*5000-4999)):(i*5000),]  # for single scenario
    param_abc <- whole_df_ABC[((i*500-499)):(i*500),] #for single set
    if(!is.na(param_abc[1,7])){
      p_lam1 <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,1)+
        # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
        #                       fill = "#009E73",colour = "#009E73",
        #                       alpha = 0.3, binwidth = 0.01) +
        ggplot2::geom_density(ggplot2::aes(x = lam1_abc),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.3) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(lambda[1]))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = lam1), linetype = "dashed", size = 0.5)
      # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
      #                     linetype = "dashed", size = 0.5,color = "red")

      p_lam2 <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,1)+
        # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
        #                       fill = "#009E73",colour = "#009E73",
        #                       alpha = 0.3, binwidth = 0.01) +
        ggplot2::geom_density(ggplot2::aes(x = lam2_abc),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.3) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(lambda[2]))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = lam2), linetype = "dashed", size = 0.5)
      # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
      #                     linetype = "dashed", size = 0.5,color = "red")

      p_mu1 <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.2)+
        # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_abc),
        #                       fill = "#009E73",colour = "#009E73",
        #                       alpha = 0.3, binwidth = 0.01) +
        ggplot2::geom_density(mapping = ggplot2::aes(x = mu1_abc),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.3) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(mu[1]))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = mu1), linetype = "dashed", size = 0.5)

      p_mu2 <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.2)+
        # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_abc),
        #                       fill = "#009E73",colour = "#009E73",
        #                       alpha = 0.3, binwidth = 0.01) +
        ggplot2::geom_density(mapping = ggplot2::aes(x = mu2_abc),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.3) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(mu[2]))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = mu2), linetype = "dashed", size = 0.5)

      p_q12 <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.5)+
        # ggplot2::geom_histogram(mapping = ggplot2::aes(x = gam_abc),
        #                       fill = "#009E73",colour = "#009E73",
        #                       alpha = 0.3, binwidth = 0.0005) +
        ggplot2::geom_density(mapping = ggplot2::aes(x = q12_abc),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.3) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(q[12]))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = q12), linetype = "dashed", size = 0.5)


      p_q21 <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.5)+
        # ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_abc),
        #                       fill = "#009E73",colour = "#009E73",
        #                       alpha = 0.3, binwidth = 0.01) +
        ggplot2::geom_density(mapping = ggplot2::aes(x = q21_abc),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.3) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(q[21]))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = q21), linetype = "dashed", size = 0.5)

      # lam1_vs_lam2 <- ggplot2::ggplot(data = param_abc) +
      #   ggplot2::theme_bw() +
      #   # xlim(0,2)+
      #   ggplot2::geom_point(mapping = ggplot2::aes(x = lam1_abc,y = lam2_abc),
      #                       colour = "royalblue",shape = 16,alpha = 0.2) +
      #   ggplot2::theme_classic() +
      #   ggplot2::theme(title = ggplot2::element_text(size = 12),
      #                  text = ggplot2::element_text(size = 12)) +
      #   ggplot2::ylab(expression(lambda[2])) +
      #   ggplot2::xlab(expression(lambda[1])) +
      #   ggplot2::geom_point(mapping = ggplot2::aes(x = lam1,y = lam2),
      #                       colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = mu), colour = "grey50")

      p_emp <- ggplot() + theme_void()

      tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/cowplot/ss_",n,"_param_",i,".tiff"),
           units="px", width=3000, height=2000,res = 300,compression="lzw")
      param_estimates <- cowplot::plot_grid(
        p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
        align = "hv", nrow = 2, ncol = 3
      )
      print(param_estimates)
      while (!is.null(dev.list()))  dev.off()
    }
  }
}

#####
## 2. cowplot with only ABC (histogram)
library(ggplot2)
load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/whole_df_ABC_ss_set0.RData"))
for(i in 1:70){
  param_abc <- whole_df_ABC[((i*500-499)):(i*500),]
  if(!is.na(param_abc[1,7])){
    p_lam1 <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,1)+
      ggplot2::geom_histogram(mapping = ggplot2::aes(x = lam1_abc),
                              fill = "royalblue",
                              # colour = "blue3",
                            alpha = 1.0, binwidth = 0.04) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(lambda[1]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = lam1), linetype = "dashed", size = 0.5)
    # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
    #                     linetype = "dashed", size = 0.5,color = "red")

    p_lam2 <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,1)+
      ggplot2::geom_histogram(mapping = ggplot2::aes(x = lam2_abc),
                              fill = "royalblue",
                            alpha = 1.0, binwidth = 0.04) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(lambda[2]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = lam2), linetype = "dashed", size = 0.5)
    # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
    #                     linetype = "dashed", size = 0.5,color = "red")

    p_mu1 <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,0.2)+
      ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu1_abc),
                            fill = "royalblue",
                            alpha = 1.0, binwidth = 0.008) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(mu[1]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = mu1), linetype = "dashed", size = 0.5)

    p_mu2 <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,0.2)+
      ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu2_abc),
                            fill = "royalblue",
                            alpha = 1.0, binwidth = 0.008) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(mu[2]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = mu2), linetype = "dashed", size = 0.5)

    p_q12 <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,1)+
      ggplot2::geom_histogram(mapping = ggplot2::aes(x = q12_abc),
                            fill = "royalblue",
                            alpha = 1.0, binwidth = 0.04) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(q[12]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = q12), linetype = "dashed", size = 0.5)


    p_q21 <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,1)+
      ggplot2::geom_histogram(mapping = ggplot2::aes(x = q21_abc),
                            fill = "royalblue",
                            alpha = 1.0, binwidth = 0.04) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(q[21]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = q21), linetype = "dashed", size = 0.5)

    p_emp <- ggplot() + theme_void()

    tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/cowplot_hist/param_",i,".tiff"),
         units="px", width=3000, height=2000,res = 300,compression="lzw")
    param_estimates <- cowplot::plot_grid(
      p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
      align = "hv", nrow = 2, ncol = 3
    )
    print(param_estimates)
    while (!is.null(dev.list()))  dev.off()
  }
}

#####
## 3. cowplot with ABC MCMC MLE
library(ggplot2)

load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_whole_df_ABC_ss_set0.RData"))
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_whole_df_MCMC_1001.RData")
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_MLE_secsse_ABC.RData")


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


for(i in 1:27){
  param_abc <- whole_df_ABC[((i*500-499)):(i*500),]
  param_mcmc <- whole_df_MCMC[((i*5001-499)):(i*5001),]
  param_mle <- MLE_all[i,]

  # if(!is.na(param_abc[,7])){
  p_lam1 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,1)+
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = lam1_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = lam1_abc,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = lam1_MLE),colour = "green4",
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
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lam1), linetype = "dashed", size = 0.5)
  # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
  #                     linetype = "dashed", size = 0.5,color = "red")

  p_lam2 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,1)+
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = lam2_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = lam2_abc,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = lam2_MLE),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda[2]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lam2), linetype = "dashed", size = 0.5)
  # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
  #                     linetype = "dashed", size = 0.5,color = "red")

  p_mu1 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,0.2)+
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = mu1_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = mu1_abc,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = mu1_MLE),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(mu[1]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = mu1), linetype = "dashed", size = 0.5)

  p_mu2 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,0.2)+
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = mu2_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = mu2_abc,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = mu2_MLE),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(mu[2]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = mu2), linetype = "dashed", size = 0.5)

  p_q12 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,0.5)+
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = q12_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = q12_abc,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = q12_MLE),colour = "green4",
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
    ggplot2::geom_vline(data= param_abc, aes(xintercept = q12), linetype = "dashed", size = 0.5)


  p_q21 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,0.5)+
    ggplot2::geom_density(data = param_mcmc,
                          ggplot2::aes(x = q21_mcmc,fill = "MCMC"),colour = "red4",
                          alpha = 0.9) +
    ggplot2::geom_density(ggplot2::aes(x = q21_abc,
                                       fill = "ABC"),colour = "blue3",
                          alpha = 0.7) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = q21_MLE),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(q[21]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = q21), linetype = "dashed", size = 0.5)


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


#####
# AMM histogram
library(ggplot2)

load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_whole_df_ABC_ss_set30.RData"))
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_whole_df_MCMC_1001.RData")
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_MLE_secsse_ABC.RData")



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


for(i in 1:27){
  param_abc <- whole_df_ABC[((i*500-499)):(i*500),]
  param_mcmc <- whole_df_MCMC[((i*1001-499)):(i*1001),]
  param_mle <- MLE_all[i,]

  # if(!is.na(param_abc[,7])){
  p_lam1 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.032,0.8)+ #1
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = lam1_mcmc,fill = "MCMC"),
                            alpha = 0.7,binwidth = 0.032) + #0.03
    ggplot2::geom_histogram(ggplot2::aes(x = lam1_abc,
                                         fill = "ABC"),
                            alpha = 0.7,binwidth = 0.032) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = lam1_MLE),color = "#59A95A",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab(expression(lambda[1]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lam1), linetype = "dashed", size = 0.5)
  # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
  #                     linetype = "dashed", size = 0.5,color = "red")

  p_lam2 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.032,0.8)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = lam2_mcmc,fill = "MCMC"),
                            alpha = 0.7,binwidth = 0.032) +
    ggplot2::geom_histogram(ggplot2::aes(x = lam2_abc,
                                         fill = "ABC"),
                            alpha = 0.7,binwidth = 0.032) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = lam2_MLE),color = "#59A95A",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab(expression(lambda[2]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lam2), linetype = "dashed", size = 0.5)
  # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
  #                     linetype = "dashed", size = 0.5,color = "red")

  p_mu1 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.008,0.2)+ #0.2
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = mu1_mcmc,fill = "MCMC"),
                            alpha = 0.7,binwidth = 0.008) +
    ggplot2::geom_histogram(ggplot2::aes(x = mu1_abc,
                                         fill = "ABC"),
                            alpha = 0.7,binwidth = 0.008) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = mu1_MLE),color = "#59A95A",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab(expression(mu[1]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = mu1), linetype = "dashed", size = 0.5)

  p_mu2 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.008,0.2)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = mu2_mcmc,fill = "MCMC"),
                            alpha = 0.7,binwidth = 0.008) +
    ggplot2::geom_histogram(ggplot2::aes(x = mu2_abc,
                                         fill = "ABC"),
                            alpha = 0.7,binwidth = 0.008) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = mu2_MLE),color = "#59A95A",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab(expression(mu[2]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = mu2), linetype = "dashed", size = 0.5)

  p_q12 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.02,0.5)+ #1
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = q12_mcmc,fill = "MCMC"),
                            alpha = 0.7,binwidth = 0.02) +
    ggplot2::geom_histogram(ggplot2::aes(x = q12_abc,
                                         fill = "ABC"),
                            alpha = 0.7,binwidth = 0.02) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = q12_MLE),color = "#59A95A",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab(expression(q[12]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = q12), linetype = "dashed", size = 0.5)


  p_q21 <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(-0.02,0.5)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = q21_mcmc,fill = "MCMC"),
                            alpha = 0.7,binwidth = 0.02) +
    ggplot2::geom_histogram(ggplot2::aes(x = q21_abc,
                                         fill = "ABC"),
                            alpha = 0.7,binwidth = 0.02) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = q21_MLE),color = "#59A95A",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab(expression(q[21]))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = q21), linetype = "dashed", size = 0.5)


  p_emp <- ggplot() + theme_void()

  tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/cowplot_AMM/ss_set30/AMM_hist_set_",i,".tiff"),
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


