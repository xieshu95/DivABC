## combine all the parameter sets(350)
library(ggplot2)
for(num_ss in c(0)){
  for(i in c(1)){
    load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round6/secsse/ss0_nltts_D/AMM_per_set_drate_test_ss",num_ss,".RData"))
    color_values <-c("ABC" = "#E90F44" ,"MCMC" = "#63ADEE", "MLE" = "#FFC839")
    # AMM <- AMM_all_df[(i*50-49):(i*50),]
    AMM <- AMM_all_df
    p_lam1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.6,1.2)+
      ggplot2::xlim(0,900)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dlam1_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dlam1_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dlam1_abc),color = "ABC"),shape = 18,alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dlam1_MLE),color = "MLE"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dlam1_mcmc),color = "MCMC"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dlam1_abc),color = "ABC"),se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Total species richness") +
      ggplot2::ylab(expression(lambda[0]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_lam2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.6,1.2)+
      ggplot2::xlim(0,900)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dlam2_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dlam2_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dlam2_abc),color = "ABC"),shape = 18,alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dlam2_MLE),color = "MLE"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dlam2_mcmc),color = "MCMC"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dlam2_abc),color = "ABC"),se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Total species richness") +
      ggplot2::ylab(expression(lambda[1]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_mu1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.5)+
      ggplot2::xlim(0,900)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dmu1_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dmu1_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dmu1_abc),color = "ABC"),shape = 18,alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dmu1_MLE),color = "MLE"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dmu1_mcmc),color = "MCMC"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dmu1_abc),color = "ABC"),se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Total species richness") +
      ggplot2::ylab(expression(mu[0]))+
      ggplot2::geom_hline(yintercept =0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_mu2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.5)+
      ggplot2::xlim(0,900)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dmu2_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dmu2_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dmu2_abc),color = "ABC"),shape = 18,alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dmu1_MLE),color = "MLE"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dmu1_mcmc),color = "MCMC"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dmu1_abc),color = "ABC"),se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Total species richness") +
      ggplot2::ylab(expression(mu[1]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_q12 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.05)+
      ggplot2::xlim(0,900)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dq12_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dq12_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dq12_abc),color = "ABC"),shape = 18,alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dq12_MLE),color = "MLE"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dq12_mcmc),color = "MCMC"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dq12_abc),color = "ABC"),se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Total species richness") +
      ggplot2::ylab(expression(q["01"]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_q21 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.05)+
      ggplot2::xlim(0,900)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dq21_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dq21_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dq21_abc),color = "ABC"),shape = 18,alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dq21_MLE),color = "MLE"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dq21_mcmc),color = "MCMC"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dq21_abc),color = "ABC"),se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Total species richness") +
      ggplot2::ylab(expression(q[10]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_div1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-1.5,1)+
      ggplot2::xlim(0,900)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dnet_div_MLE1),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dnet_div_mcmc1),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dnet_div_abc1),color = "ABC"),shape = 18, alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dnet_div_MLE1),color = "MLE"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dnet_div_mcmc1),color = "MCMC"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dnet_div_abc1),color = "ABC"),se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 11),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::ylab(expression("Net Diversification 0")) +
      ggplot2::xlab("Total species richness")+
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)


    p_div2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-1.5,1)+
      ggplot2::xlim(0,900)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dnet_div_MLE2),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dnet_div_mcmc2),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (dnet_div_abc2),color = "ABC"),shape = 18,alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dnet_div_MLE2),color = "MLE"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dnet_div_mcmc2),color = "MCMC"),se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = tree_size,y = (dnet_div_abc2),color = "ABC"),se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 11),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::ylab(expression("Net Diversification 1")) +
      ggplot2::xlab("Total species richness")+
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)

    # tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round6/secsse/ss0_nltts_D/drate_scen_",i,"_ss",num_ss,".tiff"),
    #      units="px", width=5500, height=2000,res = 400,compression="lzw")

    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round6/secsse/ss0_nltts_D/drate_treesize_all350.tiff"),
         units="px", width=6500, height=2500,res = 500,compression="lzw")
    params <- cowplot::plot_grid(
      p_lam1+ggplot2::theme(legend.position = "none"),
      p_mu1+ggplot2::theme(legend.position = "none"),
      p_q12+ggplot2::theme(legend.position = "none"),
      # p_div1+ggplot2::theme(legend.position = "none"),
      p_lam2+ggplot2::theme(legend.position = "none"),
      p_mu2+ggplot2::theme(legend.position = "none"),
      p_q21+ggplot2::theme(legend.position = "none"),
      # p_div2+ggplot2::theme(legend.position = "none"),
      align = "hv", nrow = 2, ncol = 3
      # p_lam1+ggplot2::theme(legend.position = "none"),
      # p_lam2+ggplot2::theme(legend.position = "none"),
      # p_mu1+ggplot2::theme(legend.position = "none"),
      # p_mu2+ggplot2::theme(legend.position = "none"),
      # p_q12+ggplot2::theme(legend.position = "none"),
      # p_q21+ggplot2::theme(legend.position = "none"),
      # p_div1+ggplot2::theme(legend.position = "none"),
      # p_div2+ggplot2::theme(legend.position = "none"),
      # align = "hv", nrow = 4, ncol = 2
    )
    legend <- cowplot::get_legend(
      p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
    )
    param_estimates <- cowplot::plot_grid(params,legend,
                                          rel_widths = c(12,1)
    )
    param_estimates <- cowplot::add_sub(param_estimates, "Total species richness", hjust = 1)
    print(cowplot::ggdraw(param_estimates))
    while (!is.null(dev.list()))  dev.off()
  }
}


for(num_ss in c(0)){
  for(i in c(1)){
    load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round6/secsse/ss0_nltts_D/AMM_per_set_drate_test_ss",num_ss,".RData"))
    color_values <-c("ABC" = "#E90F44" ,"MCMC" = "#63ADEE", "MLE" = "#FFC839")
    # AMM <- AMM_all_df[(i*50-49):(i*50),]
    AMM <- AMM_all_df
    p_lam1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.6,1.2)+
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dlam1_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dlam1_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dlam1_abc),color = "ABC"),shape = 18,alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dlam1_MLE),color = "MLE"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dlam1_mcmc),color = "MCMC"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dlam1_abc),color = "ABC"),method = "lm",se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Log (Tip ratio)") +
      ggplot2::ylab(expression(lambda[0]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_lam2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.6,1.2)+
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dlam2_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dlam2_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dlam2_abc),color = "ABC"),shape = 18,alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dlam2_MLE),color = "MLE"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dlam2_mcmc),color = "MCMC"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dlam2_abc),color = "ABC"),method = "lm",se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Log (Tip ratio)") +
      ggplot2::ylab(expression(lambda[1]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_mu1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.5)+
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dmu1_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dmu1_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dmu1_abc),color = "ABC"),shape = 18,alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dmu1_MLE),color = "MLE"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dmu1_mcmc),color = "MCMC"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dmu1_abc),color = "ABC"),method = "lm",se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Log (Tip ratio)") +
      ggplot2::ylab(expression(mu[0]))+
      ggplot2::geom_hline(yintercept =0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_mu2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.5)+
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dmu2_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dmu2_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dmu2_abc),color = "ABC"),shape = 18,alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dmu1_MLE),color = "MLE"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dmu1_mcmc),color = "MCMC"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dmu1_abc),color = "ABC"),method = "lm",se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Log (Tip ratio)") +
      ggplot2::ylab(expression(mu[1]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_q12 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.05)+
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dq12_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dq12_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dq12_abc),color = "ABC"),shape = 18,alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dq12_MLE),color = "MLE"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dq12_mcmc),color = "MCMC"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dq12_abc),color = "ABC"),method = "lm",se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Log (Tip ratio)") +
      ggplot2::ylab(expression(q["01"]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_q21 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.05)+
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dq21_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dq21_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dq21_abc),color = "ABC"),shape = 18,alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dq21_MLE),color = "MLE"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dq21_mcmc),color = "MCMC"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dq21_abc),color = "ABC"),method = "lm",se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Log (Tip ratio)") +
      ggplot2::ylab(expression(q[10]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_div1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-1.5,1)+
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dnet_div_MLE1),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dnet_div_mcmc1),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dnet_div_abc1),color = "ABC"),shape = 18, alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dnet_div_MLE1),color = "MLE"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dnet_div_mcmc1),color = "MCMC"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dnet_div_abc1),color = "ABC"),method = "lm",se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 11),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::ylab(expression("Net Diversification 0")) +
      ggplot2::xlab("Log (Tip ratio)")+
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)


    p_div2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-1.5,1)+
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dnet_div_MLE2),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dnet_div_mcmc2),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = log(tip_ratio),y = (dnet_div_abc2),color = "ABC"),shape = 18,alpha = 0.7) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dnet_div_MLE2),color = "MLE"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dnet_div_mcmc2),color = "MCMC"),method = "lm",se = F) +
      # ggplot2::geom_smooth(ggplot2::aes(x = log(tip_ratio),y = (dnet_div_abc2),color = "ABC"),method = "lm",se = F) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 11),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::ylab(expression("Net Diversification 1")) +
      ggplot2::xlab("Log (Tip ratio)")+
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)

    # tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round6/secsse/ss0_nltts_D/drate_scen_",i,"_ss",num_ss,".tiff"),
    #      units="px", width=5500, height=2000,res = 400,compression="lzw")

    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round6/secsse/ss0_nltts_D/drate_tipratio_all350.tiff"),
         units="px", width=6500, height=2500,res = 500,compression="lzw")
    params <- cowplot::plot_grid(
      p_lam1+ggplot2::theme(legend.position = "none"),
      p_mu1+ggplot2::theme(legend.position = "none"),
      p_q12+ggplot2::theme(legend.position = "none"),
      # p_div1+ggplot2::theme(legend.position = "none"),
      p_lam2+ggplot2::theme(legend.position = "none"),
      p_mu2+ggplot2::theme(legend.position = "none"),
      p_q21+ggplot2::theme(legend.position = "none"),
      # p_div2+ggplot2::theme(legend.position = "none"),
      align = "hv", nrow = 2, ncol = 3
    )
    legend <- cowplot::get_legend(
      p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
    )
    param_estimates <- cowplot::plot_grid(params,legend,
                                          rel_widths = c(12,1)
    )
    param_estimates <- cowplot::add_sub(param_estimates, "Log (Tip ratio)", hjust = 1)
    print(cowplot::ggdraw(param_estimates))
    while (!is.null(dev.list()))  dev.off()
  }
}

###
for(num_ss in c(0)){
  for(i in c(1)){
    load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round6/secsse/ss0_nltts_D/AMM_per_set_drate_test_ss",num_ss,".RData"))
    color_values <-c("ABC" = "#E90F44" ,"MCMC" = "#63ADEE", "MLE" = "#FFC839")
    AMM <- AMM_all_df[(i*50-49):(i*50),]
    # AMM <- AMM_all_df
    p_lam1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.6,1.2)+
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dlam1_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dlam1_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dlam1_abc),color = "ABC"),shape = 18,alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tip ratio") +
      ggplot2::ylab(expression(lambda[0]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_lam2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.6,1.2)+
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dlam2_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dlam2_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dlam2_abc),color = "ABC"),shape = 18,alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tip ratio") +
      ggplot2::ylab(expression(lambda[1]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_mu1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.5)+
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dmu1_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dmu1_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dmu1_abc),color = "ABC"),shape = 18,alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tip ratio") +
      ggplot2::ylab(expression(mu[0]))+
      ggplot2::geom_hline(yintercept =0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_mu2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.5)+
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dmu2_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dmu2_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dmu2_abc),color = "ABC"),shape = 18,alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tip ratio") +
      ggplot2::ylab(expression(mu[1]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_q12 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.05)+
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dq12_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dq12_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dq12_abc),color = "ABC"),shape = 18,alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tip ratio") +
      ggplot2::ylab(expression(q["01"]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_q21 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.05)+
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dq21_MLE),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dq21_mcmc),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dq21_abc),color = "ABC"),shape = 18,alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tip ratio") +
      ggplot2::ylab(expression(q[10]))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_div1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-1.5,1)+
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dnet_div_MLE1),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dnet_div_mcmc1),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dnet_div_abc1),color = "ABC"),shape = 18, alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 11),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::ylab(expression("Net Diversification 1")) +
      ggplot2::xlab("Tip ratio")+
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)


    p_div2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-1.5,1)+
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dnet_div_MLE2),color = "MLE"),alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dnet_div_mcmc2),color = "MCMC"),shape = 17,alpha = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = 1/tip_ratio,y = (dnet_div_abc2),color = "ABC"),shape = 18,alpha = 0.8) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(size = 11),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::ylab(expression("Net Diversification 2")) +
      ggplot2::xlab("Tip ratio")+
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))+
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)

    # tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round6/secsse/ss0_nltts_D/drate_scen_",i,"_ss",num_ss,".tiff"),
    #      units="px", width=5500, height=2000,res = 400,compression="lzw")

    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round6/secsse/ss0_nltts_D/drate_ratio_all350.tiff"),
         units="px", width=9000, height=3000,res = 450,compression="lzw")
    params <- cowplot::plot_grid(
      p_lam1+ggplot2::theme(legend.position = "none"),
      p_mu1+ggplot2::theme(legend.position = "none"),
      p_q12+ggplot2::theme(legend.position = "none"),
      p_div1+ggplot2::theme(legend.position = "none"),
      p_lam2+ggplot2::theme(legend.position = "none"),
      p_mu2+ggplot2::theme(legend.position = "none"),
      p_q21+ggplot2::theme(legend.position = "none"),
      p_div2+ggplot2::theme(legend.position = "none"),
      align = "hv", nrow = 2, ncol = 4
    )
    legend <- cowplot::get_legend(
      p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
    )
    param_estimates <- cowplot::plot_grid(params,legend,
                                          rel_widths = c(16,1)
    )
    param_estimates <- cowplot::add_sub(param_estimates, "Tip ratio", hjust = 1)
    print(cowplot::ggdraw(param_estimates))
    while (!is.null(dev.list()))  dev.off()
  }
}

