## boxplot for all rates (all rates per scenario)
load("G:/results/project 2/tip_info/round3/test_epsilon/whole_df_ABC.RData")
# load("G:/results/project 2/tip_info/round3/dec_kernel_old/MCMC_allpars/whole_df_MCMC.RData")
load("G:/results/project 2/tip_info/round3/fixed_mcmc/whole_df_MCMC.RData")
load("G:/results/project 2/tip_info/round3/dec_kernel_old/MLE_allpars/MLE_all.RData")

whole_df_MLE <- MLE_all

library(ggplot2)
colors <- c("MCMC" = "#F7903D", "ABC" = "#4D85BD", "MLE" = "#59A95A")
# tiff("G:/results/project 2/tip_info/round3/test_epsilon/boxplots/boxplot_lac.tiff", units="px", width=700, height=400)
i = 1
# param_abc <- whole_df_ABC[((i*8000-7999)):(i*8000),]
param_abc <- whole_df_ABC[((i*4000-3999)):(i*4000),]
# param_mcmc <- whole_df_MCMC[((i*80000-79999)):(i*80000),]
param_mcmc <- whole_df_MCMC[((i*40000-39999)):(i*40000),]
param_mle <- whole_df_MLE[((i*40-39)):(i*40),]
g1_lac <- ggplot2::ggplot(param_abc, aes(x = lac,y = lac_abc, group = lac)) +
  # ggplot2::theme_bw() +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = lac-0.03,y = lac_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = lac, y = lac_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = lac+0.03,y = lac_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac, y = lac),color = "black",size = 3,shape = 16) +
  labs(x = "Cladogenesis",
       y = "Estimated Cladogenesis",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Cladogenesis") +
  scale_x_continuous(limits=c(0.15, 0.85), breaks = c(0.2,0.4,0.6,0.8))+
  ylim(0,2.0)+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  # ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.3,0.5,0.7), linetype= "dashed")


g1_mu <- ggplot2::ggplot(param_abc, aes(x = lac,y = mu_abc, group = lac)) +
  # ggplot2::theme_bw() +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = lac-0.03,y = mu_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = lac, y = mu_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = lac+0.03,y = mu_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac, y = mu),color = "black",size = 3,shape = 16) +
  labs(x = "Cladogenesis",
       y = "Estimated Extinction",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Cladogenesis") +
  scale_x_continuous(limits=c(0.15, 0.85), breaks = c(0.2,0.4,0.6,0.8))+
  ylim(0,2.0)+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  # ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.3,0.5,0.7), linetype= "dashed")

g1_gam <- ggplot2::ggplot(param_abc, aes(x = lac,y = gam_abc, group = lac)) +
  # ggplot2::theme_bw() +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = lac-0.03,y = gam_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = lac, y = gam_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = lac+0.03,y = gam_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac, y = gam),color = "black",size = 3,shape = 16) +
  labs(x = "Cladogenesis",
       y = "Estimated Colonization",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Cladogenesis") +
  scale_x_continuous(limits=c(0.15, 0.85), breaks = c(0.2,0.4,0.6,0.8))+
  ylim(0,0.07)+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  # ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.3,0.5,0.7), linetype= "dashed")


g1_laa <- ggplot2::ggplot(param_abc, aes(x = lac,y = laa_abc, group = lac)) +
  # ggplot2::theme_bw() +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = lac-0.03,y = laa_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = lac, y = laa_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = lac+0.03,y = laa_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac, y = laa),color = "black",size = 3,shape = 16) +
  labs(x = "Cladogenesis",
       y = "Estimated Anagenesis",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Cladogenesis") +
  scale_x_continuous(limits=c(0.15, 0.85), breaks = c(0.2,0.4,0.6,0.8))+
  ylim(0,2.0)+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  # ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.3,0.5,0.7), linetype= "dashed")


tiff(paste0("G:/results/project 2/tip_info/round3/test_epsilon/boxplots/boxplot_scenario_", i, ".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  g1_lac,g1_mu,g1_gam,g1_laa,
  align = "hv", nrow = 2, ncol = 2
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()


i = 2
# param_abc <- whole_df_ABC[((i*8000-7999)):(i*8000),]
param_abc <- whole_df_ABC[((i*4000-3999)):(i*4000),]
param_mcmc <- whole_df_MCMC[((i*80000-79999)):(i*80000),]
param_mle <- whole_df_MLE[((i*40-39)):(i*40),]
g2_lac <- ggplot2::ggplot(param_abc, aes(x = mu,y = lac_abc, group = mu)) +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = mu-0.008,y = lac_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.005,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = mu, y = lac_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.005,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = mu+0.008,y = lac_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.004,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = mu, y = lac),color = "black",size = 3,shape = 16) +
  labs(x = "Real Extinction",
       y = "Estimated Cladogenesis",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Extinction") +
  scale_x_continuous(limits=c(0.03, 0.22), breaks = c(0.05,0.1,0.15,0.2))+
  ylim(0,2.0)+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.075,0.125,0.175), linetype= "dashed")


g2_mu <- ggplot2::ggplot(param_abc, aes(x = mu,y = mu_abc, group = mu)) +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = mu-0.008,y = mu_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.005,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = mu, y = mu_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.005,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = mu+0.008,y = mu_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.004,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = mu, y = mu),color = "black",size = 3,shape = 16) +
  labs(x = "Real Extinction",
       y = "Estimated Extinction",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Extinction") +
  scale_x_continuous(limits=c(0.03, 0.22), breaks = c(0.05,0.1,0.15,0.2))+
  ylim(0,2.0)+
  # ggtitle("Extinction")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.075,0.125,0.175), linetype= "dashed")


g2_gam <- ggplot2::ggplot(param_abc, aes(x = mu,y = gam_abc, group = mu)) +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = mu-0.008,y = gam_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.005,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = mu, y = gam_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.005,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = mu+0.008,y = gam_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.004,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = mu, y = gam),color = "black",size = 3,shape = 16) +
  labs(x = "Real Extinction",
       y = "Estimated Colonization",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Extinction") +
  scale_x_continuous(limits=c(0.03, 0.22), breaks = c(0.05,0.1,0.15,0.2))+
  ylim(0,0.07)+
  # ggtitle("Extinction")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.075,0.125,0.175), linetype= "dashed")


g2_laa <- ggplot2::ggplot(param_abc, aes(x = mu,y = laa_abc, group = mu)) +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = mu-0.008,y = laa_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.005,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = mu, y = laa_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.005,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = mu+0.008,y = laa_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.004,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = mu, y = laa),color = "black",size = 3,shape = 16) +
  labs(x = "Real Extinction",
       y = "Estimated Anagenesis",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Extinction") +
  scale_x_continuous(limits=c(0.03, 0.22), breaks = c(0.05,0.1,0.15,0.2))+
  ylim(0,2.0)+
  # ggtitle("Extinction")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.075,0.125,0.175), linetype= "dashed")

tiff(paste0("G:/results/project 2/tip_info/round3/test_epsilon/boxplots/boxplot_scenario_", i, ".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  g2_lac,g2_mu,g2_gam,g2_laa,
  align = "hv", nrow = 2, ncol = 2
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()




i = 3
# param_abc <- whole_df_ABC[((i*8000-7999)):(i*8000),]
param_abc <- whole_df_ABC[((i*4000-3999)):(i*4000),]
param_mcmc <- whole_df_MCMC[((i*80000-79999)):(i*80000),]
param_mle <- whole_df_MLE[((i*40-39)):(i*40),]
g3_lac <- ggplot2::ggplot(param_abc, aes(x = gam,y = lac_abc, group = gam)) +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = gam-0.001,y = lac_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = gam, y = lac_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = gam+0.001,y = lac_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam, y = lac),color = "black",size = 3,shape = 16) +
  labs(x = "Real Colonization",
       y = "Estimated Cladogenesis",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits=c(0.0081, 0.0269), breaks = c(0.01,0.015,0.02,0.025))+
  ylim(0,2.0)+
  # ggtitle("Colonization")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.0125,0.0175,0.0225), linetype= "dashed")

g3_mu <- ggplot2::ggplot(param_abc, aes(x = gam,y = mu_abc, group = gam)) +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = gam-0.001,y = mu_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = gam, y = mu_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = gam+0.001,y = mu_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam, y = mu),color = "black",size = 3,shape = 16) +
  labs(x = "Real Colonization",
       y = "Estimated Extinction",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits=c(0.0081, 0.0269), breaks = c(0.01,0.015,0.02,0.025))+
  ylim(0,2.0)+
  # ggtitle("Colonization")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.0125,0.0175,0.0225), linetype= "dashed")

g3_gam <- ggplot2::ggplot(param_abc, aes(x = gam,y = gam_abc, group = gam)) +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = gam-0.001,y = gam_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = gam, y = gam_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = gam+0.001,y = gam_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam, y = gam),color = "black",size = 3,shape = 16) +
  labs(x = "Real Colonization",
       y = "Estimated Colonization",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits=c(0.0081, 0.0269), breaks = c(0.01,0.015,0.02,0.025))+
  ylim(0,0.07)+
  # ggtitle("Colonization")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.0125,0.0175,0.0225), linetype= "dashed")


g3_laa <- ggplot2::ggplot(param_abc, aes(x = gam,y = laa_abc, group = gam)) +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = gam-0.001,y = laa_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = gam, y = laa_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = gam+0.001,y = laa_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam, y = laa),color = "black",size = 3,shape = 16) +
  labs(x = "Real Colonization",
       y = "Estimated Anagenesis",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits=c(0.0081, 0.0269), breaks = c(0.01,0.015,0.02,0.025))+
  ylim(0,2.0)+
  # ggtitle("Colonization")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.0125,0.0175,0.0225), linetype= "dashed")


tiff(paste0("G:/results/project 2/tip_info/round3/test_epsilon/boxplots/boxplot_scenario_", i, ".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  g3_lac,g3_mu,g3_gam,g3_laa,
  align = "hv", nrow = 2, ncol = 2
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()




i = 4
# param_abc <- whole_df_ABC[((i*8000-7999)):(i*8000),]
param_abc <- whole_df_ABC[((i*4000-3999)):(i*4000),]
param_mcmc <- whole_df_MCMC[((i*80000-79999)):(i*80000),]
param_mle <- whole_df_MLE[((i*40-39)):(i*40),]
g4_lac <- ggplot2::ggplot(param_abc, aes(x = laa,y = lac_abc, group = laa)) +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = laa-0.03,y = lac_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = laa, y = lac_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = laa+0.03,y = lac_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa, y = lac),color = "black",size = 3,shape = 16) +
  labs(x = "Real Anagenesis",
       y = "Estimated Cladogenesis",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits=c(0.15, 0.85), breaks = c(0.2,0.4,0.6,0.8))+
  ylim(0,2.0)+
  # ggtitle("Anagenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.3,0.5,0.7), linetype= "dashed")

g4_mu <- ggplot2::ggplot(param_abc, aes(x = laa,y = mu_abc, group = laa)) +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = laa-0.03,y = mu_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = laa, y = mu_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = laa+0.03,y = mu_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa, y = mu),color = "black",size = 3,shape = 16) +
  labs(x = "Real Anagenesis",
       y = "Estimated Extinction",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits=c(0.15, 0.85), breaks = c(0.2,0.4,0.6,0.8))+
  ylim(0,2.0)+
  # ggtitle("Anagenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.3,0.5,0.7), linetype= "dashed")

g4_gam <- ggplot2::ggplot(param_abc, aes(x = laa,y = gam_abc, group = laa)) +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = laa-0.03,y = gam_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = laa, y = gam_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = laa+0.03,y = gam_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa, y = gam),color = "black",size = 3,shape = 16) +
  labs(x = "Real Anagenesis",
       y = "Estimated Colonization",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits=c(0.15, 0.85), breaks = c(0.2,0.4,0.6,0.8))+
  ylim(0,0.07)+
  # ggtitle("Anagenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.3,0.5,0.7), linetype= "dashed")

g4_laa <- ggplot2::ggplot(param_abc, aes(x = laa,y = laa_abc, group = laa)) +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = laa-0.03,y = laa_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = laa, y = laa_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = laa+0.03,y = laa_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa, y = laa),color = "black",size = 3,shape = 16) +
  labs(x = "Real Anagenesis",
       y = "Estimated Anagenesis",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(limits=c(0.15, 0.85), breaks = c(0.2,0.4,0.6,0.8))+
  ylim(0,2.0)+
  # ggtitle("Anagenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  ggplot2::geom_vline(xintercept = c(0.3,0.5,0.7), linetype= "dashed")


tiff(paste0("G:/results/project 2/tip_info/round3/test_epsilon/boxplots/boxplot_scenario_", i, ".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  g4_lac,g4_mu,g4_gam,g4_laa,
  align = "hv", nrow = 2, ncol = 2
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()
