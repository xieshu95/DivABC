### plot all replicates into one figure
load("G:/results/project 2/tip_info/round2/MCMC_compare/whole_df_MCMC_first_1q.RData")
load("G:/results/project 2/tip_info/round2/MCMC_compare/whole_df_MCMC_first_5q.RData")
load("G:/results/project 2/tip_info/round2/MCMC_compare/whole_df_MCMC_first_1w.RData")


# reps <- subset(whole_df_ABC, lac == 0.5 & mu == 0.5 & gam == 0.02 & laa == 0.5)


library(ggplot2)
colors <- c("MCMC 1000"="orange","MCMC 5000"="red","MCMC 10000"="red4")
tiff("G:/results/project 2/tip_info/round2/no_logs_allss/boxplot_lac.tiff", units="px", width=700, height=350)

scen_mcmc_first_1q <- c(1:100000)
scen_mcmc_first_5q <- c(1:500000)
scen_mcmc_first_1w <- c(1:1000000)
g1 <- ggplot2::ggplot(whole_df_MCMC_first_1q[scen_mcmc_first_1q,],
                      aes(x = lac,y = lac_mcmc, group = lac)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = lac-0.018, y = lac_mcmc, color = "MCMC 1000",fill = "MCMC 1000"),
                        alpha = 0.7,width = 0.015,outlier.shape = NA)+
  # stat_boxplot(geom = "errorbar",width = 0.1) +
  ggplot2::geom_boxplot(data= whole_df_MCMC_first_5q[scen_mcmc_first_5q,],
                        aes(x = lac,y = lac_mcmc, color = "MCMC 5000",fill = "MCMC 5000"),
                        alpha = 0.8,width = 0.015,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= whole_df_MCMC_first_1w[scen_mcmc_first_1w,],
                        aes(x = lac+0.018,y = lac_mcmc, color = "MCMC 10000",fill = "MCMC 10000"),
                        alpha = 1.0,width = 0.015,outlier.shape = NA)+
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Cladogenesis") +
  ylim(0,2.0)+
  scale_x_continuous(breaks=seq(0.1,1.1,0.1))+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
print(g1)
while (!is.null(dev.list()))  dev.off()





tiff("G:/results/project 2/tip_info/round2/no_logs_allss/boxplot_mu.tiff", units="px", width=700, height=350)
g2 <- ggplot2::ggplot(whole_df_MCMC_first_1q[scen_mcmc_first_1q,],
                      aes(x = mu,y = mu_mcmc, group = mu)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = mu-0.018, y = mu_mcmc, color = "MCMC 1000",fill = "MCMC 1000"),
                        alpha = 0.7,width = 0.015,outlier.shape = NA)+
  # stat_boxplot(geom = "errorbar",width = 0.1) +
  ggplot2::geom_boxplot(data= whole_df_MCMC_first_5q[scen_mcmc_first_5q,],
                        aes(x = mu,y = mu_mcmc, color = "MCMC 5000",fill = "MCMC 5000"),
                        alpha = 0.8,width = 0.015,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= whole_df_MCMC_first_1w[scen_mcmc_first_1w,],
                        aes(x = mu+0.018,y = mu_mcmc, color = "MCMC 10000",fill = "MCMC 10000"),
                        alpha = 1.0,width = 0.015,outlier.shape = NA)+
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Cladogenesis") +
  ylim(0,2.0)+
  scale_x_continuous(breaks=seq(0.1,1.1,0.1))+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
print(g2)
while (!is.null(dev.list()))  dev.off()


tiff("G:/results/project 2/tip_info/round2/no_logs_allss/boxplot_gam.tiff", units="px", width=700, height=350)
g3 <- ggplot2::ggplot(whole_df_MCMC_first_1q[scen_mcmc_first_1q,],
                      aes(x = gam,y = gam_mcmc, group = gam)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = gam-0.0003, y = gam_mcmc, color = "MCMC 1000",fill = "MCMC 1000"),
                        alpha = 0.7,width = 0.0003,outlier.shape = NA)+
  # stat_boxplot(geom = "errorbar",width = 0.1) +
  ggplot2::geom_boxplot(data= whole_df_MCMC_first_5q[scen_mcmc_first_5q,],
                        aes(x = gam,y = gam_mcmc, color = "MCMC 5000",fill = "MCMC 5000"),
                        alpha = 0.8,width = 0.0003,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= whole_df_MCMC_first_1w[scen_mcmc_first_1w,],
                        aes(x = gam+0.0003,y = gam_mcmc, color = "MCMC 10000",fill = "MCMC 10000"),
                        alpha = 1.0,width = 0.0003,outlier.shape = NA)+
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Cladogenesis") +
  ylim(0,0.05)+
  scale_x_continuous(breaks=seq(0.01,0.032,0.004))+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
print(g3)
while (!is.null(dev.list()))  dev.off()

tiff("G:/results/project 2/tip_info/round2/no_logs_allss/boxplot_laa.tiff", units="px", width=700, height=350)
g4 <- ggplot2::ggplot(whole_df_MCMC_first_1q[scen_mcmc_first_1q,],
                      aes(x = rep,y = laa_mcmc, group = rep)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = rep-0.018, y = laa_mcmc, color = "MCMC 1000",fill = "MCMC 1000"),
                        alpha = 0.7,width = 0.015,outlier.shape = NA)+
  # stat_boxplot(geom = "errorbar",width = 0.1) +
  ggplot2::geom_boxplot(data= whole_df_MCMC_first_5q[scen_mcmc_first_5q,],
                        aes(x = rep,y = laa_mcmc, color = "MCMC 5000",fill = "MCMC 5000"),
                        alpha = 0.8,width = 0.015,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= whole_df_MCMC_first_1w[scen_mcmc_first_1w,],
                        aes(x = rep+0.018,y = laa_mcmc, color = "MCMC 10000",fill = "MCMC 10000"),
                        alpha = 1.0,width = 0.015,outlier.shape = NA)+
  labs(x = "Replicate",
       y = "Estimated rate",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Cladogenesis") +
  ylim(0,2.0)+
  scale_x_continuous(breaks=seq(0.1,1.1,0.1))+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
print(g4)
while (!is.null(dev.list()))  dev.off()
