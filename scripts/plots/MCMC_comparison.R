#### compare the MCMC results with different iterations
## three scenarios: whole 50001 iterations; first 1w iterations, last 1w iterations
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=5000),]
folder_path <- "G:/results/project 2/tip_info/round2/MCMC_single/DAISIE_MCMC"
files <- list.files(folder_path)
lac_mcmc <- c()
mu_mcmc <- c()
gam_mcmc <- c()
laa_mcmc <- c()
for(i in 1:400){
   # param_set = (param_num-1)*5 + i
   file_to_load <- grep(paste0("DAISIE_MCMC_param_set_", i,".RData"), #"_rep",rep,
                        files,
                        value = TRUE,
                        fixed = TRUE)

   if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      lac_mcmc <- c(lac_mcmc, output[,1][45001:50000])  ## [1:10000] for first_1w particles
      mu_mcmc <- c(mu_mcmc, output[,2][45001:50000])
      gam_mcmc <- c(gam_mcmc, output[,3][45001:50000])
      laa_mcmc <- c(laa_mcmc, output[,4][45001:50000])
   } else {
      lac_mcmc <- c(lac_mcmc, rep(NA,5000))
      mu_mcmc <- c(mu_mcmc, rep(NA,5000))
      gam_mcmc <- c(gam_mcmc, rep(NA,5000))
      laa_mcmc <- c(laa_mcmc, rep(NA,5000))
   }
}

whole_df_MCMC_first_5q <- data.frame(param_data3,
                            lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc)
#lac_abc,mu_abc,gam_abc,laa_abc,n_iter)
save(whole_df_MCMC_first_5q,file = "G:/results/project 2/tip_info/round2/MCMC_compare/whole_df_MCMC_first_5q.RData")

# load("G:/results/project 2/tip_info/round2/MCMC_single/whole_df_MCMC.RData")
load("G:/results/project 2/tip_info/round2/MCMC_compare/whole_df_MCMC_first_1q.RData")
load("G:/results/project 2/tip_info/round2/MCMC_compare/whole_df_MCMC_first_5q.RData")
load("G:/results/project 2/tip_info/round2/MCMC_compare/whole_df_MCMC_first_1w.RData")

# reps <- subset(whole_df_ABC, lac == 0.5 & mu == 0.5 & gam == 0.02 & laa == 0.5)



library(ggplot2)
# colors <- c("MCMC"="orange","MCMC front"="red","MCMC end"="red4")
colors <- c("MCMC 1000"="orange","MCMC 5000"="red","MCMC 10000"="red4")
for(param_rep in 1:10){
   # scen_mcmc_whole <- c(((param_rep-1)*500010+1):(param_rep*500010))
   # scen_mcmc_first_1w <- c(((param_rep-1)*100000+1):(param_rep*100000))
   # scen_mcmc_last_1w <- c(((param_rep-1)*100000+1):(param_rep*100000))
   scen_mcmc_first_1q <- c(((param_rep-1)*10000+1):(param_rep*10000))
   scen_mcmc_first_5q <- c(((param_rep-1)*50000+1):(param_rep*50000))
   scen_mcmc_first_1w <- c(((param_rep-1)*100000+1):(param_rep*100000))
   tiff(paste0("G:/results/project 2/tip_info/round2/MCMC_compare/boxplot_lac_rep",param_rep,".tiff"), units="px", width=700, height=400)
   g1 <- ggplot2::ggplot(whole_df_MCMC_first_1q[scen_mcmc_first_1q,],
                         aes(x = rep,y = lac_mcmc, group = rep)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot(aes(x = rep-0.2, y = lac_mcmc, color = "MCMC 1000",fill = "MCMC 1000"),
                            alpha = 0.7,width = 0.15,outlier.shape = NA)+
      # stat_boxplot(geom = "errorbar",width = 0.1) +
      ggplot2::geom_boxplot(data= whole_df_MCMC_first_5q[scen_mcmc_first_5q,],
                            aes(x = rep,y = lac_mcmc, color = "MCMC 5000",fill = "MCMC 5000"),
                            alpha = 0.8,width = 0.15,outlier.shape = NA)+
      ggplot2::geom_boxplot(data= whole_df_MCMC_first_1w[scen_mcmc_first_1w,],
                            aes(x = rep+0.2,y = lac_mcmc, color = "MCMC 10000",fill = "MCMC 10000"),
                            alpha = 1.0,width = 0.15,outlier.shape = NA)+
      labs(x = "Replicate",
           y = "Estimated rate",
           color = "Methods",
           fill = "Methods") +
      scale_color_manual(values = colors)+
      scale_fill_manual(values = colors) +
      ggplot2::ggtitle("Cladogenesis") +
      ylim(0,2.0)+
      scale_x_continuous(breaks=seq(1,10,1))+
      # ggtitle("Cladogenesis")+
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
      ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::geom_hline(data= whole_df_MCMC_first_1w[scen_mcmc_first_1w,],
                          aes(yintercept = lac), linetype = "dashed", size = 0.5)
   print(g1)
   while (!is.null(dev.list()))  dev.off()
}



for(param_rep in 1:10){
   # scen_mcmc_whole <- c(((param_rep+9)*500010+1):((param_rep+10)*500010))
   # scen_mcmc_first_1w <- c(((param_rep+9)*100000+1):((param_rep+10)*100000))
   # scen_mcmc_last_1w <- c(((param_rep+9)*100000+1):((param_rep+10)*100000))
   scen_mcmc_first_1q <- c(((param_rep+9)*10000+1):((param_rep+10)*10000))
   scen_mcmc_first_5q <- c(((param_rep+9)*50000+1):((param_rep+10)*50000))
   scen_mcmc_first_1w <- c(((param_rep+9)*100000+1):((param_rep+10)*100000))
   tiff(paste0("G:/results/project 2/tip_info/round2/MCMC_compare/boxplot_mu_rep",param_rep,".tiff"), units="px", width=700, height=400)
   g2 <- ggplot2::ggplot(whole_df_MCMC_first_1q[scen_mcmc_first_1q,],
                         aes(x = rep,y = mu_mcmc, group = rep)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot(aes(x = rep-0.2, y = mu_mcmc, color = "MCMC 1000",fill = "MCMC 1000"),
                            alpha = 0.7,width = 0.15,outlier.shape = NA)+
      # stat_boxplot(geom = "errorbar",width = 0.1) +
      ggplot2::geom_boxplot(data= whole_df_MCMC_first_5q[scen_mcmc_first_5q,],
                            aes(x = rep,y = mu_mcmc, color = "MCMC 5000",fill = "MCMC 5000"),
                            alpha = 0.8,width = 0.15,outlier.shape = NA)+
      ggplot2::geom_boxplot(data= whole_df_MCMC_first_1w[scen_mcmc_first_1w,],
                            aes(x = rep+0.2,y = mu_mcmc, color = "MCMC 10000",fill = "MCMC 10000"),
                            alpha = 1.0,width = 0.15,outlier.shape = NA)+
      labs(x = "Replicate",
           y = "Estimated rate",
           color = "Methods",
           fill = "Methods") +
      scale_color_manual(values = colors)+
      scale_fill_manual(values = colors) +
      ggplot2::ggtitle("Cladogenesis") +
      ylim(0,2.0)+
      scale_x_continuous(breaks=seq(1,10,1))+
      # ggtitle("Cladogenesis")+
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
      ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::geom_hline(data= whole_df_MCMC_first_1w[scen_mcmc_first_1w,],
                          aes(yintercept = mu), linetype = "dashed", size = 0.5)
   print(g2)
   while (!is.null(dev.list()))  dev.off()
}


for(param_rep in 1:10){
   # scen_mcmc_whole <- c(((param_rep+19)*500010+1):((param_rep+20)*500010))
   # scen_mcmc_first_1w <- c(((param_rep+19)*100000+1):((param_rep+20)*100000))
   # scen_mcmc_last_1w <- c(((param_rep+19)*100000+1):((param_rep+20)*100000))
   scen_mcmc_first_1q <- c(((param_rep+19)*10000+1):((param_rep+20)*10000))
   scen_mcmc_first_5q <- c(((param_rep+19)*50000+1):((param_rep+20)*50000))
   scen_mcmc_first_1w <- c(((param_rep+19)*100000+1):((param_rep+20)*100000))
   tiff(paste0("G:/results/project 2/tip_info/round2/MCMC_compare/boxplot_gam_rep",param_rep,".tiff"), units="px", width=700, height=400)
   g3 <- ggplot2::ggplot(whole_df_MCMC_first_1q[scen_mcmc_first_1q,],
                         aes(x = rep,y = gam_mcmc, group = rep)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot(aes(x = rep-0.2, y = gam_mcmc, color = "MCMC 1000",fill = "MCMC 1000"),
                            alpha = 0.7,width = 0.15,outlier.shape = NA)+
      # stat_boxplot(geom = "errorbar",width = 0.1) +
      ggplot2::geom_boxplot(data= whole_df_MCMC_first_5q[scen_mcmc_first_5q,],
                            aes(x = rep,y = gam_mcmc, color = "MCMC 5000",fill = "MCMC 5000"),
                            alpha = 0.8,width = 0.15,outlier.shape = NA)+
      ggplot2::geom_boxplot(data= whole_df_MCMC_first_1w[scen_mcmc_first_1w,],
                            aes(x = rep+0.2,y = gam_mcmc, color = "MCMC 10000",fill = "MCMC 10000"),
                            alpha = 1.0,width = 0.15,outlier.shape = NA)+
      labs(x = "Replicate",
           y = "Estimated rate",
           color = "Methods",
           fill = "Methods") +
      scale_color_manual(values = colors)+
      scale_fill_manual(values = colors) +
      ggplot2::ggtitle("Cladogenesis") +
      ylim(0,0.05)+
      scale_x_continuous(breaks=seq(1,10,1))+
      # ggtitle("Cladogenesis")+
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
      ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::geom_hline(data= whole_df_MCMC_first_1w[scen_mcmc_first_1w,],
                          aes(yintercept = gam), linetype = "dashed", size = 0.5)
   print(g3)
   while (!is.null(dev.list()))  dev.off()
}


for(param_rep in 1:10){
   # scen_mcmc_whole <- c(((param_rep+29)*500010+1):((param_rep+30)*500010))
   # scen_mcmc_first_1w <- c(((param_rep+29)*100000+1):((param_rep+30)*100000))
   # scen_mcmc_last_1w <- c(((param_rep+29)*100000+1):((param_rep+30)*100000))
   scen_mcmc_first_1q <- c(((param_rep+29)*10000+1):((param_rep+30)*10000))
   scen_mcmc_first_5q <- c(((param_rep+29)*50000+1):((param_rep+30)*50000))
   scen_mcmc_first_1w <- c(((param_rep+29)*100000+1):((param_rep+30)*100000))
   tiff(paste0("G:/results/project 2/tip_info/round2/MCMC_compare/boxplot_laa_rep",param_rep,".tiff"), units="px", width=700, height=400)
   g4 <- ggplot2::ggplot(whole_df_MCMC_first_1q[scen_mcmc_first_1q,],
                         aes(x = rep,y = laa_mcmc, group = rep)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot(aes(x = rep-0.2, y = laa_mcmc, color = "MCMC 1000",fill = "MCMC 1000"),
                            alpha = 0.7,width = 0.15,outlier.shape = NA)+
      # stat_boxplot(geom = "errorbar",width = 0.1) +
      ggplot2::geom_boxplot(data= whole_df_MCMC_first_5q[scen_mcmc_first_5q,],
                            aes(x = rep,y = laa_mcmc, color = "MCMC 5000",fill = "MCMC 5000"),
                            alpha = 0.8,width = 0.15,outlier.shape = NA)+
      ggplot2::geom_boxplot(data= whole_df_MCMC_first_1w[scen_mcmc_first_1w,],
                            aes(x = rep+0.2,y = laa_mcmc, color = "MCMC 10000",fill = "MCMC 10000"),
                            alpha = 1.0,width = 0.15,outlier.shape = NA)+
      labs(x = "Replicate",
           y = "Estimated rate",
           color = "Methods",
           fill = "Methods") +
      scale_color_manual(values = colors)+
      scale_fill_manual(values = colors) +
      ggplot2::ggtitle("Cladogenesis") +
      ylim(0,2.0)+
      scale_x_continuous(breaks=seq(1,10,1))+
      # ggtitle("Cladogenesis")+
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
      ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
      ggplot2::geom_hline(data= whole_df_MCMC_first_1w[scen_mcmc_first_1w,],
                          aes(yintercept = laa), linetype = "dashed", size = 0.5)
   print(g4)
   while (!is.null(dev.list()))  dev.off()
}
