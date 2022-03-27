### violint plot for single parameter
# file_names:
# all_6ss
# 5ss_no_totnum
# 4ss_no_totnum_end
# 4ss_no_totnum_nonend
# 4ss_no_totnum_numcol
# 4ss_no_totnum_clade_size
# 4ss_no_totnum_sdcol
# nltt_all
# nltt_no_specnltt

### results of ABC
folder_path <- "G:/results/project 2/tip_info/round2/no_logs_allss/DAISIE_ABC"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")

param_data2<-param_data[rep(seq_len(nrow(param_data)), each=1000),]

lac_abc <- c()
mu_abc <- c()
gam_abc <- c()
laa_abc <- c()
n_iter <- c()
for(i in 1:400){
  # if(i%%5 == 0){
  #   rep <- 5
  # } else {
  #   rep <- i%%5
  # }
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("DAISIE_ABC_param_set_", i,".RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    if(output$n_iter <= 2){
      lac_abc <- c(lac_abc, rep(NA,1000))
      mu_abc <- c(mu_abc, rep(NA,1000))
      gam_abc <- c(gam_abc, rep(NA,1000))
      laa_abc <- c(laa_abc, rep(NA,1000))
    } else{
      lac_abc <- c(lac_abc, output$ABC[,1])
      mu_abc <- c(mu_abc, output$ABC[,2])
      gam_abc <- c(gam_abc, output$ABC[,3])
      laa_abc <- c(laa_abc, output$ABC[,4])
    }
  } else {
    lac_abc <- c(lac_abc, rep(NA,1000))
    mu_abc <- c(mu_abc, rep(NA,1000))
    gam_abc <- c(gam_abc, rep(NA,1000))
    laa_abc <- c(laa_abc, rep(NA,1000))
  }
}
whole_df_ABC <- data.frame(param_data2,
                           # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                           lac_abc,mu_abc,gam_abc,laa_abc)
save(whole_df_ABC,file = "G:/results/project 2/tip_info/round2/no_logs_allss/whole_df_ABC.RData")

#### combine results of MCMC
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=50001),]
folder_path <- "G:/results/project 2/tip_info/round2/MCMC_all/DAISIE_MCMC"
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
    lac_mcmc <- c(lac_mcmc, output[,1])
    mu_mcmc <- c(mu_mcmc, output[,2])
    gam_mcmc <- c(gam_mcmc, output[,3])
    laa_mcmc <- c(laa_mcmc, output[,4])
  } else {
    lac_mcmc <- c(lac_mcmc, rep(NA,50001))
    mu_mcmc <- c(mu_mcmc, rep(NA,50001))
    gam_mcmc <- c(gam_mcmc, rep(NA,50001))
    laa_mcmc <- c(laa_mcmc, rep(NA,50001))
  }
}

whole_df_MCMC <- data.frame(param_data3,
                            lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc)
#lac_abc,mu_abc,gam_abc,laa_abc,n_iter)
save(whole_df_MCMC,file = "G:/results/project 2/tip_info/round2/MCMC_all/whole_df_MCMC.RData")


#### analysis
load("G:/results/project 2/tip_info/round2/no_logs_allss/whole_df_ABC.RData")
load("G:/results/project 2/tip_info/round2/MCMC_single/whole_df_MCMC.RData")
load("G:/results/project 2/tip_info/round2/MLE_single/whole_df_MLE.RData")


# reps <- subset(whole_df_ABC, lac == 0.5 & mu == 0.5 & gam == 0.02 & laa == 0.5)


library(ggplot2)
colors <- c("MCMC"="red","ABC"="blue3")
tiff("G:/results/project 2/tip_info/round2/no_logs_allss/boxplot_lac.tiff", units="px", width=800, height=500)
g1 <- ggplot2::ggplot(whole_df_ABC[c(1:100000),], aes(x = lac,y = lac_abc, group = lac)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = lac, y = lac_abc, color = "ABC",fill = "ABC"),alpha = 0.4,outlier.shape = NA)+ ##aes(x = lac, y = lac_abc, color = "ABC")
  ggplot2::geom_boxplot(data= whole_df_MCMC[c(1:5000100),],
                        aes(x = lac,y = lac_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.5,outlier.shape = NA)+
  # ggplot2::geom_point(data = whole_df2[c(1:100),],aes(y = lac_mcmc, color = "MCMC"),size = 2)+
  # ggplot2::geom_point(aes(y = lac_mcmc, color = "MCMC"))+
  ggplot2::geom_point(data= whole_df_MLE[c(1:100),],aes(x = lac, y = lac_MLE),color = "black",shape = 17,size = 1.5) +
  # ggplot2::geom_point(aes(x = lac, y = lac),color = "black",size = 3) +
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Cladogenesis") +
  xlim(0,1.1)+
  ylim(0,2.0)+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20)) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20)) +
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
print(g1)
while (!is.null(dev.list()))  dev.off()

tiff("G:/results/project 2/tip_info/round2/no_logs_allss/boxplot_mu.tiff", units="px", width=800, height=500)
g2 <- ggplot2::ggplot(whole_df_ABC[c(100001:200000),], aes(x = mu,y = mu_abc, group = mu)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = mu, y = mu_abc, color = "ABC",fill = "ABC"),alpha = 0.4,outlier.shape = NA)+ ##aes(x = lac, y = lac_abc, color = "ABC")
  ggplot2::geom_boxplot(data= whole_df_MCMC[c(5000101:10000200),],
                        aes(x = mu,y = mu_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(data= whole_df_MLE[c(101:200),],aes(x = mu, y = mu_MLE),color = "black",shape = 17,size = 1.5) +
  # ggplot2::geom_point(aes(x = mu, y = mu),color = "black",size = 3) +
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Extinction") +
  xlim(0,1.1)+
  ylim(0,2.0)+
  # ggtitle("Extinction")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20)) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20)) +
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
print(g2)
while (!is.null(dev.list()))  dev.off()

tiff("G:/results/project 2/tip_info/round2/no_logs_allss/boxplot_gam.tiff", units="px", width=800, height=500)
g3 <- ggplot2::ggplot(whole_df_ABC[c(200001:300000),], aes(x = gam,y = gam_abc, group = gam)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = gam, y = gam_abc, color = "ABC",fill = "ABC"),alpha = 0.4,outlier.shape = NA)+ ##aes(x = lac, y = lac_abc, color = "ABC")
  ggplot2::geom_boxplot(data= whole_df_MCMC[c(10000201:15000300),],
                        aes(x = gam,y = gam_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(data= whole_df_MLE[c(201:300),],aes(x = gam, y = gam_MLE),color = "black",shape = 17,size = 1.5) +
  # ggplot2::geom_point(aes(x = gam, y = gam),color = "black",size = 3) +
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Colonization") +
  xlim(0.010,0.032)+
  ylim(0,0.05)+
  # ggtitle("Colonization")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20)) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20)) +
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
print(g3)
while (!is.null(dev.list()))  dev.off()

tiff("G:/results/project 2/tip_info/round2/no_logs_allss/boxplot_laa.tiff", units="px", width=800, height=500)
g4 <- ggplot2::ggplot(whole_df_ABC[c(300001:400000),], aes(x = laa,y = laa_abc, group = laa)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = laa, y = laa_abc, color = "ABC",fill = "ABC"),alpha = 0.4,outlier.shape = NA)+ ##aes(x = lac, y = lac_abc, color = "ABC")
  ggplot2::geom_boxplot(data= whole_df_MCMC[c(15000301:20000400),],
                        aes(x = laa,y = laa_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(data= whole_df_MLE[c(301:400),],aes(x = laa, y = laa_MLE),color = "black",shape = 17,size = 1.5) +
  # ggplot2::geom_point(aes(x = laa, y = laa),color = "black",size = 3) +
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Anagenesis") +
  xlim(0,1.1)+
  ylim(0,2.0)+
  # ggtitle("Anagenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20)) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20)) +
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
print(g4)
while (!is.null(dev.list()))  dev.off()





