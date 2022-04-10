#### plot the posterior distribution for each replicate(each parameter set)
### boxplot for single parameter
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
### results of ABC final iteration
folder_path <- "G:/results/project 2/tip_info/round2/less_sd/DAISIE_ABC"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")

param_data2<-param_data[rep(seq_len(nrow(param_data)), each=500),]

lac_abc <- c()
mu_abc <- c()
gam_abc <- c()
laa_abc <- c()
n_iteration <- c()
for(i in 1:400){
  file_to_load <- grep(paste0("DAISIE_ABC_param_set_", i,".RData"),
                       files,
                       value = TRUE,
                       fixed = TRUE)
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    num_iter <- output$n_iter
    n_iteration[i] <- num_iter
    if(output$n_iter <= 2){
      lac_abc <- c(lac_abc, rep(NA,500))
      mu_abc <- c(mu_abc, rep(NA,500))
      gam_abc <- c(gam_abc, rep(NA,500))
      laa_abc <- c(laa_abc, rep(NA,500))
    } else{
      lac_abc <- c(lac_abc, output$ABC[[4]][,1])  ##num_iter-1
      mu_abc <- c(mu_abc, output$ABC[[4]][,2])
      gam_abc <- c(gam_abc, output$ABC[[4]][,3])
      laa_abc <- c(laa_abc, output$ABC[[4]][,4])
    }
  } else {
    lac_abc <- c(lac_abc, rep(NA,500))
    mu_abc <- c(mu_abc, rep(NA,500))
    gam_abc <- c(gam_abc, rep(NA,500))
    laa_abc <- c(laa_abc, rep(NA,500))
  }
}
whole_df_ABC <- data.frame(param_data2,
                           # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                           lac_abc,mu_abc,gam_abc,laa_abc)
# n_iteration
save(whole_df_ABC,file = "G:/results/project 2/tip_info/round2/less_sd/whole_df_ABC_iter4.RData")  ## whole_df_ABC_iter1.RData


#### combine results of MCMC
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=50001),]
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
save(whole_df_MCMC,file = "G:/results/project 2/tip_info/round2/MCMC_single/whole_df_MCMC.RData")


#### analysis
load("G:/results/project 2/tip_info/round2/less_sd/whole_df_ABC.RData")
load("G:/results/project 2/tip_info/round2/MCMC_single/whole_df_MCMC.RData")
load("G:/results/project 2/tip_info/round2/MLE_single/whole_df_MLE.RData")


# reps <- subset(whole_df_ABC, lac == 0.5 & mu == 0.5 & gam == 0.02 & laa == 0.5)


library(ggplot2)
colors <- c("MCMC"="red","ABC"="blue3")
for(param_rep in 1:10){
  tiff(paste0("G:/results/project 2/tip_info/round2/less_sd/boxplot_lac_rep",param_rep,".tiff"), units="px", width=700, height=400)
  g1 <- ggplot2::ggplot(whole_df_ABC[c(((param_rep-1)*5000+1):(param_rep*5000)),],
                        aes(x = rep,y = lac_abc, group = rep)) +
    ggplot2::theme_bw() +
    ggplot2::geom_boxplot(aes(x = rep+0.2, y = lac_abc, color = "ABC",fill = "ABC"),
                          alpha = 0.7,width = 0.2,outlier.shape = NA)+ ##aes(x = lac, y = lac_abc, color = "ABC")
    ggplot2::geom_boxplot(data= whole_df_MCMC[c(((param_rep-1)*500010+1):(param_rep*500010)),],
                          aes(x = rep-0.2,y = lac_mcmc, color = "MCMC",fill = "MCMC"),
                          alpha = 0.6,width = 0.2,outlier.shape = NA)+ #outlier.shape = NA
    # ggplot2::geom_point(data = whole_df2[c(1:100),],aes(y = lac_mcmc, color = "MCMC"),size = 2)+
    # ggplot2::geom_point(aes(y = lac_mcmc, color = "MCMC"))+
    ggplot2::geom_point(data= whole_df_MLE[c(((param_rep-1)*10+1):(param_rep*10)),],
                        aes(x = rep, y = lac_MLE),color = "black",shape = 17,size = 1.5) +
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
    ggplot2::geom_hline(data= whole_df_MLE[c(((param_rep-1)*10+1):(param_rep*10)),],
                        aes(yintercept = lac), linetype = "dashed", size = 0.5)
  print(g1)
  while (!is.null(dev.list()))  dev.off()
}



for(param_rep in 1:10){
  tiff(paste0("G:/results/project 2/tip_info/round2/less_sd/boxplot_mu_rep",param_rep,".tiff"), units="px", width=700, height=400)
  g2 <- ggplot2::ggplot(whole_df_ABC[c(((param_rep+9)*5000+1):((param_rep+10)*5000)),],
                        aes(x = rep,y = mu_abc, group = rep)) +
    ggplot2::theme_bw() +
    ggplot2::geom_boxplot(aes(x = rep+0.2, y = mu_abc, color = "ABC",fill = "ABC"),
                          alpha = 0.7,width = 0.2,outlier.shape = NA)+ ##aes(x = lac, y = lac_abc, color = "ABC")
    ggplot2::geom_boxplot(data= whole_df_MCMC[c(((param_rep+9)*500010+1):((param_rep+10)*500010)),],
                          aes(x = rep-0.2,y = mu_mcmc, color = "MCMC",fill = "MCMC"),
                          alpha = 0.6,width = 0.2,outlier.shape = NA)+
    ggplot2::geom_point(data= whole_df_MLE[c(((param_rep+9)*10+1):((param_rep+10)*10)),],
                        aes(x = rep, y = mu_MLE),color = "black",shape = 17,size = 1.5) +
    labs(x = "Replicate",
         y = "Estimated rate",
         color = "Methods",
         fill = "Methods") +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    ggplot2::ggtitle("Extinction") +
    scale_x_continuous(breaks=seq(1,10,1))+
    ylim(0,2.0)+
    # ggtitle("Extinction")+
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::geom_hline(data= whole_df_MLE[c(((param_rep+9)*10+1):((param_rep+10)*10)),],
                        aes(yintercept = mu), linetype = "dashed", size = 0.5)
  print(g2)
  while (!is.null(dev.list()))  dev.off()
}



for(param_rep in 1:10){
  tiff(paste0("G:/results/project 2/tip_info/round2/less_sd/boxplot_gam_rep",param_rep,".tiff"), units="px", width=700, height=400)
  g3 <- ggplot2::ggplot(whole_df_ABC[c(((param_rep+19)*5000+1):((param_rep+20)*5000)),],
                        aes(x = rep,y = gam_abc, group = rep)) +
    ggplot2::theme_bw() +
    ggplot2::geom_boxplot(aes(x = rep+0.2, y = gam_abc, color = "ABC",fill = "ABC"),
                          alpha = 0.7,width = 0.2,outlier.shape = NA)+ ##aes(x = lac, y = lac_abc, color = "ABC")
    ggplot2::geom_boxplot(data= whole_df_MCMC[c(((param_rep+19)*500010+1):((param_rep+20)*500010)),],
                          aes(x = rep-0.2,y = gam_mcmc, color = "MCMC",fill = "MCMC"),
                          alpha = 0.6,width = 0.2,outlier.shape = NA)+
    ggplot2::geom_point(data= whole_df_MLE[c(((param_rep+19)*10+1):((param_rep+20)*10)),],
                        aes(x = rep, y = gam_MLE),color = "black",shape = 17,size = 1.5) +
    labs(x = "Replicate",
         y = "Estimated rate",
         color = "Methods",
         fill = "Methods") +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    ggplot2::ggtitle("Colonization") +
    scale_x_continuous(breaks=seq(1,10,1))+
    ylim(0,0.05)+
    # ggtitle("Colonization")+
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::geom_hline(data= whole_df_MLE[c(((param_rep+19)*10+1):((param_rep+20)*10)),],
                        aes(yintercept = gam), linetype = "dashed", size = 0.5)
  print(g3)
  while (!is.null(dev.list()))  dev.off()
}




for(param_rep in 1:10){
  tiff(paste0("G:/results/project 2/tip_info/round2/less_sd/boxplot_laa_rep",param_rep,".tiff"), units="px", width=700, height=400)
  g4 <- ggplot2::ggplot(whole_df_ABC[c(((param_rep+29)*5000+1):((param_rep+30)*5000)),],
                        aes(x = rep,y = laa_abc, group = rep)) +
    ggplot2::theme_bw() +
    ggplot2::geom_boxplot(aes(x = rep+0.2, y = laa_abc, color = "ABC",fill = "ABC"),
                          alpha = 0.7,width = 0.2,outlier.shape = NA)+ ##aes(x = lac, y = lac_abc, color = "ABC")
    ggplot2::geom_boxplot(data= whole_df_MCMC[c(((param_rep+29)*500010+1):((param_rep+30)*500010)),],
                          aes(x = rep-0.2,y = laa_mcmc, color = "MCMC",fill = "MCMC"),
                          alpha = 0.6,width = 0.2,outlier.shape = NA)+
    ggplot2::geom_point(data= whole_df_MLE[c(((param_rep+29)*10+1):((param_rep+30)*10)),],
                        aes(x = rep, y = laa_MLE),color = "black",shape = 17,size = 1.5) +
    labs(x = "Replicate",
         y = "Estimated rate",
         color = "Methods",
         fill = "Methods") +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    ggplot2::ggtitle("Anagenesis") +
    scale_x_continuous(breaks=seq(1,10,1))+
    ylim(0,2.0)+
    # ggtitle("Anagenesis")+
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::geom_hline(data= whole_df_MLE[c(((param_rep+29)*10+1):((param_rep+30)*10)),],
                        aes(yintercept = laa), linetype = "dashed", size = 0.5)
  print(g4)
  while (!is.null(dev.list()))  dev.off()
}









