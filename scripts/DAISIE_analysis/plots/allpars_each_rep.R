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
folder_path <- "G:/results/project 2/tip_info/round3/est_allpars/DAISIE_ABC"
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
      lac_abc <- c(lac_abc, output$ABC[[num_iter-1]][,1])  ##num_iter-1
      mu_abc <- c(mu_abc, output$ABC[[num_iter-1]][,2])
      gam_abc <- c(gam_abc, output$ABC[[num_iter-1]][,3])
      laa_abc <- c(laa_abc, output$ABC[[num_iter-1]][,4])
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
save(whole_df_ABC,file = "G:/results/project 2/tip_info/round3/est_allpars/whole_df_ABC.RData")  ## whole_df_ABC_iter1.RData

#### analysis
load("G:/results/project 2/tip_info/round3/est_allpars/whole_df_ABC.RData")
load("G:/results/project 2/tip_info/round2/MCMC_all/whole_df_MCMC.RData")
load("G:/results/project 2/tip_info/round2/MLE_all/whole_df_MLE.RData")


# reps <- subset(whole_df_ABC, lac == 0.5 & mu == 0.5 & gam == 0.02 & laa == 0.5)


library(ggplot2)
colors <- c("MCMC"="red","ABC"="blue3")
#### plot when estimating all pars
#### scenario 1
for(param_rep in 1:10) {
  scen_ABC <- c(((param_rep-1)*5000+1):(param_rep*5000))
  scen_MCMC <- c(((param_rep-1)*500010+1):(param_rep*500010))
  scen_MLE <- c(((param_rep-1)*10+1):(param_rep*10))
  tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_lac1_rep",param_rep,".tiff"), units="px", width=700, height=400)
  g1_lac <- ggplot2::ggplot(whole_df_ABC[scen_ABC,], aes(x = rep,y = lac_abc, group = rep)) +
    ggplot2::theme_bw() +
    ggplot2::geom_boxplot(aes(x = rep+0.2, y = lac_abc, color = "ABC",fill = "ABC"),
                          alpha = 0.7,outlier.shape = NA,width = 0.2)+
    ggplot2::geom_boxplot(data= whole_df_MCMC[scen_MCMC,],
                          aes(x = rep-0.2,y = lac_mcmc, color = "MCMC",fill = "MCMC"),
                          alpha = 0.6,outlier.shape = NA,width = 0.2)+
    ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],
                        aes(x = rep, y = lac_MLE),color = "black",shape = 17,size = 1.5) +
    labs(x = "Replicate",
         y = "Estimated cladogenesis",
         color = "Methods",
         fill = "Methods") +
    scale_color_manual(values = colors)+
    scale_fill_manual(values = colors) +
    # ggplot2::ggtitle("Cladogenesis") +
    scale_x_continuous(breaks=seq(1,10,1))+
    ylim(0,2.0)+
    # ggtitle("Cladogenesis")+
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::geom_hline(data= whole_df_MLE[scen_MLE,],
                        aes(yintercept = lac), linetype = "dashed", size = 0.5)
  print(g1_lac)
  while (!is.null(dev.list()))  dev.off()
}


for(param_rep in 1:10) {
  scen_ABC <- c(((param_rep-1)*5000+1):(param_rep*5000))
  scen_MCMC <- c(((param_rep-1)*500010+1):(param_rep*500010))
  scen_MLE <- c(((param_rep-1)*10+1):(param_rep*10))
  tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_mu1_rep",param_rep,".tiff"), units="px", width=700, height=400)
  g1_mu <- ggplot2::ggplot(whole_df_ABC[scen_ABC,], aes(x = rep,y = mu_abc, group = rep)) +
    ggplot2::theme_bw() +
    ggplot2::geom_boxplot(aes(x = rep+0.2, y = mu_abc, color = "ABC",fill = "ABC"),
                          alpha = 0.7,outlier.shape = NA,width = 0.2)+
    ggplot2::geom_boxplot(data= whole_df_MCMC[scen_MCMC,],
                          aes(x = rep-0.2,y = mu_mcmc, color = "MCMC",fill = "MCMC"),
                          alpha = 0.6,outlier.shape = NA,width = 0.2)+
    ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],aes(x = rep, y = mu_MLE),
                        color = "black",shape = 17,size = 1.5) +
    labs(x = "Replicate",
         y = "Estimated extinction",
         color = "Methods",
         fill = "Methods") +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_x_continuous(breaks=seq(1,10,1))+
    # ggplot2::ggtitle("Extinction") +
    ylim(0,2.0)+
    # ggtitle("Extinction")+
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::geom_hline(yintercept = 0.00001, linetype = "dashed", size = 0.5)
  print(g1_mu)
  while (!is.null(dev.list()))  dev.off()
}


for(param_rep in 1:10) {
  scen_ABC <- c(((param_rep-1)*5000+1):(param_rep*5000))
  scen_MCMC <- c(((param_rep-1)*500010+1):(param_rep*500010))
  scen_MLE <- c(((param_rep-1)*10+1):(param_rep*10))
  tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_gam1_rep",param_rep,".tiff"), units="px", width=700, height=400)
  g1_gam <- ggplot2::ggplot(whole_df_ABC[scen_ABC,], aes(x = rep,y = gam_abc, group = rep)) +
    ggplot2::theme_bw() +
    ggplot2::geom_boxplot(aes(x = rep+0.2, y = gam_abc, color = "ABC",fill = "ABC"),
                          alpha = 0.7,outlier.shape = NA,width = 0.2)+
    ggplot2::geom_boxplot(data= whole_df_MCMC[scen_MCMC,],
                          aes(x = rep-0.2,y = gam_mcmc, color = "MCMC",fill = "MCMC"),
                          alpha = 0.6,outlier.shape = NA,width = 0.2)+
    ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],aes(x = rep, y = gam_MLE),color = "black",shape = 17,size = 1.5) +
    labs(x = "Replicate",
         y = "Estimated colonization",
         color = "Methods",
         fill = "Methods") +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    # ggplot2::ggtitle("Colonization") +
    ylim(0,0.05)+
    scale_x_continuous(breaks=seq(1,10,1))+
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::geom_hline(yintercept = 0.02, linetype = "dashed", size = 0.5)
  print(g1_gam)
  while (!is.null(dev.list()))  dev.off()
}


for(param_rep in 1:10) {
  scen_ABC <- c(((param_rep-1)*5000+1):(param_rep*5000))
  scen_MCMC <- c(((param_rep-1)*500010+1):(param_rep*500010))
  scen_MLE <- c(((param_rep-1)*10+1):(param_rep*10))
  tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_laa1_rep",param_rep,".tiff"), units="px", width=700, height=400)
  g1_laa <- ggplot2::ggplot(whole_df_ABC[scen_ABC,], aes(x = rep,y = laa_abc, group = rep)) +
    ggplot2::theme_bw() +
    ggplot2::geom_boxplot(aes(x = rep+0.2, y = laa_abc, color = "ABC",fill = "ABC"),
                          alpha = 0.7,outlier.shape = NA,width = 0.2)+
    ggplot2::geom_boxplot(data= whole_df_MCMC[scen_MCMC,],
                          aes(x = rep-0.2,y = laa_mcmc, color = "MCMC",fill = "MCMC"),
                          alpha = 0.6,outlier.shape = NA,width = 0.2)+
    ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],aes(x = rep, y = laa_MLE),color = "black",shape = 17,size = 1.5) +
    labs(x = "Replicate",
         y = "Estimated anagenesis",
         color = "Methods",
         fill = "Methods") +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    # ggplot2::ggtitle("Anagenesis") +
    ylim(0,2.0)+
    scale_x_continuous(breaks=seq(1,10,1))+
    # ggtitle("Anagenesis")+
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", size = 0.5)
  print(g1_laa)
  while (!is.null(dev.list()))  dev.off()
}


############################################################################
#### scenario 2

scen_ABC <- c(((param_rep+9)*5000+1):((param_rep+10)*5000))
scen_MCMC <- c(((param_rep+9)*500010+1):((param_rep+10)*500010))
scen_MLE <- c(((param_rep+9)*10+1):((param_rep+10)*10))
tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_lac2_rep",param_rep,".tiff"), units="px", width=700, height=400)
g2_lac <- ggplot2::ggplot(whole_df_ABC[scen_ABC,],
                          aes(x = rep,y = lac_abc, group = rep)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = rep, y = lac_abc, color = "ABC",fill = "ABC"),
                        alpha = 0.7,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_boxplot(data= whole_df_ABC[scen_MCMC,],
                        aes(x = rep,y = lac_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.6,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],
                      aes(x = rep, y = lac_MLE),color = "black",shape = 17,size = 1.5) +
  labs(x = "Replicate",
       y = "Estimated cladogenesis",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Cladogenesis") +
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
  ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", size = 0.5)
print(g2_lac)
while (!is.null(dev.list()))  dev.off()


scen_ABC <- c(((param_rep+9)*5000+1):((param_rep+10)*5000))
scen_MCMC <- c(((param_rep+9)*500010+1):((param_rep+10)*500010))
scen_MLE <- c(((param_rep+9)*10+1):((param_rep+10)*10))
tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_mu2_rep",param_rep,".tiff"), units="px", width=700, height=400)
g2_mu <- ggplot2::ggplot(whole_df_ABC[scen_ABC,], aes(x = rep,y = mu_abc, group = rep)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = rep, y = mu_abc, color = "ABC",fill = "ABC"),
                        alpha = 0.7,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_boxplot(data= whole_df_ABC[scen_MCMC,],
                        aes(x = rep,y = mu_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.6,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],aes(x = rep, y = mu_MLE),color = "black",shape = 17,size = 1.5) +
  labs(x = "Replicate",
       y = "Estimated extinction",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Extinction") +
  ylim(0,2.0)+
  scale_x_continuous(breaks=seq(1,10,1))+
  # ggtitle("Extinction")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::geom_hline(data= whole_df_MLE[scen_MLE,],
                      aes(yintercept = mu), linetype = "dashed", size = 0.5)
print(g2_mu)
while (!is.null(dev.list()))  dev.off()


scen_ABC <- c(((param_rep+9)*5000+1):((param_rep+10)*5000))
scen_MCMC <- c(((param_rep+9)*500010+1):((param_rep+10)*500010))
scen_MLE <- c(((param_rep+9)*10+1):((param_rep+10)*10))
tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_gam2_rep",param_rep,".tiff"), units="px", width=700, height=400)
g2_gam <- ggplot2::ggplot(whole_df_ABC[scen_ABC,], aes(x = rep,y = gam_abc, group = rep)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = rep, y = gam_abc, color = "ABC",fill = "ABC"),
                        alpha = 0.7,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_boxplot(data= whole_df_ABC[scen_MCMC,],
                        aes(x = rep,y = gam_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.6,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],aes(x = rep, y = gam_MLE),color = "black",shape = 17,size = 1.5) +
  labs(x = "Replicate",
       y = "Estimated colonization",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Colonization") +
  ylim(0,0.05)+
  scale_x_continuous(breaks=seq(1,10,1))+
  # ggtitle("Colonization")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::geom_hline(yintercept = 0.02, linetype = "dashed", size = 0.5)
print(g2_gam)
while (!is.null(dev.list()))  dev.off()


scen_ABC <- c(((param_rep+9)*5000+1):((param_rep+10)*5000))
scen_MCMC <- c(((param_rep+9)*500010+1):((param_rep+10)*500010))
scen_MLE <- c(((param_rep+9)*10+1):((param_rep+10)*10))
tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_laa2_rep",param_rep,".tiff"), units="px", width=700, height=400)
g2_laa <- ggplot2::ggplot(whole_df_ABC[scen_ABC,], aes(x = rep,y = laa_abc, group = rep)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = rep, y = laa_abc, color = "ABC",fill = "ABC"),
                        alpha = 0.7,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_boxplot(data= whole_df_ABC[scen_MCMC,],
                        aes(x = rep,y = laa_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.6,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],aes(x = rep, y = laa_MLE),color = "black",shape = 17,size = 1.5) +
  labs(x = "Replicate",
       y = "Estimated anagenesis",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Anagenesis") +
  ylim(0,2.0)+
  scale_x_continuous(breaks=seq(1,10,1))+
  # ggtitle("Anagenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", size = 0.5)
print(g2_laa)
while (!is.null(dev.list()))  dev.off()

############################################################################
#### scenario 3

scen_ABC <- c(((param_rep+19)*5000+1):((param_rep+20)*5000))
scen_MCMC <- c(((param_rep+19)*500010+1):((param_rep+20)*500010))
scen_MLE <- c(((param_rep+19)*10+1):((param_rep+20)*10))
tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_lac3_rep",param_rep,".tiff"), units="px", width=700, height=400)
g3_lac <- ggplot2::ggplot(whole_df_ABC[scen_ABC,],
                          aes(x = rep,y = lac_abc, group = rep)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = rep, y = lac_abc, color = "ABC",fill = "ABC"),
                        alpha = 0.7,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_boxplot(data= whole_df_MCMC[scen_MCMC,],
                        aes(x = rep,y = lac_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.6,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],
                      aes(x = rep, y = lac_MLE),color = "black",shape = 17,size = 1.5) +
  labs(x = "Replicate",
       y = "Estimated cladogenesis",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Cladogenesis") +
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
  ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", size = 0.5)
print(g3_lac)
while (!is.null(dev.list()))  dev.off()


scen_ABC <- c(((param_rep+19)*5000+1):((param_rep+20)*5000))
scen_MCMC <- c(((param_rep+19)*500010+1):((param_rep+20)*500010))
scen_MLE <- c(((param_rep+19)*10+1):((param_rep+20)*10))
tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_mu3_rep",param_rep,".tiff"), units="px", width=700, height=400)
g3_mu <- ggplot2::ggplot(whole_df_ABC[scen_ABC,], aes(x = rep,y = mu_abc, group = rep)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = rep, y = mu_abc, color = "ABC",fill = "ABC"),
                        alpha = 0.7,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_boxplot(data= whole_df_MCMC[scen_MCMC,],
                        aes(x = rep,y = mu_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.6,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],aes(x = rep, y = mu_MLE),color = "black",shape = 17,size = 1.5) +
  labs(x = "Replicate",
       y = "Estimated extinction",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Extinction") +
  ylim(0,2.0)+
  scale_x_continuous(breaks=seq(1,10,1))+
  # ggtitle("Extinction")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::geom_hline(yintercept = 0.00001, linetype = "dashed", size = 0.5)
print(g3_mu)
while (!is.null(dev.list()))  dev.off()


scen_ABC <- c(((param_rep+19)*5000+1):((param_rep+20)*5000))
scen_MCMC <- c(((param_rep+19)*500010+1):((param_rep+20)*500010))
scen_MLE <- c(((param_rep+19)*10+1):((param_rep+20)*10))
tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_gam3_rep",param_rep,".tiff"), units="px", width=700, height=400)
g3_gam <- ggplot2::ggplot(whole_df_ABC[scen_ABC,], aes(x = rep,y = gam_abc, group = rep)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = rep, y = gam_abc, color = "ABC",fill = "ABC"),
                        alpha = 0.7,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_boxplot(data= whole_df_MCMC[scen_MCMC,],
                        aes(x = rep,y = gam_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.6,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],aes(x = rep, y = gam_MLE),color = "black",shape = 17,size = 1.5) +
  labs(x = "Replicate",
       y = "Estimated colonization",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Colonization") +
  ylim(0,0.05)+
  scale_x_continuous(breaks=seq(1,10,1))+
  # ggtitle("Colonization")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::geom_hline(data= whole_df_MLE[scen_MLE,],
                      aes(yintercept = gam), linetype = "dashed", size = 0.5)
print(g3_gam)
while (!is.null(dev.list()))  dev.off()


scen_ABC <- c(((param_rep+19)*5000+1):((param_rep+20)*5000))
scen_MCMC <- c(((param_rep+19)*500010+1):((param_rep+20)*500010))
scen_MLE <- c(((param_rep+19)*10+1):((param_rep+20)*10))
tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_laa3_rep",param_rep,".tiff"), units="px", width=700, height=400)
g3_laa <- ggplot2::ggplot(whole_df_ABC[scen_ABC,], aes(x = rep,y = laa_abc, group = rep)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = rep, y = laa_abc, color = "ABC",fill = "ABC"),
                        alpha = 0.7,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_boxplot(data= whole_df_MCMC[scen_MCMC,],
                        aes(x = rep,y = laa_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.6,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],aes(x = rep, y = laa_MLE),color = "black",shape = 17,size = 1.5) +
  labs(x = "Replicate",
       y = "Estimated anagenesis",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Anagenesis") +
  ylim(0,2.0)+
  scale_x_continuous(breaks=seq(1,10,1))+
  # ggtitle("Anagenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", size = 0.5)
print(g3_laa)
while (!is.null(dev.list()))  dev.off()


############################################################################
#### scenario 4

scen_ABC <- c(((param_rep+29)*5000+1):((param_rep+30)*5000))
scen_MCMC <- c(((param_rep+29)*500010+1):((param_rep+30)*500010))
scen_MLE <- c(((param_rep+29)*10+1):((param_rep+30)*10))
tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_lac4_rep",param_rep,".tiff"), units="px", width=700, height=400)
g4_lac <- ggplot2::ggplot(whole_df_ABC[scen_ABC,],
                          aes(x = rep,y = lac_abc, group = rep)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = rep, y = lac_abc, color = "ABC",fill = "ABC"),
                        alpha = 0.7,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_boxplot(data= whole_df_MCMC[scen_MCMC,],
                        aes(x = rep,y = lac_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.6,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],
                      aes(x = lac, y = lac_MLE),color = "black",shape = 17,size = 1.5) +
  labs(x = "Replicate",
       y = "Estimated cladogenesis",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Cladogenesis") +
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
  ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", size = 0.5)
print(g4_lac)
while (!is.null(dev.list()))  dev.off()


scen_ABC <- c(((param_rep+29)*5000+1):((param_rep+30)*5000))
scen_MCMC <- c(((param_rep+29)*500010+1):((param_rep+30)*500010))
scen_MLE <- c(((param_rep+29)*10+1):((param_rep+30)*10))
tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_mu4_rep",param_rep,".tiff"), units="px", width=700, height=400)
g4_mu <- ggplot2::ggplot(whole_df_ABC[scen_ABC,], aes(x = rep,y = mu_abc, group = rep)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = rep, y = mu_abc, color = "ABC",fill = "ABC"),
                        alpha = 0.7,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_boxplot(data= whole_df_MCMC[scen_MCMC,],
                        aes(x = rep,y = mu_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.6,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],aes(x = rep, y = mu_MLE),color = "black",shape = 17,size = 1.5) +
  labs(x = "Replicate",
       y = "Estimated extinction",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Extinction") +
  ylim(0,2.0)+
  scale_x_continuous(breaks=seq(1,10,1))+
  # ggtitle("Extinction")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::geom_hline(yintercept = 0.00001, linetype = "dashed", size = 0.5)
print(g4_mu)
while (!is.null(dev.list()))  dev.off()


scen_ABC <- c(((param_rep+29)*5000+1):((param_rep+30)*5000))
scen_MCMC <- c(((param_rep+29)*500010+1):((param_rep+30)*500010))
scen_MLE <- c(((param_rep+29)*10+1):((param_rep+30)*10))
tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_gam4_rep",param_rep,".tiff"), units="px", width=700, height=400)
g4_gam <- ggplot2::ggplot(whole_df_ABC[scen_ABC,], aes(x = rep,y = gam_abc, group = rep)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = rep, y = gam_abc, color = "ABC",fill = "ABC"),
                        alpha = 0.7,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_boxplot(data= whole_df_MCMC[scen_MCMC,],
                        aes(x = rep,y = gam_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.6,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],aes(x = rep, y = gam_MLE),color = "black",shape = 17,size = 1.5) +
  labs(x = "Replicate",
       y = "Estimated colonization",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Colonization") +
  ylim(0,0.05)+
  scale_x_continuous(breaks=seq(1,10,1))+
  # ggtitle("Colonization")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::geom_hline(yintercept = 0.02, linetype = "dashed", size = 0.5)
print(g4_gam)
while (!is.null(dev.list()))  dev.off()


scen_ABC <- c(((param_rep+29)*5000+1):((param_rep+30)*5000))
scen_MCMC <- c(((param_rep+29)*500010+1):((param_rep+30)*500010))
scen_MLE <- c(((param_rep+29)*10+1):((param_rep+30)*10))
tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/boxplot_laa4_rep",param_rep,".tiff"), units="px", width=700, height=400)
g4_laa <- ggplot2::ggplot(whole_df_ABC[scen_ABC,], aes(x = rep,y = laa_abc, group = rep)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(aes(x = rep, y = laa_abc, color = "ABC",fill = "ABC"),
                        alpha = 0.7,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_boxplot(data= whole_df_MCMC[scen_MCMC,],
                        aes(x = rep,y = laa_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 0.6,outlier.shape = NA,width = 0.2)+
  ggplot2::geom_point(data= whole_df_MLE[scen_MLE,],aes(x = rep, y = laa_MLE),color = "black",shape = 17,size = 1.5) +
  labs(x = "Replicate",
       y = "Estimated anagenesis",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  # ggplot2::ggtitle("Anagenesis") +
  ylim(0,2.0)+
  scale_x_continuous(breaks=seq(1,10,1))+
  # ggtitle("Anagenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black")) +
  ggplot2::geom_hline(data= whole_df_MLE[scen_MLE,],
                      aes(yintercept = laa), linetype = "dashed", size = 0.5)
print(g4_laa)
while (!is.null(dev.list()))  dev.off()

































