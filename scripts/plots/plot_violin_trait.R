### violint plot for TraiSIE
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


folder_path <- "G:/results/project 2/tip_info/round3/TRAISIE_DD/TraiSIE_ABC_DD"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/TraiSIE_ABC_DD.csv")

param_data2<-param_data[rep(seq_len(nrow(param_data)), each=200),]

lac_abc1 <- c()
mu_abc1 <- c()
gam_abc1 <- c()
laa_abc1 <- c()
lac_abc2 <- c()
mu_abc2 <- c()
gam_abc2 <- c()
laa_abc2 <- c()
trans_abc1 <- c()
trans_abc2 <- c()
n_iter <-c()
for(i in 1:240){
  # if(i%%5 == 0){
  #   rep <- 5
  # } else {
  #   rep <- i%%5
  # }
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("TraiSIE_ABC_DI_param_set_", i,".RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    if(output$n_iter <= 2){
      lac_abc1 <- c(lac_abc1, rep(NA,200))
      mu_abc1 <- c(mu_abc1, rep(NA,200))
      gam_abc1 <- c(gam_abc1, rep(NA,200))
      laa_abc1 <- c(laa_abc1, rep(NA,200))
      lac_abc2 <- c(lac_abc2, rep(NA,200))
      mu_abc2 <- c(mu_abc2, rep(NA,200))
      gam_abc2 <- c(gam_abc2, rep(NA,200))
      laa_abc2 <- c(laa_abc2, rep(NA,200))
      trans_abc1 <- c(trans_abc1, rep(NA,200))
      trans_abc2 <- c(trans_abc2, rep(NA,200))
    } else{
      lac_abc1 <- c(lac_abc1, output$ABC[[5]][,1])
      mu_abc1 <- c(mu_abc1, output$ABC[[5]][,2])
      gam_abc1 <- c(gam_abc1, output$ABC[[5]][,3])
      laa_abc1 <- c(laa_abc1, output$ABC[[5]][,4])
      lac_abc2 <- c(lac_abc2, output$ABC[[5]][,5])
      mu_abc2 <- c(mu_abc2, output$ABC[[5]][,6])
      gam_abc2 <- c(gam_abc2, output$ABC[[5]][,7])
      laa_abc2 <- c(laa_abc2, output$ABC[[5]][,8])
      trans_abc1 <- c(trans_abc1, output$ABC[[5]][,9])
      trans_abc2 <- c(trans_abc2, output$ABC[[5]][,10])
    }
  } else {
    lac_abc1 <- c(lac_abc1, rep(NA,200))
    mu_abc1 <- c(mu_abc1, rep(NA,200))
    gam_abc1 <- c(gam_abc1, rep(NA,200))
    laa_abc1 <- c(laa_abc1, rep(NA,200))
    lac_abc2 <- c(lac_abc2, rep(NA,200))
    mu_abc2 <- c(mu_abc2, rep(NA,200))
    gam_abc2 <- c(gam_abc2, rep(NA,200))
    laa_abc2 <- c(laa_abc2, rep(NA,200))
    trans_abc1 <- c(trans_abc1, rep(NA,200))
    trans_abc2 <- c(trans_abc2, rep(NA,200))
  }
}


whole_df_ABC <- data.frame(param_data2,
                           # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                           lac_abc1,mu_abc1,gam_abc1,laa_abc1,
                           lac_abc2,mu_abc2,gam_abc2,laa_abc2,
                           trans_abc1,trans_abc2)


#lac_abc,mu_abc,gam_abc,laa_abc,n_iter)
save(whole_df_ABC,file = "G:/results/project 2/tip_info/round3/TRAISIE_DD/DD_whole_df_ABC.RData")

### start to plot
load("G:/results/project 2/tip_info/round3/TRAISIE_DD/DD_whole_df_ABC.RData")
whole_df_ABC$Transition <- whole_df_ABC$trans
whole_df_ABC$Transition[whole_df_ABC$trans == "0.02" & whole_df_ABC$trans2 == "0.02"] <- "ll"
whole_df_ABC$Transition[whole_df_ABC$trans == "0.02" & whole_df_ABC$trans2 == "0.2"] <- "lh"
whole_df_ABC$Transition[whole_df_ABC$trans == "0.2" & whole_df_ABC$trans2 == "0.02"] <- "hl"

library(ggplot2)
colors <- c("State1"="red","State2"="blue")
g1 <- ggplot2::ggplot(whole_df_ABC[c(0:12000),], aes(group = lac)) +
  ggplot2::theme_bw() +
  ggplot2::geom_violin(aes(x = lac, y = lac_abc1, color = "State1",fill = "State1"),alpha = 0.5)+ ##aes(x = lac, y = lac_abc, color = "ABC")
  ggplot2::geom_violin(aes(x = lac2, y = lac_abc2, color = "State2",fill = "State2"),alpha = 0.5)+
  # ggplot2::geom_point(data = whole_df2[c(1:100),],aes(y = lac_mcmc, color = "MCMC"),size = 2)+
  # ggplot2::geom_point(aes(y = lac_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(x = lac, y = lac),color = "red3",size = 3) +
  ggplot2::geom_point(aes(x = lac2, y = lac2),color = "blue3",size = 3) +
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "State",
       fill = "State") +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Cladogenesis") +
  xlim(0,1.1)+
  ylim(0,2.0)+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
g1


g2 <- ggplot2::ggplot(whole_df_ABC[c(50001:100000),], aes(group = mu)) +
  ggplot2::theme_bw() +
  ggplot2::geom_violin(aes(x = mu, y = mu_abc1, color = "State1",fill = "State1"),alpha = 0.5)+ ##aes(x = mu, y = mu_abc, color = "ABC")
  ggplot2::geom_violin(aes(x = mu2, y = mu_abc2, color = "State2",fill = "State2"),alpha = 0.5)+
  # ggplot2::geom_point(data = whole_df2[c(1:100),],aes(y = mu_mcmc, color = "MCMC"),size = 2)+
  # ggplot2::geom_point(aes(y = mu_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(x = mu, y = mu),color = "red3",size = 3) +
  ggplot2::geom_point(aes(x = mu2, y = mu2),color = "blue3",size = 3) +
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "State",
       fill = "State") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Extinction") +
  xlim(0,1.1)+
  ylim(0,2.0)+
  # ggtitle("Extinction")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
g2

g3 <- ggplot2::ggplot(whole_df_ABC[c(100001:150000),], aes(group = gam)) +
  ggplot2::theme_bw() +
  ggplot2::geom_violin(aes(x = gam, y = gam_abc1, color = "State1",fill = "State1"),alpha = 0.5)+ ##aes(x = gam, y = gam_abc, color = "ABC")
  ggplot2::geom_violin(aes(x = gam2, y = gam_abc2, color = "State2",fill = "State2"),alpha = 0.5)+
  # ggplot2::geom_point(data = whole_df2[c(1:100),],aes(y = gam_mcmc, color = "MCMC"),size = 2)+
  # ggplot2::geom_point(aes(y = gam_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(x = gam, y = gam),color = "red3",size = 3) +
  ggplot2::geom_point(aes(x = gam2, y = gam2),color = "blue3",size = 3) +
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "State",
       fill = "State") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Colonization") +
  xlim(0.010,0.032)+
  ylim(0,0.05)+
  # ggtitle("Colonization")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
g3

g4 <- ggplot2::ggplot(whole_df_ABC[c(150001:200000),], aes(group = laa)) +
  ggplot2::theme_bw() +
  ggplot2::geom_violin(aes(x = laa, y = laa_abc1, color = "State1",fill = "State1"),alpha = 0.5)+ ##aes(x = laa, y = laa_abc, color = "ABC")
  ggplot2::geom_violin(aes(x = laa2, y = laa_abc2, color = "State2",fill = "State2"),alpha = 0.5)+
  # ggplot2::geom_point(data = whole_df2[c(1:100),],aes(y = laa_mcmc, color = "MCMC"),size = 2)+
  # ggplot2::geom_point(aes(y = laa_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(x = laa, y = laa),color = "red3",size = 3) +
  ggplot2::geom_point(aes(x = laa2, y = laa2),color = "blue3",size = 3) +
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "State",
       fill = "State") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Anagenesis") +
  xlim(0,1.1)+
  ylim(0,2.0)+
  # ggtitle("Anagenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
g4





