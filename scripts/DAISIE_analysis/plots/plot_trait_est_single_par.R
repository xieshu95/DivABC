### analysis for TraiSIE ABC results
folder_path <- "G:/results/project 2/tip_info/trait_nltt_message/TraiSIE_ABC_DD"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/TraiSIE_ABC_DD.csv")

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
for(i in 1:200){
  # if(i%%5 == 0){
  #   rep <- 5
  # } else {
  #   rep <- i%%5
  # }
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("TraiSIE_ABC_DD_param_set_", i,".RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lac_abc1[i] <- median(output$ABC[,1])
    mu_abc1[i] <- median(output$ABC[,2])
    gam_abc1[i] <- median(output$ABC[,3])
    laa_abc1[i] <- median(output$ABC[,4])
    lac_abc2[i] <- median(output$ABC[,5])
    mu_abc2[i] <- median(output$ABC[,6])
    gam_abc2[i] <- median(output$ABC[,7])
    laa_abc2[i] <- median(output$ABC[,8])
    trans_abc1[i] <- median(output$ABC[,9])
    trans_abc2[i] <- median(output$ABC[,10])
    n_iter[i] <- output$n_iter
  } else {
    lac_abc1[i] <- NA
    mu_abc1[i] <- NA
    gam_abc1[i] <- NA
    laa_abc1[i] <- NA
    lac_abc2[i] <- NA
    mu_abc2[i] <- NA
    gam_abc2[i] <- NA
    laa_abc2[i] <- NA
    trans_abc1[i] <- NA
    trans_abc2[i] <- NA
    n_iter[i] <- NA
  }
}


# folder_path <- "G:/results/project 2/tip_info/all_6ss/MCMC_single_par/DAISIE_MCMC"
# files <- list.files(folder_path)
# lac_mcmc <- c()
# mu_mcmc <- c()
# gam_mcmc <- c()
# laa_mcmc <- c()
# for(i in 1:400){
#   if(i%%5 == 0){
#     rep <- 5
#   } else {
#     rep <- i%%5
#   }
#   # param_set = (param_num-1)*5 + i
#   file_to_load <- grep(paste0("DAISIE_MCMC_param_set_", i,".RData"), #"_rep",rep,
#                        files,
#                        value = TRUE,
#                        fixed = TRUE)
#
#   if (!identical(file_to_load, character())) {
#     load(file.path(folder_path, file_to_load))
#     lac_mcmc[i] <- median(output[,1])
#     mu_mcmc[i] <- median(output[,2])
#     gam_mcmc[i] <- median(output[,3])
#     laa_mcmc[i] <- median(output[,4])
#   } else {
#     lac_mcmc[i] <- NA
#     mu_mcmc[i] <- NA
#     gam_mcmc[i] <- NA
#     laa_mcmc[i] <- NA
#   }
# }
#
whole_df <- data.frame(param_data,
                       lac_abc1,mu_abc1,gam_abc1,laa_abc1,
                       lac_abc2,mu_abc2,gam_abc2,laa_abc2,
                       trans_abc1,trans_abc2,n_iter) #,lac_abc,mu_abc,gam_abc,laa_abc
lac_whole <- whole_df[c(1:50),]
mu_whole <- whole_df[c(51:100),]
gam_whole <- whole_df[c(101:150),]
laa_whole <- whole_df[c(151:200),]


library(ggplot2)
colors <- c("State1"="red","State2"="blue")
g1 <- ggplot2::ggplot(lac_whole) +
  ggplot2::geom_point(aes(x = lac, y = lac_abc1, color = "State1"))+
  ggplot2::geom_point(aes(x = lac2, y = lac_abc2, color = "State2"))+
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "Trait state") +
  scale_color_manual(values = colors)+
  ggplot2::ggtitle("Cladogenesis") +
  xlim(0,1.0)+
  ylim(0,2.0)+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
g1


g2 <- ggplot2::ggplot(mu_whole) +
  ggplot2::geom_point(aes(x = mu, y = mu_abc1, color = "State1"))+
  ggplot2::geom_point(aes(x = mu2, y = mu_abc2, color = "State2"))+
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "Trait state") +
  scale_color_manual(values = colors)+
  ggplot2::ggtitle("Extinction") +
  xlim(0,1.0)+
  ylim(0,2.0)+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
g2

g3 <- ggplot2::ggplot(gam_whole) +
  ggplot2::geom_point(aes(x = gam, y = gam_abc1, color = "State1"))+
  ggplot2::geom_point(aes(x = gam2, y = gam_abc2, color = "State2"))+
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "Trait state") +
  scale_color_manual(values = colors)+
  ggplot2::ggtitle("Colonization") +
  xlim(0.012,0.030)+
  ylim(0,0.05)+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
g3

g4 <- ggplot2::ggplot(laa_whole) +
  ggplot2::geom_point(aes(x = laa, y = laa_abc1, color = "State1"))+
  ggplot2::geom_point(aes(x = laa2, y = laa_abc2, color = "State2"))+
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "Trait state") +
  scale_color_manual(values = colors)+
  ggplot2::ggtitle("Anagenesis") +
  xlim(0,1.0)+
  ylim(0,2.0)+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
g4