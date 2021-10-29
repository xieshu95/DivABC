### analysis for optimize all the parameters
folder_path <- "G:/results/project 2/opt_all_new/ABC"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")

lac_abc <- c()
mu_abc <- c()
gam_abc <- c()
laa_abc <- c()
for(i in 1:800){
  if(i%%5 == 0){
    rep <- 5
  } else {
    rep <- i%%5
  }
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("DAISIE_ABC_param_set_", i,".RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lac_abc[i] <- median(output[,1])
    mu_abc[i] <- median(output[,2])
    gam_abc[i] <- median(output[,3])
    laa_abc[i] <- median(output[,4])
  } else {
    lac_abc[i] <- NA
    mu_abc[i] <- NA
    gam_abc[i] <- NA
    laa_abc[i] <- NA
  }
}


folder_path <- "G:/results/project 2/opt_all_new/MCMC"
files <- list.files(folder_path)
lac_mcmc <- c()
mu_mcmc <- c()
gam_mcmc <- c()
laa_mcmc <- c()
for(i in 1:800){
  if(i%%5 == 0){
    rep <- 5
  } else {
    rep <- i%%5
  }
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("DAISIE_MCMC_param_set_", i,".RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)

  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lac_mcmc[i] <- median(output[,1])
    mu_mcmc[i] <- median(output[,2])
    gam_mcmc[i] <- median(output[,3])
    laa_mcmc[i] <- median(output[,4])
  } else {
    lac_mcmc[i] <- NA
    mu_mcmc[i] <- NA
    gam_mcmc[i] <- NA
    laa_mcmc[i] <- NA
  }
}

whole_df <- data.frame(param_data,
                       lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,lac_abc,mu_abc,gam_abc,laa_abc) #,lac_abc,mu_abc,gam_abc,laa_abc
lac_whole <- whole_df[c(1:50,201:250,401:450,601:650),]
mu_whole <- whole_df[c(51:100,251:300,451:500,651:700),]
gam_whole <- whole_df[c(101:150,301:350,501:550,701:750),]
laa_whole <- whole_df[c(151:200,351:400,551:600,751:800),]


library(ggplot2)
colors <- c("MCMC"="red","ABC"="blue")
g1 <- ggplot2::ggplot(lac_whole, aes(x = lac)) +
  ggplot2::geom_point(aes(y = lac_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = lac_abc, color = "ABC"))+
  labs(x = "real lac",
       y = "estimated lac",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0,0.5)+
  ylim(0,1)+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
g1


g2 <- ggplot2::ggplot(mu_whole, aes(mu)) +
  ggplot2::geom_point(aes(y = mu_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = mu_abc, color = "ABC"))+
  labs(x = "real mu",
       y = "estimated mu",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0,0.5)+
  ylim(0,1)+
  # ggtitle("Extinction")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
g2

g3 <- ggplot2::ggplot(gam_whole, aes(gam)) +
  ggplot2::geom_point(aes(y = gam_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = gam_abc, color = "ABC"))+
  labs(x = "real gam",
       y = "estimated gam",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0.005,0.015)+
  ylim(0.005,0.05)+
  # ggtitle("Colonization")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
g3

g4 <- ggplot2::ggplot(laa_whole, aes(laa)) +
  ggplot2::geom_point(aes(y = laa_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = laa_abc, color = "ABC"))+
  labs(x = "real laa",
       y = "estimated laa",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0,0.5)+
  ylim(0,1)+
  # ggtitle("Anagenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
g4


### lac_whole
g_lac1 <- ggplot2::ggplot(lac_whole, aes(lac)) +
  ggplot2::geom_point(aes(y = mu_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = mu_abc, color = "ABC"))+
  labs(x = "real lac",
       y = "estimated mu",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0,0.5)+
  ylim(0,1)+
  # ggtitle("exticntion")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_hline(yintercept = 0.2, linetype = "dashed", size = 0.3)
g_lac1


g_lac2 <- ggplot2::ggplot(lac_whole, aes(lac)) +
  ggplot2::geom_point(aes(y = gam_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = gam_abc, color = "ABC"))+
  labs(x = "real lac",
       y = "estimated gam",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0,0.5)+
  ylim(0,0.05)+
  # ggtitle("colonization")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_hline(yintercept = 0.01, linetype = "dashed", size = 0.3)
g_lac2


g_lac3 <- ggplot2::ggplot(lac_whole, aes(lac)) +
  ggplot2::geom_point(aes(y = laa_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = laa_abc, color = "ABC"))+
  labs(x = "real lac",
       y = "estimated laa",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0,0.5)+
  ylim(0,1)+
  # ggtitle("anagenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_hline(yintercept = 0.4, linetype = "dashed", size = 0.3)
g_lac3

### mu_whole
g_mu1 <- ggplot2::ggplot(mu_whole, aes(mu)) +
  ggplot2::geom_point(aes(y = lac_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = lac_abc, color = "ABC"))+
  labs(x = "real mu",
       y = "estimated lac",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0,0.5)+
  ylim(0,1)+
  # ggtitle("cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_hline(yintercept = 0.4, linetype = "dashed", size = 0.3)
g_mu1


g_mu2 <- ggplot2::ggplot(mu_whole, aes(mu)) +
  ggplot2::geom_point(aes(y = gam_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = gam_abc, color = "ABC"))+
  labs(x = "real mu",
       y = "estimated gam",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0,0.5)+
  ylim(0,0.05)+
  # ggtitle("colonization")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_hline(yintercept = 0.01, linetype = "dashed", size = 0.3)
g_mu2


g_mu3 <- ggplot2::ggplot(mu_whole, aes(mu)) +
  ggplot2::geom_point(aes(y = laa_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = laa_abc, color = "ABC"))+
  labs(x = "real mu",
       y = "estimated laa",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0,0.5)+
  ylim(0,1)+
  # ggtitle("anagenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_hline(yintercept = 0.4, linetype = "dashed", size = 0.3)
g_mu3

### gam_whole
g_gam1 <- ggplot2::ggplot(gam_whole, aes(gam)) +
  ggplot2::geom_point(aes(y = lac_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = lac_abc, color = "ABC"))+
  labs(x = "real gam",
       y = "estimated lac",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0.005,0.015)+
  ylim(0,1)+
  # ggtitle("cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_hline(yintercept = 0.4, linetype = "dashed", size = 0.3)
g_gam1


g_gam2 <- ggplot2::ggplot(gam_whole, aes(gam)) +
  ggplot2::geom_point(aes(y = mu_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = mu_abc, color = "ABC"))+
  labs(x = "real gam",
       y = "estimated mu",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0.005,0.015)+
  ylim(0,1)+
  # ggtitle("extinction")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_hline(yintercept = 0.2, linetype = "dashed", size = 0.3)
g_gam2


g_gam3 <- ggplot2::ggplot(gam_whole, aes(gam)) +
  ggplot2::geom_point(aes(y = laa_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = laa_abc, color = "ABC"))+
  labs(x = "real gam",
       y = "estimated laa",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0.005,0.015)+
  ylim(0,1)+
  # ggtitle("anagenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_hline(yintercept = 0.4, linetype = "dashed", size = 0.3)
g_gam3

### laa_whole
g_laa1 <- ggplot2::ggplot(laa_whole, aes(laa)) +
  ggplot2::geom_point(aes(y = mu_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = mu_abc, color = "ABC"))+
  labs(x = "real laa",
       y = "estimated mu",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0,0.5)+
  ylim(0,1)+
  # ggtitle("exticntion")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_hline(yintercept = 0.2, linetype = "dashed", size = 0.3)
g_laa1


g_laa2 <- ggplot2::ggplot(laa_whole, aes(laa)) +
  ggplot2::geom_point(aes(y = gam_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = gam_abc, color = "ABC"))+
  labs(x = "real laa",
       y = "estimated gam",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0,0.5)+
  ylim(0,0.05)+
  # ggtitle("colonization")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_hline(yintercept = 0.01, linetype = "dashed", size = 0.3)
g_laa2


g_laa3 <- ggplot2::ggplot(laa_whole, aes(laa)) +
  ggplot2::geom_point(aes(y = lac_mcmc, color = "MCMC"))+
  ggplot2::geom_point(aes(y = lac_abc, color = "ABC"))+
  labs(x = "real laa",
       y = "estimated lac",
       color = "Methods") +
  scale_color_manual(values = colors)+
  xlim(0,0.5)+
  ylim(0,1)+
  # ggtitle("cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::geom_hline(yintercept = 0.4, linetype = "dashed", size = 0.3)
g_laa3
