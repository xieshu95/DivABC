#### Decreasing kernel
folder_path <- "G:/results/project 2/tip_info/round3/dec_kernel/allpars_DI/DAISIE_ABC"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")

param_data2<-param_data[rep(seq_len(nrow(param_data)), each=200),]
#### ABC
lac_abc <- c()
mu_abc <- c()
gam_abc <- c()
laa_abc <- c()
n_iteration <- c()
for(i in 1:160){
  file_to_load <- grep(paste0("DAISIE_ABC_param_set_", i,".RData"),
                       files,
                       value = TRUE,
                       fixed = TRUE)
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    num_iter <- output$n_iter
    n_iteration[i] <- num_iter
    if(output$n_iter <= 2){
      lac_abc <- c(lac_abc, rep(NA,200))
      mu_abc <- c(mu_abc, rep(NA,200))
      gam_abc <- c(gam_abc, rep(NA,200))
      laa_abc <- c(laa_abc, rep(NA,200))
    } else{
      lac_abc <- c(lac_abc, output$ABC[[num_iter-1]][,1])  ##num_iter-1  ## only for the last iteration
      mu_abc <- c(mu_abc, output$ABC[[num_iter-1]][,2])
      gam_abc <- c(gam_abc, output$ABC[[num_iter-1]][,3])
      laa_abc <- c(laa_abc, output$ABC[[num_iter-1]][,4])
    }
  } else {
    lac_abc <- c(lac_abc, rep(NA,200))
    mu_abc <- c(mu_abc, rep(NA,200))
    gam_abc <- c(gam_abc, rep(NA,200))
    laa_abc <- c(laa_abc, rep(NA,200))
  }
}
whole_df_ABC <- data.frame(param_data2,
                           # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                           lac_abc,mu_abc,gam_abc,laa_abc)
save(whole_df_ABC,file = "G:/results/project 2/tip_info/round3/dec_kernel/allpars_DI/whole_df_ABC.RData")


####MCMC
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=2000),]
folder_path <- "G:/results/project 2/tip_info/round3/dec_kernel/MCMC_allpars/DAISIE_MCMC"
files <- list.files(folder_path)
lac_mcmc <- c()
mu_mcmc <- c()
gam_mcmc <- c()
laa_mcmc <- c()
for(i in 1:160){
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("DAISIE_MCMC_param_set_", i,".RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)

  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lac_mcmc <- c(lac_mcmc, output[,1][3001:5000])  ## [1:10000] for first_1w particles
    mu_mcmc <- c(mu_mcmc, output[,2][3001:5000])
    gam_mcmc <- c(gam_mcmc, output[,3][3001:5000])
    laa_mcmc <- c(laa_mcmc, output[,4][3001:5000])
  } else {
    lac_mcmc <- c(lac_mcmc, rep(NA,2000))
    mu_mcmc <- c(mu_mcmc, rep(NA,2000))
    gam_mcmc <- c(gam_mcmc, rep(NA,2000))
    laa_mcmc <- c(laa_mcmc, rep(NA,2000))
  }
}

whole_df_MCMC <- data.frame(param_data3,
                                     lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc)
#lac_abc,mu_abc,gam_abc,laa_abc,n_iter)
save(whole_df_MCMC,file = "G:/results/project 2/tip_info/round3/dec_kernel/MCMC_allpars/whole_df_MCMC.RData")

#### MLE_allpars
##### MLE for all parameters
param_space <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
lac_MLE<- c()
mu_MLE <-c()
gam_MLE <- c()
laa_MLE <-c()
for(i in 1:160) {
  message("set: ", i)
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = as.numeric(obs_sim_pars$K),
                            replicates = 1)

  MLE_DI_single <- DAISIE::DAISIE_ML(
    datalist = obs_sim[[1]][[1]],
    initparsopt = c(obs_sim_pars$lac,
                    obs_sim_pars$mu,
                    obs_sim_pars$gam,
                    obs_sim_pars$laa),
    idparsopt = c(1,2,4,5),
    parsfix = Inf,
    idparsfix = 3,
    ddmodel = 0,
    cond = 1,
    eqmodel = 0,
    x_E = 0.95,
    x_I = 0.98,
    tol = c(1e-04, 1e-05, 1e-07),
    maxiter = 1000 * round((1.25) ^ 4),
    methode = "lsodes",
    optimmethod = "subplex",
    jitter = -1e-5
  )
  lac_MLE<- c(lac_MLE,MLE_DI_single$lambda_c)
  mu_MLE <-c(mu_MLE,MLE_DI_single$mu)
  gam_MLE <- c(gam_MLE,MLE_DI_single$gamma)
  laa_MLE <-c(laa_MLE,MLE_DI_single$lambda_a)
}
 MLE_all <- data.frame(param_space,lac_MLE,mu_MLE,gam_MLE,laa_MLE)
save(MLE_all,file = "G:/results/project 2/tip_info/round3/dec_kernel/MLE_allpars/MLE_all.RData")








#### MLE_single
param_space <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
lac_MLE<- c()
mu_MLE <-c()
gam_MLE <- c()
laa_MLE <-c()
for(i in 1:40) {
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = as.numeric(obs_sim_pars$K),
                            replicates = 1)

  MLE_DI_single <- DAISIE::DAISIE_ML(
    datalist = obs_sim[[1]][[1]],
    initparsopt = 0.5,
    idparsopt = 1,
    parsfix = c(0.05,Inf, 0.015,0.2),
    idparsfix = c(2,3,4,5),
    ddmodel = 0,
    cond = 1,
    eqmodel = 0,
    x_E = 0.95,
    x_I = 0.98,
    tol = c(1e-04, 1e-05, 1e-07),
    maxiter = 1000 * round((1.25) ^ 4),
    methode = "lsodes",
    optimmethod = "subplex"
  )
  lac_MLE<- c(lac_MLE,MLE_DI_single$lambda_c)
  mu_MLE <-c(mu_MLE,MLE_DI_single$mu)
  gam_MLE <- c(gam_MLE,MLE_DI_single$gamma)
  laa_MLE <-c(laa_MLE,MLE_DI_single$lambda_a)
}
MLE_lac <- data.frame(lac_MLE,mu_MLE,gam_MLE,laa_MLE)
save(MLE_lac,file = "G:/results/project 2/tip_info/round3/dec_kernel/MLE_single/MLE_lac.RData")

param_space <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
lac_MLE<- c()
mu_MLE <-c()
gam_MLE <- c()
laa_MLE <-c()
for(i in 41:80) {
  message("set: ", i)
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = as.numeric(obs_sim_pars$K),
                            replicates = 1)

  MLE_DI_single <- DAISIE::DAISIE_ML(
    datalist = obs_sim[[1]][[1]],
    initparsopt = obs_sim_pars$mu,
    idparsopt = 2,
    parsfix = c(0.5,Inf,0.015,0.2),
    idparsfix = c(1,3,4,5),
    ddmodel = 0,
    cond = 1,
    eqmodel = 0,
    x_E = 0.95,
    x_I = 0.98,
    tol = c(1e-04, 1e-05, 1e-07),
    maxiter = 1000 * round((1.25) ^ 4),
    methode = "lsodes",
    optimmethod = "subplex",
    jitter = -1e-5
  )
  lac_MLE<- c(lac_MLE,MLE_DI_single$lambda_c)
  mu_MLE <-c(mu_MLE,MLE_DI_single$mu)
  gam_MLE <- c(gam_MLE,MLE_DI_single$gamma)
  laa_MLE <-c(laa_MLE,MLE_DI_single$lambda_a)
}
MLE_mu <- data.frame(lac_MLE,mu_MLE,gam_MLE,laa_MLE)
save(MLE_mu,file = "G:/results/project 2/tip_info/round3/dec_kernel/MLE_single/MLE_mu.RData")



param_space <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
lac_MLE<- c()
mu_MLE <-c()
gam_MLE <- c()
laa_MLE <-c()
for(i in 81:120) {
  message("set: ", i)
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = as.numeric(obs_sim_pars$K),
                            replicates = 1)

  MLE_DI_single <- DAISIE::DAISIE_ML(
    datalist = obs_sim[[1]][[1]],
    initparsopt = obs_sim_pars$gam,
    idparsopt = 4,
    parsfix = c(0.5,0.05,Inf,0.2),
    idparsfix = c(1,2,3,5),
    ddmodel = 0,
    cond = 1,
    eqmodel = 0,
    x_E = 0.95,
    x_I = 0.98,
    tol = c(1e-04, 1e-05, 1e-07),
    maxiter = 1000 * round((1.25) ^ 4),
    methode = "lsodes",
    optimmethod = "subplex",
    jitter = -1e-5
  )
  lac_MLE<- c(lac_MLE,MLE_DI_single$lambda_c)
  mu_MLE <-c(mu_MLE,MLE_DI_single$mu)
  gam_MLE <- c(gam_MLE,MLE_DI_single$gamma)
  laa_MLE <-c(laa_MLE,MLE_DI_single$lambda_a)
}
MLE_gam <- data.frame(lac_MLE,mu_MLE,gam_MLE,laa_MLE)
save(MLE_gam,file = "G:/results/project 2/tip_info/round3/dec_kernel/MLE_single/MLE_gam.RData")



param_space <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
lac_MLE<- c()
mu_MLE <-c()
gam_MLE <- c()
laa_MLE <-c()
for(i in 121:160) {
  message("set: ", i)
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = as.numeric(obs_sim_pars$K),
                            replicates = 1)

  MLE_DI_single <- DAISIE::DAISIE_ML(
    datalist = obs_sim[[1]][[1]],
    initparsopt = obs_sim_pars$laa,
    idparsopt = 5,
    parsfix = c(0.5,0.05,Inf,0.015),
    idparsfix = c(1,2,3,4),
    ddmodel = 0,
    cond = 1,
    eqmodel = 0,
    x_E = 0.95,
    x_I = 0.98,
    tol = c(1e-04, 1e-05, 1e-07),
    maxiter = 1000 * round((1.25) ^ 4),
    methode = "lsodes",
    optimmethod = "subplex",
    jitter = -1e-5
  )
  lac_MLE<- c(lac_MLE,MLE_DI_single$lambda_c)
  mu_MLE <-c(mu_MLE,MLE_DI_single$mu)
  gam_MLE <- c(gam_MLE,MLE_DI_single$gamma)
  laa_MLE <-c(laa_MLE,MLE_DI_single$lambda_a)
}
MLE_laa <- data.frame(lac_MLE,mu_MLE,gam_MLE,laa_MLE)
save(MLE_laa,file = "G:/results/project 2/tip_info/round3/dec_kernel/MLE_single/MLE_laa.RData")

load("G:/results/project 2/tip_info/round3/dec_kernel/MLE_single/MLE_lac.RData")
load("G:/results/project 2/tip_info/round3/dec_kernel/MLE_single/MLE_mu.RData")
load("G:/results/project 2/tip_info/round3/dec_kernel/MLE_single/MLE_gam.RData")
load("G:/results/project 2/tip_info/round3/dec_kernel/MLE_single/MLE_laa.RData")
whole_df_MLE1 <- rbind(MLE_lac, MLE_mu, MLE_gam, MLE_laa)
whole_df_MLE <- data.frame(param_space,whole_df_MLE1)
save(whole_df_MLE,file = "G:/results/project 2/tip_info/round3/dec_kernel/MLE_single/whole_df_MLE.RData")



# load("G:/results/project 2/tip_info/round2/MCMC_single/whole_df_MCMC.RData")
load("G:/results/project 2/tip_info/round3/dec_kernel/single_par_DI/whole_df_ABC.RData")
load("G:/results/project 2/tip_info/round3/dec_kernel/MCMC_single_par/whole_df_MCMC.RData")
load("G:/results/project 2/tip_info/round3/dec_kernel/MLE_single/whole_df_MLE.RData")

library(ggplot2)
colors <- c("MCMC" = "#F7903D", "ABC" = "#4D85BD", "MLE" = "#59A95A")
tiff("G:/results/project 2/tip_info/round3/dec_kernel/single_par_DI/boxplot_lac.tiff", units="px", width=700, height=400)
i = 1
param_abc <- whole_df_ABC[((i*8000-7999)):(i*8000),]
param_mcmc <- whole_df_MCMC[((i*80000-79999)):(i*80000),]
param_mle <- whole_df_MLE[((i*40-39)):(i*40),]
g1 <- ggplot2::ggplot(param_abc, aes(x = lac,y = lac_abc, group = lac)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = lac-0.03,y = lac_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = lac, y = lac_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = lac+0.03,y = lac_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac, y = lac),color = "black",size = 3,shape = 16) +
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Cladogenesis") +
  xlim(0,1.0)+
  ylim(0,2.0)+
  # ggtitle("Cladogenesis")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
print(g1)
while (!is.null(dev.list()))  dev.off()



tiff("G:/results/project 2/tip_info/round3/dec_kernel/single_par_DI/boxplot_mu.tiff", units="px", width=700, height=400)
i = 2
param_abc <- whole_df_ABC[((i*8000-7999)):(i*8000),]
param_mcmc <- whole_df_MCMC[((i*80000-79999)):(i*80000),]
param_mle <- whole_df_MLE[((i*40-39)):(i*40),]
g2 <- ggplot2::ggplot(param_abc, aes(x = mu,y = mu_abc, group = mu)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = mu-0.008,y = mu_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.004,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = mu, y = mu_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.004,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = mu+0.008,y = mu_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.004,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = mu, y = mu),color = "black",size = 3,shape = 16) +
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Extinction") +
  xlim(0,0.25)+
  ylim(0,2.0)+
  # ggtitle("Extinction")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
print(g2)
while (!is.null(dev.list()))  dev.off()


tiff("G:/results/project 2/tip_info/round3/dec_kernel/single_par_DI/boxplot_gam.tiff", units="px", width=700, height=400)
i = 3
param_abc <- whole_df_ABC[((i*8000-7999)):(i*8000),]
param_mcmc <- whole_df_MCMC[((i*80000-79999)):(i*80000),]
param_mle <- whole_df_MLE[((i*40-39)):(i*40),]
g3 <- ggplot2::ggplot(param_abc, aes(x = gam,y = gam_abc, group = gam)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = gam-0.001,y = gam_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = gam, y = gam_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = gam+0.001,y = gam_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.0005,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam, y = gam),color = "black",size = 3,shape = 16) +
  labs(x = "Real rate",
       y = "Estimated rate",
       color = "Methods",
       fill = "Methods") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  ggplot2::ggtitle("Colonization") +
  xlim(0.005,0.032)+
  ylim(0,0.05)+
  # ggtitle("Colonization")+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
print(g3)
while (!is.null(dev.list()))  dev.off()


tiff("G:/results/project 2/tip_info/round3/dec_kernel/single_par_DI/boxplot_laa.tiff", units="px", width=700, height=400)
i = 4
param_abc <- whole_df_ABC[((i*8000-7999)):(i*8000),]
param_mcmc <- whole_df_MCMC[((i*80000-79999)):(i*80000),]
param_mle <- whole_df_MLE[((i*40-39)):(i*40),]
g4 <- ggplot2::ggplot(param_abc, aes(x = laa,y = laa_abc, group = laa)) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(data= param_mcmc,
                        aes(x = laa-0.03,y = laa_mcmc, color = "MCMC",fill = "MCMC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(aes(x = laa, y = laa_abc, color = "ABC",fill = "ABC"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_boxplot(data= param_mle,
                        aes(x = laa+0.03,y = laa_MLE, color = "MLE",fill = "MLE"),
                        alpha = 1,width = 0.02,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa, y = laa),color = "black",size = 3,shape = 16) +
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
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, color = "black")) +
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)
print(g4)
while (!is.null(dev.list()))  dev.off()

tiff(paste0("G:/results/project 2/tip_info/round3/dec_kernel/single_par_DI/boxplots/boxplot_all.tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  g1,g2,g3,g4,
  align = "hv", nrow = 2, ncol = 2
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()








###




#### boxplot for each replicate

for(param_rep in 1:10){
  scen_ABC <- c(((param_rep-1)*10000+1):(param_rep*10000))
  scen_MCMC <- c(((param_rep-1)*50000+1):(param_rep*50000))
  scen_MLE <- c(((param_rep-1)*100000+1):(param_rep*100000))
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
