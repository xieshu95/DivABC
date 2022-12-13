#####
load("G:/results/project 2/tip_info/round4/adap_daisie_pw2/obs_ss_long_with_pars.RData")

# folder_path <- "G:/results/project 2/tip_info/round4/kernel3/DAISIE_ABC_short2"
folder_path <- "G:/results/project 2/tip_info/round4/adap_daisie_pw2/DAISIE_ABC_short"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC_short.csv")

param_data <- param_data[1:27,]
param_data2<-param_data[rep(seq_len(nrow(param_data)), each=400),]

for(n in c(0,20,200)){
  lac_abc <- c()
  mu_abc <- c()
  gam_abc <- c()
  laa_abc <- c()
  n_iter <-c()
  n_iteration <- c()
  for(i in 1:27){
    # if(i%%5 == 0){
    #   rep <- 5
    # } else {
    #   rep <- i%%5
    # }
    # param_set = (param_num-1)*5 + i
    file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", i,"_ss_",n,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      n_iteration[i] <- num_iter
      if(output$n_iter <= 2){
        lac_abc <- c(lac_abc, rep(NA,400))
        mu_abc <- c(mu_abc, rep(NA,400))
        gam_abc <- c(gam_abc, rep(NA,400))
        laa_abc <- c(laa_abc, rep(NA,400))
      } else if (nrow(output$ABC[[output$n_iter]]) == 400) {
        lac_abc <- c(lac_abc, output$ABC[[num_iter]][,1])
        mu_abc <- c(mu_abc, output$ABC[[num_iter]][,2])
        gam_abc <- c(gam_abc, output$ABC[[num_iter]][,3])
        laa_abc <- c(laa_abc, output$ABC[[num_iter]][,4])
      } else {
        lac_abc <- c(lac_abc, output$ABC[[num_iter-1]][,1])
        mu_abc <- c(mu_abc, output$ABC[[num_iter-1]][,2])
        gam_abc <- c(gam_abc, output$ABC[[num_iter-1]][,3])
        laa_abc <- c(laa_abc, output$ABC[[num_iter-1]][,4])
      }
    } else {
      lac_abc <- c(lac_abc, rep(NA,400))
      mu_abc <- c(mu_abc, rep(NA,400))
      gam_abc <- c(gam_abc, rep(NA,400))
      laa_abc <- c(laa_abc, rep(NA,400))
    }
  }
  whole_df_ABC <- data.frame(param_data2,
                             # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                             lac_abc,mu_abc,gam_abc,laa_abc)
  save(whole_df_ABC,
       file = paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw2/whole_df_ABC_ss_set",n,".RData"))

}

load(paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw2/whole_df_ABC_ss_set",200,".RData"))
whole_df_ABC$net_div <- (whole_df_ABC$lac-whole_df_ABC$mu)
whole_df_ABC$net_div_ABC <- (whole_df_ABC$lac_abc-whole_df_ABC$mu_abc)

whole_df_ABC$ext_frac <- (whole_df_ABC$mu)/(whole_df_ABC$lac)
whole_df_ABC$ext_frac_ABC <- (whole_df_ABC$mu_abc)/(whole_df_ABC$lac_abc)

save(whole_df_ABC,file = paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw2/delta_whole_df_ABC_ss_set",200,".RData"))

load(paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw2/delta_whole_df_ABC_ss_set",200,".RData"))

#####
#MCMC results
folder_path <- "G:/results/project 2/tip_info/round4/adap_daisie_pw2/DAISIE_MCMC_1001"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC_short.csv")
param_data <- param_data[1:27,]
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=1001),] #5001

lac_mcmc <- c()
mu_mcmc <- c()
gam_mcmc <- c()
laa_mcmc <- c()
for(i in 1:27){
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("DAISIE_MCMC_short_param_set_", i,"_ss_1.RData"), #"_rep",rep,
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
    lac_mcmc <- c(lac_mcmc, rep(NA,1001))
    mu_mcmc <- c(mu_mcmc, rep(NA,1001))
    gam_mcmc <- c(gam_mcmc, rep(NA,1001))
    laa_mcmc <- c(laa_mcmc, rep(NA,1001))
  }
}

whole_df_MCMC <- data.frame(param_data3,
                            lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc)
save(whole_df_MCMC,file = "G:/results/project 2/tip_info/round4/adap_daisie_pw2/whole_df_MCMC_1001.RData")


whole_df_MCMC$net_div <- (whole_df_MCMC$lac-whole_df_MCMC$mu)
whole_df_MCMC$net_div_mcmc <- (whole_df_MCMC$lac_mcmc - whole_df_MCMC$mu_mcmc)

whole_df_MCMC$ext_frac <- (whole_df_MCMC$mu)/(whole_df_MCMC$lac)
whole_df_MCMC$ext_frac_MCMC <- (whole_df_MCMC$mu_mcmc)/(whole_df_MCMC$lac_mcmc)

save(whole_df_MCMC,file = "G:/results/project 2/tip_info/round4/adap_daisie_pw2/delta_whole_df_MCMC_1001.RData")
load("G:/results/project 2/tip_info/round4/adap_daisie_pw2/delta_whole_df_MCMC_1001.RData")


#####
#calculate MLE for each parameter set and combine as a dataframe
param_space <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC_short.csv")
lac_MLE <- c()
mu_MLE <- c()
gam_MLE <- c()
laa_MLE <- c()
K <- c()
for(i in 1:27) {
  message("set",i)
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = as.numeric(obs_sim_pars$K),
                            replicates = 1)

  MLE_DD_allpars <- DAISIE::DAISIE_ML(
    datalist = obs_sim[[1]][[1]],
    initparsopt = as.numeric(obs_sim_pars[c(1,2,5,3,4)]),
    idparsopt = 1:5,
    parsfix = NULL,
    idparsfix = NULL,
    ddmodel = 11,
    cond = 1,
    methode = "lsodes",
    optimmethod = "subplex",
    jitter = 1e-5
  )
  lac_MLE<- c(lac_MLE,MLE_DD_allpars$lambda_c)
  mu_MLE <-c(mu_MLE,MLE_DD_allpars$mu)
  gam_MLE <- c(gam_MLE,MLE_DD_allpars$gamma)
  laa_MLE <-c(laa_MLE,MLE_DD_allpars$lambda_a)
  K <- c(K,MLE_DD_allpars$K)
}


whole_df_MLE <- data.frame(param_space,
                           lac_MLE,mu_MLE,gam_MLE,laa_MLE)

## directly load MLE results from cluster
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC_short.csv")
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=10),]
load("G:/results/project 2/tip_info/round4/adap_daisie_pw2/MLE/MLE_381528.RData")
set <- rep(1:81,each = 10)
whole_df_MLE <- data.frame(set,param_data3,MLE_all)

whole_df_MLE$net_div <- (whole_df_MLE$lac-whole_df_MLE$mu)
whole_df_MLE$net_div_MLE <- (whole_df_MLE$lac_MLE-whole_df_MLE$mu_MLE)

whole_df_MLE$ext_frac <- (whole_df_MLE$mu)/(whole_df_MLE$lac)
whole_df_MLE$ext_frac_MLE <- (whole_df_MLE$mu_MLE)/(whole_df_MLE$lac_MLE)
save(whole_df_MLE,file = "G:/results/project 2/tip_info/round4/adap_daisie_pw2/delta_whole_df_MLE1.RData")

load("G:/results/project 2/tip_info/round4/adap_daisie_pw2/whole_df_MLE.RData")

#####
# record mean/median/sd for ABC/MCMC/MLE



#####
# plot MCMC trace
folder_path <- "G:/results/project 2/tip_info/round4/adap_daisie_pw2/DAISIE_MCMC_1001"
files <- list.files(folder_path)
for(i in 1:27){
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("DAISIE_MCMC_short_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)

  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    tiff(paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw2/MCMC_trace_1001/set_",i,".tiff"),
         units="px", width=2000, height=4000,res = 300,compression="lzw")
    b_mcmc <- coda::as.mcmc(output[,1:4])
    plot_mcmc <- plot(b_mcmc)
    print(plot_mcmc)
    while (!is.null(dev.list()))  dev.off()
  }
}


