### decide the parameter space and check the relationships between dss and ss

## calculate the MLE of each set
param_space <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC_short.csv")
lac_MLE<- c()
mu_MLE <-c()
gam_MLE <- c()
laa_MLE <-c()
for(i in 1:32) {
  message("set: ", i)
  set.seed(42)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = as.numeric(obs_sim_pars$K),
                            replicates = 1)

  MLE_single <- DAISIE::DAISIE_ML_CS(
    datalist = obs_sim[[1]][[1]],
    datatype = "single",
    initparsopt = c(0.2,0.02,30,0.02,0.2),
    idparsopt = c(1,2,3,4,5),
    parsfix = NULL,
    idparsfix = NULL,
    ddmodel = 11,
    cond = 1,
    methode = "lsodes",
    jitter = -1e-5
  )
  lac_MLE<- c(lac_MLE,MLE_single$lambda_c)
  mu_MLE <-c(mu_MLE,MLE_single$mu)
  gam_MLE <- c(gam_MLE,MLE_single$gamma)
  laa_MLE <-c(laa_MLE,MLE_single$lambda_a)
}
MLE_all <- data.frame(param_space,lac_MLE,mu_MLE,gam_MLE,laa_MLE)


### calculate the number of species for each set
num_spec <- c()
num_ana <- c()
num_clado <- c()
num_nonend <- c()
num_clade <- c()

for(i in 1:32) {
  message("set: ", i)
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  ss_set <-1
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = as.numeric(obs_sim_pars$K),
                            replicates = 1)
  num_specs <- calc_num_specs(obs_sim[[1]])

  num_spec <- c(num_spec,num_specs[1])
  num_ana <- c(num_ana,num_specs[2])
  num_clado <- c(num_clado,num_specs[3])
  num_nonend <- c(num_nonend,num_specs[4])
  num_clade <- c(num_clade,num_specs[5])

}

num <- data.frame(param_space,num_spec,num_ana,num_clado,num_nonend,num_clade)
save(num,file = "G:/results/project 2/tip_info/round4/no_ext_nltt/num_spec_seed_i.RData")

load("G:/results/project 2/tip_info/round4/no_ext_nltt/num_spec.RData")
num1 = num
load("G:/results/project 2/tip_info/round4/no_ext_nltt/num_spec_seed42.RData")

param_space <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC_short.csv")
dss1<-c()
dss2<-c()
dss3<-c()
dss4<-c()
dss5<-c()
dss6<-c()
dss7<-c()

ss1<-c()
ss2<-c()
ss3<-c()
ss4<-c()
ss5<-c()
ss6<-c()
ss7<-c()

for(i in 1:32) {
  message("set: ", i)
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  ss_set <-1
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = as.numeric(obs_sim_pars$K),
                            replicates = 1)
  init_ss_diff <- calc_epsilon_init(obs_sim,ss_set)
  ss <- calc_ss_no_ext(obs_sim[[1]],1)

  dss1<-c(dss1,init_ss_diff[1])
  dss2<-c(dss2,init_ss_diff[2])
  dss3<-c(dss3,init_ss_diff[3])
  dss4<-c(dss4,init_ss_diff[4])
  dss5<-c(dss5,init_ss_diff[5])
  dss6<-c(dss6,init_ss_diff[6])
  dss7<-c(dss7,init_ss_diff[7])

  ss1<-c(ss1,ss[1])
  ss2<-c(ss2,ss[2])
  ss3<-c(ss3,ss[3])
  ss4<-c(ss4,ss[4])
  ss5<-c(ss5,ss[5])
  ss6<-c(ss6,ss[6])
  ss7<-c(ss7,ss[7])

}

ss <- data.frame(param_space,dss1,dss2,dss3,dss4,dss5,dss6,dss7,
                  ss1,ss2,ss3,ss4,ss5,ss6,ss7)

save(ss,file = "G:/results/project 2/tip_info/round4/no_ext_nltt/ss_seed_i.RData")





























param_space_name = "DAISIE_ABC_short"
param_set = 1
idparsopt = c(1,3)
save_output = TRUE
sim_model = "DAISIE"
ss_set = 1


set.seed(1)
lac <- runif(50,0,1)
replicates <- 30
novel_sim <- list()
for (i in 1:length(lac)) {
  for (j in seq_len(replicates)) {
    novel_sim[[j]] <- DAISIE::DAISIE_sim_constant_rate(
      time = 5,
      M = 1000,
      pars = c(lac[i],0.1,40,0.02,0.2),
      replicates = 1,
      sample_freq  = Inf,
      plot_sims = FALSE,
      verbose = FALSE,
      cond = 0
    )
  }
  file_name <- paste0("sim_lac",i,".RData")
  save(novel_sim,file = paste0("G:/R/Traisie_ABC/results/lac/",file_name))
}