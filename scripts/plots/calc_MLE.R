##### calculate MLE for each parameter set and combine as a dataframe
param_space <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
lac_MLE<- c()
mu_MLE <-c()
gam_MLE <- c()
laa_MLE <-c()
# for(i in 1:400) {
#   set.seed(i)
#   obs_sim_pars <- param_space[i,]
#   obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
#                                            obs_sim_pars$mu,
#                                            obs_sim_pars$gam,
#                                            obs_sim_pars$laa),
#                             K = as.numeric(obs_sim_pars$K),
#                             replicates = 1)
#
#   MLE_DI_allpars <- DAISIE::DAISIE_ML(
#     datalist = obs_sim[[1]][[1]],
#     initparsopt = c(0.5,0.1,0.01,0.5),
#     idparsopt = c(1,2,4,5),
#     parsfix = Inf,
#     idparsfix = 3,
#     ddmodel = 0,
#     cond = 1,
#     eqmodel = 0,
#     x_E = 0.95,
#     x_I = 0.98,
#     tol = c(1e-04, 1e-05, 1e-07),
#     maxiter = 1000 * round((1.25) ^ 4),
#     methode = "lsodes",
#     optimmethod = "subplex"
#   )
#   lac_MLE<- c(lac_MLE,MLE_DI_allpars$lambda_c)
#   mu_MLE <-c(mu_MLE,MLE_DI_allpars$mu)
#   gam_MLE <- c(gam_MLE,MLE_DI_allpars$gamma)
#   laa_MLE <-c(laa_MLE,MLE_DI_allpars$lambda_a)
# }


for(i in 1:100) {
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
    parsfix = c(0.00001,Inf, 0.02,0.5),
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
save(MLE_lac,file = "G:/results/project 2/tip_info/round2/MLE_single/MLE_lac.RData")


for(i in 191:200) {
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
    parsfix = c(0.5,Inf,0.02,0.5),
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
save(MLE_mu,file = "G:/results/project 2/tip_info/round2/MLE_single/MLE_mu.RData")



param_space <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
lac_MLE<- c()
mu_MLE <-c()
gam_MLE <- c()
laa_MLE <-c()
for(i in 201:300) {
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
    parsfix = c(0.5,0.00001,Inf,0.5),
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
save(MLE_gam,file = "G:/results/project 2/tip_info/round2/MLE_single/MLE_gam.RData")



param_space <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
lac_MLE<- c()
mu_MLE <-c()
gam_MLE <- c()
laa_MLE <-c()
for(i in 301:400) {
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
    parsfix = c(0.5,0.00001,Inf,0.02),
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
save(MLE_laa,file = "G:/results/project 2/tip_info/round2/MLE_single/MLE_laa.RData")



load("G:/results/project 2/tip_info/round2/MLE_single/MLE_lac.RData")
load("G:/results/project 2/tip_info/round2/MLE_single/MLE_mu.RData")
load("G:/results/project 2/tip_info/round2/MLE_single/MLE_gam.RData")
load("G:/results/project 2/tip_info/round2/MLE_single/MLE_laa.RData")
whole_df_MLE1 <- rbind(MLE_lac, MLE_mu, MLE_gam, MLE_laa)
whole_df_MLE <- data.frame(param_space,whole_df_MLE1)
save(whole_df_MLE,file = "G:/results/project 2/tip_info/round2/MLE_single/whole_df_MLE.RData")



##### MLE for all parameters
param_space <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
lac_MLE<- c()
mu_MLE <-c()
gam_MLE <- c()
laa_MLE <-c()
for(i in 1:400) {
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
save(MLE_30,file = "G:/results/project 2/tip_info/round2/MLE_all/MLE_30.RData")

MLE_30 <- data.frame(lac_MLE,mu_MLE,gam_MLE,laa_MLE)
