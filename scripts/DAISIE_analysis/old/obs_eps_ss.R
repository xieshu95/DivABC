load("G:/results/project 2/tip_info/round3/TRAISIE_lac_50rep/lac_whole_df_ABC.RData")


folder_path <- "G:/results/project 2/tip_info/round3/TRAISIE_lac_50rep/TraiSIE_ABC_lac"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/TraiSIE_ABC_lac.csv")


# ana_endemic_nltt_error
# clado_endemic_nltt_error
# nonendemic_nltt_error
# clade_nltt_error
# num_ana_error_state1
# num_ana_error_state2
# num_clado_error_state1
# num_clado_error_state2
# num_nonend_error_state1
# num_nonend_error_state2
# num_trans12_error
# num_trans21_error

## calculate the median summary statistics error for each parameter set under different seeds
init_eps_ss1 <- c()
init_eps_ss2 <- c()
init_eps_ss3 <- c()
init_eps_ss4 <- c()
init_eps_ss5 <- c()
init_eps_ss6 <- c()
init_eps_ss7 <- c()
init_eps_ss8 <- c()
init_eps_ss9 <- c()
init_eps_ss10 <- c()
init_eps_ss11 <- c()
init_eps_ss12 <- c()



n_iter <-c()
n_iteration <- c()
for(i in 1:600){
  # if(i%%5 == 0){
  #   rep <- 5
  # } else {
  #   rep <- i%%5
  # }
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("TraiSIE_ABC_lac_param_set_", i,".RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    num_iter <- output$n_iter
    n_iteration[i] <- num_iter
    if(output$n_iter <= 3){
      init_eps_ss1[i] <- NA
      init_eps_ss2[i] <- NA
      init_eps_ss3[i] <- NA
      init_eps_ss4[i] <- NA
      init_eps_ss5[i] <- NA
      init_eps_ss6[i] <- NA
      init_eps_ss7[i] <- NA
      init_eps_ss8[i] <- NA
      init_eps_ss9[i] <- NA
      init_eps_ss10[i] <- NA
      init_eps_ss11[i] <- NA
      init_eps_ss12[i] <- NA

    } else{

      init_eps_ss1[i] <- output$init_epsilon[1]
      init_eps_ss2[i] <- output$init_epsilon[2]
      init_eps_ss3[i] <- output$init_epsilon[3]
      init_eps_ss4[i] <- output$init_epsilon[4]
      init_eps_ss5[i] <- output$init_epsilon[5]
      init_eps_ss6[i] <- output$init_epsilon[6]
      init_eps_ss7[i] <- output$init_epsilon[7]
      init_eps_ss8[i] <- output$init_epsilon[8]
      init_eps_ss9[i] <- output$init_epsilon[9]
      init_eps_ss10[i] <- output$init_epsilon[10]
      init_eps_ss11[i] <- output$init_epsilon[11]
      init_eps_ss12[i] <- output$init_epsilon[12]
    }
  } else {
    init_eps_ss1[i] <- NA
    init_eps_ss2[i] <- NA
    init_eps_ss3[i] <- NA
    init_eps_ss4[i] <- NA
    init_eps_ss5[i] <- NA
    init_eps_ss6[i] <- NA
    init_eps_ss7[i] <- NA
    init_eps_ss8[i] <- NA
    init_eps_ss9[i] <- NA
    init_eps_ss10[i] <- NA
    init_eps_ss11[i] <- NA
    init_eps_ss12[i] <- NA

  }
}

## calculate the summary statistics(not error) for each observed data
obs_ss1 <- c()
obs_ss2 <- c()
obs_ss3 <- c()
obs_ss4 <- c()
obs_ss5 <- c()
obs_ss6 <- c()
obs_ss7 <- c()
obs_ss8 <- c()
obs_ss9 <- c()
obs_ss10 <- c()
obs_ss11 <- c()
obs_ss12 <- c()
n_iter <-c()
n_iteration <- c()
for(i in 1:600){
  # if(i%%5 == 0){
  #   rep <- 5
  # } else {
  #   rep <- i%%5
  # }
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("TraiSIE_ABC_lac_param_set_", i,".RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    num_iter <- output$n_iter
    n_iteration[i] <- num_iter
    if(output$n_iter <= 3){

      obs_ss1[i] <- NA
      obs_ss2[i] <- NA
      obs_ss3[i] <- NA
      obs_ss4[i] <- NA
      obs_ss5[i] <- NA
      obs_ss6[i] <- NA
      obs_ss7[i] <- NA
      obs_ss8[i] <- NA
      obs_ss9[i] <- NA
      obs_ss10[i] <- NA
      obs_ss11[i] <- NA
      obs_ss12[i] <- NA
    } else{

      obs_sim_pars <- param_data[i,]
      set.seed(i)
      obs_sim <- get_TraiSIE_sim(parameters = as.numeric(c(obs_sim_pars$lac,
                                                           obs_sim_pars$mu,
                                                           obs_sim_pars$gam,
                                                           obs_sim_pars$laa,
                                                           obs_sim_pars$lac2,
                                                           obs_sim_pars$mu2,
                                                           obs_sim_pars$gam2,
                                                           obs_sim_pars$laa2,
                                                           obs_sim_pars$trans,
                                                           obs_sim_pars$trans2)),
                                 K = as.numeric(obs_sim_pars$K),
                                 replicates = 1)
      obs_ss <- calc_ss_trait(sim = obs_sim[[1]],replicates = 1)

      obs_ss1[i] <- obs_ss[1]
      obs_ss2[i] <- obs_ss[2]
      obs_ss3[i] <- obs_ss[3]
      obs_ss4[i] <- obs_ss[4]
      obs_ss5[i] <- obs_ss[5]
      obs_ss6[i] <- obs_ss[6]
      obs_ss7[i] <- obs_ss[7]
      obs_ss8[i] <- obs_ss[8]
      obs_ss9[i] <- obs_ss[9]
      obs_ss10[i] <- obs_ss[10]
      obs_ss11[i] <- obs_ss[11]
      obs_ss12[i] <- obs_ss[12]
    }
  } else {


    obs_ss1[i] <- NA
    obs_ss2[i] <- NA
    obs_ss3[i] <- NA
    obs_ss4[i] <- NA
    obs_ss5[i] <- NA
    obs_ss6[i] <- NA
    obs_ss7[i] <- NA
    obs_ss8[i] <- NA
    obs_ss9[i] <- NA
    obs_ss10[i] <- NA
    obs_ss11[i] <- NA
    obs_ss12[i] <- NA
  }
}


whole_df_ABC <- data.frame(param_data,
                           init_eps_ss1,init_eps_ss2,init_eps_ss3,init_eps_ss4,
                           init_eps_ss5,init_eps_ss6,init_eps_ss7,init_eps_ss8,
                           init_eps_ss9,init_eps_ss10,init_eps_ss11,init_eps_ss12,
                           obs_ss1,obs_ss2,obs_ss3,obs_ss4,obs_ss5,obs_ss6,
                           obs_ss7,obs_ss8,obs_ss9,obs_ss10,obs_ss11,obs_ss12)

whole_df_ABC_median <- data.frame(param_data,
                           init_eps_ss1/8,init_eps_ss2/8,init_eps_ss3/8,init_eps_ss4/8,
                           init_eps_ss5/8,init_eps_ss6/8,init_eps_ss7/8,init_eps_ss8/8,
                           init_eps_ss9/8,init_eps_ss10/8,init_eps_ss11/8,init_eps_ss12/8,
                           obs_ss1,obs_ss2,obs_ss3,obs_ss4,obs_ss5,obs_ss6,
                           obs_ss7,obs_ss8,obs_ss9,obs_ss10,obs_ss11,obs_ss12)
whole_df_init_ss <- data.frame(init_eps_ss1,init_eps_ss2,init_eps_ss3,init_eps_ss4,
                            init_eps_ss5,init_eps_ss6,init_eps_ss7,init_eps_ss8,
                            init_eps_ss9,init_eps_ss10,init_eps_ss11,init_eps_ss12)

whole_df_obs_ss <- data.frame(obs_ss1,obs_ss2,obs_ss3,obs_ss4,obs_ss5,obs_ss6,
                              obs_ss7,obs_ss8,obs_ss9,obs_ss10,obs_ss11,obs_ss12)

save(whole_df_init_ss,file = "G:/results/project 2/tip_info/round3/TRAISIE_lac_50rep/whole_df_init_ss.RData")
save(whole_df_obs_ss,file = "G:/results/project 2/tip_info/round3/TRAISIE_lac_50rep/whole_df_obs_ss.RData")

load("G:/results/project 2/tip_info/round3/TRAISIE_lac_50rep/whole_df_init_ss_2.RData")
load("G:/results/project 2/tip_info/round3/TRAISIE_lac_50rep/whole_df_obs_ss.RData")
whole_df_ABC <- data.frame(param_data,whole_df_init_ss_2,whole_df_obs_ss)

save(whole_df_ABC,file = "G:/results/project 2/tip_info/round3/TRAISIE_lac_50rep/obs_eps_ss2.RData")


load("G:/results/project 2/tip_info/round3/TRAISIE_lac_50rep/obs_eps_ss2.RData")

obs_eps_ss1 <- ggplot2::ggplot(data = whole_df_ABC[401:600,]) +
  ggplot2::theme_bw() +
  # xlim(0,2)+
  # ylim(0,1)+
  ggplot2::geom_point(mapping = ggplot2::aes(x = obs_ss1,y = init_eps_ss1),
                      shape = 16,alpha = 0.5) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 10),
                 text = ggplot2::element_text(size = 7)) +
  ggplot2::xlab("Observed statistic") +
  ggplot2::ylab("Initial epsilon")

obs_eps_ss1


whole_df_ABC$obs_ss5




## calculate the median summary statistics error for each parameter set under different seeds
init_eps_ss1 <- c()
init_eps_ss2 <- c()
init_eps_ss3 <- c()
init_eps_ss4 <- c()
init_eps_ss5 <- c()
init_eps_ss6 <- c()
init_eps_ss7 <- c()
init_eps_ss8 <- c()
init_eps_ss9 <- c()
init_eps_ss10 <- c()
init_eps_ss11 <- c()
init_eps_ss12 <- c()




calc_epsilon_init_obs <- function(sim){
  ss_diff_pairs <- c()
  replicates <- length(sim)
  for (i in 2:replicates){
      ss_diff <- calc_ss_diff(sim1 = sim[[i]], sim2 =sim[[1]])
      ss_diff_pairs <- data.frame(rbind(ss_diff_pairs,ss_diff))
  }
  ss_diff_pairs_median <- apply(ss_diff_pairs,2,mean)
  epsilon_init <- 8*ss_diff_pairs_median ##9 for DAISIE
  return(epsilon_init)
}




n_iter <-c()
n_iteration <- c()
for(i in 1:600){
  # if(i%%5 == 0){
  #   rep <- 5
  # } else {
  #   rep <- i%%5
  # }
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("TraiSIE_ABC_lac_param_set_", i,".RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    num_iter <- output$n_iter
    n_iteration[i] <- num_iter
    if(output$n_iter <= 3){
      init_eps_ss1[i] <- NA
      init_eps_ss2[i] <- NA
      init_eps_ss3[i] <- NA
      init_eps_ss4[i] <- NA
      init_eps_ss5[i] <- NA
      init_eps_ss6[i] <- NA
      init_eps_ss7[i] <- NA
      init_eps_ss8[i] <- NA
      init_eps_ss9[i] <- NA
      init_eps_ss10[i] <- NA
      init_eps_ss11[i] <- NA
      init_eps_ss12[i] <- NA

    } else{
      message("running param: ", i)
      obs_sim_pars <- param_data[i,]
      set.seed(i)
      obs_sim <- get_TraiSIE_sim(parameters = as.numeric(c(obs_sim_pars$lac,
                                                           obs_sim_pars$mu,
                                                           obs_sim_pars$gam,
                                                           obs_sim_pars$laa,
                                                           obs_sim_pars$lac2,
                                                           obs_sim_pars$mu2,
                                                           obs_sim_pars$gam2,
                                                           obs_sim_pars$laa2,
                                                           obs_sim_pars$trans,
                                                           obs_sim_pars$trans2)),
                                 K = as.numeric(obs_sim_pars$K),
                                 replicates = 50)
      init_epsilon <- calc_epsilon_init_obs(obs_sim)
      init_eps_ss1[i] <- init_epsilon[1]
      init_eps_ss2[i] <- init_epsilon[2]
      init_eps_ss3[i] <- init_epsilon[3]
      init_eps_ss4[i] <- init_epsilon[4]
      init_eps_ss5[i] <- init_epsilon[5]
      init_eps_ss6[i] <- init_epsilon[6]
      init_eps_ss7[i] <- init_epsilon[7]
      init_eps_ss8[i] <- init_epsilon[8]
      init_eps_ss9[i] <- init_epsilon[9]
      init_eps_ss10[i] <- init_epsilon[10]
      init_eps_ss11[i] <- init_epsilon[11]
      init_eps_ss12[i] <- init_epsilon[12]
    }
  } else {
    init_eps_ss1[i] <- NA
    init_eps_ss2[i] <- NA
    init_eps_ss3[i] <- NA
    init_eps_ss4[i] <- NA
    init_eps_ss5[i] <- NA
    init_eps_ss6[i] <- NA
    init_eps_ss7[i] <- NA
    init_eps_ss8[i] <- NA
    init_eps_ss9[i] <- NA
    init_eps_ss10[i] <- NA
    init_eps_ss11[i] <- NA
    init_eps_ss12[i] <- NA

  }
}

whole_df_init_ss_2 <- data.frame(init_eps_ss1,init_eps_ss2,init_eps_ss3,init_eps_ss4,
                               init_eps_ss5,init_eps_ss6,init_eps_ss7,init_eps_ss8,
                               init_eps_ss9,init_eps_ss10,init_eps_ss11,init_eps_ss12)


save(whole_df_init_ss_2,file = "G:/results/project 2/tip_info/round3/TRAISIE_lac_50rep/whole_df_init_ss_2.RData")





