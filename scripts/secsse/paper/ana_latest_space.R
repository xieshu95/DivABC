# secsse  analysis "secsse_latest"
# initial states
load("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/obs_sims_secsse_ABC_test.rda")
init_state <- c()
for (i in 1:350) {
  init_state[i] <- obs_sim[[i]][[1]]$initialState
}
init_state <- as.data.frame(init_state)
init_state$state_name <- rep(NA,350)
init_state$state_name[which(init_state$init_state == 0)] <-"1A"
init_state$state_name[which(init_state$init_state == 1)] <-"2A"
init_state$state_name[which(init_state$init_state == 2)] <-"1B"
init_state$state_name[which(init_state$init_state == 3)] <-"2B"


# init_state for posteriors
for (num_ss in c(1)){
  # formate results
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/obs_ss_test.rda"))
  ## ABC results
  folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/secsse_ABC_test")
  files <- list.files(folder_path)
  param_data <- load_param_space(param_space_name = paste0("secsse_ABC_test"))
  init_1A <-c()
  init_2A <-c()
  init_1B <-c()
  init_2B <-c()
  for(i in 1:10){
    file_to_load <- grep(paste0("secsse_ABC_test_param_set_",i,"_ss_",num_ss,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      if(output$n_iter >= 2){
        init_1A <-c(init_1A,output[["init_prob_list"]][[num_iter]][1])
        init_2A <-c(init_2A,output[["init_prob_list"]][[num_iter]][2])
        init_1B <-c(init_1B,output[["init_prob_list"]][[num_iter]][3])
        init_2B <-c(init_2B,output[["init_prob_list"]][[num_iter]][4])
      }
    } else {
      init_1A <-c(init_1A,NA)
      init_2A <-c(init_2A,NA)
      init_1B <-c(init_1B,NA)
      init_2B <-c(init_2B,NA)
    }
  }


  whole_df_init <- data.frame(param_data,init_state,
                             # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                             init_1A,init_2A,init_1B,init_2B)
  whole_df_init$prob_s1 <- whole_df_init$init_1A + whole_df_init$init_1B
  whole_df_init$prob_s2 <- whole_df_init$init_2A + whole_df_init$init_2B
  whole_df_init <- data.frame(whole_df_init,ss[,1:4])
  save(whole_df_init,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/whole_df_init_test_ss",num_ss,".RData"))
}





# 1. formate ABC results
## check new secsse ABC result
for (num_ss in c(1)){
  # formate results
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/obs_ss_test.rda"))
  ## ABC results
  folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/secsse_ABC_test")
  files <- list.files(folder_path)
  param_data <- load_param_space(param_space_name = paste0("secsse_ABC_test"))
  param_data2<-param_data[rep(seq_len(nrow(param_data)), each=500),] #500
  lam1_init <- c()
  lam2_init <- c()
  mu1_init <- c()
  mu2_init <- c()
  q12_init <- c()
  q21_init <- c()
  n_iter <- c()
  n_iteration <- c()
  for(i in 1:350){
    file_to_load <- grep(paste0("secsse_ABC_test_param_set_",i,"_ss_",num_ss,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      if(output$n_iter <= 2){
        lam1_init <- c(lam1_init, rep(NA,500))
        lam2_init <- c(lam2_init, rep(NA,500))
        mu1_init <- c(mu1_init, rep(NA,500))
        mu2_init <- c(mu2_init, rep(NA,500))
        q12_init <- c(q12_init, rep(NA,500))
        q21_init <- c(q21_init, rep(NA,500))
        n_iteration <- c(n_iteration,rep(NA,500))
      } else if (nrow(output$ABC[[output$n_iter]]) == 500) {
        lam1_init <- c(lam1_init, output$ABC[[num_iter]][,1])
        lam2_init <- c(lam2_init, output$ABC[[num_iter]][,2])
        mu1_init <- c(mu1_init, output$ABC[[num_iter]][,3])
        mu2_init <- c(mu2_init, output$ABC[[num_iter]][,4])
        q12_init <- c(q12_init, output$ABC[[num_iter]][,5])
        q21_init <- c(q21_init, output$ABC[[num_iter]][,6])
        n_iteration <- c(n_iteration,rep(num_iter,500))
      } else {
        lam1_init <- c(lam1_init, output$ABC[[num_iter-1]][,1])
        lam2_init <- c(lam2_init, output$ABC[[num_iter-1]][,2])
        mu1_init <- c(mu1_init, output$ABC[[num_iter-1]][,3])
        mu2_init <- c(mu2_init, output$ABC[[num_iter-1]][,4])
        q12_init <- c(q12_init, output$ABC[[num_iter-1]][,5])
        q21_init <- c(q21_init, output$ABC[[num_iter-1]][,6])
        n_iteration <- c(n_iteration,rep(num_iter,500))
      }
    } else {
      lam1_init <- c(lam1_init, rep(NA,500))
      lam2_init <- c(lam2_init, rep(NA,500))
      mu1_init <- c(mu1_init, rep(NA,500))
      mu2_init <- c(mu2_init, rep(NA,500))
      q12_init <- c(q12_init, rep(NA,500))
      q21_init <- c(q21_init, rep(NA,500))
      n_iteration <- c(n_iteration,rep(NA,500))
    }
  }
  whole_df_init <- data.frame(param_data2,n_iteration,
                             # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                             lam1_init,lam2_init,mu1_init,mu2_init,q12_init,q21_init)
  save(whole_df_init,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/whole_df_init_test_ss",num_ss,".RData"))

  # whole_df_init$dlam <- (whole_df_init$lam2-whole_df_init$lam1)/(whole_df_init$lam2+whole_df_init$lam1)
  # whole_df_init$dlam_init <- (whole_df_init$lam2_init-whole_df_init$lam1_init)/(whole_df_init$lam2_init+whole_df_init$lam1_init)
  # whole_df_init$dmu <- (whole_df_init$mu2-whole_df_init$mu1)/(whole_df_init$mu2+whole_df_init$mu1)
  # whole_df_init$dmu_init <- (whole_df_init$mu2_init-whole_df_init$mu1_init)/(whole_df_init$mu2_init+whole_df_init$mu1_init)
  # whole_df_init$dq <- (whole_df_init$q12-whole_df_init$q21)/(whole_df_init$q12+whole_df_init$q21)
  # whole_df_init$dq_init <- (whole_df_init$q12_init-whole_df_init$q21_init)/(whole_df_init$q12_init+whole_df_init$q21_init)

  whole_df_init$net_div1 <- (whole_df_init$lam1-whole_df_init$mu1)
  whole_df_init$net_div2 <- (whole_df_init$lam2-whole_df_init$mu2)
  whole_df_init$net_div_init1 <- (whole_df_init$lam1_init-whole_df_init$mu1_init)
  whole_df_init$net_div_init2 <- (whole_df_init$lam2_init-whole_df_init$mu2_init)


  whole_df_init$ext_frac1 <- (whole_df_init$mu1)/(whole_df_init$lam1)
  whole_df_init$ext_frac2 <- (whole_df_init$mu2)/(whole_df_init$lam2)
  whole_df_init$ext_frac_init1 <- (whole_df_init$mu1_init)/(whole_df_init$lam1_init)
  whole_df_init$ext_frac_init2 <- (whole_df_init$mu2_init)/(whole_df_init$lam2_init)
  save(whole_df_init,file =
         paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/delta_whole_df_init_test_ss",num_ss,".RData"))


}


######
# 2. formate MCMC results (only plot the estimation points with ABC results)
# skip
param_data <- load_param_space(param_space_name = paste0("secsse_ABC_test"))
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=5001),] #5001
folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/secsse_MCMC_test")
files <- list.files(folder_path)
lam1_mcmc <- c()
lam2_mcmc <- c()
mu1_mcmc <- c()
mu2_mcmc <- c()
q12_mcmc <- c()
q21_mcmc <- c()
for(i in 1:350){
  file_to_load <- grep(paste0("secsse_MCMC_test_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lam1_mcmc <- c(lam1_mcmc, output[,1]) #4502:5001
    lam2_mcmc <- c(lam2_mcmc, output[,2])
    mu1_mcmc <- c(mu1_mcmc, output[,3])
    mu2_mcmc <- c(mu2_mcmc, output[,4])
    q12_mcmc <- c(q12_mcmc, output[,5])
    q21_mcmc <- c(q21_mcmc, output[,6])
  } else {
    lam1_mcmc <- c(lam1_mcmc, rep(NA,5001)) #500
    lam2_mcmc <- c(lam2_mcmc, rep(NA,5001))
    mu1_mcmc <- c(mu1_mcmc, rep(NA,5001))
    mu2_mcmc <- c(mu2_mcmc, rep(NA,5001))
    q12_mcmc <- c(q12_mcmc, rep(NA,5001))
    q21_mcmc <- c(q21_mcmc, rep(NA,5001))
  }
}
whole_df_MCMC <- data.frame(param_data3,
                            lam1_mcmc,lam2_mcmc,
                            mu1_mcmc,mu2_mcmc,
                            q12_mcmc,q21_mcmc)

save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/whole_df_MCMC_test.RData"))

# whole_df_MCMC$dlam <- (whole_df_MCMC$lam2-whole_df_MCMC$lam1)/(whole_df_MCMC$lam2+whole_df_MCMC$lam1)
# whole_df_MCMC$dlam_mcmc <- (whole_df_MCMC$lam2_mcmc-whole_df_MCMC$lam1_mcmc)/(whole_df_MCMC$lam2_mcmc+whole_df_MCMC$lam1_mcmc)
# whole_df_MCMC$dmu <- (whole_df_MCMC$mu2-whole_df_MCMC$mu1)/(whole_df_MCMC$mu2+whole_df_MCMC$mu1)
# whole_df_MCMC$dmu_mcmc <- (whole_df_MCMC$mu2_mcmc-whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$mu2_mcmc+whole_df_MCMC$mu1_mcmc)
# whole_df_MCMC$dq <- (whole_df_MCMC$q12-whole_df_MCMC$q21)/(whole_df_MCMC$q12+whole_df_MCMC$q21)
# whole_df_MCMC$dq_mcmc <- (whole_df_MCMC$q12_mcmc-whole_df_MCMC$q21_mcmc)/(whole_df_MCMC$q12_mcmc+whole_df_MCMC$q21_mcmc)

whole_df_MCMC$net_div1 <- (whole_df_MCMC$lam1-whole_df_MCMC$mu1)
whole_df_MCMC$net_div2 <- (whole_df_MCMC$lam2-whole_df_MCMC$mu2)
whole_df_MCMC$net_div_MCMC1 <- (whole_df_MCMC$lam1_mcmc-whole_df_MCMC$mu1_mcmc)
whole_df_MCMC$net_div_MCMC2 <- (whole_df_MCMC$lam2_mcmc-whole_df_MCMC$mu2_mcmc)

whole_df_MCMC$ext_frac1 <- (whole_df_MCMC$mu1)/(whole_df_MCMC$lam1)
whole_df_MCMC$ext_frac2 <- (whole_df_MCMC$mu2)/(whole_df_MCMC$lam2)
whole_df_MCMC$ext_frac_MCMC1 <- (whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$lam1_mcmc)
whole_df_MCMC$ext_frac_MCMC2 <- (whole_df_MCMC$mu2_mcmc)/(whole_df_MCMC$lam2_mcmc)

save(whole_df_MCMC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/delta_whole_df_MCMC_test.RData"))

# install.packages("bayesplot")
library(bayesplot)
# plot MCMC trace
# load("G:/results/project 2/tip_info/round4/secsse_long_2/secsse_MCMC_long/secsse_MCMC_long_param_set_1_ss_1.RData")
folder_path <-  paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/secsse_MCMC_test")
files <- list.files(folder_path)
for(i in 1:350){
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("secsse_MCMC_test_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)

  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/MCMC_trace/set_",i,".tiff"),
         units="px", width=2000, height=3000,res = 300,compression="lzw")
    b_mcmc <- coda::as.mcmc(output[,1:6])
    colnames(b_mcmc) <- c("Speciation 1","Speciation 2","Extinction 1","Extinction 2", "Transition 12","Transition 21")
    plot_mcmc <- plot(b_mcmc,trace = T, density = F)
    print(plot_mcmc)
    while (!is.null(dev.list()))  dev.off()
  }
}




######


# 3. formate MLE results
# skip
param_data <- load_param_space(param_space_name = paste0("secsse_ABC_test"))
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/obs_ss_test.rda"))
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/test_MLE_secsse.RData"))
whole_df_MLE <- data.frame(param_data,MLE_all,ss[,1:4])
save(whole_df_MLE,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/whole_df_MLE.RData"))



## median
for (num_ss in c(1)){
  # load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_init_new/delta_whole_df_init_test.RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/delta_whole_df_init_test_ss",num_ss,".RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/delta_whole_df_MCMC_test.RData"))
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/whole_df_MLE.RData"))

  ## get number of iterations and mean values
  df <- whole_df_init
  n <- 500
  ABC_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

  df<-whole_df_MCMC
  n <- 5001
  MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

  MLE_median <- whole_df_MLE


  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/obs_ss_test.rda"))
  ## combine ABC MCMC MLE as "AMM"
  AMM_all_df <- cbind(ABC_median[1:21],
                      MCMC_median[,c(7:12,15,16,19,20)],
                      MLE_median[,c(7:12,20:23)])
  save(AMM_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/AMM_per_set_test_ss",num_ss,".RData"))

  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/AMM_per_set_test_ss",num_ss,".RData"))
  AMM_all_df$dlam1_init <- AMM_all_df$lam1_init - AMM_all_df$lam1
  AMM_all_df$dlam2_init <- AMM_all_df$lam2_init - AMM_all_df$lam2
  AMM_all_df$dmu1_init <- AMM_all_df$mu1_init - AMM_all_df$mu1
  AMM_all_df$dmu2_init <- AMM_all_df$mu2_init - AMM_all_df$mu2
  AMM_all_df$dq12_init <- AMM_all_df$q12_init - AMM_all_df$q12
  AMM_all_df$dq21_init <- AMM_all_df$q21_init - AMM_all_df$q21
  AMM_all_df$tree_size <- ss$tree_size
  AMM_all_df$dlam1_mcmc <- AMM_all_df$lam1_mcmc - AMM_all_df$lam1
  AMM_all_df$dlam2_mcmc <- AMM_all_df$lam2_mcmc - AMM_all_df$lam2
  AMM_all_df$dmu1_mcmc <- AMM_all_df$mu1_mcmc - AMM_all_df$mu1
  AMM_all_df$dmu2_mcmc <- AMM_all_df$mu2_mcmc - AMM_all_df$mu2
  AMM_all_df$dq12_mcmc <- AMM_all_df$q12_mcmc - AMM_all_df$q12
  AMM_all_df$dq21_mcmc <- AMM_all_df$q21_mcmc - AMM_all_df$q21

  AMM_all_df$dlam1_MLE <- AMM_all_df$lam1_MLE - AMM_all_df$lam1
  AMM_all_df$dlam2_MLE <- AMM_all_df$lam2_MLE - AMM_all_df$lam2
  AMM_all_df$dmu1_MLE <- AMM_all_df$mu1_MLE - AMM_all_df$mu1
  AMM_all_df$dmu2_MLE <- AMM_all_df$mu2_MLE - AMM_all_df$mu2
  AMM_all_df$dq12_MLE <- AMM_all_df$q12_MLE - AMM_all_df$q12
  AMM_all_df$dq21_MLE <- AMM_all_df$q21_MLE - AMM_all_df$q21

  AMM_all_df$net_div_MLE1 <- AMM_all_df$lam1_MLE-AMM_all_df$mu1_MLE
  AMM_all_df$net_div_MLE2 <- AMM_all_df$lam2_MLE-AMM_all_df$mu2_MLE

  save(AMM_all_df,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/AMM_per_set_drate_test_ss",num_ss,".RData"))
}



## run\
library(ggplot2)
for(num_ss in c(1)){
  for(i in 1:7){
    load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/AMM_per_set_drate_test_ss",num_ss,".RData"))
    color_values <-c("ABC" = "red3","MCMC" = "green2", "MLE" = "yellow2")
    AMM <- AMM_all_df[(i*50-49):(i*50),]
    p_lam1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,2.0)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_MLE),color = "MLE")) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam1_init),color = "ABC"),shape = 18) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tree size") +
      ggplot2::ylab(expression(lambda[1]))+
      ggplot2::geom_hline(yintercept = AMM$lam1[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_lam2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,2.0)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_MLE),color = "MLE")) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(lam2_init),color = "ABC"),shape = 18) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tree size") +
      ggplot2::ylab(expression(lambda[2]))+
      ggplot2::geom_hline(yintercept = AMM$lam2[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_mu1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,1.5)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_MLE),color = "MLE")) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu1_init),color = "ABC"),shape = 18) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tree size") +
      ggplot2::ylab(expression(mu[1]))+
      ggplot2::geom_hline(yintercept = AMM$mu1[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_mu2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,1.5)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_MLE),color = "MLE")) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(mu2_init),color = "ABC"),shape = 18) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tree size") +
      ggplot2::ylab(expression(mu[2]))+
      ggplot2::geom_hline(yintercept = AMM$mu2[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_q12 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,1.5)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_MLE),color = "MLE")) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q12_init),color = "ABC"),shape = 18) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tree size") +
      ggplot2::ylab(expression(q[12]))+
      ggplot2::geom_hline(yintercept = AMM$q12[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_q21 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,1.5)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_MLE),color = "MLE")) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(q21_init),color = "ABC"),shape = 18) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tree size") +
      ggplot2::ylab(expression(q[21]))+
      ggplot2::geom_hline(yintercept = AMM$q21[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/exact_rate_test",i,"_ss",num_ss,".tiff"),
         units="px", width=3600, height=2000,res = 400,compression="lzw")
    params <- cowplot::plot_grid(
      p_lam1+ggplot2::theme(legend.position = "none"),
      p_mu1+ggplot2::theme(legend.position = "none"),
      p_q12+ggplot2::theme(legend.position = "none"),
      p_lam2+ggplot2::theme(legend.position = "none"),
      p_mu2+ggplot2::theme(legend.position = "none"),
      p_q21+ggplot2::theme(legend.position = "none"),
      align = "hv", nrow = 2, ncol = 3
    )
    legend <- cowplot::get_legend(
      p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
    )
    param_estimates <- cowplot::plot_grid(params,legend,
                                          rel_widths = c(3,0.4)
    )
    print(param_estimates)
    while (!is.null(dev.list()))  dev.off()
  }
}


## net div
for(num_ss in c(1)) {
  for(i in 1:7){
    load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/AMM_per_set_drate_test_ss",num_ss,".RData"))
    color_values <-c("ABC" = "red3","MCMC" = "green2", "MLE" = "yellow2")
    AMM <- AMM_all_df[(i*50-49):(i*50),]
    p_div1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.5)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MLE1),color = "MLE")) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MCMC1),color = "MCMC"),shape = 17,alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_init1),color = "ABC"),shape = 18) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab(expression("Net Diversification State 1")) +
      ggplot2::xlab("Tree size")+
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))+
      ggplot2::geom_hline(yintercept = AMM$net_div1[1], linetype = "dashed", size = 0.5)


    p_div2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(-0.1,1.5)+
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MLE2),color = "MLE")) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_MCMC2),color = "MCMC"),shape = 17,alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(net_div_init2),color = "ABC"),shape = 18) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab(expression("Net Diversification State 2")) +
      ggplot2::xlab("Tree size")+
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))+
      ggplot2::geom_hline(yintercept = AMM$net_div2[1], linetype = "dashed", size = 0.5)

    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/net_div_set",i,"_ss",num_ss,".tiff"),
         units="px", width=2200, height=1000,res = 400,compression="lzw")
    param_estimates <- cowplot::plot_grid(
      p_div1,p_div2,
      align = "hv", nrow = 1, ncol = 2
    )
    print(param_estimates)

    params <- cowplot::plot_grid(
      p_div1+ggplot2::theme(legend.position = "none"),
      p_div2+ggplot2::theme(legend.position = "none"),
      align = "hv", nrow = 1, ncol = 2
    )
    legend <- cowplot::get_legend(
      p_div1 + theme(legend.box.margin = margin(0, 0, 0, 6))
    )
    param_estimates <- cowplot::plot_grid(params,legend,
                                          rel_widths = c(3,0.5)
    )
    print(param_estimates)
    while (!is.null(dev.list()))  dev.off()
  }
}


# tip_ratio vs drate
# run
library(ggplot2)
for(num_ss in c(1)){
  for(i in 1:7){
    load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/AMM_per_set_drate_test_ss",num_ss,".RData"))
    color_values <-c("ABC" = "red3","MCMC" = "green2", "MLE" = "yellow2")
    AMM <- AMM_all_df[(i*50-49):(i*50),]
    p_lam1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,2.0)+
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(lam1_MLE),color = "MLE")) +
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(lam1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(lam1_init),color = "ABC"),shape = 18) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tree size") +
      ggplot2::ylab(expression(lambda[1]))+
      ggplot2::geom_hline(yintercept = AMM$lam1[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_lam2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,2.0)+
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(lam2_MLE),color = "MLE")) +
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(lam2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(lam2_init),color = "ABC"),shape = 18) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tree size") +
      ggplot2::ylab(expression(lambda[2]))+
      ggplot2::geom_hline(yintercept = AMM$lam2[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_mu1 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,1.5)+
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(mu1_MLE),color = "MLE")) +
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(mu1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(mu1_init),color = "ABC"),shape = 18) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tree size") +
      ggplot2::ylab(expression(mu[1]))+
      ggplot2::geom_hline(yintercept = AMM$mu1[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_mu2 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,1.5)+
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(mu2_MLE),color = "MLE")) +
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(mu2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(mu2_init),color = "ABC"),shape = 18) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tree size") +
      ggplot2::ylab(expression(mu[2]))+
      ggplot2::geom_hline(yintercept = AMM$mu2[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_q12 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,1.5)+
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(q12_MLE),color = "MLE")) +
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(q12_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(q12_init),color = "ABC"),shape = 18) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tree size") +
      ggplot2::ylab(expression(q[12]))+
      ggplot2::geom_hline(yintercept = AMM$q12[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    p_q21 <-ggplot2::ggplot(data = AMM) +
      ggplot2::theme_bw() +
      ggplot2::ylim(0,1.5)+
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(q21_MLE),color = "MLE")) +
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(q21_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = tip_ratio,y = abs(q21_init),color = "ABC"),shape = 18) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 15),
                     text = ggplot2::element_text(size = 15)) +
      ggplot2::xlab("Tree size") +
      ggplot2::ylab(expression(q[21]))+
      ggplot2::geom_hline(yintercept = AMM$q21[1], linetype = "dashed", size = 0.3) +
      ggplot2::scale_color_manual(name = "Method",
                                  values = color_values,
                                  labels = c("ABC", "MCMC", "MLE"))

    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/tipratip_exact_rate_test",i,"_ss",num_ss,".tiff"),
         units="px", width=3600, height=2000,res = 400,compression="lzw")
    params <- cowplot::plot_grid(
      p_lam1+ggplot2::theme(legend.position = "none"),
      p_mu1+ggplot2::theme(legend.position = "none"),
      p_q12+ggplot2::theme(legend.position = "none"),
      p_lam2+ggplot2::theme(legend.position = "none"),
      p_mu2+ggplot2::theme(legend.position = "none"),
      p_q21+ggplot2::theme(legend.position = "none"),
      align = "hv", nrow = 2, ncol = 3
    )
    legend <- cowplot::get_legend(
      p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
    )
    param_estimates <- cowplot::plot_grid(params,legend,
                                          rel_widths = c(3,0.4)
    )
    print(param_estimates)
    while (!is.null(dev.list()))  dev.off()
  }
}




#
for(test in c(1,2,3,4)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/AMM_per_set_drate_test_ss0.RData"))
  color_values <-c("ABC" = "red3","MCMC" = "green2", "MLE" = "yellow2")
  p_lam1 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.3,2)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (lam1_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (lam1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (lam1_init),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~lambda[1]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$lam1[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_lam2 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.3,2)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (lam2_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (lam2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (lam2_init),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~lambda[2]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$lam2[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_mu1 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.3,2)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (mu1_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (mu1_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (mu1_init),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~mu[1]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$mu1[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_mu2 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.3,2)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (mu2_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (mu2_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (mu2_init),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~mu[2]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$mu2[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_q12 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.3,2)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (q12_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (q12_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (q12_init),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~q[12]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$q12[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  p_q21 <-ggplot2::ggplot(data = AMM_all_df) +
    ggplot2::theme_bw() +
    ggplot2::ylim(-0.3,2)+
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (q21_MLE),color = "MLE")) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (q21_mcmc),color = "MCMC"),shape = 17,alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = tree_size,y = (q21_init),color = "ABC"),shape = 18) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::xlab("Tree size") +
    ggplot2::ylab(expression(Delta~q[21]))+
    ggplot2::geom_hline(yintercept = AMM_all_df$q21[1], linetype = "dashed", size = 0.3) +
    ggplot2::scale_color_manual(name = "Method",
                                values = color_values,
                                labels = c("ABC", "MCMC", "MLE"))

  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/rate_error_test.tiff"),
       units="px", width=3000, height=2000,res = 400,compression="lzw")
  params <- cowplot::plot_grid(
    p_lam1+ggplot2::theme(legend.position = "none"),
    p_mu1+ggplot2::theme(legend.position = "none"),
    p_q12+ggplot2::theme(legend.position = "none"),
    p_lam2+ggplot2::theme(legend.position = "none"),
    p_mu2+ggplot2::theme(legend.position = "none"),
    p_q21+ggplot2::theme(legend.position = "none"),
    align = "hv", nrow = 2, ncol = 3
  )
  legend <- cowplot::get_legend(
    p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
  )
  param_estimates <- cowplot::plot_grid(params,legend,
                                        rel_widths = c(3,0.4)
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()
}

#####
library(coda)
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/secsse_MCMC_test"
files <- list.files(folder_path)
param_data <- load_param_space(param_space_name = paste0("secsse_ABC_test"))


lam1_cor <- c()
lam2_cor <- c()
mu1_cor <- c()
mu2_cor <- c()
q12_cor <- c()
q21_cor <- c()

seq <- seq(1,5001,2)
for(i in 1:350){
  file_to_load <- grep(paste0("secsse_MCMC_test_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)

  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lam1_cor <- c(lam1_cor,autocorr(coda::as.mcmc(output[seq,1]), lags = c(1), relative=TRUE))
    lam2_cor <- c(lam2_cor,autocorr(coda::as.mcmc(output[seq,2]), lags = c(1), relative=TRUE))
    mu1_cor <- c(mu1_cor,autocorr(coda::as.mcmc(output[seq,3]), lags = c(1), relative=TRUE))
    mu2_cor <- c(mu2_cor,autocorr(coda::as.mcmc(output[seq,4]), lags = c(1), relative=TRUE))
    q12_cor <- c(q12_cor,autocorr(coda::as.mcmc(output[seq,5]), lags = c(1), relative=TRUE))
    q21_cor <- c(q21_cor,autocorr(coda::as.mcmc(output[seq,6]), lags = c(1), relative=TRUE))

  } else {
    lam1_cor <- c(lam1_cor,NA)
    lam2_cor <- c(lam2_cor,NA)
    mu1_cor <- c(mu1_cor,NA)
    mu2_cor <- c(mu2_cor,NA)
    q12_cor <- c(q12_cor,NA)
    q21_cor <- c(q21_cor,NA)
  }
}

whole_df_cor <- data.frame(param_data,
                           lam1_cor,lam2_cor,mu1_cor,mu2_cor,q12_cor,q21_cor)

save(whole_df_cor,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_latest/whole_df_cor_lag1.RData"))

plot(density(whole_df_cor[,7],na.rm = T))

median(whole_df_cor[,7],na.rm = T)
median(whole_df_cor[,8],na.rm = T)
median(whole_df_cor[,9],na.rm = T)
median(whole_df_cor[,10],na.rm = T)
median(whole_df_cor[,11],na.rm = T)
median(whole_df_cor[,12],na.rm = T)
