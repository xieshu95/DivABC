## ABC results
folder_path <- "G:/results/project 2/tip_info/round4/adap_secsse/secsse_ABC_long"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC_long.csv")

param_data2<-param_data[rep(seq_len(nrow(param_data)), each=500),] #1000

for(n in c(0)){
  lam1_abc <- c()
  lam2_abc <- c()
  mu1_abc <- c()
  mu2_abc <- c()
  q12_abc <- c()
  q21_abc <- c()
  n_iter <- c()
  n_iteration <- c()
  for(i in 1:70){
    # if(i%%5 == 0){
    #   rep <- 5
    # } else {
    #   rep <- i%%5
    # }
    # param_set = (param_num-1)*5 + i
    file_to_load <- grep(paste0("secsse_ABC_long_param_set_",  i,"_ss_",n,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      if(output$n_iter <= 2){
        lam1_abc <- c(lam1_abc, rep(NA,500))
        lam2_abc <- c(lam2_abc, rep(NA,500))
        mu1_abc <- c(mu1_abc, rep(NA,500))
        mu2_abc <- c(mu2_abc, rep(NA,500))
        q12_abc <- c(q12_abc, rep(NA,500))
        q21_abc <- c(q21_abc, rep(NA,500))
        n_iteration <- c(n_iteration,rep(NA,500))
      } else if (nrow(output$ABC[[output$n_iter]]) == 500) {
        lam1_abc <- c(lam1_abc, output$ABC[[num_iter]][,1])
        lam2_abc <- c(lam2_abc, output$ABC[[num_iter]][,2])
        mu1_abc <- c(mu1_abc, output$ABC[[num_iter]][,3])
        mu2_abc <- c(mu2_abc, output$ABC[[num_iter]][,4])
        q12_abc <- c(q12_abc, output$ABC[[num_iter]][,5])
        q21_abc <- c(q21_abc, output$ABC[[num_iter]][,6])
        n_iteration <- c(n_iteration,rep(num_iter,500))
      } else {
        lam1_abc <- c(lam1_abc, output$ABC[[num_iter-1]][,1])
        lam2_abc <- c(lam2_abc, output$ABC[[num_iter-1]][,2])
        mu1_abc <- c(mu1_abc, output$ABC[[num_iter-1]][,3])
        mu2_abc <- c(mu2_abc, output$ABC[[num_iter-1]][,4])
        q12_abc <- c(q12_abc, output$ABC[[num_iter-1]][,5])
        q21_abc <- c(q21_abc, output$ABC[[num_iter-1]][,6])
        n_iteration <- c(n_iteration,rep(num_iter,500))
      }
    } else {
      lam1_abc <- c(lam1_abc, rep(NA,500))
      lam2_abc <- c(lam2_abc, rep(NA,500))
      mu1_abc <- c(mu1_abc, rep(NA,500))
      mu2_abc <- c(mu2_abc, rep(NA,500))
      q12_abc <- c(q12_abc, rep(NA,500))
      q21_abc <- c(q21_abc, rep(NA,500))
      n_iteration <- c(n_iteration,rep(NA,500))
    }
  }
  whole_df_ABC <- data.frame(param_data2,n_iteration,
                             # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                             lam1_abc,lam2_abc,mu1_abc,mu2_abc,q12_abc,q21_abc)
  save(whole_df_ABC,file = paste0("G:/results/project 2/tip_info/round4/adap_secsse/whole_df_ABC_ss_set",n,".RData"))
}


load(paste0("G:/results/project 2/tip_info/round4/adap_secsse/whole_df_ABC_ss_set",0,".RData"))
whole_df_ABC$dlam <- (whole_df_ABC$lam2-whole_df_ABC$lam1)/(whole_df_ABC$lam2+whole_df_ABC$lam1)
whole_df_ABC$dlam_ABC <- (whole_df_ABC$lam2_abc-whole_df_ABC$lam1_abc)/(whole_df_ABC$lam2_abc+whole_df_ABC$lam1_abc)
whole_df_ABC$dmu <- (whole_df_ABC$mu2-whole_df_ABC$mu1)/(whole_df_ABC$mu2+whole_df_ABC$mu1)
whole_df_ABC$dmu_ABC <- (whole_df_ABC$mu2_abc-whole_df_ABC$mu1_abc)/(whole_df_ABC$mu2_abc+whole_df_ABC$mu1_abc)
whole_df_ABC$dq <- (whole_df_ABC$q12-whole_df_ABC$q21)/(whole_df_ABC$q12+whole_df_ABC$q21)
whole_df_ABC$dq_ABC <- (whole_df_ABC$q12_abc-whole_df_ABC$q21_abc)/(whole_df_ABC$q12_abc+whole_df_ABC$q21_abc)

whole_df_ABC$net_div1 <- (whole_df_ABC$lam1-whole_df_ABC$mu1)
whole_df_ABC$net_div2 <- (whole_df_ABC$lam2-whole_df_ABC$mu2)
whole_df_ABC$net_div_ABC1 <- (whole_df_ABC$lam1_abc-whole_df_ABC$mu1_abc)
whole_df_ABC$net_div_ABC2 <- (whole_df_ABC$lam2_abc-whole_df_ABC$mu2_abc)
whole_df_ABC$dmu[1:20000] <-0


whole_df_ABC$ext_frac1 <- (whole_df_ABC$mu1)/(whole_df_ABC$lam1)
whole_df_ABC$ext_frac2 <- (whole_df_ABC$mu2)/(whole_df_ABC$lam2)
whole_df_ABC$ext_frac_ABC1 <- (whole_df_ABC$mu1_abc)/(whole_df_ABC$lam1_abc)
whole_df_ABC$ext_frac_ABC2 <- (whole_df_ABC$mu2_abc)/(whole_df_ABC$lam2_abc)
save(whole_df_ABC,file = paste0("G:/results/project 2/tip_info/round4/adap_secsse/delta_whole_df_ABC_ss_set",0,".RData"))


#### MCMC results
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC_long.csv")
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=1001),] #5001
folder_path <- "G:/results/project 2/tip_info/round4/adap_secsse/secsse_MCMC_long"
files <- list.files(folder_path)
lam1_mcmc <- c()
lam2_mcmc <- c()
mu1_mcmc <- c()
mu2_mcmc <- c()
q12_mcmc <- c()
q21_mcmc <- c()
for(i in 1:70){
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("secsse_MCMC_long_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)

  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lam1_mcmc <- c(lam1_mcmc, output[,1])
    lam2_mcmc <- c(lam2_mcmc, output[,2])
    mu1_mcmc <- c(mu1_mcmc, output[,3])
    mu2_mcmc <- c(mu2_mcmc, output[,4])
    q12_mcmc <- c(q12_mcmc, output[,5])
    q21_mcmc <- c(q21_mcmc, output[,6])
  } else {
    lam1_mcmc <- c(lam1_mcmc, rep(NA,1001))
    lam2_mcmc <- c(lam2_mcmc, rep(NA,1001))
    mu1_mcmc <- c(mu1_mcmc, rep(NA,1001))
    mu2_mcmc <- c(mu2_mcmc, rep(NA,1001))
    q12_mcmc <- c(q12_mcmc, rep(NA,1001))
    q21_mcmc <- c(q21_mcmc, rep(NA,1001))
  }
}

whole_df_MCMC <- data.frame(param_data3,
                            lam1_mcmc,lam2_mcmc,
                            mu1_mcmc,mu2_mcmc,
                            q12_mcmc,q21_mcmc)
#lac_abc,mu_abc,gam_abc,laa_abc,n_iter)
save(whole_df_MCMC,file = "G:/results/project 2/tip_info/round4/adap_secsse/whole_df_MCMC.RData")

load("G:/results/project 2/tip_info/round4/adap_secsse/whole_df_MCMC.RData")
whole_df_MCMC$dlam <- (whole_df_MCMC$lam2-whole_df_MCMC$lam1)/(whole_df_MCMC$lam2+whole_df_MCMC$lam1)
whole_df_MCMC$dlam_mcmc <- (whole_df_MCMC$lam2_mcmc-whole_df_MCMC$lam1_mcmc)/(whole_df_MCMC$lam2_mcmc+whole_df_MCMC$lam1_mcmc)
whole_df_MCMC$dmu <- (whole_df_MCMC$mu2-whole_df_MCMC$mu1)/(whole_df_MCMC$mu2+whole_df_MCMC$mu1)
whole_df_MCMC$dmu_mcmc <- (whole_df_MCMC$mu2_mcmc-whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$mu2_mcmc+whole_df_MCMC$mu1_mcmc)
whole_df_MCMC$dq <- (whole_df_MCMC$q12-whole_df_MCMC$q21)/(whole_df_MCMC$q12+whole_df_MCMC$q21)
whole_df_MCMC$dq_mcmc <- (whole_df_MCMC$q12_mcmc-whole_df_MCMC$q21_mcmc)/(whole_df_MCMC$q12_mcmc+whole_df_MCMC$q21_mcmc)

whole_df_MCMC$net_div1 <- (whole_df_MCMC$lam1-whole_df_MCMC$mu1)
whole_df_MCMC$net_div2 <- (whole_df_MCMC$lam2-whole_df_MCMC$mu2)
whole_df_MCMC$net_div_MCMC1 <- (whole_df_MCMC$lam1_mcmc-whole_df_MCMC$mu1_mcmc)
whole_df_MCMC$net_div_MCMC2 <- (whole_df_MCMC$lam2_mcmc-whole_df_MCMC$mu2_mcmc)
whole_df_MCMC$dmu[1:200040] <-0

whole_df_MCMC$ext_frac1 <- (whole_df_MCMC$mu1)/(whole_df_MCMC$lam1)
whole_df_MCMC$ext_frac2 <- (whole_df_MCMC$mu2)/(whole_df_MCMC$lam2)
whole_df_MCMC$ext_frac_MCMC1 <- (whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$lam1_mcmc)
whole_df_MCMC$ext_frac_MCMC2 <- (whole_df_MCMC$mu2_mcmc)/(whole_df_MCMC$lam2_mcmc)

save(whole_df_MCMC,file = "G:/results/project 2/tip_info/round4/adap_secsse/delta_whole_df_MCMC.RData")

#####
# plot MCMC results
# load("G:/results/project 2/tip_info/round4/secsse_long_2/secsse_MCMC_long/secsse_MCMC_long_param_set_1_ss_1.RData")

folder_path <- "G:/results/project 2/tip_info/round4/secsse_long/secsse_MCMC_long"
files <- list.files(folder_path)
for(i in 1:70){
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("secsse_MCMC_long_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)

  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    tiff(paste0("G:/results/project 2/tip_info/round4/secsse_long_2/MCMC_trace_5001/set_",i,"_lam.tiff"),
         units="px", width=2000, height=2000,res = 300,compression="lzw")
    b_mcmc <- coda::as.mcmc(output[,1:2])
    plot_mcmc <- plot(b_mcmc)
    print(plot_mcmc)
    while (!is.null(dev.list()))  dev.off()

    tiff(paste0("G:/results/project 2/tip_info/round4/secsse_long_2/MCMC_trace_5001/set_",i,"_mu.tiff"),
         units="px", width=2000, height=2000,res = 300,compression="lzw")
    b_mcmc <- coda::as.mcmc(output[,3:4])
    plot_mcmc <- plot(b_mcmc)
    print(plot_mcmc)
    while (!is.null(dev.list()))  dev.off()

    tiff(paste0("G:/results/project 2/tip_info/round4/secsse_long_2/MCMC_trace_5001/set_",i,"_q.tiff"),
         units="px", width=2000, height=2000,res = 300,compression="lzw")
    b_mcmc <- coda::as.mcmc(output[,5:6])
    plot_mcmc <- plot(b_mcmc)
    print(plot_mcmc)
    while (!is.null(dev.list()))  dev.off()
  }
}



## combine ABC, MCMC, MLE for each parameter set(use median value)
load(paste0("G:/results/project 2/tip_info/round4/adap_secsse/delta_whole_df_ABC_ss_set0.RData"))
load("G:/results/project 2/tip_info/round4/adap_secsse/delta_whole_df_MCMC.RData")
load("G:/results/project 2/tip_info/round4/adap_secsse/delta_MLE_secsse_ABC.RData")

## get number of iterations and mean values
df <- whole_df_ABC
n <- 500
ABC_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
# n <- 5000
# whole_df_ABC_median_group <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median,na.rm = TRUE)[-1]

df<-whole_df_MCMC
n <- 5001
MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
# n <- 10010
# whole_df_MCMC_median_group <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median,na.rm = TRUE)[-1]

df<- MLE_all
# n <- 10
# whole_df_MLE_median_group <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median,na.rm = TRUE)[-1]


## combine ABC MCMC MLE as "AMM"
AMM_all_df <- cbind(ABC_median,
                MCMC_median[,c(7:12,14,16,18,21,22,25,26)],
                MLE_all[,c(7:12,14,16,18,21,22,25,26)])
save(AMM_all_df,file = "G:/results/project 2/tip_info/round4/adap_secsse/AMM_per_set.RData")

AMM_reorder <- AMM_all_df[,c(1,8,28,41, 2,9,29,42, 3,10,30,43,
                             4,11,31,44, 5,12,32,45, 6,13,33,46,
                             20:23,37,38,50,51, 24:27,39,40,52,53)]

AMM_reorder<-round(AMM_reorder,5)
save(AMM_reorder,file = "G:/results/project 2/tip_info/round4/adap_secsse/AMM_reorder.RData")
# AMM_group <- cbind(whole_df_ABC_median_group,
#                    whole_df_MCMC_median_group[,7:12],
#                    whole_df_MLE_median_group[,7:12])
# save(AMM_group,file = "G:/results/project 2/tip_info/round4/adap_secsse/ABC_MCMC_MLE_per_group.RData")


load("G:/results/project 2/tip_info/round4/adap_secsse/AMM_per_set.RData")
load("G:/results/project 2/tip_info/round4/adap_secsse/AMM_reorder.RData")
