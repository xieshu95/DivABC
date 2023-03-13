load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/obs_ss_with_pars.RData")
## ABC results
folder_path <- "G:/results/project 2/tip_info/round4/adap_secsse_new_space/secsse_ABC"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC.csv")

param_data2<-param_data[rep(seq_len(nrow(param_data)), each=500),] #1000

for(n in c(0,20,30)){
  lam1_abc <- c()
  lam2_abc <- c()
  mu1_abc <- c()
  mu2_abc <- c()
  q12_abc <- c()
  q21_abc <- c()
  n_iter <- c()
  n_iteration <- c()
  for(i in 1:27){
    # if(i%%5 == 0){
    #   rep <- 5
    # } else {
    #   rep <- i%%5
    # }
    # param_set = (param_num-1)*5 + i
    file_to_load <- grep(paste0("secsse_ABC_param_set_",  i,"_ss_",n,".RData"),  #,"_rep",rep
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
  save(whole_df_ABC,file = paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/whole_df_ABC_ss_set",n,".RData"))
}


load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/whole_df_ABC_ss_set",30,".RData"))
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
whole_df_ABC$dmu[which(is.na(whole_df_ABC$dmu))] <-0


whole_df_ABC$ext_frac1 <- (whole_df_ABC$mu1)/(whole_df_ABC$lam1)
whole_df_ABC$ext_frac2 <- (whole_df_ABC$mu2)/(whole_df_ABC$lam2)
whole_df_ABC$ext_frac_ABC1 <- (whole_df_ABC$mu1_abc)/(whole_df_ABC$lam1_abc)
whole_df_ABC$ext_frac_ABC2 <- (whole_df_ABC$mu2_abc)/(whole_df_ABC$lam2_abc)
save(whole_df_ABC,file =
       paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_whole_df_ABC_ss_set",30,".RData"))


#### MCMC results

param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC.csv")
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=1001),] #5001
folder_path <- "G:/results/project 2/tip_info/round4/adap_secsse_new_space/secsse_MCMC"
files <- list.files(folder_path)
lam1_mcmc <- c()
lam2_mcmc <- c()
mu1_mcmc <- c()
mu2_mcmc <- c()
q12_mcmc <- c()
q21_mcmc <- c()
for(i in 1:27){
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("secsse_MCMC_param_set_", i,"_ss_1.RData"), #"_rep",rep,
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
save(whole_df_MCMC,file = "G:/results/project 2/tip_info/round4/adap_secsse_new_space/whole_df_MCMC_1001.RData")

load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/whole_df_MCMC_1001.RData")
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
whole_df_MCMC$dmu[which(is.na(whole_df_MCMC$dmu))] <-0

whole_df_MCMC$ext_frac1 <- (whole_df_MCMC$mu1)/(whole_df_MCMC$lam1)
whole_df_MCMC$ext_frac2 <- (whole_df_MCMC$mu2)/(whole_df_MCMC$lam2)
whole_df_MCMC$ext_frac_MCMC1 <- (whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$lam1_mcmc)
whole_df_MCMC$ext_frac_MCMC2 <- (whole_df_MCMC$mu2_mcmc)/(whole_df_MCMC$lam2_mcmc)

save(whole_df_MCMC,file = "G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_whole_df_MCMC_1001.RData")

#####
# plot MCMC results
# load("G:/results/project 2/tip_info/round4/secsse_long_2/secsse_MCMC_long/secsse_MCMC_long_param_set_1_ss_1.RData")

folder_path <- "G:/results/project 2/tip_info/round4/adap_secsse_new_space/secsse_MCMC"
files <- list.files(folder_path)
for(i in 1:27){
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("secsse_MCMC_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)

  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MCMC_trace_1001/set_",i,"_lam.tiff"),
         units="px", width=2000, height=2000,res = 300,compression="lzw")
    b_mcmc <- coda::as.mcmc(output[,1:2])
    plot_mcmc <- plot(b_mcmc)
    print(plot_mcmc)
    while (!is.null(dev.list()))  dev.off()

    tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MCMC_trace_1001/set_",i,"_mu.tiff"),
         units="px", width=2000, height=2000,res = 300,compression="lzw")
    b_mcmc <- coda::as.mcmc(output[,3:4])
    plot_mcmc <- plot(b_mcmc)
    print(plot_mcmc)
    while (!is.null(dev.list()))  dev.off()

    tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MCMC_trace_1001/set_",i,"_q.tiff"),
         units="px", width=2000, height=2000,res = 300,compression="lzw")
    b_mcmc <- coda::as.mcmc(output[,5:6])
    plot_mcmc <- plot(b_mcmc)
    print(plot_mcmc)
    while (!is.null(dev.list()))  dev.off()
  }
}


#####
# combine several reps(sign as ss_set) 27*500
load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_whole_df_ABC_ss_set0.RData"))
whole_df_ABC_0 <- whole_df_ABC
load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_whole_df_ABC_ss_set20.RData"))
whole_df_ABC_20 <- whole_df_ABC
load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_whole_df_ABC_ss_set30.RData"))
whole_df_ABC_30 <- whole_df_ABC

# try1 <- whole_df_ABC_0[c(1,2,3,501,502,503,1501,1502,1503),]
# try2 <- whole_df_ABC_20[c(1,2,3,501,502,503,1501,1502,1503),]
# try3 <- whole_df_ABC_30[c(1,2,3,501,502,503,1501,1502,1503),]

set <- rep(1:27,each = 500)
df1 <- data.frame(set,whole_df_ABC_0)
df2 <- data.frame(set,whole_df_ABC_20)
df3 <- data.frame(set,whole_df_ABC_30)
df_merge <- rbind(df1,df2,df3)
df_merge<-df_merge[order(df_merge$set),]
save(df_merge,file =
       paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/whole_ABC_merge.RData"))


load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/whole_ABC_merge.RData")


## combine ABC, MCMC, MLE for each parameter set(use median value)
load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/whole_ABC_merge.RData"))
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/delta_whole_df_MCMC_1001.RData")
load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/combined_MLE.RData"))

## get number of iterations and mean values
df <- df_merge
n <- 1500
ABC_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
# n <- 5000
# whole_df_ABC_median_group <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median,na.rm = TRUE)[-1]

df<-whole_df_MCMC
n <- 1001
MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
# n <- 10010
# whole_df_MCMC_median_group <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median,na.rm = TRUE)[-1]

df<- MLE_all
n <- 100
MLE_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median,na.rm = TRUE)[-1]


######
## combine ABC MCMC MLE as "AMM"
AMM_all_df <- cbind(ABC_median[1:14],
                    MCMC_median[,c(7:12)],
                    MLE_median[,c(7:12)])
save(AMM_all_df,file = "G:/results/project 2/tip_info/round4/adap_secsse_new_space/AMM_per_set.RData")

# AMM_reorder <- AMM_all_df[,c(1,8,28,41, 2,9,29,42, 3,10,30,43,
#                              4,11,31,44, 5,12,32,45, 6,13,33,46,
#                              20:23,37,38,50,51, 24:27,39,40,52,53)]
#
# AMM_reorder<-round(AMM_reorder,5)
# save(AMM_reorder,file = "G:/results/project 2/tip_info/round4/adap_secsse_new_space/AMM_reorder.RData")
# AMM_group <- cbind(whole_df_ABC_median_group,
#                    whole_df_MCMC_median_group[,7:12],
#                    whole_df_MLE_median_group[,7:12])
# save(AMM_group,file = "G:/results/project 2/tip_info/round4/adap_secsse/ABC_MCMC_MLE_per_group.RData")

load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/AMM_per_set.RData")
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/obs_ss_with_pars.RData")
# load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/AMM_reorder.RData")
AMM_all_df$dlam1_abc <- AMM_all_df$lam1_abc - AMM_all_df$lam1
AMM_all_df$dlam2_abc <- AMM_all_df$lam2_abc - AMM_all_df$lam2
AMM_all_df$dmu1_abc <- AMM_all_df$mu1_abc - AMM_all_df$mu1
AMM_all_df$dmu2_abc <- AMM_all_df$mu2_abc - AMM_all_df$mu2
AMM_all_df$dq12_abc <- AMM_all_df$q12_abc - AMM_all_df$q12
AMM_all_df$dq21_abc <- AMM_all_df$q21_abc - AMM_all_df$q21

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

AMM_all_df$tree_size <- pars_ss$total
AMM_all_df$tip_ratio1 <- pars_ss$state1/pars_ss$state2
AMM_all_df$tip_ratio <- AMM_all_df$tip_ratio1
AMM_all_df$tip_ratio[AMM_all_df$tip_ratio < 1]<- 1/AMM_all_df$tip_ratio[AMM_all_df$tip_ratio < 1]
save(AMM_all_df,file = "G:/results/project 2/tip_info/round4/adap_secsse_new_space/AMM_per_set_drate.RData")


## plot observed treesize /tip ratio vs estimation error
color_values <-c("ABC" = "red3","MCMC" = "blue", "MLE" = "green3")
p_lam1 <-ggplot2::ggplot(data = AMM_all_df) +
  ggplot2::theme_bw() +
  ggplot2::ylim(0,0.4)+
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dlam1_mcmc),color = "MCMC")) +
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dlam1_MLE),color = "MLE")) +
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dlam1_abc),color = "ABC")) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(lambda[1]))+
  ggplot2::scale_color_manual(name = "Method",
                             values = color_values,
                             labels = c("ABC", "MCMC", "MLE"))

p_lam2 <-ggplot2::ggplot(data = AMM_all_df) +
  ggplot2::theme_bw() +
  ggplot2::ylim(0,0.4)+
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dlam2_mcmc),color = "MCMC")) +
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dlam2_MLE),color = "MLE")) +
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dlam2_abc),color = "ABC")) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(lambda[2]))+
  ggplot2::scale_color_manual(name = "Method",
                              values = color_values,
                              labels = c("ABC", "MCMC", "MLE"))

p_mu1 <-ggplot2::ggplot(data = AMM_all_df) +
  ggplot2::theme_bw() +
  ggplot2::ylim(0,0.4)+
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dmu1_mcmc),color = "MCMC")) +
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dmu1_MLE),color = "MLE")) +
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dmu1_abc),color = "ABC")) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(mu[1]))+
  ggplot2::scale_color_manual(name = "Method",
                              values = color_values,
                              labels = c("ABC", "MCMC", "MLE"))

p_mu2 <-ggplot2::ggplot(data = AMM_all_df) +
  ggplot2::theme_bw() +
  ggplot2::ylim(0,0.4)+
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dmu2_mcmc),color = "MCMC")) +
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dmu2_MLE),color = "MLE")) +
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dmu2_abc),color = "ABC")) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(mu[2]))+
  ggplot2::scale_color_manual(name = "Method",
                              values = color_values,
                              labels = c("ABC", "MCMC", "MLE"))

p_q12 <-ggplot2::ggplot(data = AMM_all_df) +
  ggplot2::theme_bw() +
  ggplot2::ylim(0,0.4)+
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dq12_mcmc),color = "MCMC")) +
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dq12_MLE),color = "MLE")) +
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dq12_abc),color = "ABC")) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(q[12]))+
  ggplot2::scale_color_manual(name = "Method",
                              values = color_values,
                              labels = c("ABC", "MCMC", "MLE"))

p_q21 <-ggplot2::ggplot(data = AMM_all_df) +
  ggplot2::theme_bw() +
  ggplot2::ylim(0,0.4)+
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dq21_mcmc),color = "MCMC")) +
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dq21_MLE),color = "MLE")) +
  ggplot2::geom_point(ggplot2::aes(x = tree_size,y = abs(dq21_abc),color = "ABC")) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Tree size") +
  ggplot2::ylab(expression(q[21]))+
  ggplot2::scale_color_manual(name = "Method",
                              values = color_values,
                              labels = c("ABC", "MCMC", "MLE"))

tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/obs_rate_error.tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
  align = "hv", nrow = 2, ncol = 3
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()
