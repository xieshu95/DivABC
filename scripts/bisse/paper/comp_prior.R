for (num_ss in c(0)){
  # formate results
  load(paste0("Data/BiSSE/obs_ss.rda"))
  ## ABC results
  folder_path <- paste0("Data/BiSSE/exponential_prior/ABC")
  files <- list.files(folder_path)
  param_data <- load_param_space(param_space_name = paste0("bisse_ABC_test"))
  param_data <- param_data[1:150,]
  param_data2<-param_data[rep(seq_len(nrow(param_data)), each=500),] #500
  lam1_abc <- c()
  lam2_abc <- c()
  mu1_abc <- c()
  mu2_abc <- c()
  q12_abc <- c()
  q21_abc <- c()
  n_iter <- c()
  n_iteration <- c()
  for(i in 1:150){
    file_to_load <- grep(paste0("bisse_ABC_test_param_set_",i,"_ss_",num_ss,".RData"),  #,"_rep",rep
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
                             lam1_abc,lam2_abc,mu1_abc,mu2_abc,q12_abc,q21_abc)
  save(whole_df_ABC,file = paste0("Data/BiSSE/exponential_prior/whole_df_ABC_test_ss",num_ss,".RData"))
  whole_df_ABC$net_div1 <- (whole_df_ABC$lam1-whole_df_ABC$mu1)
  whole_df_ABC$net_div2 <- (whole_df_ABC$lam2-whole_df_ABC$mu2)
  whole_df_ABC$net_div_ABC1 <- (whole_df_ABC$lam1_abc-whole_df_ABC$mu1_abc)
  whole_df_ABC$net_div_ABC2 <- (whole_df_ABC$lam2_abc-whole_df_ABC$mu2_abc)
  whole_df_ABC$ext_frac1 <- (whole_df_ABC$mu1)/(whole_df_ABC$lam1)
  whole_df_ABC$ext_frac2 <- (whole_df_ABC$mu2)/(whole_df_ABC$lam2)
  whole_df_ABC$ext_frac_ABC1 <- (whole_df_ABC$mu1_abc)/(whole_df_ABC$lam1_abc)
  whole_df_ABC$ext_frac_ABC2 <- (whole_df_ABC$mu2_abc)/(whole_df_ABC$lam2_abc)
  whole_df_ABC$init_obs <- rep(c(rep(0,25*500),rep(1,25*500)),3)
  save(whole_df_ABC,file =
         paste0("Data/BiSSE/exponential_prior/delta_whole_df_ABC_test_ss",num_ss,".RData"))

}



# compare of MCMC results with uniform and exponential distribution
param_data <- load_param_space(param_space_name = paste0("bisse_ABC_test"))
param_data <- param_data[1:150,]
param_data3<-param_data[rep(seq_len(nrow(param_data)), each=5001),]
folder_path <- paste0("Data/BiSSE/exponential_prior/MCMC")
files <- list.files(folder_path)
lam1_mcmc <- c()
lam2_mcmc <- c()
mu1_mcmc <- c()
mu2_mcmc <- c()
q12_mcmc <- c()
q21_mcmc <- c()
seq <- seq(5001,10001,1)
for(i in 1:150){
  file_to_load <- grep(paste0("bisse_MCMC_test_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    lam1_mcmc <- c(lam1_mcmc, output[seq,1]) #4502:2501
    lam2_mcmc <- c(lam2_mcmc, output[seq,2])
    mu1_mcmc <- c(mu1_mcmc, output[seq,3])
    mu2_mcmc <- c(mu2_mcmc, output[seq,4])
    q12_mcmc <- c(q12_mcmc, output[seq,5])
    q21_mcmc <- c(q21_mcmc, output[seq,6])
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

save(whole_df_MCMC,file = paste0("Data/BiSSE/exponential_prior/whole_df_MCMC_test.RData"))

whole_df_MCMC$net_div1 <- (whole_df_MCMC$lam1-whole_df_MCMC$mu1)
whole_df_MCMC$net_div2 <- (whole_df_MCMC$lam2-whole_df_MCMC$mu2)
whole_df_MCMC$net_div_MCMC1 <- (whole_df_MCMC$lam1_mcmc-whole_df_MCMC$mu1_mcmc)
whole_df_MCMC$net_div_MCMC2 <- (whole_df_MCMC$lam2_mcmc-whole_df_MCMC$mu2_mcmc)

whole_df_MCMC$ext_frac1 <- (whole_df_MCMC$mu1)/(whole_df_MCMC$lam1)
whole_df_MCMC$ext_frac2 <- (whole_df_MCMC$mu2)/(whole_df_MCMC$lam2)
whole_df_MCMC$ext_frac_MCMC1 <- (whole_df_MCMC$mu1_mcmc)/(whole_df_MCMC$lam1_mcmc)
whole_df_MCMC$ext_frac_MCMC2 <- (whole_df_MCMC$mu2_mcmc)/(whole_df_MCMC$lam2_mcmc)
whole_df_MCMC$init_obs <- rep(c(rep(0,25*5001),rep(1,25*5001)),3)

save(whole_df_MCMC,file = paste0("Data/BiSSE/exponential_prior/delta_whole_df_MCMC_test.RData"))



for(i in 1:3){
  load(paste0("Data/BiSSE/whole_df_MLE.RData"))
  whole_df_MLE <- whole_df_MLE[(i*50-49):(i*50),][,1:23]
  total <- whole_df_MLE$tree_size

  ss = "ABC"
  load(paste0("Data/BiSSE/nltts_D/delta_whole_df_ABC_test_ss0.RData"))
  whole_df_ABC1 <- whole_df_ABC[(i*25000-24999):(i*25000),]
  whole_df_ABC1$ss = "ABC Uni"
  whole_df_ABC1 = whole_df_ABC1[,-7]
  whole_df_ABC1$total <- rep(total, each = 500)
  whole_df_ABC1$dlam1 <- whole_df_ABC1$lam1_abc - whole_df_ABC1$lam1
  whole_df_ABC1$dlam2 <- whole_df_ABC1$lam2_abc - whole_df_ABC1$lam2
  whole_df_ABC1$dmu1 <- whole_df_ABC1$mu1_abc - whole_df_ABC1$mu1
  whole_df_ABC1$dmu2 <- whole_df_ABC1$mu2_abc - whole_df_ABC1$mu2
  whole_df_ABC1$dq12 <- whole_df_ABC1$q12_abc - whole_df_ABC1$q12
  whole_df_ABC1$dq21 <- whole_df_ABC1$q21_abc - whole_df_ABC1$q21
  whole_df_ABC1$dnet_div1 <- whole_df_ABC1$net_div_ABC1 - whole_df_ABC1$net_div1
  whole_df_ABC1$dnet_div2 <- whole_df_ABC1$net_div_ABC2 - whole_df_ABC1$net_div2
  whole_df_ABC1$dext_frac1 <- whole_df_ABC1$ext_frac_ABC1 - whole_df_ABC1$ext_frac1
  whole_df_ABC1$dext_frac2 <- whole_df_ABC1$ext_frac_ABC2 - whole_df_ABC1$ext_frac2
  whole_df_ABC1$rep <- rep(rep(1:50, each = 500), 1)
  df <- whole_df_ABC1
  n <- 500
  ABC_median1 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median1$ss = "ABC Uni"

  ss = "ABC"
  load(paste0("Data/BiSSE/exponential_prior/delta_whole_df_ABC_test_ss0.RData"))
  whole_df_ABC2 <- whole_df_ABC[(i*25000-24999):(i*25000),]
  whole_df_ABC2$ss = "ABC Exp"
  whole_df_ABC2 = whole_df_ABC2[,-7]
  whole_df_ABC2$total <- rep(total, each = 500)
  whole_df_ABC2$dlam1 <- whole_df_ABC2$lam1_abc - whole_df_ABC2$lam1
  whole_df_ABC2$dlam2 <- whole_df_ABC2$lam2_abc - whole_df_ABC2$lam2
  whole_df_ABC2$dmu1 <- whole_df_ABC2$mu1_abc - whole_df_ABC2$mu1
  whole_df_ABC2$dmu2 <- whole_df_ABC2$mu2_abc - whole_df_ABC2$mu2
  whole_df_ABC2$dq12 <- whole_df_ABC2$q12_abc - whole_df_ABC2$q12
  whole_df_ABC2$dq21 <- whole_df_ABC2$q21_abc - whole_df_ABC2$q21
  whole_df_ABC2$dnet_div1 <- whole_df_ABC2$net_div_ABC1 - whole_df_ABC2$net_div1
  whole_df_ABC2$dnet_div2 <- whole_df_ABC2$net_div_ABC2 - whole_df_ABC2$net_div2
  whole_df_ABC2$dext_frac1 <- whole_df_ABC2$ext_frac_ABC1 - whole_df_ABC2$ext_frac1
  whole_df_ABC2$dext_frac2 <- whole_df_ABC2$ext_frac_ABC2 - whole_df_ABC2$ext_frac2
  whole_df_ABC2$rep <- rep(rep(1:50, each = 500), 1)
  df <- whole_df_ABC2
  n <- 500
  ABC_median2 <-aggregate(df,list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
  ABC_median2$ss = "ABC Exp"

  load(paste0("Data/BiSSE/nltts_D/delta_whole_df_MCMC_test.RData"))
  whole_df_MCMC1 <- whole_df_MCMC[(i*250050-250049):(i*250050),]
  whole_df_MCMC1$ss = "MCMC Uni"
  whole_df_MCMC1$total <- rep(total, each = 5001) #2001
  whole_df_MCMC1$dlam1 <- whole_df_MCMC1$lam1_mcmc - whole_df_MCMC1$lam1
  whole_df_MCMC1$dlam2 <- whole_df_MCMC1$lam2_mcmc - whole_df_MCMC1$lam2
  whole_df_MCMC1$dmu1 <- whole_df_MCMC1$mu1_mcmc - whole_df_MCMC1$mu1
  whole_df_MCMC1$dmu2 <- whole_df_MCMC1$mu2_mcmc - whole_df_MCMC1$mu2
  whole_df_MCMC1$dq12 <- whole_df_MCMC1$q12_mcmc - whole_df_MCMC1$q12
  whole_df_MCMC1$dq21 <- whole_df_MCMC1$q21_mcmc - whole_df_MCMC1$q21
  whole_df_MCMC1$dnet_div1 <- whole_df_MCMC1$net_div_MCMC1 - whole_df_MCMC1$net_div1
  whole_df_MCMC1$dnet_div2 <- whole_df_MCMC1$net_div_MCMC2 - whole_df_MCMC1$net_div2
  whole_df_MCMC1$dext_frac1 <- whole_df_MCMC1$ext_frac_MCMC1 - whole_df_MCMC1$ext_frac1
  whole_df_MCMC1$dext_frac2 <- whole_df_MCMC1$ext_frac_MCMC2 - whole_df_MCMC1$ext_frac2
  whole_df_MCMC1$rep <- rep(rep(1:50, each = 5001), 1)

  df<-whole_df_MCMC1
  n <- 5001
  MCMC_median1 <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]


  load(paste0("Data/BiSSE/exponential_prior/delta_whole_df_MCMC_test.RData"))
  whole_df_MCMC2 <- whole_df_MCMC[(i*250050-250049):(i*250050),]
  whole_df_MCMC2$ss = "MCMC Exp"
  whole_df_MCMC2$total <- rep(total, each = 5001) #2001
  whole_df_MCMC2$dlam1 <- whole_df_MCMC2$lam1_mcmc - whole_df_MCMC2$lam1
  whole_df_MCMC2$dlam2 <- whole_df_MCMC2$lam2_mcmc - whole_df_MCMC2$lam2
  whole_df_MCMC2$dmu1 <- whole_df_MCMC2$mu1_mcmc - whole_df_MCMC2$mu1
  whole_df_MCMC2$dmu2 <- whole_df_MCMC2$mu2_mcmc - whole_df_MCMC2$mu2
  whole_df_MCMC2$dq12 <- whole_df_MCMC2$q12_mcmc - whole_df_MCMC2$q12
  whole_df_MCMC2$dq21 <- whole_df_MCMC2$q21_mcmc - whole_df_MCMC2$q21
  whole_df_MCMC2$dnet_div1 <- whole_df_MCMC2$net_div_MCMC1 - whole_df_MCMC2$net_div1
  whole_df_MCMC2$dnet_div2 <- whole_df_MCMC2$net_div_MCMC2 - whole_df_MCMC2$net_div2
  whole_df_MCMC2$dext_frac1 <- whole_df_MCMC2$ext_frac_MCMC1 - whole_df_MCMC2$ext_frac1
  whole_df_MCMC2$dext_frac2 <- whole_df_MCMC2$ext_frac_MCMC2 - whole_df_MCMC2$ext_frac2
  whole_df_MCMC2$rep <- rep(rep(1:50, each = 5001), 1)

  df<-whole_df_MCMC2
  n <- 5001
  MCMC_median2 <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

  # MLE
  whole_df_MLE$net_div1 <- (whole_df_MLE$lam1-whole_df_MLE$mu1)
  whole_df_MLE$net_div2 <- (whole_df_MLE$lam2-whole_df_MLE$mu2)
  whole_df_MLE$net_div_MLE1 <- (whole_df_MLE$lam1_MLE-whole_df_MLE$mu1_MLE)
  whole_df_MLE$net_div_MLE2 <- (whole_df_MLE$lam2_MLE-whole_df_MLE$mu2_MLE)
  whole_df_MLE$ext_frac1 <- (whole_df_MLE$mu1)/(whole_df_MLE$lam1)
  whole_df_MLE$ext_frac2 <- (whole_df_MLE$mu2)/(whole_df_MLE$lam2)
  whole_df_MLE$ext_frac_MLE1 <- (whole_df_MLE$mu1_MLE)/(whole_df_MLE$lam1_MLE)
  whole_df_MLE$ext_frac_MLE2 <- (whole_df_MLE$mu2_MLE)/(whole_df_MLE$lam2_MLE)

  whole_df_MLE$init_obs <- rep(c(rep(0,25),rep(1,25)),1)
  whole_df_MLE$ss = "MLE"
  whole_df_MLE$total <- rep(total, each = 1)
  whole_df_MLE$dlam1 <- whole_df_MLE$lam1_MLE - whole_df_MLE$lam1
  whole_df_MLE$dlam2 <- whole_df_MLE$lam2_MLE - whole_df_MLE$lam2
  whole_df_MLE$dmu1 <- whole_df_MLE$mu1_MLE - whole_df_MLE$mu1
  whole_df_MLE$dmu2 <- whole_df_MLE$mu2_MLE - whole_df_MLE$mu2
  whole_df_MLE$dq12 <- whole_df_MLE$q12_MLE - whole_df_MLE$q12
  whole_df_MLE$dq21 <- whole_df_MLE$q21_MLE - whole_df_MLE$q21
  whole_df_MLE$dnet_div1 <- whole_df_MLE$net_div_MLE1 - whole_df_MLE$net_div1
  whole_df_MLE$dnet_div2 <- whole_df_MLE$net_div_MLE2 - whole_df_MLE$net_div2
  whole_df_MLE$dext_frac1 <- whole_df_MLE$ext_frac_MLE1 - whole_df_MLE$ext_frac1
  whole_df_MLE$dext_frac2 <- whole_df_MLE$ext_frac_MLE2 - whole_df_MLE$ext_frac2
  whole_df_MLE$rep <- rep(rep(1:50, each = 1), 1)


  whole_df_all <- rbind(whole_df_ABC1[,c(1:6,13,14,17,18,21:33)],
                        whole_df_ABC2[,c(1:6,13,14,17,18,22:34)],
                        whole_df_MCMC1[,c(1:6,13,14,17,18,21:33)],
                        whole_df_MCMC2[,c(1:6,13,14,17,18,22:34)],
                        whole_df_MLE[,c(1:6,24,25,28,29,33:45)])
  save(whole_df_all, file = paste0("Data/BiSSE/exponential_prior/AMM_all_prior_test",i,".RData"))

  median_all <- rbind(ABC_median1[,c(1:6,13,14,17,18,21:33)],
                      ABC_median2[,c(1:6,13,14,17,18,22:34)],
                      MCMC_median1[,c(1:6,13,14,17,18,21:33)],
                      MCMC_median2[,c(1:6,13,14,17,18,22:34)],
                      whole_df_MLE[,c(1:6,24,25,28,29,33:45)])

  save(median_all, file = paste0("Data/BiSSE/exponential_prior/AMM_median_prior_test",i,".RData"))
}

library(tidyverse)
# install.packages("ggtext")
library(ggtext)
library(ggbeeswarm)
library(ggplot2)

#####
## lam
i = 1
load(paste0("Data/BiSSE/exponential_prior/AMM_median_prior_test",i,".RData"))
# whole_df_all1<-whole_df_all
whole_df_all1<-median_all
whole_df_all1$Scenario <- 1

i = 2
load(paste0("Data/BiSSE/exponential_prior/AMM_median_prior_test",i,".RData"))
whole_df_all2<-median_all
whole_df_all2$Scenario <- 2

i = 3
load(paste0("Data/BiSSE/exponential_prior/AMM_median_prior_test",i,".RData"))
whole_df_all3<-median_all
whole_df_all3$Scenario <- 3


scen_names1 <- c(
  `1` = 'S1~":"~lambda[0]~"="~0.6~lambda[1]~"="~0.6', #Scenario~1~
  `2` = 'S2~":"~lambda[0]~"="~0.6~lambda[1]~"="~0.3',
  `3` = 'S3~":"~lambda[0]~"="~0.6~lambda[1]~"="~0.12'
)


scen_names2 <- c(
  `1` = 'S1~":"~mu[0]~"="~0.05~mu[1]~"="~0.05',
  `4` = 'S4~":"~mu[0]~"="~0.05~mu[1]~"="~0.1',
  `5` = 'S5~":"~mu[0]~"="~0.05~mu[1]~"="~0.25'
)


scen_names3 <- c(
  `1` = 'S1~":"~q["01"]~"="~0.05~q[10]~"="~0.05',
  `6` = 'S6~":"~q["01"]~"="~0.05~q[10]~"="~0.1',
  `7` = 'S7~":"~q["01"]~"="~0.05~q[10]~"="~0.25'
)

whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.025, upper = 0.975) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

library(RColorBrewer)
library(stringr)
# pal <- brewer.pal(n = 12, name = "Paired")
# cols <- c("nLTT" = pal[1] ,"D"= pal[2],"nLTT-D"= pal[9],
#           "nLTTs"= pal[3],"nLTTs-D"= pal[4],"nLTT-MPD"= pal[5],
#           "nLTT-MNTD"= pal[6],"nLTT-colless"= pal[8],"nLTT-ratio"= pal[12])
#
# whole_df_lam$ss <- factor(whole_df_lam$ss, levels = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))

p_lam1 <-ggplot2::ggplot(data = whole_df_lam, ggplot2::aes(x = ss,y = dlam1,
                                                           color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,2.2)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~lambda[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_lam2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,2.2)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~lambda[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_mu1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,2.2)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~mu[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,2.2)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~mu[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q12 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq12,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,1.4)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~q["01"])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_q21 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq21,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,1.4)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 # axis.text.x = ggplot2::element_text(size = 15),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~q[10])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div1,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.2,1.5)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div~0)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.2,1.5)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(size = 13),
                 # axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::scale_x_discrete(labels = c("ABC\nExp",
                                       "ABC\nUni",
                                       "MCMC\nExp",
                                       "MCMC\nUni",
                                       "MLE"))+
  ggplot2::ylab(expression(Delta~Net~Div~1)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("Data/BiSSE/exponential_prior/violin_compare_AMM.tiff"),
     units="px", width=6000, height=6000,res = 450,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_q12+ggplot2::theme(legend.position = "none"),
  p_q21+ggplot2::theme(legend.position = "none"),
  # p_net1+ggplot2::theme(legend.position = "none"),
  # p_net2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 6, ncol = 1
)+ ggtitle("Asymmetry in speciation")+
  ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))

legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 4))
)
param_final_lam <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.4))
print(param_final_lam)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()




### whole df
#####
## lam
i = 1
load(paste0("Data/BiSSE/exponential_prior/AMM_all_prior_test",i,".RData"))
# whole_df_all1<-whole_df_all
whole_df_all1<-whole_df_all
whole_df_all1$Scenario <- 1

i = 2
load(paste0("Data/BiSSE/exponential_prior/AMM_all_prior_test",i,".RData"))
whole_df_all2<-whole_df_all
whole_df_all2$Scenario <- 2

i = 3
load(paste0("Data/BiSSE/exponential_prior/AMM_all_prior_test",i,".RData"))
whole_df_all3<-whole_df_all
whole_df_all3$Scenario <- 3


scen_names1 <- c(
  `1` = 'S1~":"~lambda[0]~"="~0.6~lambda[1]~"="~0.6', #Scenario~1~
  `2` = 'S2~":"~lambda[0]~"="~0.6~lambda[1]~"="~0.3',
  `3` = 'S3~":"~lambda[0]~"="~0.6~lambda[1]~"="~0.12'
)


scen_names2 <- c(
  `1` = 'S1~":"~mu[0]~"="~0.05~mu[1]~"="~0.05',
  `4` = 'S4~":"~mu[0]~"="~0.05~mu[1]~"="~0.1',
  `5` = 'S5~":"~mu[0]~"="~0.05~mu[1]~"="~0.25'
)


scen_names3 <- c(
  `1` = 'S1~":"~q["01"]~"="~0.05~q[10]~"="~0.05',
  `6` = 'S6~":"~q["01"]~"="~0.05~q[10]~"="~0.1',
  `7` = 'S7~":"~q["01"]~"="~0.05~q[10]~"="~0.25'
)

whole_df_lam <- rbind(whole_df_all1,whole_df_all2,whole_df_all3)

iqr = function(z, lower = 0.025, upper = 0.975) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

library(RColorBrewer)
library(stringr)
# pal <- brewer.pal(n = 12, name = "Paired")
# cols <- c("nLTT" = pal[1] ,"D"= pal[2],"nLTT-D"= pal[9],
#           "nLTTs"= pal[3],"nLTTs-D"= pal[4],"nLTT-MPD"= pal[5],
#           "nLTT-MNTD"= pal[6],"nLTT-colless"= pal[8],"nLTT-ratio"= pal[12])
#
# whole_df_lam$ss <- factor(whole_df_lam$ss, levels = c("nLTT","D","nLTT-D","nLTTs","nLTTs-D","nLTT-MPD","nLTT-MNTD","nLTT-colless","nLTT-ratio"))

p_lam1 <-ggplot2::ggplot(data = whole_df_lam, ggplot2::aes(x = ss,y = dlam1,
                                                           color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,2.2)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 legend.title = ggplot2::element_text(size = 17,colour = "black"),
                 strip.text = element_text(size = 17,colour = "black"),
                 strip.background=element_rect(colour="gray",fill="lightgray"),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~lambda[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_lam2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dlam2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.5,2.2)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~lambda[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_mu1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu1,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,2.2)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~mu[0])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_mu2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dmu2,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,2.2)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~mu[1])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_q12 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq12,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,1.1)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~q["01"])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))


p_q21 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dq21,
                                                         color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-0.3,1.1)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 # axis.text.x = ggplot2::element_text(size = 15),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~q[10])) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net1 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div1,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.2,1.5)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::ylab(expression(Delta~Net~Div~0)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

p_net2 <-ggplot2::ggplot(data = whole_df_lam,ggplot2::aes(x = ss,y = dnet_div2,
                                                          color = ss,fill =ss)) +
  ggplot2::theme_bw() +
  ylim(-1.2,1.5)+ #1
  ggplot2::geom_violin(alpha = 0.5) +  #outlier.shape = NA
  ggplot2::stat_summary(fun.data = iqr,alpha = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 18),
                 legend.text = element_markdown(size = 15),
                 strip.text = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(size = 13),
                 # axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 13)) +
  ggplot2::scale_x_discrete(labels = c("ABC\nExp",
                                       "ABC\nUni",
                                       "MCMC\nExp",
                                       "MCMC\nUni",
                                       "MLE"))+
  ggplot2::ylab(expression(Delta~Net~Div~1)) +
  ggplot2::xlab("Method")+
  ggplot2::scale_colour_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  ggplot2::scale_fill_manual("Method",values = c("#E90F44","#f6a482","#10559a","#8ec4de","#FFC839"))+
  # ggplot2::theme(legend.position = "none") +
  ggplot2::geom_hline(data= whole_df_lam, aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(~Scenario,
             labeller = labeller(Scenario  = as_labeller(scen_names1,  label_parsed)))

tiff(paste0("Data/BiSSE/exponential_prior/violin_compare_AMM_whole.tiff"),
     units="px", width=6000, height=6000,res = 450,compression="lzw")

param_estimates_lam <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_q12+ggplot2::theme(legend.position = "none"),
  p_q21+ggplot2::theme(legend.position = "none"),
  # p_net1+ggplot2::theme(legend.position = "none"),
  # p_net2+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 6, ncol = 1
)+ ggtitle("Asymmetry in speciation")+
  ggplot2::theme(plot.title = element_text(color="black", size=22,margin = margin(0,0,6,0)))

legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 4))
)
param_final_lam <- cowplot::plot_grid(param_estimates_lam,legend,rel_widths = c(3, 0.4))
print(param_final_lam)
# param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
# print(cowplot::ggdraw(param_est_final))
while (!is.null(dev.list()))  dev.off()









