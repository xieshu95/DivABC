param_space <- readr::read_csv2("data/secsse_ABC.csv")

ss <- c()
for(i in 1:27){
  set.seed(i)
  message("set: ", i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_secsse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                       K = Inf,
                                       replicates = 1) ## replicates = 30
  init_epsilon <- calc_epsilon_init_secsse(sim = obs_sim)
  ss<-rbind(ss,init_epsilon)
}


colnames(ss) <- c("mpd","mpd_diff","mntd","mntd_diff",
                  "sdpd","sdpd_diff","sdntd","sdntd_diff",
                  "K","D","state1","state2","nltt")
rownames(ss) <- 1:27
save(ss,file = "G:/results/project 2/tip_info/round4/adap_secsse_new_space/obs_ss.RData")

pars_ss<-data.frame(param_space,ss)
pars_ss$total <- pars_ss$state1+pars_ss$state2
save(pars_ss,file = "G:/results/project 2/tip_info/round4/adap_secsse_new_space/obs_ss_with_pars.RData")
#####
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/obs_ss_with_pars.RData")

load("G:/results/project 2/tip_info/round4/adap_secsse/AMM_reorder.RData")

dlam1_MLE <- AMM_reorder$lam1_MLE - AMM_reorder$lam1
dlam2_MLE <- AMM_reorder$lam2_MLE - AMM_reorder$lam2
dmu1_MLE <- AMM_reorder$mu1_MLE - AMM_reorder$mu1
dmu2_MLE <- AMM_reorder$mu2_MLE - AMM_reorder$mu2
dq12_MLE <- AMM_reorder$q12_MLE - AMM_reorder$q12
dq21_MLE <- AMM_reorder$q21_MLE - AMM_reorder$q21


MLE_check <- data.frame(dlam1_MLE,dlam2_MLE,dmu1_MLE,dmu2_MLE,
                      dq12_MLE,dq21_MLE,pars_ss[,c(17,18,20)])
save(MLE_check,file = "G:/results/project 2/tip_info/round4/adap_secsse/MLE_check.RData")
plot(MLE_cor$total,MLE_cor$dlam1_MLE)
plot(MLE_cor$total,MLE_cor$dlam2_MLE)
plot(MLE_cor$total,MLE_cor$dmu2_MLE)
plot(MLE_cor$total,MLE_cor$dmu1_MLE)
plot(MLE_cor$total,MLE_cor$dq12_MLE)
plot(MLE_cor$total,MLE_cor$dq21_MLE)


