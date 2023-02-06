## test ss for secsse_ABC_test space
for(test in c(1,2,3,4,5)) {
  param_space <- readr::read_csv2(paste0("data/secsse_ABC_test",test,".csv"))
  ss <- c()
  for(i in 1:100){
    set.seed(i)
    message("set: ", i)
    obs_sim_pars <- param_space[i,]
    obs_sim <- get_secsse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                         K = Inf,
                                         replicates = 1) ## replicates = 30
    init_epsilon <- calc_epsilon_init_secsse(sim = obs_sim)
    ss<-rbind(ss,init_epsilon)
  }

  # colnames(ss) <- c("mpd","mpd_diff","mntd","mntd_diff",
  #                   "sdpd","sdpd_diff","sdntd","sdntd_diff",
  #                   "K","D","state1","state2","nltt")

  colnames(ss) <- c("state1","state2","tree_size","tip_ratio")
  rownames(ss) <- 1:100


  pars_ss<-data.frame(param_space,ss)
  save(pars_ss,file = paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/obs_ss_test",test,".RData"))

}

#####
load("G:/results/project 2/tip_info/round4/adap_secsse_test3/obs_ss_test5.RData")

# plot(hist(pars_ss[,9]))
# plot(density(pars_ss[,9]))

tree_size <- pars_ss$tree_size
# tree_size[tree_size >= 200]
length(tree_size[tree_size >= 200])
length(tree_size[tree_size >= 300])
length(tree_size[tree_size <= 50])





## heatmap for random combinations
# don't use given values but sample from prior

# set.seed(1)
# lam1 <- stats::rexp(200,2)
# lam2 <- stats::rexp(200,2)
# mu1 <- stats::rexp(200,10)
# mu2 <- stats::rexp(200,10)
# q12 <- stats::rexp(200,5)
# q21 <- stats::rexp(200,5)
# #
# test_ss_space <- data.frame(lam1,lam2,mu1,mu2,q12,q21)
# save(test_ss_space,file = "G:/results/project 2/tip_info/round4/adap_secsse_new_space/test_ss_space.RData")
# load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/test_ss_space.RData")
ss <- matrix(NA,nrow = 300,ncol = 17)
pars_accept <- c()
t1 <- Sys.time()
set <- 1
# set.seed(1)
while(set < 301){
  message("set",set)
  pars <- prior_gen_secsse(1:6,1:6)
  obs_sim <- get_secsse_sim(parameters = as.numeric(pars),
                            K = Inf,
                            replicates = 1)
  if (length(obs_sim[[1]]$examTraits) > 20 &&
      length(obs_sim[[1]]$examTraits) < 400 &&
      length(unique(obs_sim[[1]]$examTraits)) > 1) {
    ss[set,] <- calc_epsilon_init_secsse(sim = obs_sim)
    pars_accept <- rbind(pars_accept, pars)
    set <- set + 1
  }
}
t2 <- Sys.time()
dt <- t2-t1
dt

colnames(ss) <- c("total","ratio","K","D","nltt",
                  "mpd","mpd_diff","mpd1","mpd2",
                  "mntd","mntd_diff","mntd1","mntd2",
                  "sdpd","sdpd_diff","sdntd","sdntd_diff")
                  # "state1","state2",

save(ss,file = "G:/results/project 2/tip_info/round4/adap_secsse_test3/test_ss_df_prior.RData")

colnames(pars_accept) <- c("lam1","lam2","mu1","mu2","q12","q21")
pars_ss <-data.frame(pars_accept,ss)
save(pars_ss,file = "G:/results/project 2/tip_info/round4/adap_secsse_test3/test_ss_pars_ss.RData")

load("G:/results/project 2/tip_info/round4/adap_secsse_test3/test_ss_pars_ss.RData")

load("G:/results/project 2/tip_info/round4/adap_secsse_test3/test_ss_df_prior.RData")
# load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/test_ss_df_prior.RData")
# ss[is.nan(ss)] <- 0
rcorrDat<- Hmisc::rcorr(as.matrix(ss))
cormat <- round(rcorrDat$r,2)
# heatmap(cormat)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)

ss_name <- c("Total","Ratio","K","D","NLTT",
             "MPD","MPD-diff","MPD1","MPD2",
             "MNTD","MNTD-diff", "MNTD1","MNTD2",
             "SDPD","SDPD_12","SDNTD","SDNTD_12")
             # "State1","State2",


label_names <- "Summary statistic"
tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/heatmap_ss_without_fix_na.tiff"),
     units="px", width=5000, height=4000,res = 300,compression="lzw")
heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), size = 5) +

  ggplot2::scale_x_discrete(labels= ss_name)+
  ggplot2::scale_y_discrete(labels= ss_name)+
  ggplot2::xlab(label_names) +  #Rate differential ratio of anagenesis/Diversity dependence
  ggplot2::ylab(label_names) +
  ggplot2::guides(fill = guide_legend(title="Correlation"))+
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10)) +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.margin = ggplot2::margin(6, 0.2, 6, 0.2)) +
  ggplot2::ggtitle("Correlations between summary statistics") +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5))
print(heatmap)
while (!is.null(dev.list()))  dev.off()


## use metan package to plot heatmap
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/test_ss_df_prior.RData")
# install.packages("metan")
ss <- data.frame(ss)
library(metan)
# All numeric variables
x <- corr_coef(ss)
plot(x)






# test conditioning
## secsse parameter space(7 combinations),run 200 reps for each set
# and see how many reps can be obs(10~700 species and 2 states)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC_long.csv")
# function to get how many replicates satisfied the condition(10~700 species, 2 states)

calc_num <- function(obs_sim){
  state1 <- c()
  state2 <- c()
  total <- c()
  n <- 0
  for (rep in 1:length(obs_sim)){
    state1[rep]<-length(which(obs_sim[[rep]]$examTraits == 1))
    state2[rep]<-length(which(obs_sim[[rep]]$examTraits == 2))
    total[rep] <- length(obs_sim[[rep]]$examTraits)
    if (length(obs_sim[[rep]]$examTraits) > 10 && ## at least 50 species
        length(obs_sim[[rep]]$examTraits) < 700 &&
        length(unique(obs_sim[[rep]]$examTraits)) == 2){
      n <- n + 1
    }
  }
  num <- data.frame(state1,state2,total)
  return(list(num = num,
              n = n))
}


param_space <- param_data[c(1,11,21,31,41,51,61),]
t1 <- Sys.time()
num_cond_all <- c()
for(i in 1:7){
  set.seed(i)
  message("set: ", i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_secsse_sim(parameters = as.numeric(obs_sim_pars),
                            K = Inf,
                            replicates = 500) ## replicates = 30
  num_spec <- calc_num(obs_sim)

  data_list <- list(num_spec = num_spec,
                    num_cond = num_cond,
                    obs_sim = obs_sim)
  save(data_list,file = paste0("G:/results/project 2/tip_info/round4/secsse_long_2/condition/num_cond_group",i,".RData"))
  num_cond_all <- c(num_cond_all,num_cond)
}
save(num_cond_all,file = paste0("G:/results/project 2/tip_info/round4/secsse_long_2/condition/num_cond_all.RData"))
t2 <- Sys.time()
dt <- t2-t1


### calculate ss for secsse space
param_space <- readr::read_csv2("data/secsse_ABC_long.csv")

ss <- c()
for(i in 1:70){
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
save(ss,file = "G:/results/project 2/tip_info/round4/secsse_long_2/obs_ss_long_space.RData")


rownames(ss) <- 1:70
pars_ss<-data.frame(param_space,ss)
pars_ss$total <- pars_ss$state1+pars_ss$state2
save(pars_ss,file = "G:/results/project 2/tip_info/round4/secsse_long_2/obs_ss_long_with_pars.RData")

load("G:/results/project 2/tip_info/round4/secsse_long_2/obs_ss_long_with_pars.RData")

