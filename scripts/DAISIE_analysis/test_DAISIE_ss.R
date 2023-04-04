## heatmap for random combinations
# don't use given values but sample from prior
pars_accept <- c()
t1 <- Sys.time()
set <- 1
set.seed(1)
while(set < 501){
  message("set",set)
  obs_sim_pars <- prior_gen_secsse(1:6,1:6)
  obs_sim <- get_secsse_sim(parameters = as.numeric(pars),
                            K = Inf,
                            replicates = 1)
  if (length(obs_sim[[1]]$examTraits) > 10 &&
      length(obs_sim[[1]]$examTraits) < 600 &&
      length(unique(obs_sim[[1]]$examTraits)) > 1) {
    ss[set,] <- calc_epsilon_init_secsse(sim = obs_sim)
    pars_accept <- rbind(pars_accept, pars)
    set <- set + 1
  }
}
t2 <- Sys.time()
dt <- t2-t1
dt

colnames(ss) <- c("mpd","mpd_diff","mntd","mntd_diff",
                  "sdpd","sdpd_diff","sdntd","sdntd_diff",
                  "K","D","total","ratio","nltt")
save(ss,file = "G:/results/project 2/tip_info/round4/adap_secsse_test3/test_ss_df_prior_save.RData")

colnames(pars_accept) <- c("lam1","lam2","mu1","mu2","q12","q21")
save(pars_accept,file = "G:/results/project 2/tip_info/round4/adap_secsse_test3/test_ss_pars_accept_save.RData")

load("G:/results/project 2/tip_info/round4/adap_secsse_test3/test_ss_df_prior.RData")

cormat <- round(cor(ss[1:200,]),2)
# heatmap(cormat)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)

ss_name <- c("MPD","MPD_12","MNTD","MNTD_12",
             "SDPD","SDPD_12","SDNTD","SDNTD_12",
             "K","D","Total","Ratio","NLTT")

label_names <- "Summary statistic"
tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3/heatmap_ss_with_value_prior.tiff"),
     units="px", width=3500, height=2500,res = 300,compression="lzw")
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




### calculate ss for DAISIE space
param_space <- readr::read_csv2("data/DAISIE_ABC_short.csv")
save(param_space,file = "G:/results/project 2/tip_info/round4/DAISIE2/param_space.RData")

ss <- c()
for(i in 1:81){
  set.seed(i)
  message("set: ", i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = as.numeric(obs_sim_pars$K),
                            replicates = 1)
  init_epsilon <- calc_epsilon_init(sim = obs_sim)
  ss <- rbind(ss,init_epsilon)
}


colnames(ss) <- c("total-nltt","clade-nltt","ana","clado",
                  "nonend","num-clade","scsd","ctsd","total")
rownames(ss) <- 1:81
save(ss,file = "G:/results/project 2/tip_info/round4/DAISIE2/obs_ss.RData")

load("G:/results/project 2/tip_info/round4/DAISIE2/obs_ss.RData")

pars_ss<-data.frame(param_space,ss)
save(pars_ss,file = "G:/results/project 2/tip_info/round4/DAISIE2/obs_ss_long_with_pars.RData")

load("G:/results/project 2/tip_info/round4/DAISIE2/obs_ss_long_with_pars.RData")

### pairwise ss for DAISIE space
param_space <- readr::read_csv2("data/DAISIE_ABC_short.csv")
save(param_space,file = "G:/results/project 2/tip_info/round4/DAISIE2/param_space.RData")

ss <- c()
for(i in 1:81){
  set.seed(i)
  message("set: ", i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = as.numeric(obs_sim_pars$K),
                            replicates = 1)
  init_epsilon <- calc_epsilon_init(sim = obs_sim)
  ss <- rbind(ss,init_epsilon)
}


# total_nltt = total_nltt,
# clade_nltt = clade_nltt,
# num_ana = num_ana,
# num_clado = num_clado,
# num_nonend = num_nonend,
# num_clade = num_clade,
# clade_size = clade_size_sd,
# colon_time = colon_time_sd

colnames(ss) <- c("total-nltt","clade-nltt","ana","clado",
                  "nonend","num-clade","scsd","ctsd","total")
rownames(ss) <- 1:81
save(ss,file = "G:/results/project 2/tip_info/round4/DAISIE2/obs_ss.RData")

load("G:/results/project 2/tip_info/round4/DAISIE2/obs_ss.RData")

pars_ss<-data.frame(param_space,ss)
save(pars_ss,file = "G:/results/project 2/tip_info/round4/DAISIE2/obs_ss_long_with_pars.RData")

load("G:/results/project 2/tip_info/round4/DAISIE2/obs_ss_long_with_pars.RData")



## new heatmap code
library(heatmaply)
library(htmlwidgets)

colnames(ss) <- c("LTT","CTT","Singleton-end","Mul-end",
                  "Non-end","Num-clade","SDCS","SDCT","Num-spec")
p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss), xlab = "Summary statistics",
                                      ylab = "Summary statistics", k_col = 2, k_row = 2)
saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/heatmap_ss.html"))

