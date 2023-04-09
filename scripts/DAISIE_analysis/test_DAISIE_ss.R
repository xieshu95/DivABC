## heatmap for random combinations
# don't use given values but sample from prior
pars_accept <- c()
t1 <- Sys.time()
ss<-c()
set <- 1
set.seed(1)
while(set < 501){
  message("set",set)
  obs_sim_pars <- prior_gen(1:4,1:4)
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars[1],
                                           obs_sim_pars[2],
                                           obs_sim_pars[3],
                                           obs_sim_pars[4]),
                            K = Inf, # 20/Inf
                            replicates = 1)
  init_epsilon <- calc_epsilon_init(sim = obs_sim)
  ss <- rbind(ss,init_epsilon)
  pars_accept <- rbind(pars_accept,obs_sim_pars)
  set <- set + 1
}
t2 <- Sys.time()
dt <- t2-t1
dt

colnames(pars_accept) <- c("lac","mu","gam","laa")
save(pars_accept,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DI/ramdom_ss_pars_accept.RData")

colnames(ss) <- c("total-nltt","clade-nltt","ana","clado",
                  "nonend","num-clade","scsd","ctsd","total",
                  "nonend-nltt","singleton-nltt")
rownames(ss) <- 1:500
save(ss,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DI/random_obs_ss.RData")

load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DI/random_obs_ss.RData")
library(heatmaply)
library(htmlwidgets)

colnames(ss) <- c("LTT","CTT","Singleton-endemic","Multi-endemic",
                  "Non-endemic","Num clade","SDCS","SDCT","Num total",
                  "Nonend LTT","Singleton LTT")
p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss), xlab = "Summary statistics",
                                      ylab = "Summary statistics", k_col = 2, k_row = 2)
saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DI/heatmap_ss_random.html"))


## old code
load("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/random_obs_ss.RData")

cormat <- round(cor(ss[1:500,]),2)
# heatmap(cormat)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)

ss_name <- c("LTT","CTT","Singleton-endemic","Multi-endemic",
             "Non-endemic","Num clade","SDCS","SDCT","Num total",
             "Nonend LTT","Singleton LTT")

label_names <- "Summary statistic"
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/heatmap_ss_old.tiff"),
     units="px", width=4500, height=3500,res = 300,compression="lzw")
heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue3", high = "red3",
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


# ss <- c()
# for(i in 1:81){
#   set.seed(i)
#   message("set: ", i)
#   obs_sim_pars <- param_space[i,]
#   obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
#                                            obs_sim_pars$mu,
#                                            obs_sim_pars$gam,
#                                            obs_sim_pars$laa),
#                             K = as.numeric(obs_sim_pars$K),
#                             replicates = 1)
#   init_epsilon <- calc_epsilon_init(sim = obs_sim)
#   ss <- rbind(ss,init_epsilon)
# }
#
#
# colnames(ss) <- c("total-nltt","clade-nltt","ana","clado",
#                   "nonend","num-clade","scsd","ctsd","total")
# rownames(ss) <- 1:81
# save(ss,file = "G:/results/project 2/tip_info/round4/DAISIE2/obs_ss.RData")
#
# load("G:/results/project 2/tip_info/round4/DAISIE2/obs_ss.RData")
#
# pars_ss<-data.frame(param_space,ss)
# save(pars_ss,file = "G:/results/project 2/tip_info/round4/DAISIE2/obs_ss_long_with_pars.RData")
#
# load("G:/results/project 2/tip_info/round4/DAISIE2/obs_ss_long_with_pars.RData")
#
# ### pairwise ss for DAISIE space
# param_space <- readr::read_csv2("data/DAISIE_ABC_short.csv")
# save(param_space,file = "G:/results/project 2/tip_info/round4/DAISIE2/param_space.RData")
param_space <- readr::read_csv2("data/DAISIE_ABC_short.csv")
ss <- c()
for(i in 1:81){
  set.seed(i)
  message("set: ", i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = as.numeric(obs_sim_pars$K),  # as.numeric(obs_sim_pars$K)  Inf
                            replicates = 1)
  init_epsilon <- calc_epsilon_init(sim = obs_sim)
  ss <- rbind(ss,init_epsilon)
}


colnames(ss) <- c("total-nltt","clade-nltt","ana","clado",
                  "nonend","num-clade","scsd","ctsd","total",
                  "nonend-nltt","singleton-nltt")
rownames(ss) <- 1:81
save(ss,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/obs_ss.RData")

load("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/obs_ss.RData")

pars_ss<-data.frame(param_space,ss)
save(pars_ss,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/obs_ss_long_with_pars.RData")

load("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/obs_ss_long_with_pars.RData")

### pairwise ss for DAISIE space
param_space <- readr::read_csv2("data/DAISIE_ABC_short.csv")
save(param_space,file = "D:/Onedrive-shu/OneDrive/project 2/results/round4/DAISIE2/param_space.RData")


## new heatmap code
library(heatmaply)
library(htmlwidgets)

colnames(ss) <- c("LTT","CTT","Singleton-endemic","Multi-endemic",
                  "Non-endemic","Num clade","SDCS","SDCT","Num total",
                  "Nonend LTT","Singleton LTT")
p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss), xlab = "Summary statistics",
                                      ylab = "Summary statistics", k_col = 2, k_row = 2)
saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/adap_daisie_unif1/heatmap_ss.html"))

## for DI SPACE
param_space <- readr::read_csv2("data/DAISIE_ABC_short_DI.csv")
ss <- c()
for(i in 1:81){
  set.seed(i)
  message("set: ", i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = Inf,  # as.numeric(obs_sim_pars$K)
                            replicates = 1)
  init_epsilon <- calc_epsilon_init(sim = obs_sim)
  ss <- rbind(ss,init_epsilon)
}


colnames(ss) <- c("total-nltt","clade-nltt","ana","clado",
                  "nonend","num-clade","scsd","ctsd","total",
                  "nonend-nltt","singleton-nltt")
rownames(ss) <- 1:81
save(ss,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DI/obs_ss.RData")

load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DI/obs_ss.RData")

pars_ss<-data.frame(param_space,ss)
save(pars_ss,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DI/obs_ss_long_with_pars.RData")

load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DI/obs_ss_long_with_pars.RData")



## new heatmap code
library(heatmaply)
library(htmlwidgets)

colnames(ss) <- c("LTT","CTT","Singleton-endemic","Multi-endemic",
                  "Non-endemic","Num clade","SDCS","SDCT","Num total",
                  "Nonend LTT","Singleton LTT")
p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss), xlab = "Summary statistics",
                                      ylab = "Summary statistics", k_col = 2, k_row = 2)
saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_DI/heatmap_ss.html"))



