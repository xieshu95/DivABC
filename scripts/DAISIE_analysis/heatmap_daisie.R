## heatmap for random combinations
set.seed(42)
lac <- runif(500,0.1,0.5)
mu <- runif(500,0.01,0.2)
gam <- runif(500,0.01,0.03)
laa <- runif(500,0.1,0.5)

test_ss_space <- data.frame(lac, mu, gam, laa)
save(test_ss_space,file = "G:/results/project 2/tip_info/round4/kernel/test_ss_error_DAISIE.RData")
# load("G:/results/project 2/tip_info/round4/secsse/test_ss_1/test_ss_space.RData")
ss <- matrix(NA,nrow = 100,ncol = 7)

t1 <- Sys.time()
set <- 1
set.seed(1)
while(set < 101){
  message("set",set)
  obs_sim_pars <- test_ss_space[set,]
  obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                           obs_sim_pars$mu,
                                           obs_sim_pars$gam,
                                           obs_sim_pars$laa),
                            K = Inf,
                            replicates = 1)

  ss[set,] <- calc_epsilon_init(sim = obs_sim)
  set <- set + 1
}
t2 <- Sys.time()
dt <- t2-t1
dt
colnames(ss) <- c("total_nltt","clade_nltt","ana","clado",
                  "nonend","clade_size_sd","colon_time_sd")
save(ss,file = "G:/results/project 2/tip_info/round4/kernel/test_ss_df.RData")


cormat <- round(cor(ss),2)
# heatmap(cormat)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)

ss_name <- c("Nltt","Clade nltt","nA","nC",
             "nI","Clade size SD","Colon time SD")

label_names <- "Summary statistic"
tiff(paste0("G:/results/project 2/tip_info/round4/kernel/heatmap_ss_with_value.tiff"),
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


### calculate ss for DAISIE short space
param_space <- readr::read_csv2("data/DAISIE_ABC_short.csv")

ss <- c()
for(i in 1:32){
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
  ss<-rbind(ss,init_epsilon)
}

colnames(ss) <- c("total_nltt","clade_nltt","ana","clado",
                  "nonend","clade_size_sd","colon_time_sd")
rownames(ss) <- 1:32
pars_ss<-data.frame(param_space,ss)
pars_ss$total <- pars_ss$ana+pars_ss$clado+pars_ss$nonend
save(ss,file = "G:/results/project 2/tip_info/round4/kernel/obs_ss.RData")
save(pars_ss,file = "G:/results/project 2/tip_info/round4/kernel/obs_ss_with_pars.RData")

load("G:/results/project 2/tip_info/round4/kernel/obs_ss_with_pars.RData")

### calculate ss for secsse space
param_space <- readr::read_csv2("data/DAISIE_ABC_short.csv")
load("G:/results/project 2/tip_info/round4/kernel/test_ss_error_DAISIE.RData")
ss <- c()
ss_error <- c()
set.seed(1)
obs_sim_pars <- param_space[1,]
obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                         obs_sim_pars$mu,
                                         obs_sim_pars$gam,
                                         obs_sim_pars$laa),
                          K = as.numeric(obs_sim_pars$K),
                          replicates = 1)
for(i in 1:100){
  set.seed(i)
  message("set: ", i)
  test_pars <- test_ss_space[i,]
  test_sim <- get_DAISIE_sim(parameters = c(test_pars$lac,
                                            test_pars$mu,
                                            test_pars$gam,
                                            test_pars$laa),
                            K = 20,
                            replicates = 1)
  init_epsilon <- calc_epsilon_init(sim = obs_sim)
  ss<-rbind(ss,init_epsilon)
}


colnames(ss) <- c("mpd","mpd_diff","mntd","mntd_diff","K","D","state1","state2","nltt")
save(ss,file = "G:/results/project 2/tip_info/round4/kernel/obs_ss_long_space.RData")

rownames(ss) <- 1:70
pars_ss<-data.frame(param_space,ss)
pars_ss$total <- pars_ss$state1+pars_ss$state2
save(pars_ss,file = "G:/results/project 2/tip_info/round4/kernel/obs_ss_long_with_pars.RData")

load("G:/results/project 2/tip_info/round4/kernel/obs_ss_long_space.RData")

density(stats::rexp(1000,5))
