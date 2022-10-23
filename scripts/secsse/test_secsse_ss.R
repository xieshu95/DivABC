## heatmap for random combinations
lam1 <- runif(200,0.1,1.0)
lam2 <- runif(200,0.1,1.0)
mu1 <- runif(200,0.01,0.1)
mu2 <- runif(200,0.01,0.1)
q12 <- runif(200,0.1,0.5)
q21 <- runif(200,0.1,0.5)

test_ss_space <- data.frame(lam1,lam2,mu1,mu2,q12,q21)
save(test_ss_space,file = "G:/results/project 2/tip_info/round4/secsse/test_ss_space2.RData")
# load("G:/results/project 2/tip_info/round4/secsse/test_ss_1/test_ss_space.RData")
ss <- matrix(NA,nrow = 200,ncol = 9)

t1 <- Sys.time()
set <- 1
set.seed(1)
while(set < 201){
  message("set",set)
  pars <- test_ss_space[set,]
  obs_sim <- get_secsse_sim(parameters = as.numeric(pars),
                            K = Inf,
                            replicates = 1)
  if (length(obs_sim[[1]]$examTraits) > 20 &&
      length(unique(obs_sim[[1]]$examTraits)) > 1) {
    ss[set,] <- calc_epsilon_init_secsse(sim = obs_sim)
    set <- set + 1
  }
}
t2 <- Sys.time()
dt <- t2-t1
dt
colnames(ss) <- c("mpd","mpd_diff","mntd","mntd_diff","K","D","state1","state2","nltt")
save(ss,file = "G:/results/project 2/tip_info/round4/secsse_long/test_ss_df.RData")


cormat <- round(cor(ss),2)
# heatmap(cormat)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)

ss_name <- c("MPD","MPD_12","MNTD","MNTD_12","K","D","State 1","State 2","NLTT")

label_names <- "Summary statistic"
tiff(paste0("G:/results/project 2/tip_info/round4/secsse_long/heatmap_ss_with_value.tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
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



### calculate ss for secsse space
param_space <- readr::read_csv2("data/secsse_ABC_long.csv")

ss <- c()
for(i in 1:70){
  set.seed(i)
  message("set: ", i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_secsse_sim(parameters = as.numeric(obs_sim_pars),
                            K = Inf,
                            replicates = 1) ## replicates = 30
  sim_function <- get_secsse_sim
  prior_generating_function <- prior_gen_secsse
  prior_density_function <- prior_dens_secsse
  fixpars = as.numeric(obs_sim_pars[1:6])
  init_epsilon <- calc_epsilon_init_secsse(sim = obs_sim)
  ss<-rbind(ss,init_epsilon)
}


colnames(ss) <- c("mpd","mpd_diff","mntd","mntd_diff","K","D","state1","state2","nltt")
save(ss,file = "G:/results/project 2/tip_info/round4/secsse/obs_ss_long_space.RData")

rownames(ss) <- 1:70
pars_ss<-data.frame(param_space,ss)
pars_ss$total <- pars_ss$state1+pars_ss$state2
save(pars_ss,file = "G:/results/project 2/tip_info/round4/secsse/obs_ss_long_with_pars.RData")

load("G:/results/project 2/tip_info/round4/secsse/obs_ss_with_pars.RData")

density(stats::rexp(1000,5))
