lam1 <- runif(200,0.1,1.0)
lam2 <- runif(200,0.1,1.0)
mu1 <- runif(200,0.01,0.1)
mu2 <- runif(200,0.01,0.1)
q12 <- runif(200,0.1,0.5)
q21 <- runif(200,0.1,0.5)

test_ss_space <- data.frame(lam1,lam2,mu1,mu2,q12,q21)
save(test_ss_space,file = "G:/results/project 2/tip_info/round4/secsse/test_ss_space.RData")
# load("G:/results/project 2/tip_info/round4/secsse/test_ss_1/test_ss_space.RData")
ss <- matrix(NA,nrow = 200,ncol = 7)

t1 <- Sys.time()
set <- 1
set.seed(1)
while(set < 201){
  message("set",set)
  pars <- test_ss_space[set,]
  obs_sim <- get_secsse_sim(parameters = as.numeric(pars),
                            K = Inf,
                            replicates = 1)
  if (length(obs_sim[[1]]$examTraits) > 5 &&
      length(unique(obs_sim[[1]]$examTraits)) > 1) {
    ss[set,] <- calc_epsilon_init_secsse(sim = obs_sim)
    set <- set + 1
  }
}
t2 <- Sys.time()
dt <- t2-t1
dt
colnames(ss) <- c("mpd_12","mntd_12","K","D","num1","num2","nltt")
save(ss,file = "G:/results/project 2/tip_info/round4/secsse/test_ss_df.RData")


cormat <- round(cor(ss),2)
# heatmap(cormat)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)

ss_name <- c("MPD","MNTD","K","D","State 1","State 2","NLTT")

label_names <- "Summary statistic"
tiff(paste0("G:/results/project 2/tip_info/round4/secsse/heatmap_ss.tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  # geom_text(aes(Var2, Var1, label = value), size = 5) +

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
