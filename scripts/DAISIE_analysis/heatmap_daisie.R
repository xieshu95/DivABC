# heatmap for paper(DAISIE ABC)
calc_ss_no_ext <- function(sim,
                           replicates,
                           distance_method = "abs") {

  # Spec error
  ltt <- full_ltt(sim)
  sim_0 <- rep(0,length(ltt$brt))

  total_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = ltt$brt,
    species_number = ltt$n_spec,
    event_times2 = ltt$brt,
    species_number2 = sim_0,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )

  # Clades number error
  clade_ltt <- clade_ltt(sim)
  sim_0 <- rep(0,length(clade_ltt$colon_time))
  clade_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = clade_ltt$colon_time,
    species_number = clade_ltt$n_clade,
    event_times2 = clade_ltt$colon_time,
    species_number2 = sim_0,
    distance_method = distance_method,
    time_unit = "ago",
    normalize = FALSE
  )


  stt_last_row_sim <-
    length(sim[[1]][[1]]$stt_all[, "present"])

  num_singleton <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "nA"])

  num_clado <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "nC"])

  num_nonend <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "nI"])

  num_total <- num_singleton + num_clado + num_nonend

  clade_size_sd <- clade_size_sd(sim = sim)
  colon_time_sd <- colon_time_sd(sim = sim)

  ## added nonend_ltt and singleton-ltt
  end_ltt <- end_ltt(sim)
  nonend_ltt <- end_ltt$nonend_ltt
  singleton_ltt <- end_ltt$singleton_ltt
  if(nonend_ltt[1,1] == 0) {
    nonend_nltt <- 0
  } else {
    sim_0 <- rep(0,length(nonend_ltt$nonend_brt))
    nonend_nltt <- nLTT::nltt_diff_exact_extinct(
      event_times = nonend_ltt$nonend_brt,
      species_number = nonend_ltt$n_nonend,
      event_times2 = nonend_ltt$nonend_brt,
      species_number2 = sim_0,
      distance_method = distance_method,
      time_unit = "ago",
      normalize = FALSE
    )
  }

  if(singleton_ltt[1,1] == 0) {
    singleton_nltt <- 0
  } else {
    sim_0 <- rep(0,length(singleton_ltt$singleton_brt))
    singleton_nltt <- nLTT::nltt_diff_exact_extinct(
      event_times = singleton_ltt$singleton_brt,
      species_number = singleton_ltt$n_singleton,
      event_times2 = singleton_ltt$singleton_brt,
      species_number2 = sim_0,
      distance_method = distance_method,
      time_unit = "ago",
      normalize = FALSE
    )
  }

  num_clades <-
    as.numeric(sim[[1]][[1]]$stt_all[stt_last_row_sim, "present"])

  num_total <- num_singleton + num_clado + num_nonend

  return(
    list(clade_nltt = clade_nltt,
         total_nltt = total_nltt,
         singleton_nltt = singleton_nltt,
         nonend_nltt = nonend_nltt,
         colon_time = colon_time_sd,
         num_clades = num_clades,
         num_total = num_total,
         num_singleton = num_singleton,
         num_nonend = num_nonend,
         clade_size = clade_size_sd
    )
  )
}

calc_epsilon_init <- function(sim){
  ss <- calc_ss_no_ext(sim[[1]],1)
  eps_init <- as.numeric(unlist(ss)) * 1
  return(eps_init)
}

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
save(pars_accept,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_test/ramdom_ss_pars_accept.RData")

colnames(ss) <- c("clade-nltt","total-nltt","singleton-nltt","nonend-nltt","ctsd",
                  "num-clade","total","singleton","nonend","scsd")
rownames(ss) <- 1:500
save(ss,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_test/random_obs_ss.RData")

load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_test/random_obs_ss.RData")
library(heatmaply)
library(htmlwidgets)

colnames(ss) <- c("CTT","LTT","Singleton LTT", "Nonend LTT","SDCT",
                  "N clade","N total","N singleton","N nonend","SDCS")
p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss), xlab = "Summary statistics",
                                      ylab = "Summary statistics", k_col = 2, k_row = 2)
saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie_test/heatmap_ss_random.html"))

## for space
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


colnames(ss) <- c("clade-nltt","total-nltt","singleton-nltt","nonend-nltt","ctsd",
                  "num-clade","total","singleton","nonend","scsd")
rownames(ss) <- 1:81
save(ss,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/obs_ss.RData")

load("D:/Onedrive-shu/OneDrive/project 2/results/round5/obs_ss.RData")

pars_ss<-data.frame(param_space,ss)
save(pars_ss,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/heatmaps/obs_ss_long_with_pars.RData")

load("D:/Onedrive-shu/OneDrive/project 2/results/round5/heatmaps/obs_ss_long_with_pars.RData")

### pairwise ss for DAISIE space
param_space <- readr::read_csv2("data/DAISIE_ABC_short.csv")
save(param_space,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/heatmaps/param_space.RData")


## new heatmap code
library(heatmaply)
library(htmlwidgets)

colnames(ss) <- c("CTT","LTT","Singleton LTT", "Nonend LTT","SDCT",
                  "N clade","N total","N singleton","N nonend","SDCS")
p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss), xlab = "Summary statistics",
                                      ylab = "Summary statistics", k_col = 2, k_row = 2)
saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/heatmaps/daisie_heatmap_tree.html"))



## old code:
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

density(stats::rexp(1000,5))
