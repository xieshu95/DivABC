# heatmap for paper(DAISIE ABC)
library(heatmaply)
library(htmlwidgets)


# heatmap(cormat)
## paper
load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/obs_ss_DI.RData")
ss <- ss[,-c(1,6,11)]
ss[,6] <- ss[,5] - ss[,7]
colnames(ss) <- c("NLTT Total","NLTT Singleton-end", "NLTT Non-end","SD-CT",
                  "N Total","N end","N Non-end","SD-CS")
cormat <- round(cor(ss),2)

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/heatmap.tiff"),
     units="px", width=3000, height=2000,res = 380,compression="lzw")
heatmap <- corrplot(cormat,method = "circle",order = "FPC",tl.col = "brown", tl.srt = 15)
print(heatmap)
while (!is.null(dev.list()))  dev.off()


calc_ss <- function(sim,
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


  num_end <- num_singleton + num_clado
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

  cla_length_sim <- lapply(sim[[1]][-1],"[[", "branching_times")
  largest_clade_sim <- max(sapply(cla_length_sim,length))
  return(
    list(clade_nltt = clade_nltt, #
         total_nltt = total_nltt,
         singleton_nltt = singleton_nltt,
         nonend_nltt = nonend_nltt,
         colon_time = colon_time_sd,
         num_clades = num_clades, #
         num_total = num_total,
         num_end = num_end,
         num_nonend = num_nonend,
         clade_size = clade_size_sd,
         num_singleton = num_singleton,
         largest_clade_sim = largest_clade_sim #
    )
  )
}

calc_epsilon_init <- function(sim){
  ss <- calc_ss_no_ext(sim[[1]],1)
  eps_init <- as.numeric(unlist(ss)) * 1
  return(eps_init)
}


## new heatmap code
library(heatmaply)
library(htmlwidgets)
## paper
load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/obs_ss_DI.RData")
ss <- ss[,-c(1,6,11)]
ss[,6] <- ss[,5] - ss[,7]
colnames(ss) <- c("NLTT Total","NLTT Singleton-end", "NLTT Non-end","SD-CT",
                  "N Total","N end","N Non-end","SD-CS")
# colnames(ss) <- c("CTT","LTT","Singleton LTT", "Nonend LTT","SDCT",
#                   "N clade","N total","N singleton","N nonend","SDCS")

p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss), xlab = "Summary statistic",
                                      ylab = "Summary statistic", k_col = 2, k_row = 2)
saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/daisie_heatmap_tree_new.html"))

## method 2 for plotting heatmap


# label_names <- "Summary statistic"
# ss_name <- c("NLTT Total","NLTT Singleton-end", "NLTT Non-end","SD-CT",
#              "N Total","N end","N Non-end","SD-CS"


# tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space/heatmap_old2.tiff"),
#      units="px", width=3500, height=2500,res = 300,compression="lzw")
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
# print(heatmap)
# while (!is.null(dev.list()))  dev.off()




