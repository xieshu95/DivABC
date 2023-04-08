# exmaple traisie sims
parameters = c(0.3,0,0.01,0.1,0.3,0,0.01,0.1,0.1,0.1)
K = 20
set.seed(1)
sim <- DAISIE::DAISIE_sim_trait_dep( ##TRAISIERCPP
  time = 5,
  M = 500,
  pars = c(parameters[1],parameters[2],K,parameters[3],parameters[4]),
  replicates = 1,
  sample_freq  = Inf,
  plot_sims = FALSE,
  cond = 1,
  verbose = FALSE,
  trait_pars = DAISIE::create_trait_pars(clado_rate2 = parameters[5],
                                         ext_rate2 = parameters[6],
                                         immig_rate2 = parameters[7],
                                         ana_rate2 = parameters[8],
                                         trans_rate = parameters[9],
                                         trans_rate2 = parameters[10],
                                         M2 = 500)
)
calc_ss_traisie_test <- function(sim) {
  # Spec error
  stt <- full_ltt(sim)
  stt_0 <- rep(0,nrow(stt))
  # total number species nltt error
  total_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = stt$brt,
    species_number = stt$n_spec,
    event_times2 = stt$brt,
    species_number2 = stt_0,
    distance_method = "abs",
    time_unit = "ago",
    normalize = FALSE
  )


  # Clades number nltt error
  clade_ltt <- clade_ltt(sim)
  clade_0 <- rep(0,nrow(clade_ltt))
  clade_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = clade_ltt$colon_time,
    species_number = clade_ltt$n_clade,
    event_times2 = clade_ltt$colon_time,
    species_number2 = clade_0,
    distance_method = "abs",
    time_unit = "ago",
    normalize = FALSE
  )


  ### 5. number of singleton speciation difference between states
  stt_last_row <-
    length(sim[[1]][[1]]$stt_two_states[, "present"])
  num_singleton_state1 <-
    as.numeric(
      sim[[1]][[1]]$stt_two_states[stt_last_row, "nA"])
  num_singleton_state2 <-
    as.numeric(
      sim[[1]][[1]]$stt_two_states[stt_last_row, "nA2"])
  if(num_singleton_state1 != 0 && num_singleton_state2 != 0) {
    num_singleton_ratio <- num_singleton_state1/num_singleton_state2
  } else {
    num_singleton_ratio <- 0
  }



  ### 6. number of multiple lineage speciation(cladogenesis)
  num_multi_state1 <-
    as.numeric(
      sim[[1]][[1]]$stt_two_states[stt_last_row, "nC"])
  num_multi_state2 <-
    as.numeric(
      sim[[1]][[1]]$stt_two_states[stt_last_row, "nC2"])
  if(num_multi_state1 != 0 &&  num_multi_state2 != 0) {
    num_multi_ratio <- num_multi_state1/num_multi_state2
  } else {
    num_multi_ratio <- 0
  }

  # 7. number of Nonendemic
  nonend_state1 <-
    as.numeric(
      sim[[1]][[1]]$stt_two_states[stt_last_row, "nI"])
  nonend_state2 <-
    as.numeric(
      sim[[1]][[1]]$stt_two_states[stt_last_row, "nI2"])
  if(nonend_state1 != 0 &&  nonend_state2 != 0) {
    nonend_ratio <- nonend_state1/nonend_state2
  } else {
    nonend_ratio <- 0
  }


  ## total tip ratio
  total_state1 <- num_singleton_state1 + num_multi_state1 + nonend_state1
  total_state2 <- num_singleton_state2 + num_multi_state2 + nonend_state2
  tip_ratio <- total_state1/total_state2

  ## JSD of each state (sort)
  state1_vector <- unlist(lapply(sim[[1]][-1],"[[", "num_state1"))
  state2_vector <- unlist(lapply(sim[[1]][-1],"[[", "num_state2"))

  JSD <- suppressMessages(philentropy::JSD(rbind(state1_vector,state2_vector)))


  num_total <- total_state1 + total_state2
  num_clade <- as.numeric(
      sim[[1]][[1]]$stt_two_states[stt_last_row, "present"])

  ## number of species in the largest clade
  cla_length <- lapply(sim[[1]][-1],"[[", "branching_times")
  largest_clade <- max(sapply(cla_length,length))

  return(
    list(total_nltt = total_nltt,
         clade_nltt = clade_nltt,
         num_singleton_state1 = num_singleton_state1,
         num_singleton_state2 = num_singleton_state2,
         num_singleton_ratio = num_singleton_ratio,
         num_multi_state1 = num_multi_state1,
         num_multi_state2 = num_multi_state2,
         num_multi_ratio = num_multi_ratio,
         nonend_state1 = nonend_state1,
         nonend_state2 = nonend_state2,
         nonend_ratio = nonend_ratio,
         total_state1 = total_state1,
         total_state2 = total_state2,
         tip_ratio = tip_ratio,
         JSD = JSD,
         num_total = num_total,
         num_clade = num_clade,
         largest_clade = largest_clade)
  )
}
# ss<-calc_ss_secsse_test(sim)
# ss

calc_epsilon_init_traisie_test <- function(sim){
  ss <- calc_ss_traisie_test(sim[[1]])
  eps_init <- as.numeric(unlist(ss)) * 1
  return(eps_init)
}

# calc_epsilon_init_traisie_test(sim)

for(test in c(1:5)) {
  param_space <- readr::read_csv2(paste0("data/traisie_ABC_test",test,".csv"))
  ss <- c()
  for(i in 1:100){
    set.seed(i)
    message("set: ", i)
    obs_sim_pars <- param_space[i,]
    obs_sim <- get_TraiSIE_sim(parameters = as.numeric(obs_sim_pars),
                               K = Inf,
                               replicates = 1) ## replicates = 30
    init_epsilon <- calc_epsilon_init_traisie_test(sim = obs_sim)
    ss<-rbind(ss,init_epsilon)
  }
  colnames(ss) <- c("Nltt","CLTT",
                    "singleton1","singleton2","single-ratio",
                    "multi_end1","multi_end2","multi_ratio",
                    "nonend1","nonend2","nonend_ratio",
                    "num1","num2","tip_ratio","JSD",
                    "num_total","num_clade","largest")
  ss<-data.frame(ss)
  # colnames(ss) <- c("state1","state2","tree_size","tip_ratio")
  save(ss,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie2/DI_obs_ss_test",test,".RData"))

}

## new heatmap code
library(heatmaply)
library(htmlwidgets)
for(test in 1:5){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie1/obs_ss_test",test,".RData"))
  p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss), xlab = "Summary statistics",
                                        ylab = "Summary statistics", k_col = 2, k_row = 2)
  saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie2/ss_heatmap/DI_heatmap_ss_test_",test,".html"))

}

ss_comb <-c()
for(test in 1:5){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie1/obs_ss_test",test,".RData"))
  ss_comb <- rbind(ss_comb,ss)
}
p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss_comb), xlab = "Summary statistics",
                                      ylab = "Summary statistics", k_col = 2, k_row = 2)
saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie2/ss_heatmap/DI_heatmap_ss_combine.html"))


## old code for heatmap
for(test in 1:5){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie1/obs_ss_test",test,".RData"))
  # ss<-ss_comb
  rcorrDat<- Hmisc::rcorr(as.matrix(ss))
  cormat <- round(rcorrDat$r,2)
  # heatmap(cormat)
  head(cormat)

  library(reshape2)
  melted_cormat <- melt(cormat)
  library(ggplot2)

  ss_name <- c("Nltt","CLTT",
               "singleton1","singleton2","single-ratio",
               "multi_end1","multi_end2","multi_ratio",
               "nonend1","nonend2","nonend_ratio",
               "num1","num2","tip_ratio","JSD",
               "num_total","num_clade","largest")
  label_names <- "Summary statistic"
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie1/ss_heatmap/heatmap_ss_old_test_",test,".tiff"),
       units="px", width=4500, height=4500,res = 300,compression="lzw")
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
}


### examples of JSD(Jensen-Shannon Divergence)
library(philentropy)
a<-c(1,2,3,4,5)
b<-c(6,7,8,9,10)
c<-c(5,4,3,2,1)

m<-c(10,9,8,7,6)

d<- sort(c,decreasing = F)
x<-rbind(a,b)
y<-rbind(c,b)
z <- rbind(d,b)
JSD(x)
JSD(y)
JSD(z)
JSD(rbind(a,b))
JSD(rbind(c,m))

ss$num1 + ss$num2
