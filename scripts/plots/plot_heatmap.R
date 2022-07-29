### heatmap for all the accepted simulations

whole_df_ABC$dlac_abc <-whole_df_ABC$lac_abc -  whole_df_ABC$lac
whole_df_ABC$dmu_abc <-whole_df_ABC$mu_abc -  whole_df_ABC$mu
whole_df_ABC$dgam_abc <-whole_df_ABC$gam_abc -  whole_df_ABC$gam
whole_df_ABC$dlaa_abc <-whole_df_ABC$laa_abc -  whole_df_ABC$laa
save(whole_df_ABC,file = "G:/results/project 2/tip_info/round3/test_epsilon/whole_df_ABC.RData")


# 1. heatmap for DAISIE analysis
folder_path <- "G:/results/project 2/tip_info/round3/test_epsilon/DAISIE_ABC"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")

param_data2<-param_data[rep(seq_len(nrow(param_data)), each=100),]
#### ABC

# s1 = clade_nltt_error,
# s2 = ana_endemic_nltt_error,
# s3 = clado_endemic_nltt_error,
# s4 = nonendemic_nltt_error,
# s5 = num_col_error,
# s6 = clade_size_error,
# s7 = colon_time_error

## calculate the distance ss between observed data and simulations
ds1 <- c()
ds2 <- c()
ds3 <- c()
ds4 <- c()
ds5 <- c()
ds6 <- c()
ds7 <- c()
n_iteration <- c()
for(i in 1:160){  #160
  message("param: ", i)
  file_to_load <- grep(paste0("DAISIE_ABC_param_set_", i,".RData"),
                       files,
                       value = TRUE,
                       fixed = TRUE)
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    n_iteration <- c(n_iteration, rep(output$n_iter,100))
    obs_sim_pars <- param_data[i,]
    set.seed(i)
    obs_sim <- get_DAISIE_sim(parameters = c(obs_sim_pars$lac,
                                             obs_sim_pars$mu,
                                             obs_sim_pars$gam,
                                             obs_sim_pars$laa),
                              K = as.numeric(obs_sim_pars$K),
                              replicates = 2)
    accepted_sims <- output$sim_list
    for (j in 1:100) {
      df_stats <- calc_ss_diff (sim1 = obs_sim[[1]],
                              sim2 = accepted_sims[[j]])
      ds1 <- c(ds1,df_stats[1])
      ds2 <- c(ds2,df_stats[2])
      ds3 <- c(ds3,df_stats[3])
      ds4 <- c(ds4,df_stats[4])
      ds5 <- c(ds5,df_stats[5])
      ds6 <- c(ds6,df_stats[6])
      ds7 <- c(ds7,df_stats[7])
    }
  } else {
    n_iteration <- c(n_iteration, rep(NA,100))
    ds1 <- c(ds1, rep(NA,100))
    ds2 <- c(ds2, rep(NA,100))
    ds3 <- c(ds3, rep(NA,100))
    ds4 <- c(ds4, rep(NA,100))
    ds5 <- c(ds5, rep(NA,100))
    ds6 <- c(ds6, rep(NA,100))
    ds7 <- c(ds7, rep(NA,100))
  }
}


## calculate the distance ss between observed data and simulations
s1 <- c()
s2 <- c()
s3 <- c()
s4 <- c()
s5 <- c()
s6 <- c()
s7 <- c()
n_iteration <- c()
for(i in 1:160){
  message("param: ", i)
  file_to_load <- grep(paste0("DAISIE_ABC_param_set_", i,".RData"),
                       files,
                       value = TRUE,
                       fixed = TRUE)
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    n_iteration <- c(n_iteration, rep(output$n_iter,100))
    accepted_sims <- output$sim_list
    for (j in 1:100) {
      df_stats <- calc_ss (sim = accepted_sims[[j]],
                           replicates = 1,
                           distance_method = "abs")
      s1 <- c(s1,df_stats[1])
      s2 <- c(s2,df_stats[2])
      s3 <- c(s3,df_stats[3])
      s4 <- c(s4,df_stats[4])
      s5 <- c(s5,df_stats[5])
      s6 <- c(s6,df_stats[6])
      s7 <- c(s7,df_stats[7])
    }
  } else {
    n_iteration <- c(n_iteration, rep(NA,100))
    s1 <- c(s1, rep(NA,100))
    s2 <- c(s2, rep(NA,100))
    s3 <- c(s3, rep(NA,100))
    s4 <- c(s4, rep(NA,100))
    s5 <- c(s5, rep(NA,100))
    s6 <- c(s6, rep(NA,100))
    s7 <- c(s7, rep(NA,100))
  }
}

whole_df_ss <- data.frame(ds1,ds2,ds3,ds4,ds5,ds6,ds7,
                          s1,s2,s3,s4,s5,s6,s7)
save(whole_df_ss,file = "G:/results/project 2/tip_info/round3/test_epsilon/whole_df_ss_all.RData")
load("G:/results/project 2/tip_info/round3/test_epsilon/whole_df_ss_all.RData")

load("G:/results/project 2/tip_info/round3/test_epsilon/whole_df_ABC.RData")
whole_df_with_ss <- cbind(whole_df_ABC,whole_df_ss)
save(whole_df_with_ss,file = "G:/results/project 2/tip_info/round3/test_epsilon/whole_df_with_ss_dss.RData")

load("G:/results/project 2/tip_info/round3/test_epsilon/whole_df_with_ss_dss.RData")

#### num_spec
folder_path <- "G:/results/project 2/tip_info/round3/test_epsilon/DAISIE_ABC"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")

param_data2<-param_data[rep(seq_len(nrow(param_data)), each=100),]
s8 <- c()
s9 <- c()
s10 <- c()
s11 <- c()
n_iteration <- c()
for(i in 1:160){
  message("param: ", i)
  file_to_load <- grep(paste0("DAISIE_ABC_param_set_", i,".RData"),
                       files,
                       value = TRUE,
                       fixed = TRUE)
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    n_iteration <- c(n_iteration, rep(output$n_iter,100))
    accepted_sims <- output$sim_list
    for (j in 1:100) {
      df_stats <- calc_num_specs(sim = accepted_sims[[j]],
                           replicates = 1,
                           distance_method = "abs")
      s8 <- c(s8,df_stats[1])
      s9 <- c(s9,df_stats[2])
      s10 <- c(s10,df_stats[3])
      s11 <- c(s11,df_stats[4])
    }
  } else {
    n_iteration <- c(n_iteration, rep(NA,100))
    s8 <- c(s8, rep(NA,100))
    s9 <- c(s9, rep(NA,100))
    s10 <- c(s10, rep(NA,100))
    s11 <- c(s11, rep(NA,100))
  }
}

load("G:/results/project 2/tip_info/round3/test_epsilon/whole_df_with_ss_dss.RData")
whole_df_with_ss <-cbind(whole_df_with_ss,s8,s9,s10,s11)
save(whole_df_with_ss,file = "G:/results/project 2/tip_info/round3/test_epsilon/whole_df_with_ss_dss.RData")


# plot dss_vs_dss: 16~21; plot ss_vs_ss: 23~28
### combine all the parameter sets
ss_diff <- whole_df_with_ss[,23:28]  #16:21/23:28
ss_diff <- ss_diff %>% tidyr::drop_na()
ss_name <- c(expression("SESTT"),   ## Singleton-endemic  Delta * "SESTT"
             expression("MESTT"),   ## Multiple-endemic
             expression("NESTT"),   ## Non-endemic
             expression("N Col"),
             expression("SD-CS"),
             expression("SD-CT"))

ss_name <- c(expression(Delta * "CTT"),   ## clades
             expression(Delta * "SESTT"),   ## Singleton-endemic
             expression(Delta * "MESTT"),   ## Multiple-endemic
             expression(Delta * "NESTT"),   ## Non-endemic
             expression("SD-CS"),
             expression("SD-CT"))
##expression(Delta * "CTT"),
cormat <- round(cor(ss_diff),2)
# heatmap(cormat)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)

label_names <- "Summary statistic"
tiff(paste0("G:/results/project 2/tip_info/round3/test_epsilon/heatmap/heatmap_ss_no_ss1.tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
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




# for(i in 1:16){
#   ss_diff <- whole_df_with_ss[(i*1000-999):(i*1000),11:17]
#   ss_diff <- ss_diff %>% tidyr::drop_na()
#   ss_name <- c(expression(Delta * "CTT"),
#                expression(Delta * "SESTT"),   ## Singleton-endemic
#                expression(Delta * "MESTT"),   ## Multiple-endemic
#                expression(Delta * "NESTT"),   ## Non-endemic
#                expression("N Col"),
#                expression("SD-CS"),
#                expression("SD-CT"))
#   cormat <- round(cor(ss_diff),2)
#   # heatmap(cormat)
#   head(cormat)
#   library(reshape2)
#   melted_cormat <- melt(cormat)
#   library(ggplot2)
#
#   label_names <- "Summary statistic"
#   tiff(paste0("G:/results/project 2/tip_info/round3/dec_kernel/heatmap/scenario_",i,".tiff"),
#        units="px", width=3000, height=2000,res = 300,compression="lzw")
#   heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
#     geom_tile() +
#     ggplot2::scale_x_discrete(labels= ss_name)+
#     ggplot2::scale_y_discrete(labels= ss_name)+
#     ggplot2::xlab(label_names) +  #Rate differential ratio of anagenesis/Diversity dependence
#     ggplot2::ylab(label_names) +
#     ggplot2::guides(fill = guide_legend(title="Correlation"))+
#     ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
#     ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
#     ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12)) +
#     ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10)) +
#     ggplot2::theme(legend.text = ggplot2::element_text(size = 12)) +
#     ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
#     ggplot2::theme(plot.margin = ggplot2::margin(6, 0.2, 6, 0.2)) +
#     ggplot2::ggtitle("Correlations between summary statistics") +
#     ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5))
#   print(heatmap)
#   while (!is.null(dev.list()))  dev.off()
# }
#
# ### combine all the parameter sets
#  ss_diff <- whole_df_with_ss[,11:17]
# ss_diff <- ss_diff %>% tidyr::drop_na()
# ss_name <- c(expression(Delta * "CTT"),
#              expression(Delta * "SESTT"),   ## Singleton-endemic
#              expression(Delta * "MESTT"),   ## Multiple-endemic
#              expression(Delta * "NESTT"),   ## Non-endemic
#              expression("N Col"),
#              expression("SD-CS"),
#              expression("SD-CT"))
# cormat <- round(cor(ss_diff),2)
# # heatmap(cormat)
# head(cormat)
# library(reshape2)
# melted_cormat <- melt(cormat)
# library(ggplot2)
#
# label_names <- "Summary statistic"
# tiff(paste0("G:/results/project 2/tip_info/round3/dec_kernel/heatmap/heatmap_all.tiff"),
#      units="px", width=3000, height=2000,res = 300,compression="lzw")
# heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
#   geom_tile() +
#   ggplot2::scale_x_discrete(labels= ss_name)+
#   ggplot2::scale_y_discrete(labels= ss_name)+
#   ggplot2::xlab(label_names) +  #Rate differential ratio of anagenesis/Diversity dependence
#   ggplot2::ylab(label_names) +
#   ggplot2::guides(fill = guide_legend(title="Correlation"))+
#   ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
#   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
#   ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12)) +
#   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10)) +
#   ggplot2::theme(legend.text = ggplot2::element_text(size = 12)) +
#   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
#   ggplot2::theme(plot.margin = ggplot2::margin(6, 0.2, 6, 0.2)) +
#   ggplot2::ggtitle("Correlations between summary statistics") +
#   ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5))
# print(heatmap)
# while (!is.null(dev.list()))  dev.off()



##############################################################
#2. heatmap for TRAISIE analysis
folder_path <- "G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/TraiSIE_ABC_DD/"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/TraiSIE_ABC_DD.csv")

param_data2<-param_data[rep(seq_len(nrow(param_data)), each=100),]
#### ABC

# s1 = s$ana_endemic_nltt_error,
# s2 = s$clado_endemic_nltt_error,
# s3 = s$nonendemic_nltt_error,
# s4 = s$clade_nltt_error,
# s5 = s$num_ana_error_state1,
# s6 = s$num_ana_error_state2,
# s7 = s$num_clado_error_state1,
# s8 = s$num_clado_error_state2,
# s9 = s$num_nonend_error_state1,
# s10 = s$num_nonend_error_state2


s1 <- c()
s2 <- c()
s3 <- c()
s4 <- c()
s5 <- c()
s6 <- c()
s7 <- c()
s8 <- c()
s9 <- c()
s10 <- c()
n_iteration <- c()
for(i in 1:240){
  message("param: ", i)
  file_to_load <- grep(paste0("TraiSIE_ABC_DD_param_set_", i,".RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    n_iteration <- c(n_iteration, rep(output$n_iter,100))
    obs_sim_pars <- param_data[i,]
    obs_sim <- get_TraiSIE_sim(parameters = as.numeric(c(obs_sim_pars$lac,
                                                         obs_sim_pars$mu,
                                                         obs_sim_pars$gam,
                                                         obs_sim_pars$laa,
                                                         obs_sim_pars$lac2,
                                                         obs_sim_pars$mu2,
                                                         obs_sim_pars$gam2,
                                                         obs_sim_pars$laa2,
                                                         obs_sim_pars$trans,
                                                         obs_sim_pars$trans2)),
                               K = as.numeric(obs_sim_pars$K),
                               replicates = 2)
    accepted_sims <- output$sim_list
    for (j in 1:100) {
      df_stats <- calc_ss_diff (sim1 = obs_sim[[1]],
                                sim2 = accepted_sims[[j]])
      s1 <- c(s1,df_stats[1])
      s2 <- c(s2,df_stats[2])
      s3 <- c(s3,df_stats[3])
      s4 <- c(s4,df_stats[4])
      s5 <- c(s5,df_stats[5])
      s6 <- c(s6,df_stats[6])
      s7 <- c(s7,df_stats[7])
      s8 <- c(s8,df_stats[8])
      s9 <- c(s9,df_stats[9])
      s10 <- c(s10,df_stats[10])
    }
  } else {
    n_iteration <- c(n_iteration, rep(NA,100))
    s1 <- c(s1, rep(NA,100))
    s2 <- c(s2, rep(NA,100))
    s3 <- c(s3, rep(NA,100))
    s4 <- c(s4, rep(NA,100))
    s5 <- c(s5, rep(NA,100))
    s6 <- c(s6, rep(NA,100))
    s7 <- c(s7, rep(NA,100))
    s8 <- c(s8, rep(NA,100))
    s9 <- c(s9, rep(NA,100))
    s10 <- c(s10, rep(NA,100))
  }
}
whole_df_ss <- data.frame(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,n_iteration)
save(whole_df_ss,file = "G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/whole_df_ss.RData")
load("G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/whole_df_ss.RData")
load("G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/DD_whole_df_ABC.RData")
whole_df_with_ss <- cbind(whole_df_ABC,whole_df_ss)
save(whole_df_with_ss,file = "G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/whole_df_with_ss.RData")

## every 500 values means one combination
for(i in 1:48){
  ss_diff <- whole_df_ss[(i*500-499):(i*500),1:10]
  ss_diff <- ss_diff %>% tidyr::drop_na()
  ss_name <- c(expression(Delta * "SESTT"),   ## Singleton-endemic
               expression(Delta * "MESTT"),   ## Multiple-endemic
               expression(Delta * "NESTT"),   ## Non-endemic
               expression(Delta * "CTT"),
               expression("SE 1"),
               expression("SE 2"),
               expression("ME 1"),
               expression("ME 2"),
               expression("NE 1"),
               expression("NE 2"))
  cormat <- round(cor(ss_diff),2)
  # heatmap(cormat)
  head(cormat)
  library(reshape2)
  melted_cormat <- melt(cormat)
  library(ggplot2)

  label_names <- "Summary statistic"
  tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/heatmap/scenario_",i,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile() +
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

### combine all the parameter sets
# s1 = s$ana_endemic_nltt_error,
# s2 = s$clado_endemic_nltt_error,
# s3 = s$nonendemic_nltt_error,
# s4 = s$clade_nltt_error,
# s5 = s$num_ana_error_state1,
# s6 = s$num_ana_error_state2,
# s7 = s$num_clado_error_state1,
# s8 = s$num_clado_error_state2,
# s9 = s$num_nonend_error_state1,
# s10 = s$num_nonend_error_state2
ss_diff <- whole_df_ss[,1:10]
ss_diff <- ss_diff %>% tidyr::drop_na()
ss_name <- c(expression(Delta * "SESTT"),   ## Singleton-endemic
             expression(Delta * "MESTT"),   ## Multiple-endemic
             expression(Delta * "NESTT"),   ## Non-endemic
             expression(Delta * "CTT"),
             expression("SE 1"),
             expression("SE 2"),
             expression("ME 1"),
             expression("ME 2"),
             expression("NE 1"),
             expression("NE 2"))
cormat <- round(cor(ss_diff),2)
# heatmap(cormat)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)

label_names <- "Summary statistic"
tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/heatmap/heatmap_all.tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
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
