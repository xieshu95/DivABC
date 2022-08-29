## plot TRAISIE_DD


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




load("G:/results/project 2/tip_info/round3/TRAISIE_DD_lac_mu/DD_whole_df_ABC.RData")
whole_df_ABC$Transition <- whole_df_ABC$trans
whole_df_ABC$Transition[whole_df_ABC$trans == "0" & whole_df_ABC$trans2 == "0"] <- "nn"
whole_df_ABC$Transition[whole_df_ABC$trans == "0.02" & whole_df_ABC$trans2 == "0.2"] <- "lh"
whole_df_ABC$Transition[whole_df_ABC$trans == "0.2" & whole_df_ABC$trans2 == "0.02"] <- "hl"

library(ggplot2)
colors <- c("State1"="red","State2"="blue")
whole_df_ABC$Transition <- factor(whole_df_ABC$Transition,
                             levels = c("hl","lh","nn"),
                             labels = c(expression("high"~italic(q)[12]~"low"~italic(q)[21]),
                                        expression("low"~italic(q)[12]~"high"~italic(q)[21]),
                                        expression("no"~italic(q)[12]~"no"~italic(q)[21])))

i = 1
data <- whole_df_ABC[((i*24000-23999)):(i*24000),]
p_lac1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.15,0.55)+
  ggplot2::geom_boxplot(ggplot2::aes(x = lac, y = lac_abc1, group = lac),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac, y = lac),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Cladogenesis state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)+
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 13, color = "black"))
p_lac1


p_lac2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.45,0.85)+
  ggplot2::geom_boxplot(ggplot2::aes(x = lac2, y = lac_abc2, group = lac2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac2, y = lac2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Cladogenesis state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)+
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 13, color = "black"))
p_lac2

tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_10reps/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_lac1,p_lac2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()


i = 2
# data <- whole_df_ABC[((i*12000-11999)):(i*12000),]
data <- whole_df_ABC[((i*24000-23999)):(i*24000),]
p_mu1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.015,0.11)+
  ggplot2::geom_boxplot(ggplot2::aes(x = mu, y = mu_abc1, group = mu),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = mu, y = mu),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Extinction state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)+
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 13, color = "black"))



p_mu2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.09,0.185)+
  ggplot2::geom_boxplot(ggplot2::aes(x = mu2, y = mu_abc2, group = mu2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = mu2, y = mu2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Extinction state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)+
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 13, color = "black"))


tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_10reps/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_mu1,p_mu2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()


i = 3
# data <- whole_df_ABC[((i*12000-11999)):(i*12000),]
data <- whole_df_ABC[((i*24000-23999)):(i*24000),]
p_gam1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,0.07)+
  xlim(0.003,0.022)+
  ggplot2::geom_boxplot(ggplot2::aes(x = gam, y = gam_abc1, group = gam),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam, y = gam),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Colonization state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)+
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 13, color = "black"))



p_gam2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,0.07)+
  xlim(0.018,0.037)+
  ggplot2::geom_boxplot(ggplot2::aes(x = gam2, y = gam_abc2, group = gam2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam2, y = gam2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Colonization state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)+
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 13, color = "black"))


tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_10reps/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_gam1,p_gam2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()



i = 4
data <- whole_df_ABC[((i*24000-23999)):(i*24000),]
p_laa1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.02,0.23)+
  ggplot2::geom_boxplot(ggplot2::aes(x = laa, y = laa_abc1, group = laa),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa, y = laa),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Anagenesis state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)+
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 13, color = "black"))



p_laa2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.17,0.38)+
  ggplot2::geom_boxplot(ggplot2::aes(x = laa2, y = laa_abc2, group = laa2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa2, y = laa2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Anagenesis state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)+
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 13, color = "black"))


tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_10reps/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_laa1,p_laa2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()



#########################################################################

## plot TRAISIE_DD_with_q
load("G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/DD_whole_df_ABC.RData")
whole_df_ABC$Transition <- whole_df_ABC$trans
whole_df_ABC$Transition[whole_df_ABC$trans == "0.02" & whole_df_ABC$trans2 == "0.02"] <- "nn"
whole_df_ABC$Transition[whole_df_ABC$trans == "0.02" & whole_df_ABC$trans2 == "0.2"] <- "lh"
whole_df_ABC$Transition[whole_df_ABC$trans == "0.2" & whole_df_ABC$trans2 == "0.02"] <- "hl"

library(ggplot2)
colors <- c("State1"="red","State2"="blue")
whole_df_ABC$Transition <- factor(whole_df_ABC$Transition,
                                  levels = c("hl","lh","nn"),
                                  labels = c(expression("high"~italic(q)[12]~"low"~italic(q)[21]),
                                             expression("low"~italic(q)[12]~"high"~italic(q)[21]),
                                             expression("no"~italic(q)[12]~"no"~italic(q)[21])))



i = 1
data <- whole_df_ABC[((i*6000-5999)):(i*6000),]
p_q12 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,1.2)+
  xlim(0.15,0.55)+
  ggplot2::geom_boxplot(ggplot2::aes(x = lac, y = trans_abc1, group = lac),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac, y = trans),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Transition state 1 to 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::ylab("Estimated transition") +
  ggplot2::xlab("Real cladogenesis")+  #expression(lambda[1]^c)
  # ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)+
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 13, color = "black"))
p_q12


p_q21 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,1.2)+
  xlim(0.45,0.85)+
  ggplot2::geom_boxplot(ggplot2::aes(x = lac2, y = trans_abc2, group = lac2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac2, y = trans2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Transition state 2 to 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::ylab("Estimated transition") +
  ggplot2::xlab("Real cladogenesis")+  #expression(lambda[1]^c)
  # ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)+
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 13, color = "black"))
p_q21

tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_10reps/boxplots/trans_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_q12,p_q21,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()


i = 3
# data <- whole_df_ABC[((i*12000-11999)):(i*12000),]
data <- whole_df_ABC[((i*6000-5999)):(i*6000),]
p_trans_gam1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,1.2)+
  xlim(0.003,0.022)+
  ggplot2::geom_boxplot(ggplot2::aes(x = gam, y = trans_abc1, group = gam),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam, y = trans),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Transition state 1 to 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::ylab("Estimated transition") +
  ggplot2::xlab("Real colonization")+  #expression(lambda[1]^c)
  # ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)+
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 13, color = "black"))



p_trans_gam2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,1.2)+
  xlim(0.018,0.037)+
  ggplot2::geom_boxplot(ggplot2::aes(x = gam2, y = trans_abc2, group = gam2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam2, y = trans2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Transition state 2 to 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::ylab("Estimated transition") +
  ggplot2::xlab("Real colonization")+  #expression(lambda[1]^c)
  # ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)+
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 13, color = "black"))


tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_10reps/boxplots/trans_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_trans_gam1,p_trans_gam2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()


i = 4
data <- whole_df_ABC[((i*6000-5999)):(i*6000),]
p_laa1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,1.2)+
  xlim(0.02,0.23)+
  ggplot2::geom_boxplot(ggplot2::aes(x = laa, y = trans_abc1, group = laa),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa, y = trans),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Anagenesis state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::ylab("Estimated transition") +
  ggplot2::xlab("Real anagenesis")+  #expression(lambda[1]^c)
  # ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)+
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 13, color = "black"))



p_laa2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,1.2)+
  xlim(0.17,0.38)+
  ggplot2::geom_boxplot(ggplot2::aes(x = laa2, y = trans_abc2, group = laa2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa2, y = trans2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Anagenesis state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5, color = "black"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, color = "black")) +
  ggplot2::ylab("Estimated transition") +
  ggplot2::xlab("Real anagenesis")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)+
  ggplot2::theme(strip.text.x = ggplot2::element_text(size = 13, color = "black"))


tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_10reps/boxplots/trans_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_laa1,p_laa2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()




#########################
i = 1
data <- whole_df_ABC[((i*6000-5999)):(i*6000),]
p_lac1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.1,0.6)+
  ggplot2::geom_boxplot(ggplot2::aes(x = lac, y = lac_abc1, group = lac),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac, y = lac),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Cladogenesis state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)
p_lac1


p_lac2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.4,0.9)+
  ggplot2::geom_boxplot(ggplot2::aes(x = lac2, y = lac_abc2, group = lac2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac2, y = lac2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Cladogenesis state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)
p_lac2


p_trans1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(ggplot2::aes(x = lac, y = trans_abc1, group = lac),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac, y = trans),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Transition state 1 to 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)

p_trans1


p_trans2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ggplot2::geom_boxplot(ggplot2::aes(x = lac, y = trans_abc2, group = lac),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = lac, y = trans2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Transition state 1 to 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)
p_trans2

tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_lac1,p_lac2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()


i = 2
data <- whole_df_ABC[((i*6000-5999)):(i*6000),]
p_mu1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(-0.02,0.12)+
  ggplot2::geom_boxplot(ggplot2::aes(x = mu, y = mu_abc1, group = mu),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = mu, y = mu),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Extinction state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)



p_mu2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.08,0.22)+
  ggplot2::geom_boxplot(ggplot2::aes(x = mu2, y = mu_abc2, group = mu2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = mu2, y = mu2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Extinction state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)


tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_mu1,p_mu2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()


i = 3
data <- whole_df_ABC[((i*6000-5999)):(i*6000),]
p_gam1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,0.07)+
  xlim(0,0.022)+
  ggplot2::geom_boxplot(ggplot2::aes(x = gam, y = gam_abc1, group = gam),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam, y = gam),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Colonization state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)



p_gam2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,0.07)+
  xlim(0.018,0.04)+
  ggplot2::geom_boxplot(ggplot2::aes(x = gam2, y = gam_abc2, group = gam2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = gam2, y = gam2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Colonization state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)


tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_gam1,p_gam2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()



i = 4
data <- whole_df_ABC[((i*6000-5999)):(i*6000),]
p_laa1 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0,0.25)+
  ggplot2::geom_boxplot(ggplot2::aes(x = laa, y = laa_abc1, group = laa),
                        fill = "royalblue",colour = "blue3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa, y = laa),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Anagenesis state 1") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)



p_laa2 <- ggplot2::ggplot(data) +
  ggplot2::theme_bw() +
  ylim(0,2)+
  xlim(0.15,0.4)+
  ggplot2::geom_boxplot(ggplot2::aes(x = laa2, y = laa_abc2, group = laa2),
                        fill = "red",colour = "red3",,alpha = 0.5,outlier.shape = NA)+
  ggplot2::geom_point(aes(x = laa2, y = laa2),color = "black",size = 2) +
  ggplot2::theme_classic() +
  ggplot2::ggtitle("Anagenesis state 2") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5))+
  ggplot2::ylab("Estimated rate") +
  ggplot2::xlab("Real rate")+  #expression(lambda[1]^c)
  ggplot2::geom_abline(slope = 1, linetype = "dashed", size = 0.5)+
  ggplot2::facet_wrap(~Transition,labeller = ggplot2::label_parsed)


tiff(paste0("G:/results/project 2/tip_info/round3/TRAISIE_DD_with_q/boxplots/scenario_",i,".tiff"),
     units="px", width=3000, height=2000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  p_laa1,p_laa2,
  align = "hv", nrow = 2, ncol = 1
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()

