load("G:/results/project 2/tip_info/round3/test_epsilon/whole_df_with_ss_dss.RData")
whole_df <- data.frame(matrix(0, ncol = 32, nrow = 160))
colnames(whole_df) <- colnames(whole_df_with_ss)
for(i in 1:160) {
  param_data <- whole_df_with_ss[((i*100-99)):(i*100),]
  whole_df[i,] <- apply(param_data,2,median)
}
whole_df_median <- whole_df
save(whole_df_median,file = "G:/results/project 2/tip_info/round3/test_epsilon/whole_df_median_perrep.RData")



### plots dss_vs_dss
library(ggplot2)
load("G:/results/project 2/tip_info/round3/test_epsilon/whole_df_median_perrep.RData")
i = 1
param_abc <- whole_df_median[((i*160-159)):(i*160),]

ss1_vs_ss2 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds1,y = ds2),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "CTT")) +
  ggplot2::ylab(expression(Delta * "MESTT"))

ss1_vs_ss3 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds1,y = ds3),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "CTT")) +
  ggplot2::ylab(expression(Delta * "MESTT"))

ss1_vs_ss4 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds1,y = ds4),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "CTT")) +
  ggplot2::ylab(expression(Delta * "NESTT"))

# ss1_vs_ss5 <- ggplot2::ggplot(data = param_abc) +
#   ggplot2::theme_bw() +
#   ggplot2::geom_point(mapping = ggplot2::aes(x = ds1,y = ds5),
#                       colour = "royalblue",shape = 16,alpha = 0.8) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab(expression(Delta * "CTT")) +
#   ggplot2::ylab(expression("N Col"))

ss1_vs_ss6 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds1,y = ds6),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "CTT")) +
  ggplot2::ylab(expression("SD-CS"))

ss1_vs_ss7 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds1,y = ds7),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "CTT")) +
  ggplot2::ylab(expression("SD-CT"))





ss2_vs_ss3 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds2,y = ds3),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "SESTT")) +
  ggplot2::ylab(expression(Delta * "MESTT"))

ss2_vs_ss4 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds2,y = ds4),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "SESTT")) +
  ggplot2::ylab(expression(Delta * "NESTT"))

# ss2_vs_ss5 <- ggplot2::ggplot(data = param_abc) +
#   ggplot2::theme_bw() +
#   ggplot2::geom_point(mapping = ggplot2::aes(x = ds2,y = ds5),
#                       colour = "royalblue",shape = 16,alpha = 0.8) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab(expression(Delta * "SESTT")) +
#   ggplot2::ylab(expression("N Col"))

ss2_vs_ss6 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds2,y = ds6),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "SESTT")) +
  ggplot2::ylab(expression("SD-CS"))

ss2_vs_ss7 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds2,y = ds7),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "SESTT")) +
  ggplot2::ylab(expression("SD-CT"))



ss3_vs_ss4 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds3,y = ds4),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "MESTT")) +
  ggplot2::ylab(expression(Delta * "NESTT"))

# ss3_vs_ss5 <- ggplot2::ggplot(data = param_abc) +
#   ggplot2::theme_bw() +
#   ggplot2::geom_point(mapping = ggplot2::aes(x = ds3,y = ds5),
#                       colour = "royalblue",shape = 16,alpha = 0.8) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab(expression(Delta * "MESTT")) +
#   ggplot2::ylab(expression("N Col"))

ss3_vs_ss6 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds3,y = ds6),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "MESTT")) +
  ggplot2::ylab(expression("SD-CS"))

ss3_vs_ss7 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds3,y = ds7),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "MESTT")) +
  ggplot2::ylab(expression("SD-CT"))




# ss4_vs_ss5 <- ggplot2::ggplot(data = param_abc) +
#   ggplot2::theme_bw() +
#   ggplot2::geom_point(mapping = ggplot2::aes(x = ds4,y = ds5),
#                       colour = "royalblue",shape = 16,alpha = 0.8) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab(expression(Delta * "NESTT")) +
#   ggplot2::ylab(expression("N Col"))

ss4_vs_ss6 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds4,y = ds6),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "NESTT")) +
  ggplot2::ylab(expression("SD-CS"))

ss4_vs_ss7 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds4,y = ds7),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "NESTT")) +
  ggplot2::ylab(expression("SD-CT"))



# ss5_vs_ss6 <- ggplot2::ggplot(data = param_abc) +
#   ggplot2::theme_bw() +
#   ggplot2::geom_point(mapping = ggplot2::aes(x = ds5,y = ds6),
#                       colour = "royalblue",shape = 16,alpha = 0.8) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab(expression("N Col")) +
#   ggplot2::ylab(expression("SD-CS"))
#
# ss5_vs_ss7 <- ggplot2::ggplot(data = param_abc) +
#   ggplot2::theme_bw() +
#   ggplot2::geom_point(mapping = ggplot2::aes(x = ds5,y = ds7),
#                       colour = "royalblue",shape = 16,alpha = 0.8) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(title = ggplot2::element_text(size = 12),
#                  text = ggplot2::element_text(size = 12)) +
#   ggplot2::xlab(expression("N Col")) +
#   ggplot2::ylab(expression("SD-CT"))


ss6_vs_ss7 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ds6,y = ds7),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression("SD-CS")) +
  ggplot2::ylab(expression("SD-CT"))

p_emp <- ggplot() + theme_void()

tiff(paste0("G:/results/project 2/tip_info/round3/test_epsilon/median/dss_vs_dss.tiff"),
     units="px", width=6000, height=4000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  # ss1_vs_ss2,p_emp,p_emp,p_emp,p_emp,p_emp,
  # ss1_vs_ss3,ss2_vs_ss3,p_emp,p_emp,p_emp,p_emp,
  # ss1_vs_ss4,ss2_vs_ss4,ss3_vs_ss4,p_emp,p_emp,p_emp,
  # ss1_vs_ss5,ss2_vs_ss5,ss3_vs_ss5,ss4_vs_ss5,p_emp,p_emp,
  # ss1_vs_ss6,ss2_vs_ss6,ss3_vs_ss6,ss4_vs_ss6,ss5_vs_ss6,p_emp,
  # ss1_vs_ss7,ss2_vs_ss7,ss3_vs_ss7,ss4_vs_ss7,ss5_vs_ss7,ss6_vs_ss7,

  ss1_vs_ss2,p_emp,p_emp,p_emp,p_emp,
  ss1_vs_ss3,ss2_vs_ss3,p_emp,p_emp,p_emp,
  ss1_vs_ss4,ss2_vs_ss4,ss3_vs_ss4,p_emp,p_emp,
  ss1_vs_ss6,ss2_vs_ss6,ss3_vs_ss6,ss4_vs_ss6,p_emp,
  ss1_vs_ss7,ss2_vs_ss7,ss3_vs_ss7,ss4_vs_ss7,ss6_vs_ss7,
  align = "hv", nrow = 5, ncol = 5
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()



############################################################################

#### plots dss_vs_dss
library(ggplot2)
load("G:/results/project 2/tip_info/round3/test_epsilon/whole_df_median_perrep.RData")
i = 1
param_abc <- whole_df_median[((i*160-159)):(i*160),]

ss1_vs_ss2 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s1,y = s2),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "CTT")) +
  ggplot2::ylab(expression(Delta * "MESTT"))

ss1_vs_ss3 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s1,y = s3),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "CTT")) +
  ggplot2::ylab(expression(Delta * "MESTT"))

ss1_vs_ss4 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s1,y = s4),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "CTT")) +
  ggplot2::ylab(expression(Delta * "NESTT"))

ss1_vs_ss5 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s1,y = s5),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "CTT")) +
  ggplot2::ylab(expression("N Col"))

ss1_vs_ss6 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s1,y = s6),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "CTT")) +
  ggplot2::ylab(expression("SD-CS"))

ss1_vs_ss7 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s1,y = s7),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "CTT")) +
  ggplot2::ylab(expression("SD-CT"))





ss2_vs_ss3 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s2,y = s3),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "SESTT")) +
  ggplot2::ylab(expression(Delta * "MESTT"))

ss2_vs_ss4 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s2,y = s4),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "SESTT")) +
  ggplot2::ylab(expression(Delta * "NESTT"))

ss2_vs_ss5 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s2,y = s5),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "SESTT")) +
  ggplot2::ylab(expression("N Col"))

ss2_vs_ss6 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s2,y = s6),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "SESTT")) +
  ggplot2::ylab(expression("SD-CS"))

ss2_vs_ss7 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s2,y = s7),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "SESTT")) +
  ggplot2::ylab(expression("SD-CT"))



ss3_vs_ss4 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s3,y = s4),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "MESTT")) +
  ggplot2::ylab(expression(Delta * "NESTT"))

ss3_vs_ss5 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s3,y = s5),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "MESTT")) +
  ggplot2::ylab(expression("N Col"))

ss3_vs_ss6 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s3,y = s6),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "MESTT")) +
  ggplot2::ylab(expression("SD-CS"))

ss3_vs_ss7 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s3,y = s7),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "MESTT")) +
  ggplot2::ylab(expression("SD-CT"))




ss4_vs_ss5 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s4,y = s5),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "NESTT")) +
  ggplot2::ylab(expression("N Col"))

ss4_vs_ss6 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s4,y = s6),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "NESTT")) +
  ggplot2::ylab(expression("SD-CS"))

ss4_vs_ss7 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s4,y = s7),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(Delta * "NESTT")) +
  ggplot2::ylab(expression("SD-CT"))



ss5_vs_ss6 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s5,y = s6),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression("N Col")) +
  ggplot2::ylab(expression("SD-CS"))

ss5_vs_ss7 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s5,y = s7),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression("N Col")) +
  ggplot2::ylab(expression("SD-CT"))


ss6_vs_ss7 <- ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(mapping = ggplot2::aes(x = s6,y = s7),
                      colour = "royalblue",shape = 16,alpha = 0.8) +
  ggplot2::theme_classic() +
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression("SD-CS")) +
  ggplot2::ylab(expression("SD-CT"))

p_emp <- ggplot() + theme_void()

tiff(paste0("G:/results/project 2/tip_info/round3/test_epsilon/median/ss_vs_ss.tiff"),
     units="px", width=6000, height=4000,res = 300,compression="lzw")
param_estimates <- cowplot::plot_grid(
  ss1_vs_ss2,p_emp,p_emp,p_emp,p_emp,p_emp,
  ss1_vs_ss3,ss2_vs_ss3,p_emp,p_emp,p_emp,p_emp,
  ss1_vs_ss4,ss2_vs_ss4,ss3_vs_ss4,p_emp,p_emp,p_emp,
  ss1_vs_ss5,ss2_vs_ss5,ss3_vs_ss5,ss4_vs_ss5,p_emp,p_emp,
  ss1_vs_ss6,ss2_vs_ss6,ss3_vs_ss6,ss4_vs_ss6,ss5_vs_ss6,p_emp,
  ss1_vs_ss7,ss2_vs_ss7,ss3_vs_ss7,ss4_vs_ss7,ss5_vs_ss7,ss6_vs_ss7,
  align = "hv", nrow = 6, ncol = 6
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()



