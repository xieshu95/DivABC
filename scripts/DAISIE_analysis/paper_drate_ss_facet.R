## ss_vs_drates all figures:
#####
# 1. total vs drates for each rate (relative difference)
p_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dlac,color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,1.6)+
  ggplot2::xlim(0,650)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~lambda^c))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)


p_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dmu, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,2)+
  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~mu))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)

p_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dgam, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.01,0.02)+
  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~gamma))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)

p_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = dlaa, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1.5,1.5)+
  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~lambda^a))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)


tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/facet_total_drate_median.tiff"),
     units="px", width=6500, height=3000,res = 350,compression="lzw")
params <- cowplot::plot_grid(
  p_lac+ggplot2::theme(legend.position = "none"),
  p_mu+ggplot2::theme(legend.position = "none"),
  p_gam+ggplot2::theme(legend.position = "none"),
  p_laa+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 2
)
legend <- cowplot::get_legend(
  p_lac + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_estimates <- cowplot::plot_grid(params,legend,
                                      rel_widths = c(3,0.4)
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()

# 2. total vs drates for each rate (absolute difference)
p_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = abs(dlac),color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(0,2)+
  ggplot2::xlim(0,650)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~lambda^c))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)


p_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = abs(dmu), color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(0,2)+
  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~mu))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)

p_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = abs(dgam), color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(0,0.02)+
  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~gamma))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)

p_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total,y = abs(dlaa), color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(0,1.6)+
  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Species richness") +
  ggplot2::ylab(expression(Delta~lambda^a))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)


tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/abs_facet_total_drate_median.tiff"),
     units="px", width=6500, height=3000,res = 350,compression="lzw")
params <- cowplot::plot_grid(
  p_lac+ggplot2::theme(legend.position = "none"),
  p_mu+ggplot2::theme(legend.position = "none"),
  p_gam+ggplot2::theme(legend.position = "none"),
  p_laa+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 2
)
legend <- cowplot::get_legend(
  p_lac + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_estimates <- cowplot::plot_grid(params,legend,
                                      rel_widths = c(3,0.4)
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()

#####
# 3. num_clade vs drates for each rate (relative difference)
p_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = dlac,color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,1.6)+
  #  ggplot2::xlim(0,650)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Number of clades") +
  ggplot2::ylab(expression(Delta~lambda^c))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)


p_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = dmu, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,2)+
  #  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Number of clades") +
  ggplot2::ylab(expression(Delta~mu))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)

p_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = dgam, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.01,0.02)+
  #  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Number of clades") +
  ggplot2::ylab(expression(Delta~gamma))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)

p_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = dlaa, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1.5,1.5)+
  #  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Number of clades") +
  ggplot2::ylab(expression(Delta~lambda^a))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)


tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/facet_num_clade_drate_median.tiff"),
     units="px", width=6500, height=3000,res = 350,compression="lzw")
params <- cowplot::plot_grid(
  p_lac+ggplot2::theme(legend.position = "none"),
  p_mu+ggplot2::theme(legend.position = "none"),
  p_gam+ggplot2::theme(legend.position = "none"),
  p_laa+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 2
)
legend <- cowplot::get_legend(
  p_lac + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_estimates <- cowplot::plot_grid(params,legend,
                                      rel_widths = c(3,0.4)
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()

#####
# 4. num_clade vs drates for each rate (relative difference)
p_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total/num_clade,y = dlac,color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,1.6)+
  #  ggplot2::xlim(0,650)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Average clade size") +
  ggplot2::ylab(expression(Delta~lambda^c))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)


p_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total/num_clade,y = dmu, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,2)+
  #  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Average clade size") +
  ggplot2::ylab(expression(Delta~mu))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)

p_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total/num_clade,y = dgam, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.01,0.02)+
  #  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Average clade size") +
  ggplot2::ylab(expression(Delta~gamma))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)

p_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = total/num_clade,y = dlaa, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1.5,1.5)+
  #  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Average clade size") +
  ggplot2::ylab(expression(Delta~lambda^a))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)


tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/facet_ave_drate_median.tiff"),
     units="px", width=6500, height=3000,res = 350,compression="lzw")
params <- cowplot::plot_grid(
  p_lac+ggplot2::theme(legend.position = "none"),
  p_mu+ggplot2::theme(legend.position = "none"),
  p_gam+ggplot2::theme(legend.position = "none"),
  p_laa+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 2
)
legend <- cowplot::get_legend(
  p_lac + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_estimates <- cowplot::plot_grid(params,legend,
                                      rel_widths = c(3,0.4)
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()


#####
# 3. num_clade vs drates for each rate (relative difference)
p_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = largest_clade,y = dlac,color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,1.6)+
  #  ggplot2::xlim(0,650)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Largest clade size") +
  ggplot2::ylab(expression(Delta~lambda^c))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)


p_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = largest_clade,y = dmu, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,2)+
  #  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Largest clade size") +
  ggplot2::ylab(expression(Delta~mu))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)

p_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = largest_clade,y = dgam, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.01,0.02)+
  #  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Largest clade size") +
  ggplot2::ylab(expression(Delta~gamma))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)

p_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = largest_clade,y = dlaa, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1.5,1.5)+
  #  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("Largest clade size") +
  ggplot2::ylab(expression(Delta~lambda^a))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(~ Method)


tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/facet_largest_drate_median.tiff"),
     units="px", width=6500, height=3000,res = 350,compression="lzw")
params <- cowplot::plot_grid(
  p_lac+ggplot2::theme(legend.position = "none"),
  p_mu+ggplot2::theme(legend.position = "none"),
  p_gam+ggplot2::theme(legend.position = "none"),
  p_laa+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 2
)
legend <- cowplot::get_legend(
  p_lac + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_estimates <- cowplot::plot_grid(params,legend,
                                      rel_widths = c(3,0.4)
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()




#
p_lac <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = dlac,color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,1.6)+
  #  ggplot2::xlim(0,650)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 14),
                 text = ggplot2::element_text(size = 14)) +
  ggplot2::xlab("Number of clades") +
  ggplot2::ylab(expression(Delta~lambda^c))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(gam~ Method,labeller = labeller(gam = as_labeller(gam_names, label_parsed)))


p_mu <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = dmu, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1,2)+
  #  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 14),
                 text = ggplot2::element_text(size = 14)) +
  ggplot2::xlab("Number of clades") +
  ggplot2::ylab(expression(Delta~mu))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(gam~ Method,labeller = labeller(gam = as_labeller(gam_names, label_parsed)))

p_gam <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = dgam, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-0.01,0.02)+
  #  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 14),
                 text = ggplot2::element_text(size = 14)) +
  ggplot2::xlab("Number of clades") +
  ggplot2::ylab(expression(Delta~gamma))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(gam~ Method,labeller = labeller(gam = as_labeller(gam_names, label_parsed)))

p_laa <-ggplot2::ggplot(data = whole_df_all,mapping = aes(x = num_clade,y = dlaa, color = Method)) +
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::ylim(-1.5,1.5)+
  #  ggplot2::xlim(0,650)+
  # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("red3","#FADC8D","orange","brown4","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 14),
                 text = ggplot2::element_text(size = 14)) +
  ggplot2::xlab("Number of clades") +
  ggplot2::ylab(expression(Delta~lambda^a))+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(gam~ Method,labeller = labeller(gam = as_labeller(gam_names, label_parsed)))


tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge/DI/facet_num_clade_drate_median_gam.tiff"),
     units="px", width=7000, height=3000,res = 350,compression="lzw")
params <- cowplot::plot_grid(
  p_lac+ggplot2::theme(legend.position = "none"),
  p_mu+ggplot2::theme(legend.position = "none"),
  p_gam+ggplot2::theme(legend.position = "none"),
  p_laa+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 2
)
legend <- cowplot::get_legend(
  p_lac + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_estimates <- cowplot::plot_grid(params,legend,
                                      rel_widths = c(3,0.4)
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()


