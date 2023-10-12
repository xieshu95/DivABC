# compare ABC methods in the same generation
library(ggplot2)
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/DAISIE_ABC_short_DI"
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_ABC_short_DI.csv")
for(n in c(3)){
  ABC_df<-c()
  generation <-c()
  set_val <- c()
  Rep <- c()
  for(set in 1:160){
    message("set", set)
    true_rates <- param_data[set,]
    file_to_load <- grep(paste0("DAISIE_ABC_short_DI_param_set_", set,"_ss_",n,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)
    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))

      n_gene <- length(output$ABC)
      if(nrow(output$ABC[[n_gene]]) < 500){ #500
        n_gene <- n_gene - 1
      }
      for(i in 1:n_gene){
        ABC_df <- rbind(ABC_df,output$ABC[[i]])
      }

      # colnames(ss_dist) <- c("MPD","MNTD","SDPD","SDNTD",
      #                        "D","Total","Ratio","NLTT")
      ABC_df <- as.data.frame(ABC_df)
      generation <- c(generation, rep(1:n_gene, each = 500))
      set_val <- c(set_val, rep(set,n_gene*500))

    } else {
      ABC_df <-rbind(ABC_df,rep(NA,4))
      generation <- c(generation, 1)
      set_val <- c(set_val, set)
    }
  }
  colnames(ABC_df) <- c("lac_abc","mu_abc","gam_abc","laa_abc")#"lac","mu","gam","laa","K",
  rownames(ABC_df) <- 1:nrow(ABC_df)

  rep <- set_val %% 10
  for(i in 1:length(rep)){
    if (rep[i] == 0){
      rep[i] = 10
    }
  }


  ABC_df_all <- data.frame(param_data[set_val,],ABC_df,generation,set_val,rep)

  ABC_df_all$net_div <- (ABC_df_all$lac-ABC_df_all$mu)
  ABC_df_all$net_div_ABC <- (ABC_df_all$lac_abc-ABC_df_all$mu_abc)
  ABC_df_all$ext_frac <- (ABC_df_all$mu)/(ABC_df_all$lac)
  ABC_df_all$ext_frac_ABC <- (ABC_df_all$mu_abc)/(ABC_df_all$lac_abc)
  save(ABC_df_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/rates_all_generations",n,".RData"))
}


load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/rates_all_generations",0,".RData"))
ABC_df_all$ss = "ABC All"
ABC_df_all_0 = ABC_df_all

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/rates_all_generations",1,".RData"))
ABC_df_all$ss = "ABC Phylogenetic"
ABC_df_all_1 = ABC_df_all



load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/rates_all_generations",2,".RData"))
ABC_df_all$ss = "ABC Diversity"
ABC_df_all_2 = ABC_df_all



load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/rates_all_generations",3,".RData"))
ABC_df_all$ss = "ABC NLTT"
ABC_df_all_3 = ABC_df_all


ABC_df_all <- rbind(ABC_df_all_0,
                    ABC_df_all_1,
                    ABC_df_all_2,
                    ABC_df_all_3) #whole_df_ABC_20

ABC_df_all$dlac <- ABC_df_all$lac_abc - ABC_df_all$lac
ABC_df_all$dmu <- ABC_df_all$mu_abc - ABC_df_all$mu
ABC_df_all$dgam <- ABC_df_all$gam_abc - ABC_df_all$gam
ABC_df_all$dlaa <- ABC_df_all$laa_abc - ABC_df_all$laa
ABC_df_all$dnet_div <- ABC_df_all$net_div_ABC - ABC_df_all$net_div
ABC_df_all$dext_frac <- ABC_df_all$ext_frac_ABC - ABC_df_all$ext_frac
ABC_df_all$generation <- ABC_df_all$generation
# ABC_df_all$total <- rep(rep(pars_ss$total, each = 400), 1) # 500,5
save(ABC_df_all, file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/all_generations_all_ss.RData"))

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/all_generations_all_ss.RData"))


lac_names <- c(
  `0.4` = 'lambda^c~"="~0.4',
  `0.7` = 'lambda^c~"="~0.7'
)

mu_names <- c(
  `0` = 'mu~"="~0',
  `0.3` = 'mu~"="~0.3'
)

gam_names <- c(
  `0.003` = 'gamma~"="~0.003',
  `0.009` = 'gamma~"="~0.009'
)

laa_names <- c(
  `0.1` = 'lambda^a~"="~0.1',
  `1` = 'lambda^a~"="~1.0'
)

library(ggplot2)
## 1.  for each generation for each replicate
ABC_df_all_4gene<- ABC_df_all[(ABC_df_all$generation < 9),]
p_netdiv_all <-ggplot2::ggplot(data = ABC_df_all_4gene, aes(x = as.factor(generation), y = dnet_div, color = ss)) +
  # ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::geom_boxplot(outlier.shape=NA)+ #outlier.shape=NA
  ggplot2::ylim(-2,2)+
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(ABC_df_all_4gene$ss))))+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+  #"#FADC8D","orange",
  ggplot2::theme(title = ggplot2::element_text(size = 13),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank())+
  ggplot2::xlab(expression(Iteration))+
  ggplot2::ylab(expression(Delta~Net~diversification)) +
  ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed", size = 0.5)+ #net_div
  facet_grid(rep~laa+gam+mu+lac ,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/all_ss_drate_each_rep_netdiv.tiff"),
     units="px", width=6000, height=3000,res = 350,compression="lzw")
print(p_netdiv_all)
while (!is.null(dev.list()))  dev.off()

p_lac <-ggplot2::ggplot(data = ABC_df_all_4gene, aes(x = as.factor(generation), y = dlac, color = ss)) +
  # ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::geom_boxplot(outlier.shape=NA)+ #outlier.shape=NA
  ggplot2::ylim(-1,2)+
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(ABC_df_all_4gene$ss))))+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 13),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank())+
  ggplot2::xlab(expression(Iteration))+
  ggplot2::ylab(expression(Delta~lambda^c)) +
  ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(rep~laa+gam+mu+lac ,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/all_ss_drate_each_rep_lac.tiff"),
     units="px", width=6000, height=3000,res = 350,compression="lzw")
print(p_lac)
while (!is.null(dev.list()))  dev.off()

p_mu <-ggplot2::ggplot(data = ABC_df_all_4gene, aes(x = as.factor(generation), y = dmu, color = ss)) +
  # ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::geom_boxplot(outlier.shape=NA)+ #outlier.shape=NA
  ggplot2::ylim(-0.5,2)+
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(ABC_df_all_4gene$ss))))+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 13),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank())+
  ggplot2::xlab(expression(Iteration))+
  ggplot2::ylab(expression(Delta~mu)) +
  ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(rep~laa+gam+mu+lac ,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/all_ss_drate_each_rep_mu.tiff"),
     units="px", width=6000, height=3000,res = 350,compression="lzw")
print(p_mu)
while (!is.null(dev.list()))  dev.off()

p_gam <-ggplot2::ggplot(data = ABC_df_all_4gene, aes(x = as.factor(generation), y = dgam, color = ss)) +
  # ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::geom_boxplot(outlier.shape=NA)+ #outlier.shape=NA
  ggplot2::ylim(-0.012,0.022)+
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(ABC_df_all_4gene$ss))))+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 13),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank())+
  ggplot2::xlab(expression(Iteration))+
  ggplot2::ylab(expression(Delta~gamma)) +
  ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(rep~laa+gam+mu+lac ,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/all_ss_drate_each_rep_gam.tiff"),
     units="px", width=6000, height=3000,res = 350,compression="lzw")
print(p_gam)
while (!is.null(dev.list()))  dev.off()

p_laa <-ggplot2::ggplot(data = ABC_df_all_4gene, aes(x = as.factor(generation), y = dlaa, color = ss)) +
  # ggplot2::stat_summary(fun.data = iqr,alpha = 0.6) +
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::geom_boxplot(outlier.shape=NA)+ #outlier.shape=NA
  ggplot2::ylim(-1.2,2)+
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(ABC_df_all_4gene$ss))))+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 13),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank())+
  ggplot2::xlab(expression(Iteration))+
  ggplot2::ylab(expression(Delta~lambda^a)) +
  ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed", size = 0.5)+
  facet_grid(rep~laa+gam+mu+lac ,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                     mu = as_labeller(mu_names, label_parsed),
                                                     gam = as_labeller(gam_names, label_parsed),
                                                     laa = as_labeller(laa_names, label_parsed)))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/all_ss_drate_each_rep_laa.tiff"),
     units="px", width=6000, height=3000,res = 350,compression="lzw")
print(p_laa)
while (!is.null(dev.list()))  dev.off()


# 2. for each generation combining all the replicates within the same generation
iqr = function(z, lower = 0.025, upper = 0.975) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
ABC_df_all_4gene<- ABC_df_all[which(ABC_df_all$generation <13),]
ABC_df_all_4gene$ss <- factor(ABC_df_all_4gene$ss, levels = c("ABC All", "ABC Diversity", "ABC NLTT", "ABC Phylogenetic"))
p_netdiv_all <-ggplot2::ggplot(data = ABC_df_all_4gene,mapping = aes(x = dnet_div,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-2,2)+
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(ABC_df_all_4gene$ss))))+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 15),
                 strip.text = element_text(size = 12,colour = "black"),
                 legend.text = element_text(size = 12),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank()) +
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(ABC_df_all_4gene$ss))))+
  ggplot2::xlab(expression(Delta~Net~diversification))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(generation~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
p <- p_netdiv_all +
  ggplot2::theme(legend.position = "none")
p <-grid.arrange(p,top=NULL, right='Iteration')
legend <- cowplot::get_legend(
  p_netdiv_all + theme(legend.box.margin = margin(0, 0, 0, 6))
)
p_netdiv_all <- cowplot::plot_grid(p,legend,rel_widths = c(3,0.4))


tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/all_ss_drate_each_gene_netdiv_gene_all2.tiff"),
     units="px", width=6000, height=2800,res = 350,compression="lzw")
print(p_netdiv_all)
while (!is.null(dev.list()))  dev.off()



p_lac <-ggplot2::ggplot(data = ABC_df_all_4gene,mapping = aes(x = dlac,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1.2,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 15),
                 strip.text = element_text(size = 12,colour = "black"),
                 legend.text = element_text(size = 12),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank()) +
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(ABC_df_all_4gene$ss))))+
  ggplot2::xlab(expression(Delta~lambda^c))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(generation~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                            mu = as_labeller(mu_names, label_parsed),
                                                            gam = as_labeller(gam_names, label_parsed),
                                                            laa = as_labeller(laa_names, label_parsed)))

p <- p_lac +
  ggplot2::theme(legend.position = "none")
p <-grid.arrange(p,top=NULL, right='Iteration')
legend <- cowplot::get_legend(
  p_lac + theme(legend.box.margin = margin(0, 0, 0, 6))
)
p_lac <- cowplot::plot_grid(p,legend,rel_widths = c(3,0.4))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/all_ss_drate_each_gene_lac.tiff"),
     units="px", width=6000, height=2800,res = 350,compression="lzw")
print(p_lac)
while (!is.null(dev.list()))  dev.off()

p_mu<-ggplot2::ggplot(data = ABC_df_all_4gene,mapping = aes(x = dmu,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 15),
                 strip.text = element_text(size = 12,colour = "black"),
                 legend.text = element_text(size = 12),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank()) +
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(ABC_df_all_4gene$ss))))+
  ggplot2::xlab(expression(Delta~mu))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(generation~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))

p <- p_mu +
  ggplot2::theme(legend.position = "none")
p <-grid.arrange(p,top=NULL, right='Iteration')
legend <- cowplot::get_legend(
  p_mu + theme(legend.box.margin = margin(0, 0, 0, 6))
)
p_mu <- cowplot::plot_grid(p,legend,rel_widths = c(3,0.4))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/all_ss_drate_each_gene_mu.tiff"),
     units="px", width=6000, height=2800,res = 350,compression="lzw")
print(p_mu)
while (!is.null(dev.list()))  dev.off()

p_gam<-ggplot2::ggplot(data = ABC_df_all_4gene,mapping = aes(x = dgam,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-0.009,0.018)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 15),
                 strip.text = element_text(size = 12,colour = "black"),
                 legend.text = element_text(size = 12),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank()) +
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(ABC_df_all_4gene$ss))))+
  ggplot2::xlab(expression(Delta~gamma))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(generation~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
p <- p_gam +
  ggplot2::theme(legend.position = "none")
p <-grid.arrange(p,top=NULL, right='Iteration')
legend <- cowplot::get_legend(
  p_gam + theme(legend.box.margin = margin(0, 0, 0, 6))
)
p_gam <- cowplot::plot_grid(p,legend,rel_widths = c(3,0.4))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/all_ss_drate_each_gene_gam.tiff"),
     units="px", width=6000, height=2800,res = 350,compression="lzw")
print(p_gam)
while (!is.null(dev.list()))  dev.off()

p_laa<-ggplot2::ggplot(data = ABC_df_all_4gene,mapping = aes(x = dlaa,y = ss,color = ss)) +
  ggplot2::stat_summary(fun.data = iqr,alpha = 1) +
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02,fill = "lightgrey") +
  # ggplot2::geom_boxplot(outlier.shape = NA)+ #outlier.shape = NA
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::xlim(-1,2)+
  # ggplot2::stat_smooth(method = "lm", se = T,alpha = 0.1)+
  ggplot2::scale_colour_manual("Method",values = c("brown4","orange","red2","#FADC8D","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 15),
                 strip.text = element_text(size = 12,colour = "black"),
                 legend.text = element_text(size = 12),
                 axis.text.x = ggplot2::element_text(size = 12,colour = "black"),
                 axis.text.y = ggplot2::element_blank()) +
  ggplot2::scale_y_discrete(limits = rev(levels(as.factor(ABC_df_all_4gene$ss))))+
  ggplot2::xlab(expression(Delta~lambda^a))+
  ggplot2::ylab("Method") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)+
  facet_grid(generation~ laa+gam+mu+lac,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
p <- p_laa +
  ggplot2::theme(legend.position = "none")
p <-grid.arrange(p,top=NULL, right='Iteration')
legend <- cowplot::get_legend(
  p_laa + theme(legend.box.margin = margin(0, 0, 0, 6))
)
p_laa <- cowplot::plot_grid(p,legend,rel_widths = c(3,0.4))
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_endemic/DI/paper/all_ss_drate_each_gene_laa.tiff"),
     units="px", width=6000, height=2800,res = 350,compression="lzw")
print(p_laa)
while (!is.null(dev.list()))  dev.off()

