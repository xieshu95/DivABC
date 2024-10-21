# heatmap all paticles (drate vs epsilon)
library(ggplot2)
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space/DAISIE_ABC_DI"
files <- list.files(folder_path)
param_data <- readr::read_csv2("data/DAISIE_ABC_DI.csv")
for(n in c(0)){ # 1,2
  ss_diff <- c()
  n_iter <-c()
  n_iteration <- c()
  for(i in 1:160){
    file_to_load <- grep(paste0("DAISIE_ABC_DI_param_set_", i,"_ss_",n,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      n_iteration[i] <- num_iter
      if (nrow(output$ABC[[num_iter]]) == 400) {
        ss_diff <- rbind(ss_diff, output$ss_diff_list[[num_iter]])
      }  else {
        ss_diff <- rbind(ss_diff, output$ss_diff_list[[num_iter - 1]])
      }
    } else {
      ss_diff <- rbind(ss_diff, as.data.frame(matrix(NA, nrow = 400,ncol = 8)))
    }
  }
  colnames(ss_diff) <- c("NLTT","SELTT","NELTT","SDCT","SDCS",
                    "NTotal","NSE","NNE")
  rownames(ss_diff) <- 1:64000
}



load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space/delta_whole_df_ABC_ss_set",0,".RData"))
whole_df_ABC$dlac_abc <- whole_df_ABC$lac_abc - whole_df_ABC$lac
whole_df_ABC$dmu_abc <- whole_df_ABC$mu_abc - whole_df_ABC$mu
whole_df_ABC$dgam_abc <- whole_df_ABC$gam_abc - whole_df_ABC$gam
whole_df_ABC$dlaa_abc <- whole_df_ABC$laa_abc - whole_df_ABC$laa
whole_df_ABC$dnet_div_abc <- whole_df_ABC$net_div_ABC - whole_df_ABC$net_div

whole_df_ABC_ss<- data.frame(whole_df_ABC,ss_diff)
whole_df_ABC_ss$set <- rep(1:16,each = 4000)


library(heatmaply)
library(htmlwidgets)
ss<-whole_df_ABC_ss[,14:26]
ss = na.omit(ss)
p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss), xlab = "Summary statistics",
                                      ylab = "Summary statistics", k_col = 2, k_row = 2)
saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space/heatmap_drate_epsilon_ss0.html"))

# old code
ss<-whole_df_ABC_ss[,14:26]
ss = na.omit(ss)
cormat <- round(cor(ss),2)
# heatmap(cormat)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)
# ss_name <- c("NLTT","CTT","NELTT","SELTT","SDCS","SDCT","NME","NSE","NNE","Nclade")

label_names <- "Summary statistic"
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space/heatmap_drate_epsilon_old.tiff"),
     units="px", width=5000, height=3500,res = 300,compression="lzw")
heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue3", high = "red3",
                       limit = c(-1,1), name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), size = 5) +

  # ggplot2::scale_x_discrete(labels= ss_name)+
  # ggplot2::scale_y_discrete(labels= ss_name)+
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

#####

# seprate into 16 parameter sets(combine 10 reps)
for (i in 1:16){
  ss<-whole_df_ABC_ss[which(whole_df_ABC_ss$set == i),14:26]
  ss = na.omit(ss)
  cormat <- round(cor(ss),2)
  # heatmap(cormat)
  head(cormat)
  library(reshape2)
  melted_cormat <- melt(cormat)
  library(ggplot2)
  # ss_name <- c("NLTT","CTT","NELTT","SELTT","SDCS","SDCT","NME","NSE","NNE","Nclade")

  label_names <- "Summary statistic"
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space/heatmap_drate/heatmap_drate_epsilon_set",i,".tiff"),
       units="px", width=5000, height=3500,res = 300,compression="lzw")
  heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue3", high = "red3",
                         limit = c(-1,1), name="Correlation") +
    geom_text(aes(Var2, Var1, label = value), size = 5) +

    # ggplot2::scale_x_discrete(labels= ss_name)+
    # ggplot2::scale_y_discrete(labels= ss_name)+
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







iqr = function(z, lower = 0.1, upper = 0.9) {
  data.frame(
    y = median(z),
    ymin = quantile(z, lower),
    ymax = quantile(z, upper)
  )
}

lac_names <- c(
  `0.4` = 'lambda[c]~"="~0.4',
  `0.7` = 'lambda[c]~"="~0.7'
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
  `0.1` = 'lambda[a]~"="~0.1',
  `1` = 'lambda[a]~"="~1.0'
)
whole_df_ABC_ss$rep <- rep(rep(1:10, each = 400), 16)
p_1 <-ggplot2::ggplot(data = whole_df_ABC_ss,mapping = aes(x = dlac_abc,y = SELTT)) +
  # ggplot2::stat_summary(fun.data = iqr,alpha = 0.3) +
  ggplot2::geom_point()+
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  # ggplot2::ylim(-0.8,0.8)+
  ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
  ggplot2::scale_colour_manual(values = c("red4","#FADC8D","orange","#8CC269","#4393C3"))+
  ggplot2::theme(title = ggplot2::element_text(size = 12),
                 text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab(expression(log[10]~(Tree~size))) +
  ggplot2::ylab(expression(Delta~Net~diversification))+
  facet_grid(rep~ lac+mu+gam+laa,labeller = labeller(lac  = as_labeller(lac_names,  label_parsed),
                                                 mu = as_labeller(mu_names, label_parsed),
                                                 gam = as_labeller(gam_names, label_parsed),
                                                 laa = as_labeller(laa_names, label_parsed)))
