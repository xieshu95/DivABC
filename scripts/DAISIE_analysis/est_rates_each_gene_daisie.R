## plot estimation for each generation
library(ggplot2)
folder_path <- "G:/results/project 2/tip_info/round4/adap_daisie_pw/DAISIE_ABC_short"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC_short.csv")
for(set in 1:27){
  message("set", set)
  true_rates <- param_data[set,]
  file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", set,"_ss_20.RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    ABC_df<-c()
    n_gene <- length(output$ABC)
    if(nrow(output$ABC[[n_gene]]) < 200){ #500
      n_gene <- n_gene - 1
    }
    for(i in 1:n_gene){
      ABC_df <- rbind(ABC_df,output$ABC[[i]])
    }

    # colnames(ss_dist) <- c("MPD","MNTD","SDPD","SDNTD",
    #                        "D","Total","Ratio","NLTT")
    colnames(ABC_df) <- c("lac","mu","gam","laa")
    rownames(ABC_df) <- 1:nrow(ABC_df)
    ABC_df <- as.data.frame(ABC_df)
    ABC_df$generation <- as.factor(rep(1:n_gene, each = 200))

    g1 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = lac)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = lac), linetype = "dashed", size = 0.5)
    # print(g1)
    g2 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = mu)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = mu), linetype = "dashed", size = 0.5)

    g3 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = gam)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = gam), linetype = "dashed", size = 0.5)


    g4 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = laa)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = laa), linetype = "dashed", size = 0.5)

    tiff(paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw/rate_each_gene/ss_20_param_",set,".tiff"),
         units="px", width=2000, height=2000,res = 300,compression="lzw")
    dss <- cowplot::plot_grid(
      g1,g2,g3,g4,
      align = "hv", nrow = 2, ncol = 2
    )
    print(dss)
    while (!is.null(dev.list()))  dev.off()
  }
}
