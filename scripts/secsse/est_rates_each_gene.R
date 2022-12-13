## plot estimation for each generation
library(ggplot2)
folder_path <- "G:/results/project 2/tip_info/round4/adap_secsse_new_space/secsse_ABC"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC.csv")
for(set in 1:27){
  message("set", set)
  true_rates <- param_data[set,]
  file_to_load <- grep(paste0("secsse_ABC_param_set_", set,"_ss_0.RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    ABC_df<-c()
    n_gene <- length(output$ABC)
    if(nrow(output$ABC[[n_gene]]) < 500){
      n_gene <- n_gene - 1
    }
    for(i in 1:n_gene){
      ABC_df <- rbind(ABC_df,output$ABC[[i]])
    }

    # colnames(ss_dist) <- c("MPD","MNTD","SDPD","SDNTD",
    #                        "D","Total","Ratio","NLTT")
    colnames(ABC_df) <- c("lam1","lam2","mu1","mu2","q12","q21")
    rownames(ABC_df) <- 1:nrow(ABC_df)
    ABC_df <- as.data.frame(ABC_df)
    ABC_df$generation <- as.factor(rep(1:n_gene, each = 500))

    g1 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = lam1)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = lam1), linetype = "dashed", size = 0.5)
    # print(g1)
    g2 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = lam2)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = lam2), linetype = "dashed", size = 0.5)

    g3 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = mu1)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = mu1), linetype = "dashed", size = 0.5)


    g4 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = mu2)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = mu2), linetype = "dashed", size = 0.5)

    g5 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = q12)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = q12), linetype = "dashed", size = 0.5)

    g6 <- ggplot2::ggplot(ABC_df, aes(x = generation,y = q21)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = q21), linetype = "dashed", size = 0.5)


    tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/rate_each_gene/ss_0_param_",set,".tiff"),
         units="px", width=3000, height=2000,res = 300,compression="lzw")
    dss <- cowplot::plot_grid(
      g1,g3,g5,g2,g4,g6,
      align = "hv", nrow = 2, ncol = 3
    )
    print(dss)
    while (!is.null(dev.list()))  dev.off()
  }
}
