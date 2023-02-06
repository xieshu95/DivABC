## plot estimation for each generation (single replication)
library(ggplot2)
folder_path <- "G:/results/project 2/tip_info/round4/DAISIE_new_test/DAISIE_ABC_short"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC_short.csv")
for(n in c(20,21,22)){
  for(set in 1:27){
    message("set", set)
    true_rates <- param_data[set,]
    file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", set,"_ss_",n,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      ABC_df<-c()
      n_gene <- length(output$ABC)
      if(nrow(output$ABC[[n_gene]]) < 300){ #500
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
      ABC_df$generation <- as.factor(rep(1:n_gene, each = 300))

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

      tiff(paste0("G:/results/project 2/tip_info/round4/DAISIE_new_test/rate_each_gene/ss_",n,"_param_",set,".tiff"),
           units="px", width=2000, height=2000,res = 300,compression="lzw")
      dss <- cowplot::plot_grid(
        g1,g2,g3,g4,
        align = "hv", nrow = 2, ncol = 2
      )
      print(dss)
      while (!is.null(dev.list()))  dev.off()
    }
  }
}

## combine three replicates(ss 20,21,22)
library(ggplot2)
folder_path <- "G:/results/project 2/tip_info/round4/DAISIE_new_test/DAISIE_ABC_short"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC_short.csv")

for(set in 1:27){
  message("set", set)
  true_rates <- param_data[set,]
  file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", set,"_ss_20.RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)
  ABC_df1<-c()
  ABC_df2<-c()
  ABC_df3<-c()
  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    n_gene <- length(output$ABC)
    if(nrow(output$ABC[[n_gene]]) < 300){ #500
      n_gene <- n_gene - 1
    }
    for(i in 1:n_gene){
      ABC_df1 <- rbind(ABC_df1,output$ABC[[i]])
    }
    colnames(ABC_df1) <- c("lac","mu","gam","laa")
    rownames(ABC_df1) <- 1:nrow(ABC_df1)
    ABC_df1 <- as.data.frame(ABC_df1)
    ABC_df1$generation <- as.factor(rep(1:n_gene, each = 300))
  } else {
    ABC_df1 <-NA
  }

  file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", set,"_ss_21.RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    n_gene <- length(output$ABC)
    if(nrow(output$ABC[[n_gene]]) < 300){ #500
      n_gene <- n_gene - 1
    }
    for(i in 1:n_gene){
      ABC_df2 <- rbind(ABC_df2,output$ABC[[i]])
    }
    colnames(ABC_df2) <- c("lac","mu","gam","laa")
    rownames(ABC_df2) <- 1:nrow(ABC_df2)
    ABC_df2 <- as.data.frame(ABC_df2)
    ABC_df2$generation <- as.factor(rep(1:n_gene, each = 300))
  }

  file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", set,"_ss_22.RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    n_gene <- length(output$ABC)
    if(nrow(output$ABC[[n_gene]]) < 300){ #500
      n_gene <- n_gene - 1
    }
    for(i in 1:n_gene){
      ABC_df3 <- rbind(ABC_df3,output$ABC[[i]])
    }
    colnames(ABC_df3) <- c("lac","mu","gam","laa")
    rownames(ABC_df3) <- 1:nrow(ABC_df3)
    ABC_df3 <- as.data.frame(ABC_df3)
    ABC_df3$generation <- as.factor(rep(1:n_gene, each = 300))
  }
  ABC_df <- rbind(ABC_df1,ABC_df2,ABC_df3)
  ABC_df <- na.omit(ABC_df)
  if(length(ABC_df) > 0){
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

    tiff(paste0("G:/results/project 2/tip_info/round4/DAISIE_new_test/rate_each_gene/comb_param_",set,".tiff"),
         units="px", width=2000, height=2000,res = 300,compression="lzw")
    dss <- cowplot::plot_grid(
      g1,g2,g3,g4,
      align = "hv", nrow = 2, ncol = 2
    )
    print(dss)
    while (!is.null(dev.list()))  dev.off()
  }
}
