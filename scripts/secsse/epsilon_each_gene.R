## plot the d(ss) for each generation
library(ggplot2)
folder_path <- "G:/results/project 2/tip_info/round4/adap_secsse/secsse_ABC_long"
files <- list.files(folder_path)
for(set in 1:70){
  message("set", set)
  file_to_load <- grep(paste0("secsse_ABC_long_param_set_", set,"_ss_0.RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))

    ss_dist<-c()
    n_gene <- length(output$ss_diff_list)
    if(length(output$ss_diff_list[[n_gene]]) < 500){
      n_gene <- n_gene - 1
    }
    for(i in 1:n_gene){
      ss_dist <- rbind(ss_dist,output$ss_diff_list[[i]])
    }

    colnames(ss_dist) <- c("MPD","MNTD","SDPD","SDNTD",
                           "D","Total","Ratio","NLTT")
    rownames(ss_dist) <- 1:nrow(ss_dist)
    ss_dist <- as.data.frame(ss_dist)
    ss_dist$generation <- rep(1:n_gene, each = 500)

    g1 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = MPD)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g1)

    g2 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = MNTD)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g2)

    g3 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = SDPD)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g3)

    g4 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = SDNTD)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g4)

    g5 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = D)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g5)

    g6 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = Total)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g6)

    g7 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = Ratio)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g7)

    g8 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = NLTT)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g8)
    tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse/dss/ss_0_param_",set,".tiff"),
         units="px", width=4000, height=2000,res = 300,compression="lzw")
    dss <- cowplot::plot_grid(
      g1,g2,g3,g4,g5,g6,g7,g8,
      align = "hv", nrow = 2, ncol = 4
    )
    print(dss)
    while (!is.null(dev.list()))  dev.off()
  }
}


### violin
for(set in 1:10){
  message("set", set)
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse/secsse_ABC_long/secsse_ABC_long_param_set_",set,"_ss_0.RData"))
  ss_dist<-c()
  n_gene <- length(output$ss_diff_list)
  if(length(output$ss_diff_list[[n_gene]]) < 500){
    n_gene <- n_gene - 1
  }
  for(i in 1:n_gene){
    ss_dist <- rbind(ss_dist,output$ss_diff_list[[i]])
  }

  colnames(ss_dist) <- c("MPD","MNTD","SDPD","SDNTD",
                         "D","Total","Ratio","NLTT")
  rownames(ss_dist) <- 1:nrow(ss_dist)
  ss_dist <- as.data.frame(ss_dist)
  ss_dist$generation <- rep(1:n_gene, each = 500)

  g1 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = MPD)) +
    ggplot2::theme_bw() +
    ggplot2::geom_violin()

  g2 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = MNTD)) +
    ggplot2::theme_bw() +
    ggplot2::geom_violin()

  g3 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = SDPD)) +
    ggplot2::theme_bw() +
    ggplot2::geom_violin()

  g4 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = SDNTD)) +
    ggplot2::theme_bw() +
    ggplot2::geom_violin()

  g5 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = D)) +
    ggplot2::theme_bw() +
    ggplot2::geom_violin()

  g6 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = Total)) +
    ggplot2::theme_bw() +
    ggplot2::geom_violin()

  g7 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = Ratio)) +
    ggplot2::theme_bw() +
    ggplot2::geom_violin()

  g8 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = NLTT)) +
    ggplot2::theme_bw() +
    ggplot2::geom_violin()


  tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse/dss/violin_ss_0_param_",set,".tiff"),
       units="px", width=4000, height=2000,res = 300,compression="lzw")
  dss <- cowplot::plot_grid(
    g1,g2,g3,g4,g5,g6,g7,g8,
    align = "hv", nrow = 2, ncol = 4
  )
  print(dss)
  while (!is.null(dev.list()))  dev.off()

}

