## plot the d(ss) for each generation
library(ggplot2)
folder_path <- "G:/results/project 2/tip_info/round4/adap_secsse_new_space/secsse_ABC"
files <- list.files(folder_path)
for(set in 1:27){
  message("set", set)
  file_to_load <- grep(paste0("secsse_ABC_param_set_", set,"_ss_30.RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))

    ss_dist<-c()
    n_gene <- length(output$ss_diff_list)
    if(nrow(output$ss_diff_list[[n_gene]]) < 500){
      n_gene <- n_gene - 1
    }
    for(i in 1:n_gene){
      ss_dist <- rbind(ss_dist,output$ss_diff_list[[i]])
    }

    # colnames(ss_dist) <- c("MPD","MNTD","SDPD","SDNTD",
    #                        "D","Total","Ratio","NLTT")
    colnames(ss_dist) <- c("MPD_12","MPD_S1","MPD_S2",
                           "MNTD_12","MNTD_S1","MNTD_S2",
                           "D","Num_S1","Num_S2","NLTT")
    rownames(ss_dist) <- 1:nrow(ss_dist)
    ss_dist <- as.data.frame(ss_dist)
    ss_dist$generation <- rep(1:n_gene, each = 500)

    g1 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = MPD_12)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g1)
    g2 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = MPD_S1)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    g3 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = MPD_S2)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()

    g4 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = MNTD_12)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    g5 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = MNTD_S1)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    g6 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = MNTD_S2)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g2)

    # g7 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = SDPD)) +
    #   ggplot2::theme_bw() +
    #   ggplot2::geom_boxplot()
    # # print(g3)
    #
    # g8 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = SDNTD)) +
    #   ggplot2::theme_bw() +
    #   ggplot2::geom_boxplot()
    # print(g4)

    g7 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = D)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g5)

    g8 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = Num_S1)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()

    g9 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = Num_S2)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()

    # g7 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = Ratio)) +
    #   ggplot2::theme_bw() +
    #   ggplot2::geom_boxplot()
    # print(g7)

    g10 <- ggplot2::ggplot(ss_dist, aes(x = as.factor(generation),y = NLTT)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g8)
    tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/dss/rep3_param_",set,".tiff"),
         units="px", width=5000, height=2000,res = 300,compression="lzw")
    dss <- cowplot::plot_grid(
      g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,
      align = "hv", nrow = 2, ncol = 5
    )
    print(dss)
    while (!is.null(dev.list()))  dev.off()
  }
}


### violin
for(set in 1:10){
  message("set", set)
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/secsse_ABC_long/secsse_ABC_long_param_set_",set,"_ss_0.RData"))
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


  tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/dss/violin_ss_0_param_",set,".tiff"),
       units="px", width=4000, height=2000,res = 300,compression="lzw")
  dss <- cowplot::plot_grid(
    g1,g2,g3,g4,g5,g6,g7,g8,
    align = "hv", nrow = 2, ncol = 4
  )
  print(dss)
  while (!is.null(dev.list()))  dev.off()

}

#####
folder_path <- "G:/results/project 2/tip_info/round4/adap_secsse_new_space/secsse_ABC_long"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC_long.csv")
epsilon_matix <- matrix(NA,70,8)
colnames(epsilon_matix) <- c("MPD","SDPD","MNTD","SDNTD",
                             "D","Total","Ratio","NLTT")
for(i in 1:70){
  # if(i%%5 == 0){
  #   rep <- 5
  # } else {
  #   rep <- i%%5
  # }
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("secsse_ABC_param_set_", i,"_ss_0.RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)
  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))

    if(output$n_iter <= 3){
      epsilon_matix[i,] <- rep(NA,8)
    } else if (nrow(output$ABC[[output$n_iter]]) == 500) {
      epsilon_matix[i,] <- output$epsilon[output$n_iter,]
    } else {
      epsilon_matix[i,] <- output$epsilon[(output$n_iter-1),]
    }
  } else {
    epsilon_matix[i,] <- rep(NA,8)
  }
}
epsilon <- data.frame(param_data,epsilon_matix)
save(epsilon,
     file = paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/epsilon_last_gene_ss_set_0.RData"))




