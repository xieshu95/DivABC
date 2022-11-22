#####
## plot the d(ss) for each generation
library(ggplot2)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC_short.csv")
folder_path <- "G:/results/project 2/tip_info/round4/adap_daisie/DAISIE_ABC_short"
files <- list.files(folder_path)
for(set in 1:81){
  message("set", set)
  file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", set,"_ss_0.RData"),  #,"_rep",rep
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


    colnames(ss_dist) <- c("Nltt","Cltt","Anagenesis","Cladogenesis",
                           "Nonendemic","SCSD","CTSD")
    rownames(ss_dist) <- 1:nrow(ss_dist)
    ss_dist <- as.data.frame(ss_dist)
    ss_dist$generation <- as.factor(rep(1:n_gene, each = 500))

    g1 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Nltt)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g1)

    g2 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Cltt)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g2)

    g3 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Anagenesis)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g3)

    g4 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Cladogenesis)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g4)

    g5 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Nonendemic)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g5)

    g6 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = SCSD)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g6)

    g7 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = CTSD)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g7)


    tiff(paste0("G:/results/project 2/tip_info/round4/adap_daisie/dss/ss_0_param_",set,".tiff"),
         units="px", width=4000, height=2000,res = 300,compression="lzw")
    dss <- cowplot::plot_grid(
      g1,g2,g3,g4,g5,g6,g7,
      align = "hv", nrow = 2, ncol = 4
    )
    print(dss)
    while (!is.null(dev.list()))  dev.off()
  }
}


#####
## plot the d(ss) for each generation
library(ggplot2)
folder_path <- "G:/results/project 2/tip_info/round4/adap_daisie/DAISIE_ABC_short"
files <- list.files(folder_path)
epsilon_matix <- matrix(NA,81,7)
colnames(epsilon_matix) <- c("Nltt","Cltt","Anagenesis","Cladogenesis",
                       "Nonendemic","SCSD","CTSD")
for(i in 1:81){
  # if(i%%5 == 0){
  #   rep <- 5
  # } else {
  #   rep <- i%%5
  # }
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", i,"_ss_0.RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))

    if(output$n_iter <= 3){
      epsilon_matix[i,] <- rep(NA,7)
    } else if (nrow(output$ABC[[output$n_iter]]) == 500) {
      epsilon_matix[i,] <- output$epsilon[output$n_iter,]
    } else {
      epsilon_matix[i,] <- output$epsilon[(output$n_iter-1),]
    }
  } else {
    epsilon_matix[i,] <- rep(NA,7)
  }
}
epsilon <- data.frame(param_data,epsilon_matix)
save(epsilon,
     file = paste0("G:/results/project 2/tip_info/round4/adap_daisie/epsilon_last_gene_ss_set_0.RData"))
