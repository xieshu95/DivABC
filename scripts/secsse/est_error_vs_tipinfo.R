## relationship between estimation errors vs total species number(simulations of last generation)
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
    n_gene <- length(output$ABC)
    if(nrow(output$ABC[[n_gene]]) < 500){
      n_gene <- n_gene - 1
    }
    #get ABC for last generation
    ABC <- output$ABC[[n_gene]]
    colnames(ABC) <- c("lam1","lam2","mu1","mu2","q12","q21")
    rownames(ABC) <- 1:nrow(ABC)
    ABC <- as.data.frame(ABC)
    ABC$dlam1 <- ABC$lam1 - true_rates$lam1
    ABC$dlam2 <- ABC$lam2 - true_rates$lam2
    ABC$dmu1 <- ABC$mu1 - true_rates$mu1
    ABC$dmu2 <- ABC$mu2 - true_rates$mu2
    ABC$dq12 <- ABC$q12 - true_rates$q12
    ABC$dq21 <- ABC$q21 - true_rates$q21

    num_state1 <- c()
    num_state2 <- c()
    for(i in 1:500){
      num_state1[i] <- length(which(output$sim_list[[i]]$examTraits == 1))
      num_state2[i] <- length(which(output$sim_list[[i]]$examTraits == 2))
    }
    ABC$num_state1 <- num_state1
    ABC$num_state2 <- num_state2
    ABC$tree_size <- num_state1 + num_state2
    ABC$tip_ratio <- num_state1 / num_state2
    ABC$tip_ratio_or <- ABC$tip_ratio
    ABC$tip_ratio[ABC$tip_ratio < 1]<- 1/ABC$tip_ratio[ABC$tip_ratio < 1]


    g1 <- ggplot2::ggplot(ABC, aes(x = tree_size,y = abs(dlam1))) +
      ggplot2::theme_bw() +
      ggplot2::geom_point()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = 0), linetype = "dashed", size = 0.5) #lam1
    # print(g1)
    g2 <- ggplot2::ggplot(ABC, aes(x = tree_size,y = abs(dlam2))) +
      ggplot2::theme_bw() +
      ggplot2::geom_point()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = 0), linetype = "dashed", size = 0.5)

    g3 <- ggplot2::ggplot(ABC, aes(x = tree_size,y = abs(dmu1))) +
      ggplot2::theme_bw() +
      ggplot2::geom_point()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = 0), linetype = "dashed", size = 0.5)


    g4 <- ggplot2::ggplot(ABC, aes(x = tree_size,y = abs(dmu2))) +
      ggplot2::theme_bw() +
      ggplot2::geom_point()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = 0), linetype = "dashed", size = 0.5)

    g5 <- ggplot2::ggplot(ABC, aes(x = tree_size,y = abs(dq12))) +
      ggplot2::theme_bw() +
      ggplot2::geom_point()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = 0), linetype = "dashed", size = 0.5)

    g6 <- ggplot2::ggplot(ABC, aes(x = tree_size,y = abs(dq21))) +
      ggplot2::theme_bw() +
      ggplot2::geom_point()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = 0), linetype = "dashed", size = 0.5)


    tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/ABCerror_vs_tipinfo/tree_size/diff_ss_0_param_",set,".tiff"),
         units="px", width=3000, height=2000,res = 300,compression="lzw")
    dss <- cowplot::plot_grid(
      g1,g3,g5,g2,g4,g6,
      align = "hv", nrow = 2, ncol = 3
    )
    print(dss)
    while (!is.null(dev.list()))  dev.off()
  }
}


## relationship between estimation errors vs total species number(simulations of last generation)
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
    n_gene <- length(output$ABC)
    if(nrow(output$ABC[[n_gene]]) < 500){
      n_gene <- n_gene - 1
    }
    #get ABC for last generation
    ABC <- output$ABC[[n_gene]]
    colnames(ABC) <- c("lam1","lam2","mu1","mu2","q12","q21")
    rownames(ABC) <- 1:nrow(ABC)
    ABC <- as.data.frame(ABC)
    ABC$dlam1 <- ABC$lam1 - true_rates$lam1
    ABC$dlam2 <- ABC$lam2 - true_rates$lam2
    ABC$dmu1 <- ABC$mu1 - true_rates$mu1
    ABC$dmu2 <- ABC$mu2 - true_rates$mu2
    ABC$dq12 <- ABC$q12 - true_rates$q12
    ABC$dq21 <- ABC$q21 - true_rates$q21

    num_state1 <- c()
    num_state2 <- c()
    for(i in 1:500){
      num_state1[i] <- length(which(output$sim_list[[i]]$examTraits == 1))
      num_state2[i] <- length(which(output$sim_list[[i]]$examTraits == 2))
    }
    ABC$num_state1 <- num_state1
    ABC$num_state2 <- num_state2
    ABC$tree_size <- num_state1 + num_state2
    ABC$tip_ratio <- num_state1 / num_state2
    ABC$tip_ratio_or <- ABC$tip_ratio
    ABC$tip_ratio[ABC$tip_ratio < 1]<- 1/ABC$tip_ratio[ABC$tip_ratio < 1]


    g1 <- ggplot2::ggplot(ABC, aes(x = tree_size,y = lam1)) +
      ggplot2::theme_bw() +
      ggplot2::geom_point()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = lam1), linetype = "dashed", size = 0.5)
    # print(g1)
    g2 <- ggplot2::ggplot(ABC, aes(x = tree_size,y = lam2)) +
      ggplot2::theme_bw() +
      ggplot2::geom_point()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = lam2), linetype = "dashed", size = 0.5)

    g3 <- ggplot2::ggplot(ABC, aes(x = tree_size,y = mu1)) +
      ggplot2::theme_bw() +
      ggplot2::geom_point()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = mu1), linetype = "dashed", size = 0.5)


    g4 <- ggplot2::ggplot(ABC, aes(x = tree_size,y = mu2)) +
      ggplot2::theme_bw() +
      ggplot2::geom_point()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = mu2), linetype = "dashed", size = 0.5)

    g5 <- ggplot2::ggplot(ABC, aes(x = tree_size,y = q12)) +
      ggplot2::theme_bw() +
      ggplot2::geom_point()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = q12), linetype = "dashed", size = 0.5)

    g6 <- ggplot2::ggplot(ABC, aes(x = tree_size,y = q21)) +
      ggplot2::theme_bw() +
      ggplot2::geom_point()+
      ggplot2::geom_hline(data= true_rates, aes(yintercept = q21), linetype = "dashed", size = 0.5)


    tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/ABCerror_vs_tipinfo/tip_ratio/ss_0_param_",set,".tiff"),
         units="px", width=3000, height=2000,res = 300,compression="lzw")
    dss <- cowplot::plot_grid(
      g1,g3,g5,g2,g4,g6,
      align = "hv", nrow = 2, ncol = 3
    )
    print(dss)
    while (!is.null(dev.list()))  dev.off()
  }
}
