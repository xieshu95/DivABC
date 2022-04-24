### combine mcmc chains of 10 replicates into one plot
library(coda)
library(ggmcmc)
library(ggplot2)
mcmc.list <- mcmc.list()
folder_path <- "G:/results/project 2/tip_info/round2/MCMC_compare/set_initial/DAISIE_MCMC"
files <- list.files(folder_path)
for(param_set in 1:10){
  for(i in (param_set*10-9):(param_set*10)){
    # param_set = (param_num-1)*5 + i
    file_to_load <- grep(paste0("DAISIE_MCMC_param_set_", i,".RData"), #"_rep",rep,
                         files,
                         value = TRUE,
                         fixed = TRUE)

    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      mcmc.list[[i-(param_set-1)*10]] <- output
    }
  }
  a <-ggs(mcmc.list)
  tiff(paste0("G:/results/project 2/tip_info/round2/MCMC_compare/set_initial/param_set",param_set,".tiff"), units="px", width=500, height=300)
  mcmc_comb <- ggs_traceplot(a[1:5001*10,])+
    ggplot2::theme_bw()+
    ggplot2::ylim(0,2.0) +
    ggplot2::geom_hline(yintercept = param_set*0.1, linetype = "dashed", size = 0.5)+
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black"))
  print(mcmc_comb)
  while (!is.null(dev.list()))  dev.off()
}



#### plot mu scenarios:
for(param_set in 11:20){
  for(i in (param_set*10-9):(param_set*10)){
    # param_set = (param_num-1)*5 + i
    file_to_load <- grep(paste0("DAISIE_MCMC_param_set_", i,".RData"), #"_rep",rep,
                         files,
                         value = TRUE,
                         fixed = TRUE)

    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      mcmc.list[[i-(param_set-1)*10]] <- output
    }
  }
  a <-ggs(mcmc.list)
  tiff(paste0("G:/results/project 2/tip_info/round2/MCMC_compare/set_initial/param_set",param_set,".tiff"), units="px", width=500, height=300)
  mcmc_comb <- ggs_traceplot(a[which(a$Parameter == 2),])+
    ggplot2::theme_bw()+
    ggplot2::ylim(0,2.0) +
    ggplot2::geom_hline(yintercept = (param_set-10)*0.1, linetype = "dashed", size = 0.5)+
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black"))
  print(mcmc_comb)
  while (!is.null(dev.list()))  dev.off()
}

#### plot gam scenarios:
for(param_set in 21:30){
  for(i in (param_set*10-9):(param_set*10)){
    # param_set = (param_num-1)*5 + i
    file_to_load <- grep(paste0("DAISIE_MCMC_param_set_", i,".RData"), #"_rep",rep,
                         files,
                         value = TRUE,
                         fixed = TRUE)

    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      mcmc.list[[i-(param_set-1)*10]] <- output
    }
  }
  a <-ggs(mcmc.list)
  tiff(paste0("G:/results/project 2/tip_info/round2/MCMC_compare/set_initial/param_set",param_set,".tiff"), units="px", width=500, height=300)
  mcmc_comb <- ggs_traceplot(a[which(a$Parameter == 3),])+
    ggplot2::theme_bw()+
    ggplot2::ylim(0,0.05) +
    ggplot2::geom_hline(yintercept = (param_set-20)*0.1, linetype = "dashed", size = 0.5)+
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black"))
  print(mcmc_comb)
  while (!is.null(dev.list()))  dev.off()
}


#### plot laa scenarios:
for(param_set in 31:40){
  for(i in (param_set*10-9):(param_set*10)){
    # param_set = (param_num-1)*5 + i
    file_to_load <- grep(paste0("DAISIE_MCMC_param_set_", i,".RData"), #"_rep",rep,
                         files,
                         value = TRUE,
                         fixed = TRUE)

    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      mcmc.list[[i-(param_set-1)*10]] <- output
    }
  }
  a <-ggs(mcmc.list)
  tiff(paste0("G:/results/project 2/tip_info/round2/MCMC_compare/set_initial/param_set",param_set,".tiff"), units="px", width=500, height=300)
  mcmc_comb <- ggs_traceplot(a[which(a$Parameter == 4),])+
    ggplot2::theme_bw()+
    ggplot2::ylim(0,2.0) +
    ggplot2::geom_hline(yintercept = (param_set-30)*0.002+0.01, linetype = "dashed", size = 0.5)+
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5, color = "black"))+
    ggplot2::theme(legend.text = ggplot2::element_text(size = 10, color = "black")) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 10, color = "black")) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20, color = "black")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20, color = "black"))
  print(mcmc_comb)
  while (!is.null(dev.list()))  dev.off()
}
