## change the new MCMC results into the old version (5001 chains)
folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/new MCMC/DAISIE_MCMC_short_DI_50001"
files <- list.files(folder_path)
seq <- seq(1,50001,10)
for(i in 1:160){
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("DAISIE_MCMC_short_DI_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)

  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    output<-coda::as.mcmc(output[seq,])
    save(output,
         file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/new MCMC/DAISIE_MCMC_short_DI/",file_to_load))
  }
}



folder_path <- "D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_z_final/DI/DAISIE_MCMC_short_DI"
files <- list.files(folder_path)
for(i in c(12,14,15,18,20,53,55,56,57,58,60,75,94,95,128,131,132,133,134,135,137,139)){
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("DAISIE_MCMC_short_DI_param_set_", i,"_ss_1.RData"), #"_rep",rep,
                       files,
                       value = TRUE,
                       fixed = TRUE)

  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    output = output
    save(output,
         file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/new MCMC/DAISIE_MCMC_short_DI/",file_to_load))
  }
}
