
load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_merge_old/obs_sims_DI.RData")
for(i in 1:160){
  sim = obs_sim[[i]][[1]]
  stac <- unlist(lapply(sim[[1]][-1],"[[", "stac"))
  print(i)
  print(stac)
}

sim = obs_sim[[18]][[1]]  #18/159
brt <- lapply(sim[[1]][-1],"[[", "branching_times")
stac <- unlist(lapply(sim[[1]][-1],"[[", "stac"))
brt_length <- unlist(lapply(brt, length))
multi_brt <-c(unique(sort(unlist(brt[which((stac ==2 & brt_length >2) | stac ==3)]),
                          decreasing = TRUE)), 0)

singleton_brt <-c(unique(sort(unlist(brt[which(stac ==2 & brt_length ==2)]),
                              decreasing = TRUE)), 0)

nonend_brt <- c(unique(sort(unlist(brt[which(stac ==4)]),
                            decreasing = TRUE)), 0)


sim_1 = obs_sim[[18]][[1]]
sim_2 = obs_sim[[2]][[1]]

