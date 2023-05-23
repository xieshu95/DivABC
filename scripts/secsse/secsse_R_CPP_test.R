# test secsse
# 1. calculate summary statistics
library(profvis)
calc_ss_secsse_test <- function(sim) {
  # mpd_all
  mpd_all <- treestats::mean_pair_dist(phy = sim$phy)
  mpd_s1 <- calc_mpd_trait(sim = sim,state_type = 1)
  mpd_s2 <- calc_mpd_trait(sim = sim,state_type = 2)
  mpd_diff <- calc_mpd_trait(sim = sim,state_type = 0)

  # mntd_all
  mntd_all <- treestats::mntd(phy = sim$phy)
  mntd_s1 <- calc_mntd_trait(sim = sim,state_type = 1)
  mntd_s2 <- calc_mntd_trait(sim = sim,state_type = 2)
  mntd_diff <- calc_mntd_trait(sim = sim,state_type = 0)

  # K statistic
  K <- adiv::K(sim$phy,
               trait = sim$examTraits,
               nrep = 1000, alter = c("two-sided"))
  K <- K$obs

  # D statistic
  D <- calc_D(sim)

  # state 1
  num_state1 <- length(which(sim$examTraits == 1))
  num_state2 <- length(which(sim$examTraits == 2))
  total_spec <- num_state1 + num_state2
  tip_ratio <- max(num_state1,num_state2)/min(num_state1,num_state2)


  # nLTT
  nltt <- treestats::nLTT_base(sim$phy)

  ## standard deviation of pairwise distance
  sdpd_all <- calc_sdpd_trait(sim = sim,state_type = 3)
  sdpd_diff <- calc_sdpd_trait(sim = sim,state_type = 0)

  ## standard deviation of nearest taxon distance
  sdntd_all <- calc_sdntd_trait(sim = sim,state_type = 3)
  sdntd_diff <- calc_sdntd_trait(sim = sim,state_type = 0)

  colless <- treestats::colless(sim$phy)
  spect <- treestats::laplacian_spectrum(sim$phy)
  spect_log_median <- median(log(spect$eigenvalues))
  spect_prin <- log(spect$principal_eigenvalue)
  sackin <- treestats::sackin(sim$phy)

  return(
    list(state1 = num_state1,
         state2 = num_state2,
         total_spec = total_spec,
         tip_ratio = tip_ratio,
         mpd_all = mpd_all,
         mpd_diff = mpd_diff,
         mpd_s1 = mpd_s1,
         mpd_s2 = mpd_s2,
         mntd_all = mntd_all,
         mntd_diff = mntd_diff,
         mntd_s1 = mntd_s1,
         mntd_s2 = mntd_s2,
         sdpd_all = sdpd_all,
         sdpd_diff = sdpd_diff,
         sdntd_all = sdntd_all,
         sdntd_diff = sdntd_diff,
         K = K,
         D = D,
         nltt = nltt,
         colless,
         spect_log_median,
         spect_prin,
         sackin)
  )
}
# ss<-calc_ss_secsse_test(sim)
# ss

calc_epsilon_init_secsse_test <- function(sim){
  ss <- calc_ss_secsse_test(sim[[1]])
  eps_init <- as.numeric(unlist(ss)) * 1
  return(eps_init)
}

# old secsse R simulation

# for(test in c(1,2,3)) {
  test = 6
  param_space <- readr::read_csv2(paste0("data/secsse_ABC_test",test,".csv"))
  ss <- c()
  obs_sim <- list()
  set.seed(1)
  profvis( {
    for(i in 1:100){
      message("set: ", i)
      obs_sim_pars <- param_space[i,]
      obs_sim[[i]] <- get_secsse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                                K = Inf,
                                                replicates = 1) ## replicates = 30
      init_epsilon <- calc_epsilon_init_secsse_test(sim = obs_sim[[i]])
      ss<-rbind(ss,init_epsilon)
    }
  })

  colnames(ss) <- c("state1","state2","tree_size","tip_ratio",
                    "mpd","mpd_diff","mpd_s1","mpd_s2",
                    "mntd","mntd_diff","mntd_s1","mntd_s2",
                    "sdpd","sdpd_diff","sdntd","sdntd_diff",
                    "K","D","nltt","colless",
                    "spect_log_median","spect_prin","sackin")
  ss<-data.frame(ss)
  # colnames(ss) <- c("state1","state2","tree_size","tip_ratio")
  save(ss,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/obs_ss_test",test,".RData"))
  save(obs_sim,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/obs_sim_test",test,".RData"))
# }



#####
# fit new secsse cpp simulation

## create trait matrix that show the state is same or not between each two tips
# 0 means different
# 1 means both states are 1
# 2 means both states are 2
create_trait_matrix <- function(sim) {
  n <- length(sim$phy$tip.label)
  trait_matrix <- matrix(0, nrow = n, ncol = n)
  trait <- sim$obs_traits
  for (i in 1:n){
    for (j in 1:n) {
      if(trait[i] == trait[j]) {  ## if two states are different, keep 0
        trait_matrix[i,j] <- trait[i]
      }
    }
  }
  dimnames(trait_matrix)[1:2]<-list(sim$phy$tip.label)
  return(trait_matrix)
}
## input is secsse simulation with phy and traits
# state_type = 0 means distance between species with different states
# state_type = 1 means distance between species with both state 1
# state_type = 2 means distance between species with both state 2
# state_type = 3 means distance between all the species
calc_mpd_trait <- function(sim,state_type = 0)
{
  dis <- stats::cophenetic(sim$phy)
  # dis<- dis[order(readr::parse_number(rownames(dis))),
  #           order(readr::parse_number(colnames(dis)))]
  trait <- create_trait_matrix(sim)
  if(state_type == 3) {
    mpd <- mean(dis[lower.tri(dis)])
  } else {
    mpd <- mean(dis[lower.tri(dis) & trait == state_type])
  }
  if(is.na(mpd)) {
    mpd <- 0
  }
  return(mpd)
}

calc_mntd_trait <- function(sim,state_type = 0)
{
  dis <- stats::cophenetic(sim$phy)
  # dis<- dis[order(readr::parse_number(rownames(dis))),
  #           order(readr::parse_number(colnames(dis)))]
  trait <- create_trait_matrix(sim)
  diag(dis) <- NA
  if(state_type != 3) {
    dis[which(trait != state_type)]<- NA
    dis <- dis[ , colSums(is.na(dis)) < nrow(dis)]
  }
  mntd <- mean(apply(dis,2,min,na.rm=TRUE))
  if(is.na(mntd)) {
    mntd <- 0
  }
  return(mntd)
}

# calculate standard deviation of pairwise distanse
calc_sdpd_trait <- function(sim,state_type = 0)
{
  dis <- stats::cophenetic(sim$phy)
  # dis<- dis[order(readr::parse_number(rownames(dis))),
  #           order(readr::parse_number(colnames(dis)))]
  trait <- create_trait_matrix(sim)
  if(state_type == 3) {
    sdpd <- sd(dis[lower.tri(dis)])
  } else {
    sdpd <- sd(dis[lower.tri(dis) & trait == state_type])
  }
  sdpd
}

calc_sdntd_trait <- function(sim,state_type = 0)
{
  dis <- stats::cophenetic(sim$phy)
  # dis<- dis[order(readr::parse_number(rownames(dis))),
  #           order(readr::parse_number(colnames(dis)))]
  trait <- create_trait_matrix(sim)
  diag(dis) <- NA
  if(state_type != 3) {
    dis[which(trait != state_type)]<- NA
    dis <- dis[ , colSums(is.na(dis)) < nrow(dis)]
  }
  sdntd <- sd(apply(dis,2,min,na.rm=TRUE))
  sdntd
}


calc_D <- function (sim) {
  trait = data.frame(sim$phy$tip.label,sim$obs_traits)
  colnames(trait) <- c("tips","trait_val")
  data <- caper::comparative.data(sim$phy, trait, tips)
  PhyloD <- caper::phylo.d(data, binvar=trait_val,permut = 500)
  return(as.numeric(PhyloD$DEstimate))
}

calc_ss_secsse_test_cpp <- function(sim) {
  # mpd_all
  mpd_all <- treestats::mean_pair_dist(phy = sim$phy)
  mpd_s1 <- calc_mpd_trait(sim = sim,state_type = 1)
  mpd_s2 <- calc_mpd_trait(sim = sim,state_type = 2)
  mpd_diff <- calc_mpd_trait(sim = sim,state_type = 0)

  # mntd_all
  mntd_all <- treestats::mntd(phy = sim$phy)
  mntd_s1 <- calc_mntd_trait(sim = sim,state_type = 1)
  mntd_s2 <- calc_mntd_trait(sim = sim,state_type = 2)
  mntd_diff <- calc_mntd_trait(sim = sim,state_type = 0)

  # K statistic
  K <- adiv::K(sim$phy,
               trait = sim$obs_traits,
               nrep = 1000, alter = c("two-sided"))
  K <- K$obs

  # D statistic
  D <- calc_D(sim)

  # state 1
  num_state1 <- length(which(sim$obs_traits == 1))
  num_state2 <- length(which(sim$obs_traits == 2))
  total_spec <- num_state1 + num_state2
  tip_ratio <- max(num_state1,num_state2)/min(num_state1,num_state2)


  # nLTT
  nltt <- treestats::nLTT_base(sim$phy)

  ## standard deviation of pairwise distance
  sdpd_all <- calc_sdpd_trait(sim = sim,state_type = 3)
  sdpd_diff <- calc_sdpd_trait(sim = sim,state_type = 0)

  ## standard deviation of nearest taxon distance
  sdntd_all <- calc_sdntd_trait(sim = sim,state_type = 3)
  sdntd_diff <- calc_sdntd_trait(sim = sim,state_type = 0)

  colless <- treestats::colless(sim$phy)
  spect <- treestats::laplacian_spectrum(sim$phy)
  spect_log_median <- median(log(spect$eigenvalues))
  spect_prin <- log(spect$principal_eigenvalue)
  sackin <- treestats::sackin(sim$phy)

  return(
    list(state1 = num_state1,
         state2 = num_state2,
         total_spec = total_spec,
         tip_ratio = tip_ratio,
         mpd_all = mpd_all,
         mpd_diff = mpd_diff,
         mpd_s1 = mpd_s1,
         mpd_s2 = mpd_s2,
         mntd_all = mntd_all,
         mntd_diff = mntd_diff,
         mntd_s1 = mntd_s1,
         mntd_s2 = mntd_s2,
         sdpd_all = sdpd_all,
         sdpd_diff = sdpd_diff,
         sdntd_all = sdntd_all,
         sdntd_diff = sdntd_diff,
         K = K,
         D = D,
         nltt = nltt,
         colless,
         spect_log_median,
         spect_prin,
         sackin)
  )
}
calc_epsilon_init_secsse_test_cpp <- function(sim){
  ss <- calc_ss_secsse_test_cpp(sim[[1]])
  eps_init <- as.numeric(unlist(ss)) * 1
  return(eps_init)
}


# new secsse Rcpp simulations
# for(test in c(1,2,3,4,5,6)) {
  test = 2
  param_space <- readr::read_csv2(paste0("data/secsse_ABC_test",test,".csv"))
  ss <- c()
  obs_sim <- list()
  set.seed(1)
  # profvis({
    for(i in 1:100){
      message("set: ", i)
      obs_sim_pars <- param_space[i,]
      obs_sim[[i]] <- get_secsse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                                    K = Inf,
                                                    replicates = 1) ## replicates = 30
      init_epsilon <- calc_epsilon_init_secsse_test_cpp(sim = obs_sim[[i]])
      ss<-rbind(ss,init_epsilon)
    }
  # })

  colnames(ss) <- c("state1","state2","tree_size","tip_ratio",
                    "mpd","mpd_diff","mpd_s1","mpd_s2",
                    "mntd","mntd_diff","mntd_s1","mntd_s2",
                    "sdpd","sdpd_diff","sdntd","sdntd_diff",
                    "K","D","nltt","colless",
                    "spect_log_median","spect_prin","sackin")
  ss<-data.frame(ss)
  # colnames(ss) <- c("state1","state2","tree_size","tip_ratio")
  save(ss,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/cpp_obs_ss_test",test,".RData"))
  save(obs_sim,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/cpp_obs_sim_test",test,".RData"))
# }
## compare secsse_sim_R and secsse_sim_cpp results

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/obs_ss_test1.RData"))
p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss), xlab = "Summary statistics",
                                      ylab = "Summary statistics", k_col = 2, k_row = 2)
saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/heatmap_ss_test1.html"))

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/cpp_obs_ss_test1.RData"))
p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss), xlab = "Summary statistics",
                                      ylab = "Summary statistics", k_col = 2, k_row = 2)
saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/cpp_heatmap_ss_test1.html"))

load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/obs_sim_test2.RData"))
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/cpp_obs_sim_test2.RData"))


##
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/obs_ss_test6.RData"))
ss_old = ss
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/cpp_obs_ss_test2.RData"))
# ss_old = ss
# load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/cpp_nonext_false_obs_sim_test2.RData"))


ss_comb <- rbind(ss_old,ss)
ss_comb <- ss_comb[,-21]
ss_comb$Method <- c(rep("R",100),rep("Cpp",100))
# install.packages("reshape2")
library(reshape2)
ss_melt <- melt(ss_comb, id = c("Method"))
library(ggplot2)
p <- ggplot2::ggplot(data = ss_melt, aes(x = variable, y = value, color = Method)) +
  # `geom_col()` uses `stat_identity()`: it leaves the data as is.
  ggplot2::geom_boxplot()+
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::scale_y_log10()
tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/compare_test6.tiff"),
     units="px", width=7000, height=2500,res = 350,compression="lzw")
print(p)
while (!is.null(dev.list()))  dev.off()

## change RData to rds
load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_cpp_test/cpp_obs_sim_test6.RData"))
saveRDS(obs_sim,file = "data/obs_sims_secsse_ABC_test6.rds")
## CHANGE RDS TO rda(save in inst/extdata)
obs_sims<-readRDS("data/obs_sims_secsse_ABC_test6.rds")
# save(obs_sims,file = "inst/extdata/obs_sims_secsse_ABC_test6.rda")
save(obs_sims,file = "inst/extdata/obs_sims_secsse_MCMC_test6.rda")

a<-readr::read_csv2("data/secsse_ABC_test1.csv")


##
ss <- c()
obs_sim <- list()
for(i in 1:100){
  message("set: ", i)
  obs_sim_pars <- c(0.2,0.4,0.05,0.05,0.1,0.1)
  obs_sim[[i]] <- get_secsse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                            K = Inf,
                                            replicates = 1) ## replicates = 30
  init_epsilon <- calc_epsilon_init_secsse_test_cpp(sim = obs_sim[[i]])
  ss<-rbind(ss,init_epsilon)
}
ss1 = ss
obs_sim1 = obs_sim

ss <- c()
obs_sim <- list()
for(i in 1:100){
  message("set: ", i)
  obs_sim_pars <- c(0.2,0.4,0.05,0.05,0.2,0.2)
  obs_sim[[i]] <- get_secsse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                            K = Inf,
                                            replicates = 1) ## replicates = 30
  init_epsilon <- calc_epsilon_init_secsse_test_cpp(sim = obs_sim[[i]])
  ss<-rbind(ss,init_epsilon)
}
ss2 = ss
obs_sim2 = obs_sim

ss_comb <- rbind(ss1,ss2)
colnames(ss_comb) <- c("state1","state2","tree_size","tip_ratio",
                  "mpd","mpd_diff","mpd_s1","mpd_s2",
                  "mntd","mntd_diff","mntd_s1","mntd_s2",
                  "sdpd","sdpd_diff","sdntd","sdntd_diff",
                  "K","D","nltt","colless",
                  "spect_log_median","spect_prin","sackin")
ss_comb <- ss_comb[,-21]
ss_comb<-data.frame(ss_comb)
ss_comb$Method <- c(rep("q1",100),rep("q2",100))
# install.packages("reshape2")
library(reshape2)
ss_melt <- melt(ss_comb, id = c("Method"))
library(ggplot2)
p <- ggplot2::ggplot(data = ss_melt, aes(x = variable, y = value, color = Method)) +
  # `geom_col()` uses `stat_identity()`: it leaves the data as is.
  ggplot2::geom_boxplot()+
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::scale_y_log10()
p


##
ss <- c()
obs_sim <- list()
for(i in 1:100){
  message("set: ", i)
  obs_sim_pars <- c(0.3,0.3,0.05,0.05,0.1,0.1)
  obs_sim[[i]] <- get_secsse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                            K = Inf,
                                            replicates = 1) ## replicates = 30
  init_epsilon <- calc_epsilon_init_secsse_test_cpp(sim = obs_sim[[i]])
  ss<-rbind(ss,init_epsilon)
}
ss3 = ss
obs_sim3 = obs_sim

ss <- c()
obs_sim <- list()
for(i in 1:100){
  message("set: ", i)
  obs_sim_pars <- c(0.3,0.3,0.05,0.05,0.2,0.2)
  obs_sim[[i]] <- get_secsse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                            K = Inf,
                                            replicates = 1) ## replicates = 30
  init_epsilon <- calc_epsilon_init_secsse_test_cpp(sim = obs_sim[[i]])
  ss<-rbind(ss,init_epsilon)
}
ss4 = ss
obs_sim4 = obs_sim

ss_comb <- rbind(ss3,ss4)
colnames(ss_comb) <- c("state1","state2","tree_size","tip_ratio",
                       "mpd","mpd_diff","mpd_s1","mpd_s2",
                       "mntd","mntd_diff","mntd_s1","mntd_s2",
                       "sdpd","sdpd_diff","sdntd","sdntd_diff",
                       "K","D","nltt","colless",
                       "spect_log_median","spect_prin","sackin")
ss_comb <- ss_comb[,-21]
ss_comb<-data.frame(ss_comb)
ss_comb$Method <- c(rep("q1",100),rep("q2",100))
# install.packages("reshape2")
library(reshape2)
ss_melt <- melt(ss_comb, id = c("Method"))
library(ggplot2)
p <- ggplot2::ggplot(data = ss_melt, aes(x = variable, y = value, color = Method)) +
  # `geom_col()` uses `stat_identity()`: it leaves the data as is.
  ggplot2::geom_boxplot()+
  ggplot2::theme_bw() +
  ggplot2::theme_classic() +
  ggplot2::scale_y_log10()
p
