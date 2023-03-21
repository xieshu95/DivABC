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

for(test in c(1,2,3,4,5,6)) {
  param_space <- readr::read_csv2(paste0("data/secsse_ABC_test",test,".csv"))
  ss <- c()
  for(i in 1:100){
    set.seed(i)
    message("set: ", i)
    obs_sim_pars <- param_space[i,]
    obs_sim <- get_secsse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                         K = Inf,
                                         replicates = 1) ## replicates = 30
    init_epsilon <- calc_epsilon_init_secsse_test(sim = obs_sim)
    ss<-rbind(ss,init_epsilon)
  }

  colnames(ss) <- c("state1","state2","tree_size","tip_ratio",
                    "mpd","mpd_diff","mpd_s1","mpd_s2",
                    "mntd","mntd_diff","mntd_s1","mntd_s2",
                    "sdpd","sdpd_diff","sdntd","sdntd_diff",
                    "K","D","nltt","colless",
                    "spect_log_median","spect_prin","sackin")
  ss<-data.frame(ss)
  # colnames(ss) <- c("state1","state2","tree_size","tip_ratio")
  save(ss,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/obs_ss_test",test,".RData"))

}

t2 <- Sys.time()
t2

## new heatmap code
library(heatmaply)
library(htmlwidgets)
for(test in 1:6){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/obs_ss_test",test,".RData"))
  p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss), xlab = "Summary statistics",
                                        ylab = "Summary statistics", k_col = 2, k_row = 2)
  saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/ss_heatmap/heatmap_ss_test_",test,".html"))

}

## combine all the tests(simulations)
ss_comb <-c()
for(test in 1:6){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/obs_ss_test",test,".RData"))
  ss_comb <- rbind(ss_comb,ss)
}
p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss_comb), xlab = "Summary statistics",
                                      ylab = "Summary statistics", k_col = 2, k_row = 2)
saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/ss_heatmap/heatmap_ss_combine.html"))


## old code for heatmap
for(test in 1:6){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/obs_ss_test",test,".RData"))
  # ss<-ss_comb
  rcorrDat<- Hmisc::rcorr(as.matrix(ss))
  cormat <- round(rcorrDat$r,2)
  # heatmap(cormat)
  head(cormat)

  library(reshape2)
  melted_cormat <- melt(cormat)
  library(ggplot2)

  ss_name <- c("state1","state2","tree_size","tip_ratio",
               "mpd","mpd_diff","mpd_s1","mpd_s2",
               "mntd","mntd_diff","mntd_s1","mntd_s2",
               "sdpd","sdpd_diff","sdntd","sdntd_diff",
               "K","D","nltt","colless",
               "spect","spect_prin","sackin")
  label_names <- "Summary statistic"
  tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/ss_heatmap/heatmap_ss_old_test_",test,".tiff"),
       units="px", width=4500, height=4500,res = 300,compression="lzw")
  heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red",
                         limit = c(-1,1), name="Correlation") +
    geom_text(aes(Var2, Var1, label = value), size = 5) +

    ggplot2::scale_x_discrete(labels= ss_name)+
    ggplot2::scale_y_discrete(labels= ss_name)+
    ggplot2::xlab(label_names) +  #Rate differential ratio of anagenesis/Diversity dependence
    ggplot2::ylab(label_names) +
    ggplot2::guides(fill = guide_legend(title="Correlation"))+
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10)) +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 12)) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 0.2, 6, 0.2)) +
    ggplot2::ggtitle("Correlations between summary statistics") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5))
  print(heatmap)
  while (!is.null(dev.list()))  dev.off()
}

# install.packages("heatmaply")

## check calc_error_secsse
set.seed(10)
parameters <- c(0.2,0.2,0,0,0.1,0.1)
idparlist <- secsse::cla_id_paramPos(traits = c(1,2),
                                     num_concealed_states = 2)
idparlist$lambdas[1,] <- rep(c(parameters[1], parameters[2]),2)
idparlist$mus[1:4]<- rep(c(parameters[3], parameters[4]),2)
masterBlock <- matrix(c(parameters[5], parameters[6]),
                      ncol=2,nrow=2,byrow=TRUE)
diag(masterBlock) <- NA
q <-secsse::q_doubletrans(c(1,2),masterBlock,diff.conceal=F)
q[1,3]<- q[2,4] <- q[3,1] <- q[4,2] <- 0

lambdas <- secsse::prepare_full_lambdas(c(1,2),2,idparlist$lambdas)
states <- names(idparlist$mus)
initialState<- sample(states,1)
speciesTraits <- c(initialState,initialState)

sim1 <- secsse_sim(timeSimul = 18,
                   states = states,
                   lambdas = lambdas,
                   mus = idparlist$mus,
                   qs = q,
                   speciesTraits = speciesTraits,
                   maxSpec = 400)

sim2 <- secsse_sim(timeSimul = 18,
                   states = states,
                   lambdas = lambdas,
                   mus = idparlist$mus,
                   qs = q,
                   speciesTraits = speciesTraits,
                   maxSpec = 400)
t1 <- Sys.time()
calc_error_secsse(sim1,sim2)
t2 <- Sys.time()
dt <- t2-t1



### random space
ss <- matrix(NA,nrow = 300,ncol = 23)
pars_accept <- c()
t1 <- Sys.time()
set <- 1
# set.seed(1)
while(set < 301){
  message("set",set)
  pars <- prior_gen_secsse(1:6,1:6)
  obs_sim <- get_secsse_sim(parameters = as.numeric(pars),
                            K = Inf,
                            replicates = 1)
  if (length(obs_sim[[1]]$examTraits) > 20 &&
      length(obs_sim[[1]]$examTraits) < 600 &&
      length(unique(obs_sim[[1]]$examTraits)) > 1) {
    ss[set,] <- calc_epsilon_init_secsse_test(sim = obs_sim)
    pars_accept <- rbind(pars_accept, pars)
    set <- set + 1
  }
}
t2 <- Sys.time()
dt <- t2-t1
dt


colnames(ss) <- c("state1","state2","tree_size","tip_ratio",
                  "mpd","mpd_diff","mpd_s1","mpd_s2",
                  "mntd","mntd_diff","mntd_s1","mntd_s2",
                  "sdpd","sdpd_diff","sdntd","sdntd_diff",
                  "K","D","nltt","colless",
                  "spect_log_median","spect_prin","sackin")

save(ss,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/random_space_ss.RData")

colnames(pars_accept) <- c("lam1","lam2","mu1","mu2","q12","q21")
pars_ss <-data.frame(pars_accept,ss)
save(pars_ss,file = "D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/random_space_pars_ss.RData")
p_heatmap <- heatmaply::heatmaply_cor(x = cor(ss), xlab = "Summary statistics",
                                      ylab = "Summary statistics", k_col = 2, k_row = 2)
saveWidget(p_heatmap, paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse_fix_MCMC/ss_heatmap/heatmap_ss_random.html"))
