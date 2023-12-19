## check tree size of the new space
calc_num <- function(sim){
  num_state1 <- length(which(sim$obs_traits == 1))
  num_state2 <- length(which(sim$obs_traits == 2))
  total_spec <- num_state1 + num_state2
  tip_ratio <- max(num_state1,num_state2)/min(num_state1,num_state2)
  return(
    list(state1 = num_state1,
         state2 = num_state2,
         total_spec = total_spec)
    # tip_ratio = tip_ratio)
  )
}

calc_epsilon_init_secsse_test <- function(sim){
  ss <- calc_num(sim[[1]]) #calc_ss_secsse_test
  eps_init <- as.numeric(unlist(ss)) * 1
  return(eps_init)
}


par1 <- c(0.5,0.5,0.05,0.05,0.1,0.1)
par2 <- c(0.25,0.5,0.05,0.05,0.1,0.1)
par3 <- c(0.1,0.5,0.05,0.05,0.1,0.1)
par4 <- c(0.5,0.5,0.1,0.05,0.1,0.1)
par5 <- c(0.5,0.5,0.25,0.05,0.1,0.1)
par6 <- c(0.5,0.5,0.05,0.05,0.2,0.1)
par7 <- c(0.5,0.5,0.05,0.05,0.5,0.1)

secsse_ABC <- rbind(par1,par2,par3,par4,par5,par6,par7)
colnames(secsse_ABC) <- c("lam1","lam2","mu1","mu2","q12","q21")
# param_space_name <- paste0("secsse_ABC_test")
# param_space <- load_param_space(param_space_name = param_space_name)
library(ggplot2)
for (par in 1:7){
  ss <- c()
  obs_sim <- list()
  set.seed(100)
  for(i in 1:1000){
    set.seed(i)
    message("set: ", i)
    obs_sim_pars <- secsse_ABC[par,]
    obs_sim[[i]] <- get_secsse_sim(parameters = as.numeric(obs_sim_pars),
                                   pool_init_states = c("1A","1B"), #c("1A","1B") or NULL
                                   replicates = 1) ## replicates = 30
    init_epsilon <- calc_epsilon_init_secsse_test(sim = obs_sim[[i]])
    ss<-rbind(ss,init_epsilon)
  }

  ss_df<- data.frame(variable = c(rep("S1", nrow(ss)),
                               rep("S2", nrow(ss)),
                               rep("All", nrow(ss))),
                  value=c(ss[,1], ss[,2],ss[,3]))

  p1 <- ggplot(ss_df,aes(x = value, fill = variable))+
    ggplot2::theme_classic() +
    geom_histogram(position = "identity", alpha = 0.3, bins = 50)
    # geom_density(alpha=.4)
    # geom_histogram(alpha=0.5, bins = 100)
  # p1
  #
  #
  #
  # p1 <- ggplot(ss, aes(ss[,1]))+
  #   geom_histogram(color='gray50',
  #                  alpha=0.2, bins = 100)
  # p2 <- ggplot(ss, aes(ss[,2]))+
  #   geom_histogram(color='gray50',
  #                  alpha=0.2, bins = 100)
  # p3 <- ggplot(ss, aes(ss[,3]))+
  #   geom_histogram(color='gray50',
  #                  alpha=0.2, bins = 100)
  tiff(paste0("D:/Onedrive-shu/OneDrive/num_plots/num_spec",par,".tiff"),
       units="px", width=3000, height=2000,res = 350,compression="lzw")
  # p_all <- cowplot::plot_grid(
  #   p1,p2,p3,align = "hv", nrow = 1, ncol = 3
  # )
  print(p1)
  # param_est_final <- cowplot::add_sub(param_est_final, "Tip ratio", hjust = 1)
  # print(cowplot::ggdraw(param_est_final))
  while (!is.null(dev.list()))  dev.off()
}


## observed trees
load("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/obs_ss_test.rda")
for(i in 1:7){
  size<- ss$tree_size[(i*50-49):(i*50)]
  plot(hist(size,breaks = 15))
}

