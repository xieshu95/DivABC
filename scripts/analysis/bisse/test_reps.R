## Fig S1 test whether 50 replicates are sufficient
load("Data/BiSSE/obs_ss.rda")

calc_ss_bisse_test <- function(sim) {
  # mpd_all
  mpd_all <- treestats::mean_pair_dist(phy = sim$phy)
  # mntd_all
  mntd_all <- treestats::mntd(phy = sim$phy)
  # D statistic
  D <- calc_D(sim)
  # state 1
  num_state1 <- length(which(sim$obs_traits == 1))
  num_state2 <- length(which(sim$obs_traits == 2))
  total_spec <- num_state1 + num_state2
  tip_ratio <- max(num_state1,num_state2)/min(num_state1,num_state2)
  # nLTT
  nltt <- treestats::nLTT_base(sim$phy)
  phy_s1<-ape::drop.tip(sim$phy,  ## phy1 with only state1 tips
                        tip = sim$phy$tip.label[which(sim$obs_traits == 2)])
  phy_s2<-ape::drop.tip(sim$phy,
                        tip = sim$phy$tip.label[which(sim$obs_traits == 1)])
  nltt1 <- treestats::nLTT_base(phy_s1)
  nltt2 <- treestats::nLTT_base(phy_s2)

  return(
    list(state1 = num_state1,
         state2 = num_state2,
         total_spec = total_spec,
         tip_ratio = tip_ratio,
         mpd_all = mpd_all,
         mntd_all = mntd_all,
         D = D,
         nltt = nltt,
         nltt1 = nltt1,
         nltt2 = nltt2)
  )
}

calc_epsilon_init_bisse_test <- function(sim){
  ss <- calc_ss_bisse_test(sim[[1]])
  eps_init <- as.numeric(unlist(ss)) * 1
  return(eps_init)
}

param_space_name <- paste0("bisse_ABC_test")
param_space <- load_param_space(param_space_name = param_space_name)
param_space200 <- param_space[rep(seq_len(nrow(param_space)), each = 4), , drop = FALSE]

ss <- c()
obs_sim <- list()
set.seed(1)
init_state <- rep(c(rep("1",100),rep("2",100)),7)
for(i in 1:1400){
  message("set: ", i)
  obs_sim_pars <- param_space200[i,]
  obs_sim[[i]] <- get_bisse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                           pool_init_states = init_state[i],
                                           replicates = 1)
  init_epsilon <- calc_epsilon_init_bisse_test(sim = obs_sim[[i]])
  ss<-rbind(ss,init_epsilon)
}

colnames(ss) <- c("state1","state2","tree_size","tip_ratio",
                  "mpd",
                  "mntd",
                  "D","nltt","nltt1","nltt2")
ss<-data.frame(ss)


load("Data/Bisse/obs_ss_500reps.rda")
ss500 <- ss
ss500$Scenario <- rep(1:7, each = 500)
load("Data/Bisse/obs_ss_200reps.rda")
ss200 <- ss
ss200$Scenario <- rep(1:7, each = 200)
load("Data/Bisse/obs_ss.rda")
ss50<- ss[c(1,2,3,4,5,9,18,19,20,21)]
ss50$Scenario <- rep(1:7, each = 50)

df <- rbind(ss50,ss200,ss500)
df$Replicates <- factor(rep(c("50","200","500"),
                            times = c(nrow(ss50),
                                      nrow(ss200),
                                      nrow(ss500))))
df$Replicates <- factor(df$Replicates, levels = c("50","200","500"))
# plot in response letter
library(ggplot2)
p1 <- ggplot(df, aes(x = state1, color = Replicates, fill = Replicates)) +
  geom_density(alpha = 0.2,size = 0.8) +
  labs(title = "",
       x = "Richness state 0", y = "Density") +
  ggplot2::theme_bw() +
  ggplot2::theme_classic()


p2 <- ggplot(df, aes(x = state2, color = Replicates, fill = Replicates)) +
  geom_density(alpha = 0.2,size = 0.8) +
  labs(title = "",
       x = "Richness state 1", y = "Density") +
  ggplot2::theme_bw() +
  ggplot2::theme_classic()

p3 <- ggplot(df, aes(x = tree_size, color = Replicates, fill = Replicates)) +
  geom_density(alpha = 0.2,size = 0.8) +
  xlim(0,1100)+
  labs(title = "",
       x = "Total richness", y = "Density") +
  ggplot2::theme_bw() +
  ggplot2::theme_classic()

p4 <- ggplot(df, aes(x = nltt, color = Replicates, fill = Replicates)) +
  geom_density(alpha = 0.2,size = 0.8) +
  labs(title = "",
       x = "nLTT", y = "Density") +
  ggplot2::theme_bw() +
  ggplot2::theme_classic()


final_plot <- cowplot::plot_grid(
  p1+ggplot2::theme(legend.position = "none"),
  p2+ggplot2::theme(legend.position = "none"),
  p3+ggplot2::theme(legend.position = "none"),
  p4+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 2
)+ ggtitle("Statistical metrics comparison")+
  ggplot2::theme(plot.title = element_text(color="black", size=18,margin = margin(0,0,6,0)))

legend <- cowplot::get_legend(
  p1 + theme(legend.box.margin = margin(0, 0, 0, 4))
)


tiff(paste0("Data/Bisse/comparison.png"),
     units="px", width=4000, height=4500,res = 500,compression="lzw")
final_plot <- cowplot::plot_grid(final_plot,legend,rel_widths = c(3, 0.4))
print(final_plot)
while (!is.null(dev.list()))  dev.off()
