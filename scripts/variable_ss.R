### 控制其他变量，只改变lamc， 0~1均匀分布sample 50个值，进行simulation，每个参数100reps
# 共5000个点，做散点图，横坐标lamc， 纵坐标ss（summary statistics）
#pars <-c(0.2,0.1,40,0.005,0.1)
set.seed(1)
lac <- runif(50,0,1)
replicates <- 100
novel_sim <- list()
for (i in 1:length(lac)) {
  for (j in seq_len(replicates)) {
    novel_sim[[j]] <- DAISIE::DAISIE_sim_constant_rate(
      time = 5,
      M = 1000,
      pars = c(lac[i],0.8,40,0.02,0.5),
      replicates = 1,
      sample_freq  = Inf,
      plot_sims = FALSE,
      verbose = FALSE,
      cond = 5
    )
  }
  file_name <- paste0("sim_lac",i,".RData")
  save(novel_sim,file = paste0("G:/R/Traisie_ABC/results/lac/",file_name))
}

#### getting obs_simulation,
set.seed(1)
obs_sim <- DAISIE::DAISIE_sim_constant_rate(
  time = 5,
  M = 1000,
  pars = c(0.5,0.3,40,0.02,0.5),
  replicates = 1,
  sample_freq  = Inf,
  plot_sims = FALSE,
  verbose = FALSE,
  cond = 5
)
save(obs_sim,file = "G:/R/Traisie-ABC/results/obs_sim.RData")

#### After getting simulations, next step is to calculate ss and plot
load(file.path("G:/R/Traisie-ABC/results/obs_sim.RData"))
obs_simulation <- obs_sim
var_value <- lac
var_param_name = "lac"
num_particles <- 100
replicates <- 50

get_particles_combine <- function (num_particles,
                                   obs_simulation,
                                   replicates,
                                   var_param_name,
                                   var_value) {
  var_append <- base::rep (var_value,each = replicates)
  spec_nltt_diff <- c()
  endemic_nltt_diff <- c()
  nonendemic_nltt_diff <- c()
  n_spec_diff <- c()
  n_col_diff <- c()
  for (i in 1:num_particles) {
    path <- file.path(paste0(
      "results/",var_param_name,"/sim_",var_param_name,i,".RData"))
    load(file.path(path))
    sim_compare <- novel_sim
    ss_diff_list <- nltt_within_param (obs_rep = 1,
                                       sim1 = obs_simulation,
                                       sim2 = sim_compare,
                                       replicates = replicates)
    spec_nltt_diff <- append(spec_nltt_diff,
                             ss_diff_list$spec_nltt_error)
    endemic_nltt_diff <- append(endemic_nltt_diff,
                                ss_diff_list$endemic_nltt_error)
    nonendemic_nltt_diff <- append(nonendemic_nltt_diff,
                                   ss_diff_list$nonendemic_nltt_error)
    n_spec_diff <- append(n_spec_diff,
                          ss_diff_list$num_spec_error)
    n_col_diff <- append(n_col_diff,
                         ss_diff_list$num_col_error)
  }
  data <- data.frame(var_append,
                     spec_nltt_diff,
                     endemic_nltt_diff,
                     nonendemic_nltt_diff,
                     n_spec_diff,
                     n_col_diff)
  colnames(data) <- c(var_param_name,
                      "spec_nltt",
                      "endemic_nltt",
                      "nonendemic_nltt",
                      "n_spec",
                      "n_col")
  return(data)
}


###plots
load(file.path("G:/R/Traisie-ABC/results/obs_sim.RData"))

set.seed(1)
lac <- runif(100,0,1)
data <-get_particles_combine (num_particles = 100,
                              obs_simulation = obs_sim,
                              replicates = 50,
                              var_param_name = "lac",
                              var_value = lac
)
save(data,file = "G:/R/Traisie-ABC/results/lac_ss.RData")
library(ggplot2)
spec_nltt_plot <- ggplot(data, aes(x = lac, y = spec_nltt)) +
  geom_point()
endemic_nltt_plot <- ggplot(data, aes(x = lac, y = endemic_nltt)) +
  geom_point()
nonendemic_nltt_plot <- ggplot(data, aes(x = lac, y = nonendemic_nltt)) +
  geom_point()
n_spec_plot <- ggplot(data, aes(x = lac, y = n_spec)) +
  geom_point()
n_col_plot <- ggplot(data, aes(x = lac, y = n_col)) +
  geom_point()
ggsave(spec_nltt_plot, file=paste0("G:/R/Traisie-ABC/plots/lac/spec_nltt_plot.png"), width = 20, height = 15, units = "cm")
ggsave(endemic_nltt_plot, file=paste0("G:/R/Traisie-ABC/plots/lac/endemic_nltt_plot.png"), width = 20, height = 15, units = "cm")
ggsave(nonendemic_nltt_plot, file=paste0("G:/R/Traisie-ABC/plots/lac/nonendemic_nltt_plot.png"), width = 20, height = 15, units = "cm")
ggsave(n_spec_plot, file=paste0("G:/R/Traisie-ABC/plots/lac/n_spec_plot.png"), width = 20, height = 15, units = "cm")
ggsave(n_col_plot, file=paste0("G:/R/Traisie-ABC/plots/lac/n_col_plot.png"), width = 20, height = 15, units = "cm")


set.seed(1)
mu <- runif(100,0,1)
data <-get_particles_combine (num_particles = 100,
                              obs_simulation = obs_sim,
                              replicates = 50,
                              var_param_name = "mu",
                              var_value = mu
)
save(data,file = "G:/R/Traisie-ABC/results/mu_ss.RData")
library(ggplot2)
spec_nltt_plot <- ggplot(data, aes(x = mu, y = spec_nltt)) +
  geom_point()
endemic_nltt_plot <- ggplot(data, aes(x = mu, y = endemic_nltt)) +
  geom_point()
nonendemic_nltt_plot <- ggplot(data, aes(x = mu, y = nonendemic_nltt)) +
  geom_point()
n_spec_plot <- ggplot(data, aes(x = mu, y = n_spec)) +
  geom_point()
n_col_plot <- ggplot(data, aes(x = mu, y = n_col)) +
  geom_point()
ggsave(spec_nltt_plot, file=paste0("G:/R/Traisie-ABC/plots/mu/spec_nltt_plot.png"), width = 20, height = 15, units = "cm")
ggsave(endemic_nltt_plot, file=paste0("G:/R/Traisie-ABC/plots/mu/endemic_nltt_plot.png"), width = 20, height = 15, units = "cm")
ggsave(nonendemic_nltt_plot, file=paste0("G:/R/Traisie-ABC/plots/mu/nonendemic_nltt_plot.png"), width = 20, height = 15, units = "cm")
ggsave(n_spec_plot, file=paste0("G:/R/Traisie-ABC/plots/mu/n_spec_plot.png"), width = 20, height = 15, units = "cm")
ggsave(n_col_plot, file=paste0("G:/R/Traisie-ABC/plots/mu/n_col_plot.png"), width = 20, height = 15, units = "cm")

set.seed(1)
gam <- runif(100,0,0.1)
data <-get_particles_combine (num_particles = 100,
                              obs_simulation = obs_sim,
                              replicates = 50,
                              var_param_name = "gam",
                              var_value = gam
)
save(data,file = "G:/R/Traisie-ABC/results/gam_ss.RData")
library(ggplot2)
spec_nltt_plot <- ggplot(data, aes(x = gam, y = spec_nltt)) +
  geom_point()
endemic_nltt_plot <- ggplot(data, aes(x = gam, y = endemic_nltt)) +
  geom_point()
nonendemic_nltt_plot <- ggplot(data, aes(x = gam, y = nonendemic_nltt)) +
  geom_point()
n_spec_plot <- ggplot(data, aes(x = gam, y = n_spec)) +
  geom_point()
n_col_plot <- ggplot(data, aes(x = gam, y = n_col)) +
  geom_point()
ggsave(spec_nltt_plot, file=paste0("G:/R/Traisie-ABC/plots/gam/spec_nltt_plot.png"), width = 20, height = 15, units = "cm")
ggsave(endemic_nltt_plot, file=paste0("G:/R/Traisie-ABC/plots/gam/endemic_nltt_plot.png"), width = 20, height = 15, units = "cm")
ggsave(nonendemic_nltt_plot, file=paste0("G:/R/Traisie-ABC/plots/gam/nonendemic_nltt_plot.png"), width = 20, height = 15, units = "cm")
ggsave(n_spec_plot, file=paste0("G:/R/Traisie-ABC/plots/gam/n_spec_plot.png"), width = 20, height = 15, units = "cm")
ggsave(n_col_plot, file=paste0("G:/R/Traisie-ABC/plots/gam/n_col_plot.png"), width = 20, height = 15, units = "cm")


set.seed(1)
laa <- runif(100,0,1)
data <-get_particles_combine (num_particles = 100,
                              obs_simulation = obs_sim,
                              replicates = 50,
                              var_param_name = "laa",
                              var_value = laa
)
save(data,file = "G:/R/Traisie-ABC/results/laa_ss.RData")
library(ggplot2)
spec_nltt_plot <- ggplot(data, aes(x = laa, y = spec_nltt)) +
  geom_point()
endemic_nltt_plot <- ggplot(data, aes(x = laa, y = endemic_nltt)) +
  geom_point()
nonendemic_nltt_plot <- ggplot(data, aes(x = laa, y = nonendemic_nltt)) +
  geom_point()
n_spec_plot <- ggplot(data, aes(x = laa, y = n_spec)) +
  geom_point()
n_col_plot <- ggplot(data, aes(x = laa, y = n_col)) +
  geom_point()
ggsave(spec_nltt_plot, file=paste0("G:/R/Traisie-ABC/plots/laa/spec_nltt_plot.png"), width = 20, height = 15, units = "cm")
ggsave(endemic_nltt_plot, file=paste0("G:/R/Traisie-ABC/plots/laa/endemic_nltt_plot.png"), width = 20, height = 15, units = "cm")
ggsave(nonendemic_nltt_plot, file=paste0("G:/R/Traisie-ABC/plots/laa/nonendemic_nltt_plot.png"), width = 20, height = 15, units = "cm")
ggsave(n_spec_plot, file=paste0("G:/R/Traisie-ABC/plots/laa/n_spec_plot.png"), width = 20, height = 15, units = "cm")
ggsave(n_col_plot, file=paste0("G:/R/Traisie-ABC/plots/laa/n_col_plot.png"), width = 20, height = 15, units = "cm")

