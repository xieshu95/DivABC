
island = DAISIE::DAISIE_sim_constant_rate(
  time = 5,
  M = 1000,
  pars = c(0.4,0.2,20,0.008,0.1),
  replicates = 1,
  sample_freq  = 25,
  plot_sims = TRUE,
  verbose = TRUE,
  cond = 0
)
DAISIE::DAISIE_plot_island(island = island[[1]],island_age = 5)

set.seed(1)
island = DAISIE::DAISIE_sim_trait_dependent(
  time = 5,
  M = 500,
  pars = c(0.1,0.05,20,0.002,0.025),
  replicates = 1,
  sample_freq  = 25,
  plot_sims = TRUE,
  cond = 0,
  verbose = TRUE,
  trait_pars = DAISIE::create_trait_pars(clado_rate2 = 0.7,
                                         ext_rate2 = 0.35,
                                         immig_rate2 = 0.014,
                                         ana_rate2 = 0.175,
                                         trans_rate = 0,
                                         trans_rate2 = 0,
                                         M2 = 500))
DAISIE::DAISIE_plot_island(island = island[[1]],island_age = 5)


set.seed(1)
island = DAISIE::DAISIE_sim_trait_dependent(
  time = 5,
  M = 500,
  pars = c(0.1,0.2,20,0.008,0.1),
  replicates = 1,
  sample_freq  = 25,
  plot_sims = FALSE,
  cond = 0,
  verbose = TRUE,
  trait_pars = DAISIE::create_trait_pars(clado_rate2 = 0.7,
                                         ext_rate2 = 0.2,
                                         immig_rate2 = 0.008,
                                         ana_rate2 = 0.1,
                                         trans_rate = 0.5,
                                         trans_rate2 = 0.5,
                                         M2 = 500))
DAISIE::DAISIE_plot_island(island = island[[1]],island_age = 5)


