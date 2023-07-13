library(ape)
library(ggtree)
library(nLTT)

# Generate Tree and Traits -----------------------------------------------------
# examples
# Number of species
# n_species = 10
# # Generate tree with 10 species
# random_tree = rtree(n_species)
# plot(random_tree)
# # Generate trait matrix
# trait_df = data.frame(
#   species = random_tree$tip.label,
#   trait1 = rnorm(n_species), # Example of continuous trait
#   # Example of categorical traits
#   trait2 = sample(c("A", "B", "C"), replace = T, size = n_species))
#
#
# # Plotting tree ----------------------------------------------------------------
# plot_tree = ggtree(random_tree, layout = "fan", right = TRUE, size = 0.1)
#
# # Add trait information into tree
# plot_tree %<+% trait_df + geom_tippoint(aes(color = trait2, alpha = 0.5))


## secsse--plot traits at tips
obs_sim_pars <- c(0.1,0.1,0,0,0.05,0.05)
set.seed(1)
sim_1 <- get_secsse_sim(
  parameters = as.numeric(obs_sim_pars),
  pool_init_states = NULL,
  replicates = 1)[[1]]


##
load("D:/Onedrive-shu/OneDrive/project 2/secsse_tips.RData")
plot(sim_1$phy)
phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                       tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
phy1_s2<-ape::drop.tip(sim_1$phy,
                       tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])
plot(phy1_s1)
plot(phy1_s2)

# whole tree
trait_df = data.frame(
  species = sim_1$phy$tip.label,
  # Example of categorical traits
  trait = as.factor(sim_1$obs_traits))

plot_tree <- ggtree(sim_1$phy,right = TRUE, size = 0.1)+
  geom_tiplab()
plot_tree %<+% trait_df + geom_tippoint(aes(color = trait))
# plot ltt
ltt.plot(sim_1$phy)

# phy_1
trait_df = data.frame(
  species = phy1_s1$tip.label,
  # Example of categorical traits
  trait = rep("1",length(phy1_s1$tip.label)))

plot_tree <- ggtree(phy1_s1,right = TRUE, size = 0.1)+
  geom_tiplab()
plot_tree %<+% trait_df + geom_tippoint(aes(color = trait))
ltt.plot(sim_1$phy)
nltt_plot(sim_1$phy, col = "red")

# phy_2
trait_df = data.frame(
  species = phy1_s1$tip.label,
  # Example of categorical traits
  trait = rep("1",length(phy1_s1$tip.label)))

plot_tree <- ggtree(phy1_s1,right = TRUE, size = 0.1)+
  geom_tiplab()
plot_tree %<+% trait_df + geom_tippoint(aes(color = trait))
ltt.plot(sim_1$phy)
