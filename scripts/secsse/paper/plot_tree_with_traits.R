## plot phylogenetic tree with tip states
library(ape)
library(ggtree)
library(nLTT)
# 1. generate the entire tree
t<-read.tree(text='(((H:1, G:1):1, (F:1, E:1):1):1 , ((D:1, C:1):1, (B:1, A:1):1):1):1;')
plot(t)

# trait states
trait1 <- c(1,0,1,0,1,0,1,0)
trait2 <- c(1,1,0,0,1,1,0,0)
trait3 <- c(1,0,0,0,1,1,1,0)
trait4 <- c(1,1,1,1,0,0,0,0)

# whole tree
trait_df = data.frame(
  species = t$tip.label,
  # Example of categorical traits
  trait_1 = as.factor(trait1),
  trait_2 = as.factor(trait2),
  trait_3 = as.factor(trait3),
  trait_4 = as.factor(trait4))

# 1. calulating D statistics
trait = data.frame(t$tip.label,trait1)
colnames(trait) <- c("tips","trait_val")
data <- caper::comparative.data(t, trait, tips)
PhyloD <- caper::phylo.d(data, binvar=trait_val,permut = 500)
PhyloD$DEstimate


trait = data.frame(t$tip.label,trait2)
colnames(trait) <- c("tips","trait_val")
data <- caper::comparative.data(t, trait, tips)
PhyloD <- caper::phylo.d(data, binvar=trait_val,permut = 500)
PhyloD$DEstimate


trait = data.frame(t$tip.label,trait3)
colnames(trait) <- c("tips","trait_val")
data <- caper::comparative.data(t, trait, tips)
PhyloD <- caper::phylo.d(data, binvar=trait_val,permut = 500)
PhyloD$DEstimate

# plot tree with three sets of states

plot_tree <- ggtree(t,right = TRUE, size = 0.5)+
  geom_tiplab()

# p<- plot_tree %<+% trait_df +
#   geom_tippoint(aes(x = x + 0.5,color = trait_1),size = 2) +
#   geom_tippoint(aes(x = x + 1,color = trait_2),size = 2) +
#   geom_tippoint(aes(x = x + 1.5,color = trait_3),size = 2) +
#   # geom_tippoint(aes(x = x + 2,color = trait_4),size = 2) +
#   ggplot2::scale_color_manual(name = "State",
#                               values = c("blue3","red3"),
#                               labels = c("0", "1"))

p<- plot_tree %<+% trait_df +
  geom_tippoint(aes(x = x + 0.5,fill = trait_1),color = "black",size = 3,shape = 21) +
  geom_tippoint(aes(x = x + 1,fill = trait_2),color = "black",size = 3,shape = 21) +
  geom_tippoint(aes(x = x + 1.5,fill = trait_3),color = "black",size = 3,shape = 21) +
  # geom_tippoint(aes(x = x + 2,fill = trait_4),color = "black",size = 3,shape = 21) +
  ggplot2::scale_fill_manual(name = "State",
                             values = c("white","black"),
                             labels = c("0", "1"))

p<- revts(p) +
  theme_tree2() +
  scale_x_continuous(breaks=c(-3:0), labels=(-3:0))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/tree_states.tiff"),
     units="px", width=3800, height=1800,res = 500,compression="lzw")
print(p)
while (!is.null(dev.list()))  dev.off()



tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/nltt_whole.tiff"),
     units="px", width=2200, height=2200,res = 500,compression="lzw")
nltt0 <- nltt_plot(t, col = "black")
while (!is.null(dev.list()))  dev.off()




p<- plot_tree %<+% trait_df +
  geom_tippoint(aes(x = x + 0.5,fill = trait_1),color = "black",size = 3,shape = 21) +
  # geom_tippoint(aes(x = x + 1,fill = trait_2),color = "black",size = 3,shape = 21) +
  # geom_tippoint(aes(x = x + 1.5,fill = trait_3),color = "black",size = 3,shape = 21) +
  # geom_tippoint(aes(x = x + 2,fill = trait_4),color = "black",size = 3,shape = 21) +
  ggplot2::scale_fill_manual(name = "State",
                             values = c("white","black"),
                             labels = c("0", "1"))

p<- revts(p) +
  theme_tree2() +
  scale_x_continuous(breaks=c(-3:0), labels=(-3:0))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/tree_state1.tiff"),
     units="px", width=3800, height=1800,res = 500,compression="lzw")
print(p)
while (!is.null(dev.list()))  dev.off()

p<- plot_tree %<+% trait_df +
  # geom_tippoint(aes(x = x + 0.5,fill = trait_1),color = "black",size = 3,shape = 21) +
  geom_tippoint(aes(x = x + 0.5,fill = trait_2),color = "black",size = 3,shape = 21) +
  # geom_tippoint(aes(x = x + 1.5,fill = trait_3),color = "black",size = 3,shape = 21) +
  # geom_tippoint(aes(x = x + 2,fill = trait_4),color = "black",size = 3,shape = 21) +
  ggplot2::scale_fill_manual(name = "State",
                             values = c("white","black"),
                             labels = c("0", "1"))

p<- revts(p) +
  theme_tree2() +
  scale_x_continuous(breaks=c(-3:0), labels=(-3:0))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/tree_state2.tiff"),
     units="px", width=3800, height=1800,res = 500,compression="lzw")
print(p)
while (!is.null(dev.list()))  dev.off()


p<- plot_tree %<+% trait_df +
  # geom_tippoint(aes(x = x + 0.5,fill = trait_1),color = "black",size = 3,shape = 21) +
  # geom_tippoint(aes(x = x + 0.5,fill = trait_2),color = "black",size = 3,shape = 21) +
  geom_tippoint(aes(x = x + 0.5,fill = trait_3),color = "black",size = 3,shape = 21) +
  # geom_tippoint(aes(x = x + 2,fill = trait_4),color = "black",size = 3,shape = 21) +
  ggplot2::scale_fill_manual(name = "State",
                             values = c("white","black"),
                             labels = c("0", "1"))

p<- revts(p) +
  theme_tree2() +
  scale_x_continuous(breaks=c(-3:0), labels=(-3:0))

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/tree_state3.tiff"),
     units="px", width=3800, height=1800,res = 500,compression="lzw")
print(p)
while (!is.null(dev.list()))  dev.off()


#####
# tree with trait1 (t1 means trait set 1)
phy0_t1<-ape::drop.tip(t,  ## phy1 with only state1 tips
                       tip = t$tip.label[which(trait1 == 1)])
phy1_t1<-ape::drop.tip(t,
                       tip = t$tip.label[which(trait1 == 0)])


plot(phy0_t1)
plot(phy1_t1)

trait_df0_t1 = data.frame(
  species = phy0_t1$tip.label,
  # Example of categorical traits
  trait = as.factor(c(0,0,0,0)))

trait_df1_t1 = data.frame(
  species = phy1_t1$tip.label,
  # Example of categorical traits
  trait = as.factor(c(1,1,1,1)))

plot_tree0_t1 <- ggtree(phy0_t1,right = TRUE, size = 0.5)+
  geom_tiplab()
plot_tree1_t1 <- ggtree(phy1_t1,right = TRUE, size = 0.5)+
  geom_tiplab()

p0_t1 <- plot_tree0_t1 %<+% trait_df0_t1 +
  geom_tippoint(aes(x = x + 0.5),fill = "white",color = "black",size = 3,shape = 21)

# p0_t1<- revts(p0_t1) +
#   theme_tree2() +
#   scale_x_continuous(breaks=c(-3:0), labels=(-3:0))

p1_t1 <- plot_tree1_t1 %<+% trait_df1_t1 +
  geom_tippoint(aes(x = x + 0.5),fill = "black",color = "black",size = 3,shape = 21)

p1_t1<- revts(p1_t1) +
  theme_tree2() +
  scale_x_continuous(breaks=c(-3:0), labels=(-3:0))

# p_t1 <- cowplot::plot_grid(
#   p0_t1,p1_t1,
#   align = "hv", nrow = 2, ncol = 1
# )

# tree with trait2 (t2 means trait set 2)
phy0_t2<-ape::drop.tip(t,  ## phy1 with only state1 tips
                       tip = t$tip.label[which(trait2 == 1)])
phy1_t2<-ape::drop.tip(t,
                       tip = t$tip.label[which(trait2 == 0)])
plot(phy0_t2)
plot(phy1_t2)

trait_df0_t2 = data.frame(
  species = phy0_t2$tip.label,
  # Example of categorical traits
  trait = as.factor(c(0,0,0,0)))

trait_df1_t2 = data.frame(
  species = phy1_t2$tip.label,
  # Example of categorical traits
  trait = as.factor(c(1,1,1,1)))

plot_tree0_t2 <- ggtree(phy0_t2,right = TRUE, size = 0.5)+
  geom_tiplab()
plot_tree1_t2 <- ggtree(phy1_t2,right = TRUE, size = 0.5)+
  geom_tiplab()

p0_t2 <- plot_tree0_t2 %<+% trait_df0_t2 +
  geom_tippoint(aes(x = x + 0.5),fill = "white",color = "black",size = 3,shape = 21)

# p0_t2<- revts(p0_t2) +
#   theme_tree2() +
#   scale_x_continuous(breaks=c(-3:0), labels=(-3:0))

p1_t2 <- plot_tree1_t2 %<+% trait_df1_t2 +
  geom_tippoint(aes(x = x + 0.5),fill = "black",color = "black",size = 3,shape = 21)
p1_t2<- revts(p1_t2) +
  theme_tree2() +
  scale_x_continuous(breaks=c(-3:0), labels=(-3:0))

# p_t1 <- cowplot::plot_grid(
#   p0_t1,p1_t1,
#   align = "hv", nrow = 2, ncol = 1
# )

# tree with trait3 (t3 means trait set 3)
phy0_t3<-ape::drop.tip(t,  ## phy1 with only state1 tips
                       tip = t$tip.label[which(trait3 == 1)])
phy1_t3<-ape::drop.tip(t,
                       tip = t$tip.label[which(trait3 == 0)])
plot(phy0_t3)
plot(phy1_t3)

trait_df0_t3 = data.frame(
  species = phy0_t3$tip.label,
  # Example of categorical traits
  trait = as.factor(c(0,0,0,0)))

trait_df1_t3 = data.frame(
  species = phy1_t3$tip.label,
  # Example of categorical traits
  trait = as.factor(c(1,1,1,1)))

plot_tree0_t3 <- ggtree(phy0_t3,right = TRUE, size = 0.5)+
  geom_tiplab()
plot_tree1_t3 <- ggtree(phy1_t3,right = TRUE, size = 0.5)+
  geom_tiplab()

p0_t3 <- plot_tree0_t3 %<+% trait_df0_t3 +
  geom_tippoint(aes(x = x + 0.5),fill = "white",color = "black",size = 3,shape = 21)

# p0_t3<- revts(p0_t3) +
#   theme_tree2() +
#   scale_x_continuous(breaks=c(-3:0), labels=(-3:0))
#

p1_t3 <- plot_tree1_t3 %<+% trait_df1_t3 +
  geom_tippoint(aes(x = x + 0.5),fill = "black",color = "black",size = 3,shape = 21)

p1_t3<- revts(p1_t3) +
  theme_tree2() +
  scale_x_continuous(breaks=c(-3:0), labels=(-3:0))

p1 <- cowplot::plot_grid(
  p0_t1,p0_t2,p0_t3,p1_t1,p1_t2,p1_t3,
  align = "hv", ncol = 3
)


tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/tree_state_sep.tiff"),
     units="px", width=3800, height=1800,res = 500,compression="lzw")
print(p1)
while (!is.null(dev.list()))  dev.off()


## nltt plots

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/nltt_state0.tiff"),
     units="px", width=2200, height=2200,res = 500,compression="lzw")
nltt0 <- nltt_plot(phy0_t1, col = "red3")
nltt_lines(phy0_t2, col = "green3")
nltt_lines(phy0_t3, col = "blue3")
legend("topleft", c("Tree1", "Tree2", "Tree3"), col = c("red3", "green3","blue3"), lty = 1)
while (!is.null(dev.list()))  dev.off()



tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_final/nltt_state1.tiff"),
     units="px", width=2200, height=2200,res = 500,compression="lzw")
nltt1 <- nltt_plot(phy1_t1, col = "red3")
nltt_lines(phy1_t2, col = "green3")
nltt_lines(phy1_t3, col = "blue3")
legend("topleft", c("Tree1", "Tree2", "Tree3"), col = c("red3", "green3","blue3"), lty = 1)
while (!is.null(dev.list()))  dev.off()


##### buchong

ltt.plot(t)
nltt_plot(phy1, col = "red")
nltt_lines(phy2, col = "black")
nltt_lines(t, col = "blue")
legend("topleft", c("tree1", "tree2"), col = c("red", "blue"), lty = 1)




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
plot_tree %<+% trait_df +
  geom_tippoint(aes(x = x + 1.5,color = trait),size = 2) +
  geom_tippoint(aes(x = x + 1.5,color = trait),size = 2) +
  geom_tippoint(aes(x = x + 1.5,color = trait),size = 2) +


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
