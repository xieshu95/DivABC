## plot a state-dependent tree (but just one phylogeny)
library(diversitree)


pars <- c(0.1, 0.01, 0.01, 0.01, 0.01, 0.1)
set.seed(8)

phy <- trees(pars,
             "bisse",
             max.taxa = 15,
             max.t = Inf,
             x0 = 0)[[1]]

h <- history.from.sim.discrete(phy, 0:1)

# h$history$sp43<-rbind(h$history$sp43,c(17.1,1),c(19,0),c(22,1))

lik <- make.mk2(phy, phy$tip.state)
st.m <- asr.marginal(lik, pars[5:6])

plot(h, phy, main = "Marginal ASR", show.node.state = TRUE)

##切割 tree
require(geiger)
library(phytools)
trees <- treeSlice(phy,slice = 0.01)
plot(trees,root.edge = T)


