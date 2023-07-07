load("D:/Onedrive-shu/OneDrive/project 2/results/round5/secsse/secsse_2/obs_sims_secsse_ABC_test.rda")

test <- c()
for(i in 1:100){
  sim_1 <- obs_sim[[i]][[1]]
  phy1_1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                        tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
  phy1_2<-ape::keep.tip(sim_1$phy,  ## phy1 with only state1 tips
                        tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])

  test[i] <- all.equal(phy1_1,phy1_2)
}

library(ape)
tr <- ape::read.tree(text = "(A:1,(B:1,(C:1,(D:1,E:1):1):1):1:1);")
tr0 <-drop.tip(tr, c("A", "B"), root.edge = 0) # = (C:1,(D:1,E:1):1);
tr1 <-drop.tip(tr, c("A", "B"), root.edge = 1) # = (C:1,(D:1,E:1):1):1;
tr2 <-drop.tip(tr, c("A", "B"), root.edge = 2) # = (C:1,(D:1,E:1):1):2;
tr3 <-drop.tip(tr, c("A", "B"), root.edge = 3) # = (C:1,(D:1,E:1):1):3;

