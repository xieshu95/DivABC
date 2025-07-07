#' Calculates error metrics between two simulations under musse model
#' NLTTs_Delta
calc_error_musse_D<- function(sim_1,
                                   sim_2,
                                   distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2 | sim_1$obs_traits == 3)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1 | sim_1$obs_traits == 3)])
  phy1_s3<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1 | sim_1$obs_traits == 2)])

  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2 | sim_2$obs_traits == 3)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1 | sim_2$obs_traits == 3)])
  phy2_s3<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1 | sim_2$obs_traits == 2)])
  # nLTT
  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)
  nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
  nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)
  nltt_s3 <- treestats::nLTT(phy1_s3,phy2_s3)

  # drop one state
  phy1_s23<-ape::drop.tip(sim_1$phy,  ## phy1 with state 2&3
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])
  phy1_s13<-ape::drop.tip(sim_1$phy,  ## phy1 with state 1&3
                          tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
  phy1_s12<-ape::drop.tip(sim_1$phy,  ## phy1 with state 1&2
                          tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 3)])

  phy2_s23<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1)])
  phy2_s13<-ape::drop.tip(sim_2$phy,
                          tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2)])
  phy2_s12<-ape::drop.tip(sim_2$phy,
                          tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 3)])

  D12 <- abs(calc_D_drop(phy1_s12,sim_1$obs_traits[-which(sim_1$obs_traits == "3")])
             - calc_D_drop(phy2_s12,sim_2$obs_traits[-which(sim_2$obs_traits == "3")]))

  D13 <- abs(calc_D_drop(phy1_s13,sim_1$obs_traits[-which(sim_1$obs_traits == "2")])
             - calc_D_drop(phy2_s13,sim_2$obs_traits[-which(sim_2$obs_traits == "2")]))

  D23 <- abs(calc_D_drop(phy1_s23,sim_1$obs_traits[-which(sim_1$obs_traits == "1")])
           - calc_D_drop(phy2_s23,sim_2$obs_traits[-which(sim_2$obs_traits == "1")]))


  return(
    c(nltt,
      nltt_s1,
      nltt_s2,
      nltt_s3,
      D12,
      D13,
      D23
    )
  )
}




#' NLTTs_Phylosig M
calc_error_musse_M <- function(sim_1,
                               sim_2,
                               distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2 | sim_1$obs_traits == 3)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1 | sim_1$obs_traits == 3)])
  phy1_s3<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1 | sim_1$obs_traits == 2)])

  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2 | sim_2$obs_traits == 3)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1 | sim_2$obs_traits == 3)])
  phy2_s3<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1 | sim_2$obs_traits == 2)])

  # phylogenetic signal M statistic
  M1 <- calc_M(sim_1)
  M2 <- calc_M(sim_2)
  M <- abs (M1 - M2)

  # nLTT
  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)
  nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
  nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)
  nltt_s3 <- treestats::nLTT(phy1_s3,phy2_s3)

  return(
    c(nltt,
      nltt_s1,
      nltt_s2,
      nltt_s3,
      M
    )
  )
}


#' NLTTs_Phylosig M
# calc_error_musse_M_Msep <- function(sim_1,
#                                sim_2,
#                                distance_method = "abs") {
#
#   # drop tips and only keep tips with a single state(1/2)
#   phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
#                          tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2 | sim_1$obs_traits == 3)])
#   phy1_s2<-ape::drop.tip(sim_1$phy,
#                          tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1 | sim_1$obs_traits == 3)])
#   phy1_s3<-ape::drop.tip(sim_1$phy,
#                          tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1 | sim_1$obs_traits == 2)])
#
#   phy2_s1<-ape::drop.tip(sim_2$phy,
#                          tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2 | sim_2$obs_traits == 3)])
#   phy2_s2<-ape::drop.tip(sim_2$phy,
#                          tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1 | sim_2$obs_traits == 3)])
#   phy2_s3<-ape::drop.tip(sim_2$phy,
#                          tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1 | sim_2$obs_traits == 2)])
#
#   # phylogenetic signal Delta statistic
#   M1 <- calc_M(sim_1)
#   M2 <- calc_M(sim_2)
#   M <- abs (M1 - M2)
#
#   # phy1_s12<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
#   #                        tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 3)])
#   # phy2_s12<-ape::drop.tip(sim_2$phy,
#   #                        tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 3)])
#
#
#   # nLTT
#   nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)
#   nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
#   nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)
#   nltt_s3 <- treestats::nLTT(phy1_s3,phy2_s3)
#
#   return(
#     c(nltt,
#       nltt_s1,
#       nltt_s2,
#       nltt_s3,
#       M
#     )
#   )
# }

#' calculate NLTT statistics + transfered D statistic (e.g. S1 VS Sothers)
calc_error_musse_D_trans<- function(sim_1,
                              sim_2,
                              distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2 | sim_1$obs_traits == 3)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1 | sim_1$obs_traits == 3)])
  phy1_s3<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1 | sim_1$obs_traits == 2)])
  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2 | sim_2$obs_traits == 3)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1 | sim_2$obs_traits == 3)])
  phy2_s3<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1 | sim_2$obs_traits == 2)])
  # nLTT
  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)
  nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
  nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)
  nltt_s3 <- treestats::nLTT(phy1_s3,phy2_s3)

  # transfer to two states
  D1_23 <- abs(calc_D_trans(sim_1,"1") - calc_D_trans(sim_2,"1"))
  D2_13 <- abs(calc_D_trans(sim_1,"2") - calc_D_trans(sim_2,"2"))
  D3_12 <- abs(calc_D_trans(sim_1,"3") - calc_D_trans(sim_2,"3"))

  return(
    c(nltt,
      nltt_s1,
      nltt_s2,
      nltt_s3,
      D1_23,
      D2_13,
      D3_12
    )
  )
}



#' NLTTs_Delta
calc_error_musse_all<- function(sim_1,
                              sim_2,
                              distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2 | sim_1$obs_traits == 3)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1 | sim_1$obs_traits == 3)])
  phy1_s3<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1 | sim_1$obs_traits == 2)])

  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2 | sim_2$obs_traits == 3)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1 | sim_2$obs_traits == 3)])
  phy2_s3<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1 | sim_2$obs_traits == 2)])
  # nLTT
  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)
  nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
  nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)
  nltt_s3 <- treestats::nLTT(phy1_s3,phy2_s3)



  # drop one state
  phy1_s23<-ape::drop.tip(sim_1$phy,  ## phy1 with state 2&3
                          tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])
  phy1_s13<-ape::drop.tip(sim_1$phy,  ## phy1 with state 1&3
                          tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
  phy1_s12<-ape::drop.tip(sim_1$phy,  ## phy1 with state 1&2
                          tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 3)])

  phy2_s23<-ape::drop.tip(sim_2$phy,
                          tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1)])
  phy2_s13<-ape::drop.tip(sim_2$phy,
                          tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2)])
  phy2_s12<-ape::drop.tip(sim_2$phy,
                          tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 3)])

  D12 <- abs(calc_D_drop(phy1_s12,sim_1$obs_traits[-which(sim_1$obs_traits == "3")])
             - calc_D_drop(phy2_s12,sim_2$obs_traits[-which(sim_2$obs_traits == "3")]))

  D13 <- abs(calc_D_drop(phy1_s13,sim_1$obs_traits[-which(sim_1$obs_traits == "2")])
             - calc_D_drop(phy2_s13,sim_2$obs_traits[-which(sim_2$obs_traits == "2")]))

  D23 <- abs(calc_D_drop(phy1_s23,sim_1$obs_traits[-which(sim_1$obs_traits == "1")])
             - calc_D_drop(phy2_s23,sim_2$obs_traits[-which(sim_2$obs_traits == "1")]))

  # transfer to two states
  D1_23 <- abs(calc_D_trans(sim_1,"1") - calc_D_trans(sim_2,"1"))
  D2_13 <- abs(calc_D_trans(sim_1,"2") - calc_D_trans(sim_2,"2"))
  D3_12 <- abs(calc_D_trans(sim_1,"3") - calc_D_trans(sim_2,"3"))

  M <- abs(calc_M(sim_1) - calc_M(sim_2))
  return(
    c(nltt,
      nltt_s1,
      nltt_s2,
      nltt_s3,
      D12,
      D13,
      D23,
      D1_23,
      D2_13,
      D3_12,
      M
    )
  )
}

#' function to calculate statistic D when drop tips until only two states exist
calc_D_drop <- function (phy,obs_trait) {
  trait = data.frame(phy$tip.label,obs_trait)
  colnames(trait) <- c("tips","trait_val")
  data <- caper::comparative.data(phy, trait, tips)
  PhyloD <- caper::phylo.d(data, binvar=trait_val,permut = 200)
  return(as.numeric(PhyloD$DEstimate))
}

#' function to calculate statistic D when only keep a single state and transfer all
#' the tips with the other states to "0"
calc_D_trans <- function (sim,keep_state) {
  trait = data.frame(sim$phy$tip.label,sim$obs_traits)
  trait$sim.obs_traits[which(trait$sim.obs_traits != keep_state)] = "0"
  colnames(trait) <- c("tips","trait_val")
  data <- caper::comparative.data(sim$phy, trait, tips)
  PhyloD <- caper::phylo.d(data, binvar=trait_val,permut = 200)
  return(as.numeric(PhyloD$DEstimate))
}
