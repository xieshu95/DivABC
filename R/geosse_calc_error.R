#' Calculates error metrics between two simulations under geosse model
#' ss0 NLTTs_Delta
calc_error_geosse_D<- function(sim_1,
                              sim_2,
                              distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::keep.tip(sim_1,  ## phy1 with only tips with state0/stateAB
                         tip = sim_1$tip.label[which(sim_1$tip.state == 0)])
  phy1_s2<-ape::keep.tip(sim_1,
                         tip = sim_1$tip.label[which(sim_1$tip.state == 1)])
  phy1_s3<-ape::keep.tip(sim_1,
                         tip = sim_1$tip.label[which(sim_1$tip.state == 2)])

  phy2_s1<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 0)])
  phy2_s2<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 1)])
  phy2_s3<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 2)])
  # nLTT
  nltt <- treestats::nLTT(sim_1,sim_2)
  nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
  nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)
  nltt_s3 <- treestats::nLTT(phy1_s3,phy2_s3)

  # drop one state
  phy1_s23<-ape::drop.tip(sim_1,  ## phy1 with state 2&3
                          tip = sim_1$tip.label[which(sim_1$tip.state == 0)])
  phy1_s13<-ape::drop.tip(sim_1,  ## phy1 with state 1&3
                          tip = sim_1$tip.label[which(sim_1$tip.state == 1)])
  phy1_s12<-ape::drop.tip(sim_1,  ## phy1 with state 1&2
                          tip = sim_1$tip.label[which(sim_1$tip.state == 2)])

  phy2_s23<-ape::drop.tip(sim_2,
                          tip = sim_2$tip.label[which(sim_2$tip.state == 0)])
  phy2_s13<-ape::drop.tip(sim_2,
                          tip = sim_2$tip.label[which(sim_2$tip.state == 1)])
  phy2_s12<-ape::drop.tip(sim_2,
                          tip = sim_2$tip.label[which(sim_2$tip.state == 2)])

  D12 <- abs(calc_D_drop(phy1_s12,sim_1$tip.state[-which(sim_1$tip.state == "2")])
             - calc_D_drop(phy2_s12,sim_2$tip.state[-which(sim_2$tip.state == "2")]))

  D13 <- abs(calc_D_drop(phy1_s13,sim_1$tip.state[-which(sim_1$tip.state == "1")])
             - calc_D_drop(phy2_s13,sim_2$tip.state[-which(sim_2$tip.state == "1")]))

  D23 <- abs(calc_D_drop(phy1_s23,sim_1$tip.state[-which(sim_1$tip.state == "0")])
             - calc_D_drop(phy2_s23,sim_2$tip.state[-which(sim_2$tip.state == "0")]))


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


#' ss1 NLTTs_Phylosig M
calc_error_geosse_M <- function(sim_1,
                               sim_2,
                               distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::keep.tip(sim_1,  ## phy1 with only state1 tips
                         tip = sim_1$tip.label[which(sim_1$tip.state == 0)])
  phy1_s2<-ape::keep.tip(sim_1,
                         tip = sim_1$tip.label[which(sim_1$tip.state == 1)])
  phy1_s3<-ape::keep.tip(sim_1,
                         tip = sim_1$tip.label[which(sim_1$tip.state == 2)])

  phy2_s1<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 0)])
  phy2_s2<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 1)])
  phy2_s3<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 2)])

  # phylogenetic signal M statistic
  M <- abs(calc_M_geosse(sim_1) - calc_M_geosse(sim_2))

  # nLTT
  nltt <- treestats::nLTT(sim_1,sim_2)
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


#' ss2 calculate NLTT statistics + transfered D statistic (e.g. S1 VS Sothers)
calc_error_geosse_D_trans<- function(sim_1,
                                    sim_2,
                                    distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::keep.tip(sim_1,  ## phy1 with only state1 tips
                         tip = sim_1$tip.label[which(sim_1$tip.state == 0)])
  phy1_s2<-ape::keep.tip(sim_1,
                         tip = sim_1$tip.label[which(sim_1$tip.state == 1)])
  phy1_s3<-ape::keep.tip(sim_1,
                         tip = sim_1$tip.label[which(sim_1$tip.state == 2)])
  phy2_s1<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 0)])
  phy2_s2<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 1)])
  phy2_s3<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 2)])
  # nLTT
  nltt <- treestats::nLTT(sim_1,sim_2)
  nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
  nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)
  nltt_s3 <- treestats::nLTT(phy1_s3,phy2_s3)

  # transfer to two states
  D1_23 <- abs(calc_D_trans_geosse(sim_1,"0") - calc_D_trans_geosse(sim_2,"0"))
  D2_13 <- abs(calc_D_trans_geosse(sim_1,"1") - calc_D_trans_geosse(sim_2,"1"))
  D3_12 <- abs(calc_D_trans_geosse(sim_1,"2") - calc_D_trans_geosse(sim_2,"2"))

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

#' ss3 Calculates error metrics between two simulations under geosse model
#' NLTTs_Delta
calc_error_geosse_Ddrop_M<- function(sim_1,
                               sim_2,
                               distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::keep.tip(sim_1,  ## phy1 with only tips with state0/stateAB
                         tip = sim_1$tip.label[which(sim_1$tip.state == 0)])
  phy1_s2<-ape::keep.tip(sim_1,
                         tip = sim_1$tip.label[which(sim_1$tip.state == 1)])
  phy1_s3<-ape::keep.tip(sim_1,
                         tip = sim_1$tip.label[which(sim_1$tip.state == 2)])

  phy2_s1<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 0)])
  phy2_s2<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 1)])
  phy2_s3<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 2)])
  # nLTT
  nltt <- treestats::nLTT(sim_1,sim_2)
  nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
  nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)
  nltt_s3 <- treestats::nLTT(phy1_s3,phy2_s3)

  # drop one state
  phy1_s23<-ape::drop.tip(sim_1,  ## phy1 with state 2&3
                          tip = sim_1$tip.label[which(sim_1$tip.state == 0)])
  phy1_s13<-ape::drop.tip(sim_1,  ## phy1 with state 1&3
                          tip = sim_1$tip.label[which(sim_1$tip.state == 1)])
  phy1_s12<-ape::drop.tip(sim_1,  ## phy1 with state 1&2
                          tip = sim_1$tip.label[which(sim_1$tip.state == 2)])

  phy2_s23<-ape::drop.tip(sim_2,
                          tip = sim_2$tip.label[which(sim_2$tip.state == 0)])
  phy2_s13<-ape::drop.tip(sim_2,
                          tip = sim_2$tip.label[which(sim_2$tip.state == 1)])
  phy2_s12<-ape::drop.tip(sim_2,
                          tip = sim_2$tip.label[which(sim_2$tip.state == 2)])

  D12 <- abs(calc_D_drop(phy1_s12,sim_1$tip.state[-which(sim_1$tip.state == "2")])
             - calc_D_drop(phy2_s12,sim_2$tip.state[-which(sim_2$tip.state == "2")]))

  D13 <- abs(calc_D_drop(phy1_s13,sim_1$tip.state[-which(sim_1$tip.state == "1")])
             - calc_D_drop(phy2_s13,sim_2$tip.state[-which(sim_2$tip.state == "1")]))

  D23 <- abs(calc_D_drop(phy1_s23,sim_1$tip.state[-which(sim_1$tip.state == "0")])
             - calc_D_drop(phy2_s23,sim_2$tip.state[-which(sim_2$tip.state == "0")]))
  M <- abs(calc_M_geosse(sim_1) - calc_M_geosse(sim_2))

  return(
    c(nltt,
      nltt_s1,
      nltt_s2,
      nltt_s3,
      D12,
      D13,
      D23,
      M
    )
  )
}

#' ss4 calculate NLTT statistics + transfered D statistic (e.g. S1 VS Sothers)
calc_error_geosse_Dtrans_M<- function(sim_1,
                                     sim_2,
                                     distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::keep.tip(sim_1,  ## phy1 with only state1 tips
                         tip = sim_1$tip.label[which(sim_1$tip.state == 0)])
  phy1_s2<-ape::keep.tip(sim_1,
                         tip = sim_1$tip.label[which(sim_1$tip.state == 1)])
  phy1_s3<-ape::keep.tip(sim_1,
                         tip = sim_1$tip.label[which(sim_1$tip.state == 2)])
  phy2_s1<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 0)])
  phy2_s2<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 1)])
  phy2_s3<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 2)])
  # nLTT
  nltt <- treestats::nLTT(sim_1,sim_2)
  nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
  nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)
  nltt_s3 <- treestats::nLTT(phy1_s3,phy2_s3)

  # transfer to two states
  D1_23 <- abs(calc_D_trans_geosse(sim_1,"0") - calc_D_trans_geosse(sim_2,"0"))
  D2_13 <- abs(calc_D_trans_geosse(sim_1,"1") - calc_D_trans_geosse(sim_2,"1"))
  D3_12 <- abs(calc_D_trans_geosse(sim_1,"2") - calc_D_trans_geosse(sim_2,"2"))

  M <- abs(calc_M_geosse(sim_1) - calc_M_geosse(sim_2))

  return(
    c(nltt,
      nltt_s1,
      nltt_s2,
      nltt_s3,
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
  PhyloD <- caper::phylo.d(data, binvar=trait_val,permut = 100)
  return(as.numeric(PhyloD$DEstimate))
}

#' function to calculate statistic D when only keep a single state and transfer all
#' the tips with the other states to "0"
calc_D_trans_geosse <- function (sim,keep_state) {
  trait = data.frame(sim$tip.label,sim$tip.state)
  trait$sim.tip.state[which(trait$sim.tip.state != keep_state)] = "4"
  colnames(trait) <- c("tips","trait_val")
  data <- caper::comparative.data(sim, trait, tips)
  PhyloD <- caper::phylo.d(data, binvar=trait_val,permut = 100)
  return(as.numeric(PhyloD$DEstimate))
}


calc_M_geosse <- function (sim) {
  trait_df <- data.frame(B1 =as.factor(sim$tip.state), row.names = sim$tip.label)
  trait_dist <- phylosignalDB::gower_dist(x = trait_df,  type = list(factor = 1))
  # phyloM <- phylosignalDB::phylosignal_M(trait_dist, phy = sim, reps = 120, output_M_permuted = TRUE)
  phyloM <- mean(phylosignalDB::phylosignal_M(trait_dist, phy = sim, reps = 100, output_M_permuted = TRUE)$M_permuted)
  return(as.numeric(phyloM))
}


#' ss5 NLTTs_Delta
calc_error_geosse_all<- function(sim_1,
                                 sim_2,
                                 distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::keep.tip(sim_1,  ## phy1 with only tips with state0/stateAB
                         tip = sim_1$tip.label[which(sim_1$tip.state == 0)])
  phy1_s2<-ape::keep.tip(sim_1,
                         tip = sim_1$tip.label[which(sim_1$tip.state == 1)])
  phy1_s3<-ape::keep.tip(sim_1,
                         tip = sim_1$tip.label[which(sim_1$tip.state == 2)])

  phy2_s1<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 0)])
  phy2_s2<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 1)])
  phy2_s3<-ape::keep.tip(sim_2,
                         tip = sim_2$tip.label[which(sim_2$tip.state == 2)])
  # nLTT
  nltt <- treestats::nLTT(sim_1,sim_2)
  nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
  nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)
  nltt_s3 <- treestats::nLTT(phy1_s3,phy2_s3)



  # drop one state
  phy1_s23<-ape::drop.tip(sim_1,  ## phy1 with state 2&3
                          tip = sim_1$tip.label[which(sim_1$tip.state == 0)])
  phy1_s13<-ape::drop.tip(sim_1,  ## phy1 with state 1&3
                          tip = sim_1$tip.label[which(sim_1$tip.state == 1)])
  phy1_s12<-ape::drop.tip(sim_1,  ## phy1 with state 1&2
                          tip = sim_1$tip.label[which(sim_1$tip.state == 2)])

  phy2_s23<-ape::drop.tip(sim_2,
                          tip = sim_2$tip.label[which(sim_2$tip.state == 0)])
  phy2_s13<-ape::drop.tip(sim_2,
                          tip = sim_2$tip.label[which(sim_2$tip.state == 1)])
  phy2_s12<-ape::drop.tip(sim_2,
                          tip = sim_2$tip.label[which(sim_2$tip.state == 2)])

  D12 <- abs(calc_D_drop(phy1_s12,sim_1$tip.state[-which(sim_1$tip.state == "2")])
             - calc_D_drop(phy2_s12,sim_2$tip.state[-which(sim_2$tip.state == "2")]))

  D13 <- abs(calc_D_drop(phy1_s13,sim_1$tip.state[-which(sim_1$tip.state == "1")])
             - calc_D_drop(phy2_s13,sim_2$tip.state[-which(sim_2$tip.state == "1")]))

  D23 <- abs(calc_D_drop(phy1_s23,sim_1$tip.state[-which(sim_1$tip.state == "0")])
             - calc_D_drop(phy2_s23,sim_2$tip.state[-which(sim_2$tip.state == "0")]))

  # transfer to two states
  D1_23 <- abs(calc_D_trans_geosse(sim_1,"0") - calc_D_trans_geosse(sim_2,"0"))
  D2_13 <- abs(calc_D_trans_geosse(sim_1,"1") - calc_D_trans_geosse(sim_2,"1"))
  D3_12 <- abs(calc_D_trans_geosse(sim_1,"2") - calc_D_trans_geosse(sim_2,"2"))

  M <- abs(calc_M_geosse(sim_1) - calc_M_geosse(sim_2))
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

calc_ss_geosse <- function(sim) {
  # keep only one state
  phy_s1<-ape::keep.tip(sim,  ## phy with only tips with state0/stateAB
                        tip = sim$tip.label[which(sim$tip.state == 0)])
  phy_s2<-ape::keep.tip(sim,
                        tip = sim$tip.label[which(sim$tip.state == 1)])
  phy_s3<-ape::keep.tip(sim,
                        tip = sim$tip.label[which(sim$tip.state == 2)])

  # drop only one state
  phy_s23<-ape::drop.tip(sim,  ## phy with state 2&3
                         tip = sim$tip.label[which(sim$tip.state == 0)])
  phy_s13<-ape::drop.tip(sim,  ## phy with state 1&3
                         tip = sim$tip.label[which(sim$tip.state == 1)])
  phy_s12<-ape::drop.tip(sim,  ## phy with state 1&2
                         tip = sim$tip.label[which(sim$tip.state == 2)])
  # mpd_all
  mpd_all <- treestats::mean_pair_dist(phy = sim)
  mpd_s1 <- treestats::mean_pair_dist(phy = phy_s1)
  mpd_s2 <- treestats::mean_pair_dist(phy = phy_s2)
  mpd_s3 <- treestats::mean_pair_dist(phy = phy_s3)
  # mpd_diff <- calc_mpd_trait(sim = sim,state_type = 0)

  # mntd_all
  mntd_all <- treestats::mntd(phy = sim)
  mntd_s1 <- treestats::mntd(phy = phy_s1)
  mntd_s2 <- treestats::mntd(phy = phy_s2)
  mntd_s3 <- treestats::mntd(phy = phy_s3)

  # # D statistic
  # D <- calc_D(sim)

  # state 1
  num_state1 <- length(which(sim$tip.state == 0))
  num_state2 <- length(which(sim$tip.state == 1))
  num_state3 <- length(which(sim$tip.state == 2))
  total_spec <- num_state1 + num_state2 + num_state3
  ratio_state1 <- length(which(sim$tip.state == 0))/total_spec
  ratio_state2 <- length(which(sim$tip.state == 1))/total_spec
  ratio_state3 <- length(which(sim$tip.state == 2))/total_spec

  # tip_ratio <- max(num_state1,num_state2)/min(num_state1,num_state2)

  M <- calc_M_geosse(sim)

  # nLTT
  nltt <- treestats::nLTT_base(sim)
  nltt1 <- treestats::nLTT_base(phy_s1)
  nltt2 <- treestats::nLTT_base(phy_s2)
  nltt3 <- treestats::nLTT_base(phy_s3)

  D12 <- calc_D_drop(phy_s12,sim$tip.state[-which(sim$tip.state == "2")])
  D13 <- calc_D_drop(phy_s13,sim$tip.state[-which(sim$tip.state == "1")])
  D23 <- calc_D_drop(phy_s23,sim$tip.state[-which(sim$tip.state == "0")])

  # transfer to two states
  D1_23 <- calc_D_trans_geosse(sim,"0")
  D2_13 <- calc_D_trans_geosse(sim,"1")
  D3_12 <- calc_D_trans_geosse(sim,"2")


  return(
    list(total_spec = total_spec,
         state1 = ratio_state1,
         state2 = ratio_state2,
         state3 = ratio_state3,
         mpd_all = mpd_all,
         mpd_s1 = mpd_s1,
         mpd_s2 = mpd_s2,
         mpd_s3 = mpd_s3,
         mntd_all = mntd_all,
         mntd_s1 = mntd_s1,
         mntd_s2 = mntd_s2,
         mntd_s3 = mntd_s3,
         nltt = nltt,
         nltt1 = nltt1,
         nltt2 = nltt2,
         nltt3 = nltt3,
         D12 = D12,
         D13 = D13,
         D23 = D23,
         D1_23 = D1_23,
         D2_13 = D2_13,
         D3_12 = D3_12,
         M = M)
  )
}
