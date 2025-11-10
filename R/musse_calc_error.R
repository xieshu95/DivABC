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


calc_ss_musse <- function(sim) {
  # mpd_all
  mpd_all <- treestats::mean_pair_dist(phy = sim$phy)
  mpd_s1 <- calc_mpd_trait(sim = sim,state_type = 1)
  mpd_s2 <- calc_mpd_trait(sim = sim,state_type = 2)
  mpd_s3 <- calc_mpd_trait(sim = sim,state_type = 3)
  # mpd_diff <- calc_mpd_trait(sim = sim,state_type = 0)

  # mntd_all
  mntd_all <- treestats::mntd(phy = sim$phy)
  mntd_s1 <- calc_mntd_trait(sim = sim,state_type = 1)
  mntd_s2 <- calc_mntd_trait(sim = sim,state_type = 2)
  mntd_s3 <- calc_mntd_trait(sim = sim,state_type = 3)

  # K statistic
  K <- adiv::K(sim$phy,
               trait = as.numeric(sim$obs_traits),
               nrep = 1000, alter = c("two-sided"))
  K <- K$obs

  # # D statistic
  # D <- calc_D(sim)

  # state 1
  num_state1 <- length(which(sim$obs_traits == 1))
  num_state2 <- length(which(sim$obs_traits == 2))
  num_state3 <- length(which(sim$obs_traits == 3))
  total_spec <- num_state1 + num_state2 + num_state3
  ratio_state1 <- length(which(sim$obs_traits == 1))/total_spec
  ratio_state2 <- length(which(sim$obs_traits == 2))/total_spec
  ratio_state3 <- length(which(sim$obs_traits == 3))/total_spec

  # tip_ratio <- max(num_state1,num_state2)/min(num_state1,num_state2)


  M <- calc_M(sim)

  phy_s1<-ape::drop.tip(sim$phy,  ## phy with only state1 tips
                         tip = sim$phy$tip.label[which(sim$obs_traits == 2 | sim$obs_traits == 3)])
  phy_s2<-ape::drop.tip(sim$phy,
                         tip = sim$phy$tip.label[which(sim$obs_traits == 1 | sim$obs_traits == 3)])
  phy_s3<-ape::drop.tip(sim$phy,
                         tip = sim$phy$tip.label[which(sim$obs_traits == 1 | sim$obs_traits == 2)])

  # nLTT
  nltt <- treestats::nLTT_base(sim$phy)
  nltt1 <- treestats::nLTT_base(phy_s1)
  nltt2 <- treestats::nLTT_base(phy_s2)
  nltt3 <- treestats::nLTT_base(phy_s3)


  # drop one state
  phy_s23<-ape::drop.tip(sim$phy,  ## phy with state 2&3
                          tip = sim$phy$tip.label[which(sim$obs_traits == 1)])
  phy_s13<-ape::drop.tip(sim$phy,  ## phy with state 1&3
                          tip = sim$phy$tip.label[which(sim$obs_traits == 2)])
  phy_s12<-ape::drop.tip(sim$phy,  ## phy with state 1&2
                          tip = sim$phy$tip.label[which(sim$obs_traits == 3)])


  D12 <- calc_D_drop(phy_s12,sim$obs_traits[-which(sim$obs_traits == "3")])
  D13 <- calc_D_drop(phy_s13,sim$obs_traits[-which(sim$obs_traits == "2")])
  D23 <- calc_D_drop(phy_s23,sim$obs_traits[-which(sim$obs_traits == "1")])

  # transfer to two states
  D1_23 <- calc_D_trans(sim,"1")
  D2_13 <- calc_D_trans(sim,"2")
  D3_12 <- calc_D_trans(sim,"3")


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
         # sdpd_all = sdpd_all,
         # sdpd_diff = sdpd_diff,
         # sdntd_all = sdntd_all,
         # sdntd_diff = sdntd_diff,
         # K = K,
         # D = D,
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
         # colless = colless,
         # spect_log_median = spect_log_median,
         # spect_prin = spect_prin,
         # sackin = sackin,
         M = M)
  )
}