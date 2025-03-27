#' Calculates error metrics between two simulations under musse model
#' NLTTs_Delta
calc_error_musse_Delta <- function(sim_1,
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

  # phylogenetic signal Delta statistic
  Delta1 <- calc_Delta(sim_1)
  Delta2 <- calc_Delta(sim_2)
  Delta <- abs (Delta1 - Delta2)

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
      Delta
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

  # phylogenetic signal Delta statistic
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
calc_error_musse_M_Msep <- function(sim_1,
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

  # phylogenetic signal Delta statistic
  M1 <- calc_M(sim_1)
  M2 <- calc_M(sim_2)
  M <- abs (M1 - M2)

  phy1_s12<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 3)])
  phy2_s12<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 3)])






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
