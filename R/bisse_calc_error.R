#' Calculates error metrics between two simulations
#' NLTTs_D
calc_error_bisse <- function(sim_1,
                              sim_2,
                              distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])

  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1)])

  # D statistic
  D1 <- calc_D(sim_1)
  D2 <- calc_D(sim_2)
  D <- abs (D1 - D2)

  # nLTT
  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)
  nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
  nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)

  # spect_1 <- treestats::laplacian_spectrum(sim_1$phy)
  # spect_2 <- treestats::laplacian_spectrum(sim_2$phy)
  # spect <- abs(log(spect_1$principal_eigenvalue) -
  #                log(spect_2$principal_eigenvalue) )
  return(
    c(nltt,
      nltt_s1,
      nltt_s2,
      D
      )
  )
}

# NLTTS+D+NUM
calc_error_bisse_num <- function(sim_1,
                              sim_2,
                              distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])

  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1)])

  # D statistic
  D1 <- calc_D(sim_1)
  D2 <- calc_D(sim_2)
  D <- abs (D1 - D2)
  num_state1 <- abs(sum(sim_1$obs_traits == 1) - sum(sim_2$obs_traits == 1))
  num_state2 <- abs(sum(sim_1$obs_traits == 2) - sum(sim_2$obs_traits == 2))

  # nLTT
  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)
  nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
  nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)

  return(
    c(nltt,
      nltt_s1,
      nltt_s2,
      D,
      num_state1,
      num_state2
    )
  )
}





## NLTTs
calc_error_bisse_nltts <- function(sim_1,
                                   sim_2,
                                   distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])

  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1)])


  # nLTT
  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)
  nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
  nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)

  return(
    c(nltt,
      nltt_s1,
      nltt_s2)
  )
}


## nltt_D
calc_error_bisse_D_nltt <- function(sim_1,
                                   sim_2,
                                   distance_method = "abs") {
  # nLTT
  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)

  # D statistic
  D1 <- calc_D(sim_1)
  D2 <- calc_D(sim_2)
  D <- abs (D1 - D2)

  return(
    c(nltt,
      D)
  )
}


## D
calc_error_bisse_D <- function(sim_1,
                                     sim_2,
                                     distance_method = "abs") {

  # D statistic
  D1 <- calc_D(sim_1)
  D2 <- calc_D(sim_2)
  D <- abs (D1 - D2)

  return(
    c(D)
  )
}

## nltt
calc_error_bisse_nltt <- function(sim_1,
                                sim_2,
                                distance_method = "abs") {

  # nLTT
  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)

  return(
    c(nltt)
  )
}


# MPDs_D
calc_error_bisse_mpd_D <- function(sim_1,
                              sim_2,
                              distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])

  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1)])

  # mpd s1
  mpd1_s1  <- treestats::mean_pair_dist(phy1_s1) ## sim1 state1 mpd
  mpd2_s1  <- treestats::mean_pair_dist(phy2_s1) ## sim2 state1 mpd
  mpd_s1  <- abs(mpd1_s1  - mpd2_s1 )

  mpd1_s2 <- treestats::mean_pair_dist(phy1_s2)
  mpd2_s2 <- treestats::mean_pair_dist(phy2_s2)
  mpd_s2 <- abs(mpd1_s2 - mpd2_s2)

  # D statistic
  D1 <- calc_D(sim_1)
  D2 <- calc_D(sim_2)
  D <- abs (D1 - D2)

  return(
    c(mpd_s1,
      mpd_s2,
      D
    )
  )
}

# MNTDs_D
calc_error_bisse_mntd_D <- function(sim_1,
                              sim_2,
                              distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])

  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1)])


  # mntd_diff
  mntd1_s1 <- treestats::mntd(phy1_s1)
  mntd2_s1 <- treestats::mntd(phy2_s1)
  mntd_s1 <- abs(mntd1_s1 - mntd2_s1)

  mntd1_s2 <- treestats::mntd(phy1_s2)
  mntd2_s2 <- treestats::mntd(phy2_s2)
  mntd_s2 <- abs(mntd1_s2 - mntd2_s2)

  # D statistic
  D1 <- calc_D(sim_1)
  D2 <- calc_D(sim_2)
  D <- abs (D1 - D2)

  return(
    c(mntd_s1,
      mntd_s2,
      D
    )
  )
}

# colless_D
calc_error_bisse_colless_D <- function(sim_1,
                              sim_2,
                              distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])

  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1)])

  # D statistic
  D1 <- calc_D(sim_1)
  D2 <- calc_D(sim_2)
  D <- abs (D1 - D2)

  colless <- abs(treestats::colless(sim_1$phy) - treestats::colless(sim_2$phy))
  colless_s1 <- abs(treestats::colless(phy1_s1) - treestats::colless(phy2_s1))
  colless_s2 <- abs(treestats::colless(phy1_s2) - treestats::colless(phy2_s2))


  return(
    c(colless_s1,
      colless_s2,
      D
    )
  )
}

# MPDs_nltt
calc_error_bisse_mpd_nltt <- function(sim_1,
                                       sim_2,
                                       distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])

  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1)])

  # mpd s1
  mpd1_s1  <- treestats::mean_pair_dist(phy1_s1) ## sim1 state1 mpd
  mpd2_s1  <- treestats::mean_pair_dist(phy2_s1) ## sim2 state1 mpd
  mpd_s1  <- abs(mpd1_s1  - mpd2_s1 )

  mpd1_s2 <- treestats::mean_pair_dist(phy1_s2)
  mpd2_s2 <- treestats::mean_pair_dist(phy2_s2)
  mpd_s2 <- abs(mpd1_s2 - mpd2_s2)

  # nLTT
  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)

  return(
    c(mpd_s1,
      mpd_s2,
      nltt
    )
  )
}

#' mntd_nltt
calc_error_bisse_mntd_nltt <- function(sim_1,
                                        sim_2,
                                        distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])

  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1)])

  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)
  # mntd_diff
  mntd1_s1 <- treestats::mntd(phy1_s1)
  mntd2_s1 <- treestats::mntd(phy2_s1)
  mntd_s1 <- abs(mntd1_s1 - mntd2_s1)

  mntd1_s2 <- treestats::mntd(phy1_s2)
  mntd2_s2 <- treestats::mntd(phy2_s2)
  mntd_s2 <- abs(mntd1_s2 - mntd2_s2)


  return(
    c(mntd_s1,
      mntd_s2,
      nltt
    )
  )
}


# colless_nltt
calc_error_bisse_colless_nltt <- function(sim_1,
                                        sim_2,
                                        distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])

  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1)])

  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)

  colless <- abs(treestats::colless(sim_1$phy) - treestats::colless(sim_2$phy))
  colless_s1 <- abs(treestats::colless(phy1_s1) - treestats::colless(phy2_s1))
  colless_s2 <- abs(treestats::colless(phy1_s2) - treestats::colless(phy2_s2))


  return(
    c(colless_s1,
      colless_s2,
      nltt
    )
  )
}





#' tip ratio-nltt
calc_error_bisse_ratio_nltt <- function(sim_1,
                              sim_2,
                              distance_method = "abs") {


  # tip ratio
  tip_ratio_sim1 <- min(sum(sim_1$obs_traits == 1),sum(sim_1$obs_traits == 2))/
    max(sum(sim_1$obs_traits == 1),sum(sim_1$obs_traits == 2))
  tip_ratio_sim2 <- min(sum(sim_2$obs_traits == 1),sum(sim_2$obs_traits == 2))/
    max(sum(sim_2$obs_traits == 1),sum(sim_2$obs_traits == 2))
  tip_ratio <- abs(tip_ratio_sim1 - tip_ratio_sim2)

  # nLTT
  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)

  return(
    c(tip_ratio,
      nltt
    )
  )
}


#####
## create trait matrix that show the state is same or not between each two tips
# 0 means different
# 1 means both states are 1
# 2 means both states are 2
create_trait_matrix <- function(sim) {
  n <- length(sim$phy$tip.label)
  trait_matrix <- matrix(0, nrow = n, ncol = n)
  trait <- sim$obs_traits
  for (i in 1:n){
    for (j in 1:n) {
      if(trait[i] == trait[j]) {  ## if two states are different, keep 0
        trait_matrix[i,j] <- trait[i]
      }
    }
  }
  dimnames(trait_matrix)[1:2]<-list(sim$phy$tip.label)
  return(trait_matrix)
}
## input is bisse simulation with phy and traits
# state_type = 0 means distance between species with different states
# state_type = 1 means distance between species with both state 1
# state_type = 2 means distance between species with both state 2
# state_type = 3 means distance between all the species
calc_mpd_trait <- function(sim,state_type = 0)
{
  dis <- stats::cophenetic(sim$phy)
  # dis<- dis[order(readr::parse_number(rownames(dis))),
  #           order(readr::parse_number(colnames(dis)))]
  trait <- create_trait_matrix(sim)
  if(state_type == 3) {
    mpd <- mean(dis[lower.tri(dis)])
  } else {
    mpd <- mean(dis[lower.tri(dis) & trait == state_type])
  }
  if(is.na(mpd)) {
    mpd <- 0
  }
  return(mpd)
}

calc_mntd_trait <- function(sim,state_type = 0)
{
  dis <- stats::cophenetic(sim$phy)
  # dis<- dis[order(readr::parse_number(rownames(dis))),
  #           order(readr::parse_number(colnames(dis)))]
  trait <- create_trait_matrix(sim)
  diag(dis) <- NA
  if(state_type != 3) {
    dis[which(trait != state_type)]<- NA
    dis <- dis[ , colSums(is.na(dis)) < nrow(dis)]
  }
  mntd <- mean(apply(dis,2,min,na.rm=TRUE))
  if(is.na(mntd)) {
    mntd <- 0
  }
  return(mntd)
}

# calculate standard deviation of pairwise distanse
calc_sdpd_trait <- function(sim,state_type = 0)
{
  dis <- stats::cophenetic(sim$phy)
  # dis<- dis[order(readr::parse_number(rownames(dis))),
  #           order(readr::parse_number(colnames(dis)))]
  trait <- create_trait_matrix(sim)
  if(state_type == 3) {
    sdpd <- sd(dis[lower.tri(dis)])
  } else {
    sdpd <- sd(dis[lower.tri(dis) & trait == state_type])
  }
  sdpd
}

calc_sdntd_trait <- function(sim,state_type = 0)
{
  dis <- stats::cophenetic(sim$phy)
  # dis<- dis[order(readr::parse_number(rownames(dis))),
  #           order(readr::parse_number(colnames(dis)))]
  trait <- create_trait_matrix(sim)
  diag(dis) <- NA
  if(state_type != 3) {
    dis[which(trait != state_type)]<- NA
    dis <- dis[ , colSums(is.na(dis)) < nrow(dis)]
  }
  sdntd <- sd(apply(dis,2,min,na.rm=TRUE))
  sdntd
}


calc_D <- function (sim) {
  trait = data.frame(sim$phy$tip.label,sim$obs_traits)
  colnames(trait) <- c("tips","trait_val")
  data <- caper::comparative.data(sim$phy, trait, tips)
  PhyloD <- caper::phylo.d(data, binvar=trait_val,permut = 100)
  return(as.numeric(PhyloD$DEstimate))
}

# sim <- get_bisse_sim(parameters  =  c(0.6,0.6,0.05,0.05,0.1,0.1),
#                           pool_init_states = c("1","2"))[[1]]

calc_Delta <- function (sim) {
  Delta <- delta(as.numeric(sim$obs_traits),sim$phy, lambda0 = 0.5,se = 0.5,sim = 100,thin = 1,burn = 10)
  return(as.numeric(log(Delta+1))) #log(Delta+1)  or Delta
}

# phylogenetic signal M
calc_M <- function (sim) {
  trait_df <- data.frame(B1 =as.factor(sim$obs_traits), row.names = sim$phy$tip.label)
  trait_dist <- phylosignalDB::gower_dist(x = trait_df)
  phyloM <- phylosignalDB::phylosignal_M(trait_dist, phy = sim$phy, reps = 1)
  return(as.numeric(phyloM$stat))
}


#####
#' Calculate summary statistic differences between simulated data and a specific
#' replicate of (simulated) observed data.
#'
#' @param sim A datalist of observed data with more than one replicate.
#' @param replicates The number of replicates used for calculating summary
#'   statistics.
#'
#' @return A list with numeric vectors of diff statistics for:
#' \itemize{
#'   \item{\code{$ana_endemic_nltt}}
#'   \item{\code{$clado_endemic_nltt}}
#'   \item{\code{$nonendemic_nltt}}
#'   \item{\code{$num_col_sim}}
#'   \item{\code{$clade_size_sd}}
#'   \item{\code{$colon_time_sd}}
#' }
#' @author Shu Xie
#' @export

calc_ss_bisse <- function(sim) {

  # # mpd_all
  # mpd_all <- calc_mpd_trait(sim = sim,state_type = 3)
  #
  # # mpd_diff
  # mpd_diff <- calc_mpd_trait(sim = sim,state_type = 0)
  #
  # # mntd_all
  # mntd_all <- calc_mntd_trait(sim = sim,state_type = 3)
  #
  # # mntd_diff
  # mntd_diff <- calc_mntd_trait(sim = sim,state_type = 0)
  #
  # # K statistic
  # K <- adiv::K(sim$phy,
  #              trait = sim$obs_traits,
  #              nrep = 1000, alter = c("two-sided"))
  # K <- K$obs
  #
  #
  # # D statistic
  # D <- calc_D(sim)


  # state 1
  num_state1 <- sum(sim$obs_traits == 1)
  num_state2 <- sum(sim$obs_traits == 2)
  total_spec <- num_state1 + num_state2
  tip_ratio <- max(num_state1,num_state2)/min(num_state1,num_state2)


  # # nLTT
  # nltt <- treestats::nLTT_base(sim$phy)

  # ## standard deviation of pairwise distance
  # sdpd_all <- calc_sdpd_trait(sim = sim,state_type = 3)
  # sdpd_diff <- calc_sdpd_trait(sim = sim,state_type = 0)
  #
  # ## standard deviation of nearest taxon distance
  # sdntd_all <- calc_sdntd_trait(sim = sim,state_type = 3)
  # sdntd_diff <- calc_sdntd_trait(sim = sim,state_type = 0)
  #
  #
  # ## mean pairwise distance with state1
  # mpd_1 <- calc_mpd_trait(sim = sim,state_type = 1)
  # mntd_1 <- calc_mntd_trait(sim = sim,state_type = 1)
  #
  # ## mean pairwise distance with state1
  # mpd_2 <- calc_mpd_trait(sim = sim,state_type = 2)
  # mntd_2 <- calc_mntd_trait(sim = sim,state_type = 2)

  # colless <- treestats::colless(sim$phy)
  # spect <- treestats::laplacian_spectrum(sim$phy)
  # spect_log_median <- median(log(spect$eigenvalues))
  # spect_prin <- log(spect$principal_eigenvalue)
  # sackin <- treestats::sackin(sim$phy)

  return(
    list(state1 = num_state1,
         state2 = num_state2,
         total_spec = total_spec,
         tip_ratio = tip_ratio)
    # mpd_all = mpd_all,
    # mpd_diff = mpd_diff,
    # mpd_1 = mpd_1,
    # mpd_2 = mpd_2,
    # mntd_all = mntd_all,
    # mntd_diff = mntd_diff,
    # mntd_1 = mntd_1,
    # mntd_2 = mntd_2,
    # sdpd_all = sdpd_all,
    # sdpd_diff = sdpd_diff,
    # sdntd_all = sdntd_all,
    # sdntd_diff = sdntd_diff,
    # K = K,
    # D = D,
    # nltt = nltt)
  )
}

# nltts + Delta
calc_error_bisse_Delta <- function(sim_1,
                             sim_2,
                             distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])

  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1)])

  # D statistic
  Delta1 <- calc_Delta(sim_1)
  Delta2 <- calc_Delta(sim_2)
  Delta <- abs (Delta1 - Delta2)

  # nLTT
  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)
  nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
  nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)

  # spect_1 <- treestats::laplacian_spectrum(sim_1$phy)
  # spect_2 <- treestats::laplacian_spectrum(sim_2$phy)
  # spect <- abs(log(spect_1$principal_eigenvalue) -
  #                log(spect_2$principal_eigenvalue) )
  return(
    c(nltt,
      nltt_s1,
      nltt_s2,
      Delta
    )
  )
}


# nltts + phylosignal M
calc_error_bisse_M <- function(sim_1,
                             sim_2,
                             distance_method = "abs") {

  # drop tips and only keep tips with a single state(1/2)
  phy1_s1<-ape::drop.tip(sim_1$phy,  ## phy1 with only state1 tips
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 2)])
  phy1_s2<-ape::drop.tip(sim_1$phy,
                         tip = sim_1$phy$tip.label[which(sim_1$obs_traits == 1)])

  phy2_s1<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 2)])
  phy2_s2<-ape::drop.tip(sim_2$phy,
                         tip = sim_2$phy$tip.label[which(sim_2$obs_traits == 1)])

  # M statistic
  M1 <- calc_M(sim_1)
  M2 <- calc_M(sim_2)
  M <- abs (M1 - M2)

  # nLTT
  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)
  nltt_s1 <- treestats::nLTT(phy1_s1,phy2_s1)
  nltt_s2 <- treestats::nLTT(phy1_s2,phy2_s2)

  # spect_1 <- treestats::laplacian_spectrum(sim_1$phy)
  # spect_2 <- treestats::laplacian_spectrum(sim_2$phy)
  # spect <- abs(log(spect_1$principal_eigenvalue) -
  #                log(spect_2$principal_eigenvalue) )
  return(
    c(nltt,
      nltt_s1,
      nltt_s2,
      M
    )
  )
}



