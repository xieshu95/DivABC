#' Calculates error metrics between two simulations
calc_error_secsse <- function(sim_1,
                              sim_2,
                              distance_method = "abs") {

  # mpd_all
  mpd_1 <- treestats::mean_pair_dist(sim_1$phy)
  mpd_2 <- treestats::mean_pair_dist(sim_2$phy)
  mpd <- abs(mpd_1 - mpd_2)


  vpd_1 <- treestats::var_pair_dist(sim_1$phy)
  vpd_2 <- treestats::var_pair_dist(sim_2$phy)
  vpd <- abs(vpd_1 - vpd_2)

  # mntd_all
  mntd_1 <- treestats::mntd(phy = sim_1$phy)
  mntd_2 <- treestats::mntd(phy = sim_2$phy)
  mntd <- abs(mntd_1 - mntd_2)

  # mntd_diff
  mntd_diff_sim1 <- calc_mntd_trait(sim = sim_1,state_type = 0)
  mntd_diff_sim2 <- calc_mntd_trait(sim = sim_2,state_type = 0)
  mntd_diff <- abs(mntd_diff_sim1 - mntd_diff_sim2)

  # D statistic
  D1 <- calc_D(sim_1)
  D2 <- calc_D(sim_2)
  D <- abs (D1 - D2)

  # state 1&2
  num_state1_sim1 <- length(which(sim_1$examTraits == 1))
  num_state2_sim1 <- length(which(sim_1$examTraits == 2))

  num_state1_sim2 <- length(which(sim_2$examTraits == 1))
  num_state2_sim2 <- length(which(sim_2$examTraits == 2))

  num_state1 <- abs(num_state1_sim1 - num_state1_sim2)
  num_state2 <- abs(num_state2_sim1 - num_state2_sim2)

  # nLTT
  nltt <- treestats::nLTT(sim_1$phy,sim_2$phy)

  spect_1 <- treestats::laplacian_spectrum(sim_1$phy)
  spect_2 <- treestats::laplacian_spectrum(sim_2$phy)
  spect <- abs(log(spect_1$principal_eigenvalue) -
                 log(spect_2$principal_eigenvalue) )

  return(
    c(mpd,
      vpd,
      mntd,
      mntd_diff,
      D,
      num_state1,
      num_state2,
      nltt,
      spect)
  )
}

## create trait matrix that show the state is same or not between each two tips
# 0 means different
# 1 means both states are 1
# 2 means both states are 2
create_trait_matrix <- function(sim) {
  n <- length(sim$phy$tip.label)
  trait_matrix <- matrix(0, nrow = n, ncol = n)
  trait <- sim$examTraits
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
## input is secsse simulation with phy and traits
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
  trait = data.frame(sim$phy$tip.label,sim$examTraits)
  colnames(trait) <- c("tips","trait_val")
  data <- caper::comparative.data(sim$phy, trait, tips)
  PhyloD <- caper::phylo.d(data, binvar=trait_val,permut = 500)
  return(as.numeric(PhyloD$DEstimate))
}



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

calc_ss_secsse <- function(sim) {

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
  #              trait = sim$examTraits,
  #              nrep = 1000, alter = c("two-sided"))
  # K <- K$obs
  #
  #
  # # D statistic
  # D <- calc_D(sim)


  # state 1
  num_state1 <- length(which(sim$examTraits == 1))
  num_state2 <- length(which(sim$examTraits == 2))
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

