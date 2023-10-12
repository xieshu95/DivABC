scenario = "secsse_ABC_long"
param_set = 7
idparsopt = 1:6
save_output = TRUE
sim_model = "secsse"
ss_set = 0

b <- run_ABC(scenario,
             param_set,
             idparsopt,
             "secsse",
             save_output,
             ss_set)



obs_data = obs_sim
sim_function = sim_function
init_epsilon_values = init_epsilon
prior_generating_function = prior_generating_function
prior_density_function = prior_density_function
number_of_particles = 10
sigma = 0.2
stop_rate = 0.01
replicates = 1  ## simulation replicates for each parameter set
num_iterations = 3
K = as.numeric(obs_sim_pars$K)
idparsopt = as.numeric(idparsopt)
fixpars = fixpars
ss_set = ss_set

scenario = "DAISIE_ABC_short"
param_set = 1
idparsopt = 1:4
save_output = TRUE
sim_model = "DAISIE"
ss_set = 0
pairwise_method = 2


b <- run_ABC(scenario,
             param_set,
             idparsopt,
             "DAISIE",
             save_output,
             ss_set,
             pairwise_method)



scenario = "secsse_MCMC_long"
param_set = 1
idparsopt = 1:6
save_output = FALSE
sim_model = "secsse"
ss_set = 1
b <- run_MCMC_secsse(scenario,
                     param_set,
                     idparsopt,
                     save_output)



# scenario = "DAISIE"
# param_set = 1
# idparsopt = 1:6
# save_output = TRUE
# sim_model = "DAISIE"
# ss_set = 1

param_space <- readr::read_csv2("data/secsse_ABC.csv")
# param_space <- load_param_space(scenario = scenario)
# param_space <- read.csv2(file = 'data/DAISIE_ABC.csv')
# seed <- param_set ##as.integer(Sys.time()) %% 1000000L * param_set
set.seed(42)

message("Param space name: ", scenario)
message("Running param set: ", param_set)
message("seed: ", seed)

check_create_folders(
  scenario = scenario,
  save_output = save_output
)
message("sim_model: ", sim_model)
obs_sim_pars <- param_space[param_set,]
obs_sim <- get_secsse_sim(parameters = as.numeric(obs_sim_pars),
                          K = Inf,
                          replicates = 1) ## replicates = 30
sim_function <- get_secsse_sim
prior_generating_function <- prior_gen_secsse
prior_density_function <- prior_dens_secsse
fixpars = as.numeric(obs_sim_pars[1:6])
init_epsilon <- calc_epsilon_init_secsse(sim = obs_sim)
# init_epsilon6<-init_epsilon
#
# ss <-rbind(init_epsilon1,init_epsilon2,init_epsilon3,
#            init_epsilon4,init_epsilon5,init_epsilon6)
#
# colnames(ss) <- c("mpd","mpd_12","mntd","mntd_12","K","D","state1","state2")
# save(ss,file = "G:/results/project 2/tip_info/round4/secsse/obs_ss.RData")
#
# rownames(ss) <- 1:6
# pars_ss<-data.frame(param_space,ss)
# save(pars_ss,file = "G:/results/project 2/tip_info/round4/secsse/obs_ss_with_pars.RData")
obs_data = obs_sim
sim_function = sim_function
init_epsilon_values = init_epsilon
prior_generating_function = prior_generating_function
prior_density_function = prior_density_function
number_of_particles = 5
sigma = 0.5
stop_rate = 0.002
replicates = 1  ## simulation replicates for each parameter set
num_iterations = 3
K = as.numeric(obs_sim_pars$K)
idparsopt = as.numeric(idparsopt)
fixpars = fixpars
ss_set = ss_set


eps <- c()
for(i in 1:50) {
  eps[i] <- exp(-0.4 * (i - 1))*1
}
round(eps,5)



set.seed(42)
param_space <- readr::read_csv2("data/secsse_ABC.csv")
# param_space <- load_param_space(scenario = scenario)
obs_sim_pars <- param_space[param_set,]
parameters <- as.numeric(obs_sim_pars)
replicates = 1

# parameters<-c(0.5,0.5,0.01,0.01,0.2,0.2)

## calculation of mpd
library(picante)
data(phylocom)
## examples in the package
mpd(phylocom$sample, cophenetic(phylocom$phylo), abundance.weighted=FALSE)
phylocom$sample
phylocom$phylo
cophenetic(phylocom$phylo)

## use the example in treestats test (treestats vs picante)
set.seed(42)
focal_tree <- ape::rphylo(n = 5, birth = 1, death = 0)
plot(focal_tree)
focal_tree$edge.length

n <- length(focal_tree$tip.label)

sample_mat <- matrix(c(1,1,1,1), nrow = 1, ncol = n)  ## NUMBER OF INDIVIDUALS IN EACH SPECIES
colnames(sample_mat) <- focal_tree$tip.label
sample_mat

samp = sample_mat
dis = cophenetic(focal_tree)
abundance.weighted=FALSE

# a2 <- picante::mpd(samp = sample_mat,
#                    dis = cophenetic(focal_tree),
#                    abundance.weighted = FALSE)
# a2

### mpd in package picante
mpd <- function(samp, dis, abundance.weighted=FALSE)
{
  N <- dim(samp)[1]
  mpd <- numeric(N)
  for (i in 1:N) {
    sppInSample <- names(samp[i, samp[i, ] > 0])
    if (length(sppInSample) > 1) {
      sample.dis <- dis[sppInSample, sppInSample]
      if (abundance.weighted) {
        sample.weights <- t(as.matrix(samp[i,sppInSample,drop=FALSE])) %*% as.matrix(samp[i,sppInSample,drop=FALSE])
        mpd[i] <- weighted.mean(sample.dis,sample.weights)

      }
      else {
        mpd[i] <- mean(sample.dis[lower.tri(sample.dis)])
      }
    }
    else{
      mpd[i] <- NA
    }
  }
  mpd
}

set.seed(42)
focal_tree <- ape::rphylo(n = 5, birth = 1, death = 0)
plot(focal_tree)
n <- length(focal_tree$tip.label)

sample_mat <- matrix(c(1,1,1,1,1,1,1), nrow = 1, ncol = 7)  ## NUMBER OF INDIVIDUALS IN EACH SPECIES
colnames(sample_mat) <- focal_tree$tip.label
sample_mat

samp = sample_mat
dis = cophenetic(sim_try$phy)
abundance.weighted=FALSE
mntd <- function(samp, dis, abundance.weighted=FALSE)
{
  N <- dim(samp)[1]
  mntd <- numeric(N)
  for (i in 1:N) {
    sppInSample <- names(samp[i,samp[i,] > 0])
    if (length(sppInSample) > 1) {
      sample.dis <- dis[sppInSample,sppInSample]
      diag(sample.dis) <- NA
      if (abundance.weighted)
      {
        mntds <- apply(sample.dis,2,min,na.rm=TRUE)
        sample.weights <- samp[i,sppInSample]
        mntd[i] <- weighted.mean(mntds, sample.weights)
      }
      else
      {
        mntd[i] <- mean(apply(sample.dis,2,min,na.rm=TRUE))
      }
    }
    else {
      mntd[i] <- NA
    }
  }
  mntd
}



### from treestats test
set.seed(42)
focal_tree <- ape::rphylo(n = 5, birth = 1, death = 0)
plot(focal_tree)
n <- length(focal_tree$tip.label)

sample_mat <- matrix(c(1,1,1,1,1), nrow = 1, ncol = 5)  ## NUMBER OF INDIVIDUALS IN EACH SPECIES
colnames(sample_mat) <- focal_tree$tip.label
sample_mat
a2 <- picante::mpd(sample_mat, cophenetic(focal_tree),
                   abundance.weighted = FALSE)
a2




a1 <- treestats::mean_pair_dist(focal_tree)

n <- length(focal_tree$tip.label)
sample_mat <- matrix(data = 1, nrow = n, ncol = n)
colnames(sample_mat) <- focal_tree$tip.label

a2 <- picante::mpd(sample_mat, cophenetic(focal_tree),
                   abundance.weighted = FALSE)[[1]]
testthat::expect_equal(a1, a2)

ltab <- treestats::phylo_to_l(focal_tree)
testthat::expect_equal(treestats::mean_pair_dist(focal_tree),
                       treestats::mean_pair_dist(ltab))

dis2 = cophenetic(focal_tree)


library(tidyr)
trait_try <- obs_sim[[1]]$speciesTraits[1:10]
n_spec <- length(trait_try)
trait_dis <- matrix(0, nrow = n_spec, ncol = n_spec)

sim = sim_try

## input sim is secsse simulation data
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
  return(trait_matrix)
}

# create_trait_matrix <- function(sim) {
#   n <- length(sim$phy$tip.label)
#   trait_matrix <- matrix(0, nrow = n, ncol = n)
#   trait <- sim$examTraits
#   for (i in 1:n){
#     for (j in 1:n) {
#       if(trait[i] == 1 &&  trait[j] == 1) {
#         trait_matrix[i,j] <- 1
#       } else if (trait[i] == 2 &&  trait[j] == 2) {
#         trait_matrix[i,j] <- 2
#       }
#     }
#   }
#   return(trait_matrix)
# }


### extract numeric in the string
if(readr::parse_number(trait_try[i]) == 1 &&
   readr::parse_number(trait_try[j]) == 1) {
  trait_dis[i,j] <- 1
} else if(readr::parse_number(trait_try[i]) == 2 &&
          readr::parse_number(trait_try[j]) == 2) {
  trait_dis[i,j] <- 2
}

### run secsse simulation
set.seed(3)
obs_sim_pars <- c(0.2,0.2,0,0,0.2,0.2)
obs_sim <- get_secsse_sim(parameters = as.numeric(obs_sim_pars),
                          K = Inf,
                          replicates = 1)
sim_try = obs_sim[[1]]
plot(sim_try$phy)
sim_try
sim_try$examTraits
sim_try$speciesID
dis = cophenetic(sim_try$phy)
dis2<- dis[order(readr::parse_number(rownames(dis))),
           order(readr::parse_number(colnames(dis)))]
dis2
create_trait_matrix(sim_try)

