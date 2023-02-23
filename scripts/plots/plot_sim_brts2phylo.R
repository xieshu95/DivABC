### brts2phylo function from Pedro's package: phylometricsims
brts2phylo <- function(times,root=FALSE,tip.label=NULL)
{
  times = sort(times)
  n <- as.integer(length(times))+1
  if ( root ) {
    n <- n-1
  }
  nbr <- 2*n - 2

  # create the data types for edges and edge-lengths
  edge <- matrix(NA, nbr, 2)
  edge.length <- numeric(nbr)

  h <- numeric(2*n - 1) # initialized with 0's
  pool <- 1:n
  # VERY VERY IMPORTANT: the root MUST have index n+1 !!!
  nextnode <- 2L*n - 1L
  if ( n > 1) {
    for (i in 1:(n - 1)) {
      # sample two nodes that have no parent yet
      y <- sample(pool, size = 2)
      # compute the edge indices (we just order the edges from 1 to 2n-2)
      ind <- (i - 1)*2 + 1:2
      # set the source node of the new edges (i.e. the new internal node)
      edge[ind, 1] <- nextnode
      # set the destination of the new edges (i.e. the two sampled nodes)
      edge[ind, 2] <- y
      # compute the edge length from the difference between the node heights (child <-> parent)
      edge.length[ind] <- times[i] - h[y]
      # store the node height of this new internal node
      # we cannot use times because then we would get into trouble with the indices and we would need to check for tip nodes ...
      h[nextnode] <- times[i]
      # reset the pool of available nodes to merge
      pool <- c(pool[! pool %in% y], nextnode)
      # increase the node index counter
      nextnode <- nextnode - 1L
    }
  }

  phy <- list(edge = edge, edge.length = edge.length)
  if (is.null(tip.label))
    tip.label <- paste("t", 1:n, sep = "")
  phy$tip.label <- sample(tip.label)
  phy$Nnode <- n - 1L

  if ( root ) {
    phy$root.edge <- times[n] - times[n-1]
    phy$root <- times[n] - times[n-1]
  }

  class(phy) <- "phylo"
  # phy <- ape::reorder.phylo(phy)
  ## to avoid crossings when converting with as.hclust:
  phy$edge[phy$edge[, 2] <= n, 2] <- 1:n

  return(phy)
}


lineage_brts <- function(sims) {
  brts <- list()
  for (i in 2:length(sims[[1]])){
    brts[[i - 1]] <-  sims[[1]][[i]]$branching_times
  }
  return(brts)
}


load("G:/results/project 1/final_all/trait_CES_param_set_200.RData")
sim1 <- output$passed_oceanic_sims_1[[102]]
brts <- lineage_brts(sims = sim1)
brts1 <- brts[c(37,38)]
phylo_tree <- list()
tcols <- c()
for (i in 1:length(brts1)){
  # if(length(brts1[[i]]) == 2){
  #   phylo_tree[[i]] <- brts2phylo(times = brts1[[i]][-1], root = FALSE, tip.label = NULL)
  #   phylo_tree[[i]]$single_tip <- TRUE
  # } else {
    phylo_tree[[i]] <- brts2phylo(times = brts1[[i]][-1], root = TRUE, tip.label = NULL)
    phylo_tree[[i]]$single_tip <- FALSE
  # }

  phylo_tree[[i]]$tip.label <- gsub("t", paste0("t",i,"."), phylo_tree[[i]]$tip.label)
  # if(length(phylo_tree[[i]]$tip.label) == 1){
  #   phylo_tree[[i]]$edge.length <- brts1[[i]][2]
  # }
  tcols[i] <- brts1[[i]][2]
}

plot(phylo_tree[[1]])
plot(phylo_tree[[2]])
plot(phylo_tree[[3]])


names(phylo_tree) <- c(LETTERS[1:length(phylo_tree)])
metadata <- tibble::tibble(
  clade = names(phylo_tree),
  endemic = FALSE,  # whether each clade is endemic
  uncertain = FALSE  # whether colonization time is known for sure
)
age <- 5
# tiff(paste0("G:/R/Traisie-ABC/plots/phylotrees_ppt/tree12.tiff"),units="px", width=400, height=200)
p <- DAISIE:::DAISIE_plot_input(
  phylo_tree,
  age,
  tcols,
  metadata,
  mapping = NULL,  ###ggplot2::aes(color = endemic, linetype = uncertain)
  pargs = list(size = 2)
)
print(p)
# while (!is.null(dev.list()))  dev.off()

# phylo_tree1 = phylo_tree


# trees = phylo_tree
# age = age
# tcols = tcols
# metadata = metadata
# mapping = ggplot2::aes(color = endemic, linetype = uncertain)
# pargs = list(size = 3)
# xlen = 0.001
# bckgd = "white"
#
# library(phytools)
# plotTree(trees[[1]],node.numbers=T)

# library(RPANDA)
DAISIE_tree <- phylo_tree[[1]]
plot(DAISIE_tree)
data(Cetacea)
plot(Cetacea)
a <-RPANDA::spectR(Cetacea,meth="standard",zero_bound=FALSE)
plot_spectR(a)
b <-RPANDA::spectR(DAISIE_tree,meth="standard",zero_bound=FALSE)
plot_spectR(b)


remotes::install_github("thijsjanzen/treestats")
library(RPANDA)
library(treestats)
set.seed(42)
focal_tree <- ape::rphylo(n = 10, birth = 1, death = 0)
plot(focal_tree)
ref <- RPANDA::spectR(focal_tree)
stat <- treestats::laplacian_spectrum(focal_tree)
plot_spectR(stat)
stat2 <- treestats::laplacian_spectrum(
  phy = treestats::phylo_to_l(focal_tree)
)

