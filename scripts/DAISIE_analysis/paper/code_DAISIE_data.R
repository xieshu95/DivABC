rm(list = ls())
library(tidyverse)
library(ape)
library(DAISIE)

trees <- read.tree("D:/R/TraisieABC/scripts/DAISIE_analysis/paper/trees.txt")
names(trees) <- c( "A","B", "C", "D", "E", "F")
DAISIE_plot_input(trees, age = 10, tcols = c(4.5, 9, 8.5, 5.5, 7, 8.5))
p <- DAISIE_plot_input(
  trees,
  age = 10,
  tcols = c(4.5, 9, 8.5, 5.5, 7, 8.5),
)
if (!requireNamespace('tibble', quietly = TRUE)) {
  cat("Package tibble needed for this function to work. Please install it.")
} else {

  names(trees) <- c("A", "B", "C", "D", "E", "F")

  # Toy colonization events for each clade
  tcols <- c(4.5, 9, 8.5, 5.5, 7, 8.5)

  # Toy metadata
  metadata <- tibble::tibble(
    clade = names(trees),
    endemic = c(FALSE, rep(TRUE, 5)),
    endemic_lab = if_else(endemic, "Endemic", "Non-endemic")
  )

  # Island age
  age <- 10

  # Make a plot
  p <- DAISIE_plot_input(
    trees,
    age,
    tcols,
    metadata,
    mapping = ggplot2::aes(color = endemic_lab),
  )
  p +
    labs(color = NULL) + scale_color_discrete(na.translate = FALSE)
}

### tree2
## tips: single clade use the same as tcols, clades with multiple endemics use a higher value
trees <- read.tree("D:/R/TraisieABC/scripts/DAISIE_analysis/paper/trees2.txt")
names(trees) <- c( "A","B", "C", "D", "E")
DAISIE_plot_input(trees, age = 5, tcols = c(1.2, 0.3, 1.4, 2.7, 4.5))
p <- DAISIE_plot_input(
  trees,
  age = 5,
  tcols = c(1.2, 0.3, 1.4, 2.7, 4.5),
)

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/DAISIE PAPER/Figures/tree.tiff"),
     units="px", width=2500, height=1500,res = 500,compression="lzw")
print(p)
while (!is.null(dev.list()))  dev.off()

if (!requireNamespace('tibble', quietly = TRUE)) {
  cat("Package tibble needed for this function to work. Please install it.")
} else {

  names(trees) <- c("A", "B", "C", "D", "E")

  # Toy colonization events for each clade
  tcols <- c(1.2, 0.3, 1.4, 2.7, 4.5)

  # Toy metadata
  metadata <- tibble::tibble(
    clade = names(trees),
    endemic = c(rep(FALSE,2), rep(TRUE, 3)),
    endemic_lab = if_else(endemic, "Endemic", "Non-endemic")
  )

  # Island age
  age <- 5

  # Make a plot
  p <- DAISIE_plot_input(
    trees,
    age,
    tcols,
    metadata,
    mapping = ggplot2::aes(color = endemic_lab),
  )
  p +
    labs(color = NULL) + scale_color_discrete(na.translate = FALSE)
}

tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/DAISIE PAPER/Figures/tree_endemism.tiff"),
     units="px", width=2000, height=1600,res = 500,compression="lzw")
print(p)
while (!is.null(dev.list()))  dev.off()
