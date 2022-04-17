## from DAISIE package

DAISIE_plot_input <- function(
  trees, age = NULL, tcols = NULL, metadata = NULL, mapping = NULL,
  xlen = 0.001, pargs = NULL, bckgd = "white"
) {
  # check whether necessary packages have been installed.
  needs_installation_of_dependencies <- FALSE
  dependencies <- c(
    "dplyr",
    "ggplot2",
    "ggtree",
    "tidytree",
    "tidyr",
    "purrr",
    "ape",
    "phytools",
    "tibble"
  )
  for (dependency in dependencies) {
    if (!requireNamespace(dependency, quietly = TRUE)) {
      cat(paste0(
        "Package ",
        dependency ,
        " is needed for this function to work. Please install it."))
      needs_installation_of_dependencies <- TRUE
    }
  }

  if (!needs_installation_of_dependencies) {

    # lint fix
    clade <- NULL
    data <- NULL
    node <- NULL
    . <- NULL
    y <- NULL
    tcol <- NULL

    # Function to stick a layer of white rectangles to hide some parts
    white_rect <- function(xmin, xmax, ymin, ymax, color = "white") {
      ggplot2::geom_rect(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = color,
        fill = color
      )
    }

    if (is.null(age)) age <- max(purrr::map_dbl(
      trees, ~ max(ape::node.depth.edgelength(.x))
    ))
    if (is.null(tcols)) tcols <- rep(age, length(trees))

    # Before we add fake tips, identify the actual tips and assign clades
    tipclades <- list(
      label = purrr::map(trees, ~ .x$tip.label),
      clade = purrr::map2(names(trees), trees, ~ rep(.x, length(.y$tip.label)))
    ) %>% purrr::map_dfc(~ do.call("c", .x))

    # For each tree...
    trees <- purrr::map(trees, function(tree) {

      # Add an extra branch around the age of the island (this is a hack)
      # if(length(tree$tip.label) == 1){
      #   crown <- tree$edge.length
      # } else {
        crown <- max(ape::node.depth.edgelength(tree))

      pos <- age - crown
      tree$root.edge <- pos
        phytools::bind.tip(
        tree, "new", edge.length = xlen, where = "root", position = pos
      )
    })

    # Concatenate all the trees together
    tree <- purrr::reduce(trees, ape::bind.tree)

    # Total number of tips
    ntips <- ape::Ntip(tree)

    # Plot the meta-tree
    p <- ggtree::ggtree(tree, ladderize = FALSE)
    p <- ggtree::revts(p)
    p <- p + ggtree::theme_tree2()

    # Hide the extra tips near the island age (those are here to make sure that
    # the stems of the different subtrees go all the way to the island age)
    p <- p + ggplot2::ylim(c(1, ntips))
    p <- p + white_rect(-age, -age + xlen, 1, ntips, color = bckgd)

    # For each clade, hide the stem up to the colonization event
    ymax <- 0
    ymin <- 0

    for (i in seq_along(trees)) {
      ymin <- ymax + 1
      ymax <- ymin + ape::Ntip(trees[[i]]) - 1
      p <- p + white_rect(-age, -tcols[i], ymin, ymax, color = bckgd)
    }

    # Add a dashed line for island emergence
    p <- p + ggplot2::geom_vline(xintercept = -age, lty = 2)

    # Assign clades to tips
    p$data <- suppressWarnings(p$data %>% dplyr::full_join(tipclades))

    # Infer the clades of the internal nodes based on tips
    nodeclades <- p$data %>%
      dplyr::group_by(clade) %>%
      tidyr::nest() %>%
      dplyr::filter(!is.na(clade)) %>%
      dplyr::mutate(mrca = purrr::map_if(
        data,
        ~ nrow(.x) > 1,
        ~ ape::getMRCA(tree, .x$label),
        .else = ~ .x$node[1]
      ) %>% unlist) %>%
      dplyr::mutate(node = purrr::map2(
        data,
        mrca,
        function(x, y) {
          out <- y
          if (nrow(x) > 1) out <- c(out, tidytree::offspring(tree, y))
          out
        }
      )) %>%
      dplyr::select(-data) %>%
      tidyr::unnest(cols = c(node)) %>%
      .[order(.$node),]

    p$data$clade[p$data$node %in% nodeclades$node] <- nodeclades$clade
    p$data$mrca <- NA
    p$data$mrca[p$data$node %in% nodeclades$node] <- nodeclades$mrca

    # Assign clades with optional extra data
    if (!is.null(metadata)) {
      metadata <- metadata %>% tibble::add_row(clade = NA)
      p$data <- p$data %>%
        dplyr::group_by(clade) %>%
        tidyr::nest() %>%
        dplyr::right_join(metadata, by = "clade") %>%
        tidyr::unnest(cols = c(data))
    }

    # Apply aesthetic mapping
    if (!is.null(mapping)) p <- p + mapping

    # Add points at colonization events
    mrca <- p$data %>%
      dplyr::filter(!is.na(clade)) %>%
      dplyr::group_by(clade, mrca) %>%
      dplyr::summarize(y = y[node == mrca]) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(tcol = tcols)

    if (!is.null(metadata)) mrca <- mrca %>%
      dplyr::right_join(metadata, by = "clade")

    pargs <- c(list(data = mrca, mapping = ggplot2::aes(x = -tcol, y = y)), pargs)
    geom_point_ <- ggplot2::geom_point
    p <- p + do.call("geom_point_", pargs)

    return(p)
  }
}
