library(ggplot2)
plot_delta_stt <- function(
    time,
    convert_sim1,
    convert_sim2,
    diversity_type
) {
  y_axis_label <- "N species"

  sim1 <- convert_sim1$all_species
  sim2 <- convert_sim2$all_species
  #This must be a list with the 10 indep lines
  if (is.null(sim1)) {
    return()
  }

  if(diversity_type == "obs"){
    type_data <- "Total"
    main <- "Observed Data"
  } else if (diversity_type == "sim"){
    type_data <- "Total"
    main <- "Simulated Data"
  } else if (diversity_type == "delta"){
    type_data <- "Total"
    main <- expression(Delta ~ "nLTT State 1") ##
  }
  # Plot standard stt (start by opening empty canvas)
  suppressWarnings(
    graphics::plot(
      x = NULL,
      y = NULL,
      xlim = rev(c(0, time)),
      ylim = c(0, as.integer(max(sim1$stt_average[, "Total"],
                      sim2$stt_average[, "Total"]))),
      ylab = y_axis_label,
      bty = "l",
      xaxs = "i",
      xlab = "Time before present",
      main = main,
      # log = "y",
      cex.lab = 1.2,
      cex.main = 1.2,
      cex.axis = 1.2
    )
  )
  x1 <- rep(sim1$stt_average[, "Time"],each = 2)
  x1 <- x1[2:(length(x1) - 1)]
  y1 <- rep(sim1$stt_average[, type_data],each = 2)
  y1 <- y1[1:(length(y1) - 2)]


  x2 <- rep(sim2$stt_average[, "Time"],each = 2)
  x2 <- x2[2:(length(x2) - 1)]
  y2 <- rep(sim2$stt_average[, type_data],each = 2)
  y2 <- y2[1:(length(y2) - 2)]


  if(diversity_type == "obs"){
    graphics::polygon(
      c(x1,rev(x2)),
      c(y1,rev(y2)),
      col = adjustcolor("dodgerblue1", alpha.f = 0.4),
      border = NA
    )
    graphics::lines(x1,y1,lwd = 2.2, col = "dark blue") ##"#F8766D"
    graphics::lines(x2,y2,lwd = 2.2, col = "gray")
  } else if (diversity_type == "sim"){
    graphics::polygon(
      c(x1,rev(x2)),
      c(y1,rev(y2)),
      col = adjustcolor("red", alpha.f = 0.4),
      border = NA
    )
    graphics::lines(x1,y1,lwd = 2.2, col = "#B10026")
    graphics::lines(x2,y2,lwd = 2.2, col = "gray")
  } else if (diversity_type == "delta"){
    graphics::polygon(
      c(x1,rev(x2)),
      c(y1,rev(y2)),
      col = adjustcolor("#B7E6A5", alpha.f = 0.4),
      border = NA
    )
    graphics::lines(x1,y1,lwd = 2.2, col = "dark blue")
    graphics::lines(x2,y2,lwd = 2.2, col = "#B10026")
  }
  legend("topleft", c("Tree1", "Tree2"), col = c("blue3","red3" ), lty = 1)
}

## tree1 state1
set.seed(5)
sim1 <- DAISIE::DAISIE_sim_cr (
  time = 4,
  M = 1000,
  pars = c(0.2,0,Inf,0.0015,0),
  replicates = 1,
  divdepmodel = "CS",
  sample_freq = Inf,
  plot_sims = FALSE
)

sim1_state1 <- list()
sim1_state1[[1]] <- sim1[[1]][1:2]
sim1_state1[[1]][[1]][["stt_all"]]<- sim1_state1[[1]][[1]][["stt_all"]][1:4,]
sim1_state1[[1]][[1]][["stt_all"]][,1] <-c(8,4,1,0)
sim1_state1[[1]][[1]][["stt_all"]][,2] <-c(0,1,0,0)
sim1_state1[[1]][[1]][["stt_all"]][,3] <-c(0,0,0,0)
sim1_state1[[1]][[1]][["stt_all"]][,4] <-c(0,0,2,2)
sim1_state1[[1]][[1]][["stt_all"]][,5] <-c(0,1,1,1)

sim1_state1[[1]][[2]]$branching_times <- c(8,4)
sim1_state1[[1]][[2]]$stac <-2
DAISIE:::DAISIE_plot_sims(sim1_state2)

sim1_state2 <- list()
sim1_state2[[1]] <- sim1[[1]][1:3]
sim1_state2[[1]][[1]][["stt_all"]]<- sim1_state2[[1]][[1]][["stt_all"]][1:6,]
sim1_state2[[1]][[1]][["stt_all"]][,1] <-c(8,6,5,3,2,0)
sim1_state2[[1]][[1]][["stt_all"]][,2] <-c(0,1,2,1,0,0)
sim1_state2[[1]][[1]][["stt_all"]][,3] <-c(0,0,0,0,0,0)
sim1_state2[[1]][[1]][["stt_all"]][,4] <-c(0,0,0,2,4,4)
sim1_state2[[1]][[1]][["stt_all"]][,5] <-c(0,1,2,2,2,2)

sim1_state2[[1]][[2]]$branching_times <- c(8,5,2)
sim1_state2[[1]][[2]]$stac <-2
sim1_state2[[1]][[3]]$branching_times <- c(8,6,3)
sim1_state2[[1]][[3]]$stac <-2

## sim 2
sim2_state1 <- list()
sim2_state1[[1]] <- sim1[[1]][1:3]
sim2_state1[[1]][[1]][["stt_all"]]<- sim2_state1[[1]][[1]][["stt_all"]][1:4,]
sim2_state1[[1]][[1]][["stt_all"]][,1] <-c(8,6,5,0)
sim2_state1[[1]][[1]][["stt_all"]][,2] <-c(0,1,2,2)
sim2_state1[[1]][[1]][["stt_all"]][,3] <-c(0,0,0,0)
sim2_state1[[1]][[1]][["stt_all"]][,4] <-c(0,0,0,0)
sim2_state1[[1]][[1]][["stt_all"]][,5] <-c(0,1,2,2)

sim2_state1[[1]][[2]]$branching_times <- c(8,5)
sim2_state1[[1]][[2]]$stac <-2
sim2_state1[[1]][[3]]$branching_times <- c(8,6)
sim2_state1[[1]][[3]]$stac <-2
DAISIE:::DAISIE_plot_sims(sim2_state2)

sim2_state2 <- list()
sim2_state2[[1]] <- sim1[[1]][1:4]
sim2_state2[[1]][[1]][["stt_all"]]<- sim2_state2[[1]][[1]][["stt_all"]][1:6,]
sim2_state2[[1]][[1]][["stt_all"]][,1] <-c(8,6,5,4,1,0)
sim2_state2[[1]][[1]][["stt_all"]][,2] <-c(0,1,2,3,2,2)
sim2_state2[[1]][[1]][["stt_all"]][,3] <-c(0,0,0,0,0,0)
sim2_state2[[1]][[1]][["stt_all"]][,4] <-c(0,0,0,0,2,2)
sim2_state2[[1]][[1]][["stt_all"]][,5] <-c(0,1,2,3,3,3)

sim2_state2[[1]][[2]]$branching_times <- c(8,6)
sim2_state2[[1]][[2]]$stac <-2
sim2_state2[[1]][[3]]$branching_times <- c(8,5,1)
sim2_state2[[1]][[3]]$stac <-2
sim2_state2[[1]][[4]]$branching_times <- c(8,4)
sim2_state2[[1]][[4]]$stac <-4


plot1 = DAISIE:::DAISIE_convert_to_classic_plot(sim1_state1)
plot2 = DAISIE:::DAISIE_convert_to_classic_plot(sim1_state2)
plot3 = DAISIE:::DAISIE_convert_to_classic_plot(sim2_state1)
plot4 = DAISIE:::DAISIE_convert_to_classic_plot(sim2_state2)

diversity_type = "delta"
p_s1 <- plot_delta_stt(8,plot1,plot3,diversity_type)

tiff(paste0("D:/Onedrive-shu/OneDrive/project 3/figures/nltt_state1.tiff"),
     units="px", width=3000, height=1800,res = 500,compression="lzw")
p_s1 <- plot_delta_stt(8,plot1,plot3,diversity_type)
print(p_s1)
while (!is.null(dev.list()))  dev.off()


diversity_type = "delta"
p_s2 <- plot_delta_stt(8,plot2,plot4,diversity_type)

tiff(paste0("D:/Onedrive-shu/OneDrive/project 3/figures/nltt_state2.tiff"),
     units="px", width=3000, height=1800,res = 500,compression="lzw")
p_s2 <- plot_delta_stt(8,plot2,plot4,diversity_type)
print(p_s2)
while (!is.null(dev.list()))  dev.off()
