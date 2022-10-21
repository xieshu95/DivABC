#' Measures if the input is a valid collection of simulation
#' outputs.
#'
#' @param time Island age
#' @param convert_sim1 coverted DAISIE simulation
#'        \code{\link[DAISIE]{is_simulation_outputs}()}
#' @param convert_sim2 coverted DAISIE simulation
#'        \code{\link[DAISIE]{is_simulation_outputs}()}
#' @param diversity_type  Default is \code{"Total"}, \code{"Endemic"}
#'   or \code{"Non-endemic"} should be plotted.
#' @inheritParams default_params_doc
#' @author Shu Xie

## plot for two sims
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
    main <- expression(Delta ~ "nLTT")
  }
  # Plot standard stt (start by opening empty canvas)
  suppressWarnings(
    graphics::plot(
      x = NULL,
      y = NULL,
      xlim = rev(c(0, time)),
      ylim = c(0, max(sim1$stt_average[, "Total"],
                      sim2$stt_average[, "Total"])),
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

}



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
DAISIE:::DAISIE_plot_sims(sim1)
set.seed(20)
sim2 <- DAISIE::DAISIE_sim_cr (
  time = 4,
  M = 1000,
  pars = c(0.3,0,Inf,0.001,0),
  replicates = 1,
  divdepmodel = "CS",
  sample_freq = Inf,
  plot_sims = FALSE
)
DAISIE:::DAISIE_plot_sims(sim2)

set.seed(1)
sim3 <- DAISIE::DAISIE_sim_cr (
  time = 4,
  M = 1000,
  pars = c(0.1,0,Inf,0.00001,0),
  replicates = 1,
  divdepmodel = "CS",
  sample_freq = Inf,
  plot_sims = FALSE
)
DAISIE:::DAISIE_plot_sims(sim3)


plot1 = DAISIE:::DAISIE_convert_to_classic_plot(sim1)
plot2 = DAISIE:::DAISIE_convert_to_classic_plot(sim2)
plot3 = DAISIE:::DAISIE_convert_to_classic_plot(sim3)
time = 4
convert_sim1 = plot1
convert_sim2 = plot2
# convert_sim3 = plot3

par(mfrow=c(1,3))
diversity_type = "obs"
p1 <- plot_delta_stt(4,plot1,plot3,diversity_type)
diversity_type = "sim"
p2 <- plot_delta_stt(4,plot2,plot3,diversity_type)
diversity_type = "delta"
p3 <- plot_delta_stt(4,plot1,plot2,diversity_type)

