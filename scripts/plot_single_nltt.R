## dispersion of nltt(1000 reps) from the same parameter set
load(file = "results/sim_gam1.RData")
load(file = "results/sim_gam2.RData")
load(file = "results/sim_lam3.RData")
load(file = "results/sim_gam4.RData")
load(file = "results/sim_gam5.RData")

## nltt_name includes: "spec_nltt","endemic_nltt","nonendemic_nltt","num_spec","num_col"
# sumsta: summary statistic
plot_single_nltt <- function(obs_rep, sim1,sim2,replicates,nltt_name){
  nltt_list <- nltt_within_param (obs_rep = obs_rep, sim1 = sim1,sim2 = sim2, replicates = replicates)
  if (nltt_name == "spec_nltt") {
    sumsta <- nltt_list$spec_nltt_error
    sumsta_label <- " species \u0394nLTT"
  } else if (nltt_name == "endemic_nltt") {
    sumsta <- nltt_list$endemic_nltt_error
    sumsta_label <- " endemic species \u0394nLTT"
  } else if (nltt_name == "nonendemic_nltt") {
    sumsta <- nltt_list$nonendemic_nltt_error
    sumsta_label <- " nonendemic species \u0394nLTT"
  } else if (nltt_name == "num_spec") {
    sumsta <- nltt_list$num_spec_error
    sumsta_label <- " species number"
  } else if (nltt_name == "num_col") {
    sumsta <- nltt_list$num_col_error
    sumsta_label <- " colonist number"
  }

  data <- data.frame(sumsta)
  colnames(data) <- c("summary statistics")
  data <- tidyr::gather(data) #change gather to pivot_longer
  title_label <- paste0(sumsta_label)

  ggplot2::ggplot(data = data, ggplot2::aes(x = value, fill = key)) +
    ggplot2::geom_histogram(binwidth = 1,
                            alpha = 0.5,
                            position = "identity",
                            color = "black") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::ylab("Count") +
    ggplot2::xlab("summary statistics") +
    ggplot2::ggtitle(title_label)
}

sim_example <- sim_gam1
plot_single_nltt (1,sim_example,sim_example,500,nltt_name = "spec_nltt")
plot_single_nltt (1,sim_example,sim_example,500,nltt_name = "endemic_nltt")
plot_single_nltt (1,sim_example,sim_example,500,nltt_name = "nonendemic_nltt")
plot_single_nltt (1,sim_example,sim_example,500,nltt_name = "num_spec")
plot_single_nltt (1,sim_example,sim_example,500,nltt_name = "num_col")


sim_example <- sim_gam1
plot_single_nltt (1,sim_example,sim_example,500,nltt_name = "spec_nltt")
plot_single_nltt (2,sim_example,sim_example,500,nltt_name = "spec_nltt")
plot_single_nltt (3,sim_example,sim_example,500,nltt_name = "spec_nltt")
plot_single_nltt (4,sim_example,sim_example,500,nltt_name = "spec_nltt")
plot_single_nltt (5,sim_example,sim_example,500,nltt_name = "spec_nltt")
plot_single_nltt (6,sim_example,sim_example,500,nltt_name = "spec_nltt")
plot_single_nltt (7,sim_example,sim_example,500,nltt_name = "spec_nltt")
plot_single_nltt (8,sim_example,sim_example,500,nltt_name = "spec_nltt")
plot_single_nltt (9,sim_example,sim_example,500,nltt_name = "spec_nltt")
plot_single_nltt (10,sim_example,sim_example,500,nltt_name = "spec_nltt")





