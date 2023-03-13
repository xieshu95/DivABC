## dispersion of nltt(1000 reps) from different parameter set
load(file = "results/sim_gam1.RData")
load(file = "results/sim_gam2.RData")
load(file = "results/sim_lam3.RData")
load(file = "results/sim_gam4.RData")
load(file = "results/sim_gam5.RData")

## nltt_name includes: "spec_nltt","endemic_nltt","nonendemic_nltt","num_spec","num_col"
# sumsta: summary statistic

## this function use sim1[[1]] as the observed data
plot_multi_nltt <- function(obs_rep,sim1, sim2,sim3, sim4, sim5,replicates,nltt_name,ss_name){
  nltt_list1 <- nltt_within_param(obs_rep = obs_rep,sim1 = sim1,sim2 = sim1, replicates = replicates)
  nltt_list2 <- nltt_within_param(obs_rep = obs_rep,sim1 = sim1,sim2 = sim2, replicates = replicates)
  nltt_list3 <- nltt_within_param(obs_rep = obs_rep,sim1 = sim1,sim2 = sim3, replicates = replicates)
  nltt_list4 <- nltt_within_param(obs_rep = obs_rep,sim1 = sim1,sim2 = sim4, replicates = replicates)
  nltt_list5 <- nltt_within_param(obs_rep = obs_rep,sim1 = sim1,sim2 = sim5, replicates = replicates)

  if (nltt_name == "spec_nltt") {
    sumsta1 <- nltt_list1$spec_nltt_error
    sumsta2 <- nltt_list2$spec_nltt_error
    sumsta3 <- nltt_list3$spec_nltt_error
    sumsta4 <- nltt_list4$spec_nltt_error
    sumsta5 <- nltt_list5$spec_nltt_error
    sumsta_label <- " species \u0394nLTT"
  } else if (nltt_name == "endemic_nltt") {
    sumsta1 <- nltt_list1$endemic_nltt_error
    sumsta2 <- nltt_list2$endemic_nltt_error
    sumsta3 <- nltt_list3$endemic_nltt_error
    sumsta4 <- nltt_list4$endemic_nltt_error
    sumsta5 <- nltt_list5$endemic_nltt_error
    sumsta_label <- " endemic species \u0394nLTT"
  } else if (nltt_name == "nonendemic_nltt") {
    sumsta1 <- nltt_list1$nonendemic_nltt_error
    sumsta2 <- nltt_list2$nonendemic_nltt_error
    sumsta3 <- nltt_list3$nonendemic_nltt_error
    sumsta4 <- nltt_list4$nonendemic_nltt_error
    sumsta5 <- nltt_list5$nonendemic_nltt_error
    sumsta_label <- " nonendemic species \u0394nLTT"
  } else if (nltt_name == "num_spec") {
    sumsta1 <- nltt_list1$num_spec_error
    sumsta2 <- nltt_list2$num_spec_error
    sumsta3 <- nltt_list3$num_spec_error
    sumsta4 <- nltt_list4$num_spec_error
    sumsta5 <- nltt_list5$num_spec_error
    sumsta_label <- " species number"
  } else if (nltt_name == "num_col") {
    sumsta1 <- nltt_list1$num_col_error
    sumsta2 <- nltt_list2$num_col_error
    sumsta3 <- nltt_list3$num_col_error
    sumsta4 <- nltt_list4$num_col_error
    sumsta5 <- nltt_list5$num_col_error
    sumsta_label <- " colonist number"
  }

  data <- data.frame(sumsta1,sumsta2,sumsta3,sumsta4,sumsta5)
  colnames(data) <- c(paste0(ss_name, 1),
                      paste0(ss_name, 2),
                      paste0(ss_name, 3),
                      paste0(ss_name, 4),
                      paste0(ss_name, 5))
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

## set sim_gam1[[1]](first replicate) as the observed data
plot_multi_nltt (1,sim_gam1,sim_gam2,sim_lam3,sim_gam4,sim_gam5,500,nltt_name = "spec_nltt",ss_name = "gam")
plot_multi_nltt (1,sim_gam1,sim_gam2,sim_lam3,sim_gam4,sim_gam5,500,nltt_name = "endemic_nltt",ss_name = "gam")
plot_multi_nltt (1,sim_gam1,sim_gam2,sim_lam3,sim_gam4,sim_gam5,500,nltt_name = "nonendemic_nltt",ss_name = "gam")
plot_multi_nltt (1,sim_gam1,sim_gam2,sim_lam3,sim_gam4,sim_gam5,500,nltt_name = "num_spec",ss_name = "gam")
plot_multi_nltt (1,sim_gam1,sim_gam2,sim_lam3,sim_gam4,sim_gam5,500,nltt_name = "num_col",ss_name = "gam")


load(file = "results/sim_lam1.RData")
load(file = "results/sim_lam2.RData")
load(file = "results/sim_lam3.RData")
load(file = "results/sim_lam4.RData")
load(file = "results/sim_lam5.RData")
#### this function use simi[[1]] as the observed data, compare the diff_nltt of itself
plot_multi_nltt_sep <- function(obs_rep,sim1, sim2,sim3, sim4, sim5,replicates,nltt_name,ss_name){
  nltt_list1 <- nltt_within_param(obs_rep = obs_rep,sim1 = sim1,sim2 = sim1, replicates = replicates)
  nltt_list2 <- nltt_within_param(obs_rep = obs_rep,sim1 = sim2,sim2 = sim2, replicates = replicates)
  nltt_list3 <- nltt_within_param(obs_rep = obs_rep,sim1 = sim3,sim2 = sim3, replicates = replicates)
  nltt_list4 <- nltt_within_param(obs_rep = obs_rep,sim1 = sim4,sim2 = sim4, replicates = replicates)
  nltt_list5 <- nltt_within_param(obs_rep = obs_rep,sim1 = sim5,sim2 = sim5, replicates = replicates)

  if (nltt_name == "spec_nltt") {
    sumsta1 <- nltt_list1$spec_nltt_error
    sumsta2 <- nltt_list2$spec_nltt_error
    sumsta3 <- nltt_list3$spec_nltt_error
    sumsta4 <- nltt_list4$spec_nltt_error
    sumsta5 <- nltt_list5$spec_nltt_error
    sumsta_label <- " species \u0394nLTT"
  } else if (nltt_name == "endemic_nltt") {
    sumsta1 <- nltt_list1$endemic_nltt_error
    sumsta2 <- nltt_list2$endemic_nltt_error
    sumsta3 <- nltt_list3$endemic_nltt_error
    sumsta4 <- nltt_list4$endemic_nltt_error
    sumsta5 <- nltt_list5$endemic_nltt_error
    sumsta_label <- " endemic species \u0394nLTT"
  } else if (nltt_name == "nonendemic_nltt") {
    sumsta1 <- nltt_list1$nonendemic_nltt_error
    sumsta2 <- nltt_list2$nonendemic_nltt_error
    sumsta3 <- nltt_list3$nonendemic_nltt_error
    sumsta4 <- nltt_list4$nonendemic_nltt_error
    sumsta5 <- nltt_list5$nonendemic_nltt_error
    sumsta_label <- " nonendemic species \u0394nLTT"
  } else if (nltt_name == "num_spec") {
    sumsta1 <- nltt_list1$num_spec_error
    sumsta2 <- nltt_list2$num_spec_error
    sumsta3 <- nltt_list3$num_spec_error
    sumsta4 <- nltt_list4$num_spec_error
    sumsta5 <- nltt_list5$num_spec_error
    sumsta_label <- " species number"
  } else if (nltt_name == "num_col") {
    sumsta1 <- nltt_list1$num_col_error
    sumsta2 <- nltt_list2$num_col_error
    sumsta3 <- nltt_list3$num_col_error
    sumsta4 <- nltt_list4$num_col_error
    sumsta5 <- nltt_list5$num_col_error
    sumsta_label <- " colonist number"
  }

  data <- data.frame(sumsta1,sumsta2,sumsta3,sumsta4,sumsta5)
  colnames(data) <- c(paste0(ss_name, 1),
                      paste0(ss_name, 2),
                      paste0(ss_name, 3),
                      paste0(ss_name, 4),
                      paste0(ss_name, 5))
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

## set sim_gam1[[1]](first replicate) as the observed data
plot_multi_nltt_sep (1,sim_lam1,sim_lam2,sim_lam3,sim_lam4,sim_lam5,500,nltt_name = "spec_nltt",ss_name = "lam")
plot_multi_nltt_sep (1,sim_lam1,sim_lam2,sim_lam3,sim_lam4,sim_lam5,500,nltt_name = "endemic_nltt",ss_name = "lam")
plot_multi_nltt_sep (1,sim_lam1,sim_lam2,sim_lam3,sim_lam4,sim_lam5,500,nltt_name = "nonendemic_nltt",ss_name = "lam")
plot_multi_nltt_sep (1,sim_lam1,sim_lam2,sim_lam3,sim_lam4,sim_lam5,500,nltt_name = "num_spec",ss_name = "lam")
plot_multi_nltt_sep (1,sim_lam1,sim_lam2,sim_lam3,sim_lam4,sim_lam5,500,nltt_name = "num_col",ss_name = "lam")


plot_multi_nltt_sep (1,sim_mu1,sim_mu2,sim_lam3,sim_mu4,sim_mu5,500,nltt_name = "spec_nltt",ss_name = "mu")
plot_multi_nltt_sep (1,sim_mu1,sim_mu2,sim_lam3,sim_mu4,sim_mu5,500,nltt_name = "endemic_nltt",ss_name = "mu")
plot_multi_nltt_sep (1,sim_mu1,sim_mu2,sim_lam3,sim_mu4,sim_mu5,500,nltt_name = "nonendemic_nltt",ss_name = "mu")
plot_multi_nltt_sep (1,sim_mu1,sim_mu2,sim_lam3,sim_mu4,sim_mu5,500,nltt_name = "num_spec",ss_name = "mu")
plot_multi_nltt_sep (1,sim_mu1,sim_mu2,sim_lam3,sim_mu4,sim_mu5,500,nltt_name = "num_col",ss_name = "mu")
