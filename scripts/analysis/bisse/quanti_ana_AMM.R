##### combination
# quantitative analyses combining 6 parameters among ABC/MCMC/MLE
load("Data/BiSSE/nltts_D/AMM_per_set_test_ss0.RData")
if(!require(dplyr)) install.packages("dplyr");   library(dplyr)
if(!require(tidyr)) install.packages("tidyr");   library(tidyr)
if(!require(rstatix)) install.packages("rstatix"); library(rstatix)
L_mean <- c()
L_sd <- c()
for (i in 1:7) {
  theta_true <- AMM_all_df[(50*i-49):(50*i),1:6]
  colnames(theta_true) <- c("param1","param2","param3","param4","param5","param6")
  mle_est <- AMM_all_df[(50*i-49):(50*i),32:37]
  colnames(mle_est) <- c("param1","param2","param3","param4","param5","param6")
  mcmc_est <- AMM_all_df[(50*i-49):(50*i),22:27]
  colnames(mcmc_est) <- c("param1","param2","param3","param4","param5","param6")
  abc_est  <- AMM_all_df[(50*i-49):(50*i),8:13]
  colnames(abc_est) <- c("param1","param2","param3","param4","param5","param6")
  df_true <- theta_true %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="true")
  df_mle  <- mle_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="MLE")
  df_mcmc <- mcmc_est   %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="MCMC")
  df_abc  <- abc_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ABC")

  df_all <- bind_rows(df_mle, df_mcmc, df_abc) %>%
    left_join(df_true, by=c("rep","param")) %>%
    mutate(error = estimate - true)



  library(dplyr)
  library(multcompView)

  summary_by_rep <- df_all %>%
    dplyr::group_by(method, rep) %>%
    dplyr::summarise(
      euclid= sqrt(sum((error)^2, na.rm = TRUE)),
      bias = mean(error,na.rm = TRUE),
      .groups = "drop"
    )
  aov_res <- aov(euclid ~ method, data = summary_by_rep)
  anova_p <- summary(aov_res)[[1]][["Pr(>F)"]][1]
  # 2) Tukey HSD
  tuk <- TukeyHSD(aov_res, "method", conf.level = 0.95)
  letters_obj <- multcompView::multcompLetters4(aov_res, tuk)
  letters_obj <- letters_obj$method
  letters_df_all <- data.frame(method = names(letters_obj$Letters),
                               letter = as.character(letters_obj$Letters),
                               stringsAsFactors = FALSE)

  summary_euc <- summary_by_rep %>%
    group_by(method) %>%
    summarise(mean_euclid = mean(euclid, na.rm = TRUE),
              sd_euclid   = sd(euclid, na.rm = TRUE),
              n = n(), .groups = "drop") %>%
    left_join(letters_df_all, by = "method") %>%
    arrange(desc(mean_euclid))
  cat("ANOVA p-value:", signif(anova_p, 6), "\n\n")
  print(summary_euc, n = Inf)
}

