# quantitative analyses combining 6 parameters among ss sets
if(!require(dplyr)) install.packages("dplyr");   library(dplyr)
if(!require(tidyr)) install.packages("tidyr");   library(tidyr)
if(!require(rstatix)) install.packages("rstatix"); library(rstatix)
# example
load("Data/MuSSE/AMM_per_set_test_ss0.RData")  # subtree
theta_true <- AMM_all_df[1:90,1:7]
colnames(theta_true) <- c("param1","param2","param3","param4","param5","param6","param7")
ss1_est <- AMM_all_df[1:90,9:15]
colnames(ss1_est) <- c("param1","param2","param3","param4","param5","param6","param7")

load("Data/MuSSE/AMM_per_set_test_ss1.RData")   # M
ss2_est <- AMM_all_df[1:90,9:15]
colnames(ss2_est) <- c("param1","param2","param3","param4","param5","param6","param7")

load("Data/MuSSE/AMM_per_set_test_ss2.RData")  # merge
ss3_est <- AMM_all_df[1:90,9:15]
colnames(ss3_est) <- c("param1","param2","param3","param4","param5","param6","param7")

load("Data/MuSSE/AMM_per_set_test_ss0.RData") #MCMC
ss4_est <- AMM_all_df[1:90,16:22]
colnames(ss4_est) <- c("param1","param2","param3","param4","param5","param6","param7")

load("Data/MuSSE/AMM_per_set_test_ss0.RData") #MLE
ss5_est <- AMM_all_df[1:90,23:29]
colnames(ss5_est) <- c("param1","param2","param3","param4","param5","param6","param7")

df_true <- theta_true %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="true")
df_ss1  <- ss1_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss1")
df_ss2  <- ss2_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss2")
df_ss3  <- ss3_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss3")
df_ss4  <- ss4_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss4")
df_ss5  <- ss5_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss5")


df_all <- bind_rows(df_ss1, df_ss2,df_ss3, df_ss4, df_ss5) %>%
  left_join(df_true, by=c("rep","param")) %>%
  mutate(error = estimate - true)

summary_by_rep <- df_all %>%
  dplyr::group_by(method, rep) %>%
  dplyr::summarise(
    euclid     = sqrt(sum((error)^2)),
    .groups = "drop"
  )

aov_res <- aov(euclid ~ method, data = summary_by_rep)
anova_p <- summary(aov_res)[[1]][["Pr(>F)"]][1]
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
