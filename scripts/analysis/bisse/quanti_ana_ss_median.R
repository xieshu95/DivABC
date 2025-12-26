# quantitative analyses combining 6 parameters among ss sets
if(!require(dplyr)) install.packages("dplyr");   library(dplyr)
if(!require(tidyr)) install.packages("tidyr");   library(tidyr)
if(!require(rstatix)) install.packages("rstatix"); library(rstatix)
load("Data/BiSSE/D/AMM_per_set_test_ss3.RData")
theta_true <- AMM_all_df[1:350,1:6]
colnames(theta_true) <- c("param1","param2","param3","param4","param5","param6")
ss1_est <- AMM_all_df[1:350,8:13]
colnames(ss1_est) <- c("param1","param2","param3","param4","param5","param6")

load("Data/BiSSE/nltt/AMM_per_set_test_ss9.RData")
ss2_est <- AMM_all_df[1:350,8:13]
colnames(ss2_est) <- c("param1","param2","param3","param4","param5","param6")

load("Data/BiSSE/nltt_D/AMM_per_set_test_ss2.RData")
ss3_est <- AMM_all_df[1:350,8:13]
colnames(ss3_est) <- c("param1","param2","param3","param4","param5","param6")

load("Data/BiSSE/nltt_ratio/AMM_per_set_test_ss12.RData")
ss4_est <- AMM_all_df[1:350,8:13]
colnames(ss4_est) <- c("param1","param2","param3","param4","param5","param6")

load("Data/BiSSE/nltt_mpds/AMM_per_set_test_ss7.RData")
ss5_est <- AMM_all_df[1:350,8:13]
colnames(ss5_est) <- c("param1","param2","param3","param4","param5","param6")

load("Data/BiSSE/nltt_mntds/AMM_per_set_test_ss10.RData")
ss6_est <- AMM_all_df[1:350,8:13]
colnames(ss6_est) <- c("param1","param2","param3","param4","param5","param6")

load("Data/BiSSE/nltt_colless/AMM_per_set_test_ss11.RData")
ss7_est <- AMM_all_df[1:350,8:13]
colnames(ss7_est) <- c("param1","param2","param3","param4","param5","param6")

load("Data/BiSSE/nltts/AMM_per_set_test_ss1.RData")
ss8_est <- AMM_all_df[1:350,8:13]
colnames(ss8_est) <- c("param1","param2","param3","param4","param5","param6")

load("Data/BiSSE/nltts_D/AMM_per_set_test_ss0.RData")
ss9_est <- AMM_all_df[1:350,8:13]
colnames(ss9_est) <- c("param1","param2","param3","param4","param5","param6")


mle_est <- AMM_all_df[1:350,32:37]
colnames(mle_est) <- c("param1","param2","param3","param4","param5","param6")
mcmc_est <- AMM_all_df[1:350,22:27]
colnames(mcmc_est) <- c("param1","param2","param3","param4","param5","param6")


load("Data/BiSSE/nltt_mntds_D/AMM_per_set_test_ss16.RData")
ss10_est <- AMM_all_df[1:350,8:13]
colnames(ss10_est) <- c("param1","param2","param3","param4","param5","param6")

load("Data/BiSSE/nltts_mntds_D/AMM_per_set_test_ss15.RData")
ss11_est <- AMM_all_df[1:350,8:13]
colnames(ss11_est) <- c("param1","param2","param3","param4","param5","param6")

df_true <- theta_true %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="true")
df_ss1  <- ss1_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss1")
df_ss2  <- ss2_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss2")
df_ss3  <- ss3_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss3")
df_ss4  <- ss4_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss4")
df_ss5  <- ss5_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss5")
df_ss6  <- ss6_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss6")
df_ss7  <- ss7_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss7")
df_ss8  <- ss8_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss8")
df_ss9  <- ss9_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss9")
df_ss10  <- ss10_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss10")
df_ss11  <- ss11_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="ss11")
df_mle  <- mle_est    %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="MLE")
df_mcmc <- mcmc_est   %>% mutate(rep=1:n()) %>% pivot_longer(-rep, names_to="param", values_to="estimate") %>% mutate(method="MCMC")

df_all <- bind_rows(df_ss1, df_ss2,df_ss3, df_ss4, df_ss5,df_ss6, df_ss7, df_ss8,df_ss9,df_ss10,df_ss11,df_mle,df_mcmc) %>%
  left_join(df_true, by=c("rep","param")) %>%
  mutate(error = estimate - true)

#── 2. calculate Bias/MSE/MAE ──────────────────#
library(dplyr)
library(multcompView)

summary_by_rep <- df_all %>%
  dplyr::group_by(method, rep) %>%
  dplyr::summarise(
    euclid= sqrt(sum((error)^2, na.rm = TRUE)),
    bias = mean(error,na.rm = TRUE),
    mae = mean(abs(error),na.rm = TRUE),
    mse= mean(sqrt(sum((error)^2, na.rm = TRUE))),
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

