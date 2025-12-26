# geosse analysis
# abcrf data analysis
library(abcrf)
library(treestats)
library(ape)
library(R.utils)
library(MASS)

# generate training data frame with single parameter
load("Data/GeoSSE/abcrf_ref_df.RData")
sum_cols <- names(unlist(ref_df[1,]))[8:18]


train_lambda1 <- ref_df[, c("sA", sum_cols)]
train_lambda2 <- ref_df[, c("sB", sum_cols)]
train_lambda3 <- ref_df[, c("sAB",sum_cols)]
train_mu1     <- ref_df[, c("xA", sum_cols)]
train_mu2     <- ref_df[, c("xB", sum_cols)]
train_q1     <- ref_df[, c("dA", sum_cols)]
train_q2     <- ref_df[, c("dB", sum_cols)]

colnames(train_lambda1) <- c("sA",
                             "nLTT","nLTT1","nLTT2","nLTT3",
                             "D12","D13","D23",
                             "D1_23","D2_13","D3_12","M")
colnames(train_lambda2) <- c("sB",
                             "nLTT","nLTT1","nLTT2","nLTT3",
                             "D12","D13","D23",
                             "D1_23","D2_13","D3_12","M")
colnames(train_lambda3) <- c("sAB",
                             "nLTT","nLTT1","nLTT2","nLTT3",
                             "D12","D13","D23",
                             "D1_23","D2_13","D3_12","M")
colnames(train_mu1)     <- c("xA","nLTT","nLTT1","nLTT2","nLTT3",
                             "D12","D13","D23",
                             "D1_23","D2_13","D3_12","M")
colnames(train_mu2)     <- c("xB","nLTT","nLTT1","nLTT2","nLTT3",
                             "D12","D13","D23",
                             "D1_23","D2_13","D3_12","M")
colnames(train_q1)     <- c("dA","nLTT","nLTT1","nLTT2","nLTT3",
                            "D12","D13","D23",
                            "D1_23","D2_13","D3_12","M")
colnames(train_q2)     <- c("dB","nLTT","nLTT1","nLTT2","nLTT3",
                            "D12","D13","D23",
                            "D1_23","D2_13","D3_12","M")

# train regAbcrf for each parameter ----------
set.seed(42)
ntree = 500
cat("Training regAbcrf for lambda1...\n")
model_lambda1 <- regAbcrf(sA ~ ., data = train_lambda1, ntree = ntree,mtry = 5)
cat("Training regAbcrf for lambda2...\n")
model_lambda2 <- regAbcrf(sB ~ ., data = train_lambda2, ntree = ntree,mtry = 5)
cat("Training regAbcrf for lambda3...\n")
model_lambda3 <- regAbcrf(sAB ~ ., data = train_lambda3, ntree = ntree,mtry = 5)
cat("Training regAbcrf for mu1...\n")
model_mu1 <- regAbcrf(xA ~ ., data = train_mu1, ntree = ntree,mtry = 5)
cat("Training regAbcrf for mu2...\n")
model_mu2 <- regAbcrf(xB ~ ., data = train_mu2, ntree = ntree,mtry = 5)
cat("Training regAbcrf for q12...\n")
model_q1 <- regAbcrf(dA ~ ., data = train_q1, ntree = ntree,mtry = 5)
cat("Training regAbcrf for q21...\n")
model_q2 <- regAbcrf(dB ~ ., data = train_q2, ntree = ntree,mtry = 5)



plot(model_lambda1, main = "Variable importance for Speciation 1")
plot(model_lambda2, main = "Variable importance for Speciation 2")
plot(model_lambda3, main = "Variable importance for Speciation 3")
plot(model_mu1,     main = "Variable importance for Extinction 1")
plot(model_mu2,     main = "Variable importance for Extinction 2")
plot(model_q1,     main = "Variable importance for Transition 12")
plot(model_q2,     main = "Variable importance for Transition 21")

importance1 <- model_lambda1[["model.rf"]][["variable.importance"]]/sum(model_lambda1[["model.rf"]][["variable.importance"]])
importance2 <- model_lambda2[["model.rf"]][["variable.importance"]]/sum(model_lambda2[["model.rf"]][["variable.importance"]])
importance3 <- model_lambda3[["model.rf"]][["variable.importance"]]/sum(model_lambda3[["model.rf"]][["variable.importance"]])
importance4 <- model_mu1[["model.rf"]][["variable.importance"]]/sum(model_mu1[["model.rf"]][["variable.importance"]])
importance5 <- model_mu2[["model.rf"]][["variable.importance"]]/sum(model_mu2[["model.rf"]][["variable.importance"]])
importance6 <- model_q1[["model.rf"]][["variable.importance"]]/sum(model_q1[["model.rf"]][["variable.importance"]])
importance7 <- model_q2[["model.rf"]][["variable.importance"]]/sum(model_q2[["model.rf"]][["variable.importance"]])
importance_all <- (importance1 + importance2 + importance3 + importance4 + importance5 + importance6)/6

model <- model_lambda1
model[["model.rf"]][["variable.importance"]] <- importance_all
plot(model, main = "Variable importance in GeoSSE")


