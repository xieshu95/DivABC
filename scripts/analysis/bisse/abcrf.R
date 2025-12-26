#' Summary statistics selection using ABCRF
library(abcrf)
library(treestats)
library(ape)
library(R.utils)
library(MASS)

# generate training data frame with single parameter
load("Data/BiSSE/abcrf_ref_df.RData")
sum_cols <- names(unlist(ref_df[1,7:20]))

train_lambda1 <- ref_df[, c("lambda1", sum_cols)]
train_lambda2 <- ref_df[, c("lambda2", sum_cols)]
train_mu1     <- ref_df[, c("mu1",     sum_cols)]
train_mu2     <- ref_df[, c("mu2",     sum_cols)]
train_q12     <- ref_df[, c("q12",     sum_cols)]
train_q21     <- ref_df[, c("q21",     sum_cols)]
colnames(train_lambda1) <- c("lambda1","Ratio",
                             "MPD","MPD0","MPD1",
                             "MNTD","MNTD0","MNTD1",
                             "D","nLTT","nLTT0","nLTT1",
                             "Colless","Colless0","Colless1")
colnames(train_lambda2) <- c("lambda2","Ratio",
                             "MPD","MPD0","MPD1",
                             "MNTD","MNTD0","MNTD1",
                             "D","nLTT","nLTT0","nLTT1",
                             "Colless","Colless0","Colless1")
colnames(train_mu1)     <- c("mu1","Ratio",
                             "MPD","MPD0","MPD1",
                             "MNTD","MNTD0","MNTD1",
                             "D","nLTT","nLTT0","nLTT1",
                             "Colless","Colless0","Colless1")
colnames(train_mu2)     <- c("mu2","Ratio",
                             "MPD","MPD0","MPD1",
                             "MNTD","MNTD0","MNTD1",
                             "D","nLTT","nLTT0","nLTT1",
                             "Colless","Colless0","Colless1")
colnames(train_q12)     <- c("q12","Ratio",
                             "MPD","MPD0","MPD1",
                             "MNTD","MNTD0","MNTD1",
                             "D","nLTT","nLTT0","nLTT1",
                             "Colless","Colless0","Colless1")
colnames(train_q21)     <- c("q21","Ratio",
                             "MPD","MPD0","MPD1",
                             "MNTD","MNTD0","MNTD1",
                             "D","nLTT","nLTT0","nLTT1",
                             "Colless","Colless0","Colless1")

# ---------- train regAbcrf for each parameter ----------
set.seed(10)
ntree = 500
cat("Training regAbcrf for Speciation 0...\n")
model_lambda1 <- regAbcrf(lambda1 ~ ., data = train_lambda1, ntree = ntree,mtry = 3)
cat("Training regAbcrf for Speciation 1...\n")
model_lambda2 <- regAbcrf(lambda2 ~ ., data = train_lambda2, ntree = ntree,mtry = 3)
cat("Training regAbcrf for Extinction 0...\n")
model_mu1 <- regAbcrf(mu1 ~ ., data = train_mu1, ntree = ntree,mtry = 3)
cat("Training regAbcrf for Extinction 1...\n")
model_mu2 <- regAbcrf(mu2 ~ ., data = train_mu2, ntree = ntree,mtry = 3)
cat("Training regAbcrf for Transition 01...\n")
model_q12 <- regAbcrf(q12 ~ ., data = train_q12, ntree = ntree,mtry = 3)
cat("Training regAbcrf for Transition 10...\n")
model_q21 <- regAbcrf(q21 ~ ., data = train_q21, ntree = ntree,mtry = 3)



model_lambda1[["model.rf"]][["variable.importance"]] <- model_lambda1[["model.rf"]][["variable.importance"]]/sum(model_lambda1[["model.rf"]][["variable.importance"]])
model_lambda2[["model.rf"]][["variable.importance"]] <-  model_lambda2[["model.rf"]][["variable.importance"]]/sum(model_lambda2[["model.rf"]][["variable.importance"]])
model_mu1[["model.rf"]][["variable.importance"]] <- model_mu1[["model.rf"]][["variable.importance"]]/sum(model_mu1[["model.rf"]][["variable.importance"]])
model_mu2[["model.rf"]][["variable.importance"]] <- model_mu2[["model.rf"]][["variable.importance"]]/sum(model_mu2[["model.rf"]][["variable.importance"]])
model_q12[["model.rf"]][["variable.importance"]] <- model_q12[["model.rf"]][["variable.importance"]]/sum(model_q12[["model.rf"]][["variable.importance"]])
model_q21[["model.rf"]][["variable.importance"]] <- model_q21[["model.rf"]][["variable.importance"]]/sum(model_q21[["model.rf"]][["variable.importance"]])


plot(model_lambda1, main = "Variable importance for Speciation 0")
plot(model_lambda2, main = "Variable importance for Speciation 1")
plot(model_mu1, main = "Variable importance for Extinction 0")
plot(model_mu2, main = "Variable importance for Extinction 1")
plot(model_q12, main = "Variable importance for Transition 01")
plot(model_q21, main = "Variable importance for Transition 10")


importance_mean <- (model_lambda1[["model.rf"]][["variable.importance"]] +
                      model_lambda2[["model.rf"]][["variable.importance"]] +
                      model_mu1[["model.rf"]][["variable.importance"]] +
                      model_mu2[["model.rf"]][["variable.importance"]] +
                      model_q12[["model.rf"]][["variable.importance"]] +
                      model_q21[["model.rf"]][["variable.importance"]])/6
model <- model_lambda1
model[["model.rf"]][["variable.importance"]] <- importance_mean
plot(model, main = "Variable importance")

