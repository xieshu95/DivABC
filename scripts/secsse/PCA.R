load("G:/results/project 2/tip_info/round4/secsse_long/test_ss_df_500.RData")
# install.packages("factoextra")
library(factoextra)
res.pca <- prcomp(ss, scale = TRUE)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_eig(res.pca)

var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}
# Compute Coordinates
#::::::::::::::::::::::::::::::::::::::::
loadings <- res.pca$rotation
sdev <- res.pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev))

fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

var.cos2 <- var.coord^2

var <- get_pca_var(res.pca)
var


library("corrplot")
corrplot(var$contrib, is.corr=FALSE)

var$contrib

fviz_contrib(res.pca, choice = "var", axes = 1:2)
