
## 3D plots
library(reshape2)
library(plotly)
library(htmlwidgets)
library(webshot)
for(i in 1:100){
  load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3_new3/loglik_trace/test5/loglik5/pars_loglik",i,".RData"))
  plot_matrix <- t(acast(pars_ll, mu1~mu2, value.var="loglik"))
  p <- plot_ly(
    x = as.numeric(colnames(plot_matrix)),
    y = as.numeric(rownames(plot_matrix)),
    z = plot_matrix, type = "heatmap",colors = "Greys",
  )
  saveWidget(p, paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3_new3/loglik_trace/test5/heatmap/tree_",i,".html"))
  plotly::export(p = p, #the graph to export
                 file = paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3_new3/loglik_trace/test5/heatmap/tree_",i,".png"))
}




# plot(x = pars_ll[,1],y = pars_ll[,2])



## visualized 3D(can change angle)

# plot_ly(
#   x = as.numeric(colnames(plot_matrix)),
#   y = as.numeric(rownames(plot_matrix)),
#   z = plot_matrix
# ) %>%
#   add_surface() %>%
#   layout(
#     title = "",
#     scene = list(
#       xaxis = list(type = "log", title = "Total observations"),
#       yaxis = list(type = "log", title = "Firm size"),
#       zaxis = list(title = "Median"),
#       camera = list(eye = list(x = 1.95, y = -1.25, z = 1.25))
#     ))

library(plotly)





#
# persp(x = as.numeric(colnames(plot_matrix)),
#       y = as.numeric(rownames(plot_matrix)),
#       z = plot_matrix,
#       xlab = "mu1",
#       ylab = "mu2",
#       zlab = "loglik",
#       ticktype ='detailed',
#       theta = 120,
#       phi = 40,
#       col = "red", shade = 0.3)



p1 <- pars_ll[which(pars_ll$mu1 == 0.5),]
p1
plot(x = p1[,2],y = p1[,3])


library(plotly)
p <- plot_ly(
  x = as.numeric(colnames(plot_matrix)),
  y = as.numeric(rownames(plot_matrix)),
  z = plot_matrix, type = "heatmap"
)
p
