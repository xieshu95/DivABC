## plot phylogenetic tree with tip states
library(ape)
# 1. generate the entire tree
t<-read.tree(text='(((H:1, G:1):1, (F:1, E:1):1):1 , ((D:1, C:1):1, (B:1, A:1):1):1):1;')
plot(t)





