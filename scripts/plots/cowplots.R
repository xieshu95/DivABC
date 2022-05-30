### plot relationships between rates

# folder_path <- "G:/results/project 2/tip_info/round3/est_allpars/DAISIE_ABC"
# files <- list.files(folder_path)
# param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC.csv")
# i = 1
# file_to_load <- grep(paste0("DAISIE_ABC_param_set_", i,".RData"),
#                      files,
#                      value = TRUE,
#                      fixed = TRUE)
# load(file.path(folder_path, file_to_load))
library(ggplot2)
load("G:/results/project 2/tip_info/round3/est_allpars/whole_df_ABC.RData")
# param_abc <- list()
for(i in 1:40){
  i= 1
  param_abc <- whole_df_ABC[((i*5000-4999)):(i*5000),]
  p_lac <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
    #                       fill = "#009E73",colour = "#009E73",
    #                       alpha = 0.3, binwidth = 0.01) +
    ggplot2::geom_density(ggplot2::aes(x = lac_abc),
                          fill = "royalblue",colour = "blue3",
                          alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda^c))+
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)

  p_mu <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_abc),
    #                       fill = "#009E73",colour = "#009E73",
    #                       alpha = 0.3, binwidth = 0.01) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = mu_abc),
                          fill = "royalblue",colour = "blue3",
                          alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(mu))+
    ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", size = 0.5)

  p_gam <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,0.07)+
    # ggplot2::geom_histogram(mapping = ggplot2::aes(x = gam_abc),
    #                       fill = "#009E73",colour = "#009E73",
    #                       alpha = 0.3, binwidth = 0.0005) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = gam_abc),
                          fill = "royalblue",colour = "blue3",
                          alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(gamma))+
    ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed", size = 0.5)


  p_laa <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    # ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_abc),
    #                       fill = "#009E73",colour = "#009E73",
    #                       alpha = 0.3, binwidth = 0.01) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = laa_abc),
                          fill = "royalblue",colour = "blue3",
                          alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda^a))+
    ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.5)

  mu_vs_lac <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = mu_abc),
                        colour = "royalblue",shape = 16,alpha = 0.2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(mu)) +
    ggplot2::xlab(expression(lambda^c)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = mu),
                        colour = "black",shape = 16,size = 2.5)
    # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
    # ggplot2::geom_hline(data= param_abc, aes(yintercept = mu), colour = "grey50")

  gam_vs_lac <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = gam_abc),
                        colour = "royalblue",shape = 16,alpha = 0.2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(gamma)) +
    ggplot2::xlab(expression(lambda^c)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = gam),
                        colour = "black",shape = 16,size = 2.5)
    # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
    # ggplot2::geom_hline(data= param_abc, aes(yintercept = gam), colour = "grey50")

  laa_vs_lac <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = laa_abc),
                        colour = "royalblue",shape = 16,alpha = 0.2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(lambda^a)) +
    ggplot2::xlab(expression(lambda^c)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = laa),
                        colour = "black",shape = 16,size = 2.5)
    # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
    # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")

  gam_vs_mu <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    ggplot2::geom_point(mapping = ggplot2::aes(x = mu_abc,y = gam_abc),
                        colour = "royalblue",shape = 16,alpha = 0.2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(gamma)) +
    ggplot2::xlab(expression(mu)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = mu,y = gam),
                        colour = "black",shape = 16,size = 2.5)
    # ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), colour = "grey50") +
    # ggplot2::geom_hline(data= param_abc, aes(yintercept = gam), colour = "grey50")

  laa_vs_mu <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,2)+
    ggplot2::geom_point(mapping = ggplot2::aes(x = mu_abc,y = laa_abc),
                        colour = "royalblue",shape = 16,alpha = 0.2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(lambda^a)) +
    ggplot2::xlab(expression(mu)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = mu,y = laa),
                        colour = "black",shape = 16,size = 2.5)
    # ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), colour = "grey50") +
    # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")

  laa_vs_gam <- ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,0.07)+
    ggplot2::geom_point(mapping = ggplot2::aes(x = gam_abc,y = laa_abc),
                        colour = "royalblue",shape = 16,alpha = 0.2) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(lambda^a)) +
    ggplot2::xlab(expression(gamma)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = gam,y = laa),
                        colour = "black",shape = 16,size = 2.5)
    # ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), colour = "grey50") +
    # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")

  p_emp <- ggplot() + theme_void()

  tiff(paste0("G:/results/project 2/tip_info/round3/est_allpars/cowplots/param_",i,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_lac,p_emp,p_emp,p_emp,
    mu_vs_lac,p_mu,p_emp,p_emp,
    gam_vs_lac,gam_vs_mu,p_gam,p_emp,
    laa_vs_lac,laa_vs_mu,laa_vs_gam,p_laa,
    align = "hv", nrow = 4, ncol = 4
  )
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()
}


# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
#
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#
#   numPlots = length(plots)
#
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#
#   if (numPlots==1) {
#     print(plots[[1]])
#
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }
#
#







