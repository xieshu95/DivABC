folder_path <- "G:/results/project 2/tip_info/round4/secsse/secsse_ABC_lam"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC.csv")

param_data2<-param_data[rep(seq_len(nrow(param_data)), each=200),]

lam1_abc <- c()
lam2_abc <- c()
mu1_abc <- c()
mu2_abc <- c()
q12_abc <- c()
q21_abc <- c()
n_iter <- c()
n_iteration <- c()
for(i in 1:6){
  # if(i%%5 == 0){
  #   rep <- 5
  # } else {
  #   rep <- i%%5
  # }
  # param_set = (param_num-1)*5 + i
  file_to_load <- grep(paste0("secsse_ABC_param_set_", i,"_ss_2.RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))
    num_iter <- output$n_iter
    n_iteration[i] <- num_iter
    if(output$n_iter <= 2){
      lam1_abc <- c(lam1_abc, rep(NA,200))
      lam2_abc <- c(lam2_abc, rep(NA,200))
      mu1_abc <- c(mu1_abc, rep(NA,200))
      mu2_abc <- c(mu2_abc, rep(NA,200))
      q12_abc <- c(q12_abc, rep(NA,200))
      q21_abc <- c(q21_abc, rep(NA,200))
    } else{
      lam1_abc <- c(lam1_abc, output$ABC[[num_iter]][,1])
      lam2_abc <- c(lam2_abc, output$ABC[[num_iter]][,2])
      mu1_abc <- c(mu1_abc, output$ABC[[num_iter]][,3])
      mu2_abc <- c(mu2_abc, output$ABC[[num_iter]][,4])
      q12_abc <- c(q12_abc, output$ABC[[num_iter]][,5])
      q21_abc <- c(q21_abc, output$ABC[[num_iter]][,6])
    }
  } else {
    lam1_abc <- c(lam1_abc, rep(NA,200))
    lam2_abc <- c(lam2_abc, rep(NA,200))
    mu1_abc <- c(mu1_abc, rep(NA,200))
    mu2_abc <- c(mu2_abc, rep(NA,200))
    q12_abc <- c(q12_abc, rep(NA,200))
    q21_abc <- c(q21_abc, rep(NA,200))
  }
}
whole_df_ABC <- data.frame(param_data2,
                           # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                           lam1_abc,lam2_abc,mu1_abc,mu2_abc,q12_abc,q21_abc)
save(whole_df_ABC,file = "G:/results/project 2/tip_info/round4/secsse/whole_df_ABC_lam.RData")


library(ggplot2)
load(paste0("G:/results/project 2/tip_info/round4/secsse/whole_df_ABC_lam.RData"))
for(i in 1:6){
  param_abc <- whole_df_ABC[((i*200-199)):(i*200),]

  if(!is.na(param_abc[1,7])){
    p_lam1 <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      # xlim(0,2)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(ggplot2::aes(x = lam1_abc),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(lambda[1]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = lam1), linetype = "dashed", size = 0.5)
    # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
    #                     linetype = "dashed", size = 0.5,color = "red")

    p_lam2 <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      # xlim(0,2)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(ggplot2::aes(x = lam2_abc),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(lambda[2]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = lam2), linetype = "dashed", size = 0.5)
    # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
    #                     linetype = "dashed", size = 0.5,color = "red")

    p_mu1 <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      # xlim(0,2)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = mu1_abc),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(mu[1]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = mu1), linetype = "dashed", size = 0.5)

    p_mu2 <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      # xlim(0,2)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = mu2_abc),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(mu[2]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = mu2), linetype = "dashed", size = 0.5)

    p_q12 <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      # xlim(0,0.07)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = gam_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.0005) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = q12_abc),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(q[12]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = q12), linetype = "dashed", size = 0.5)


    p_q21 <-ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      # xlim(0,2)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = q21_abc),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(q[21]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = q21), linetype = "dashed", size = 0.5)

    # lam1_vs_lam2 <- ggplot2::ggplot(data = param_abc) +
    #   ggplot2::theme_bw() +
    #   # xlim(0,2)+
    #   ggplot2::geom_point(mapping = ggplot2::aes(x = lam1_abc,y = lam2_abc),
    #                       colour = "royalblue",shape = 16,alpha = 0.2) +
    #   ggplot2::theme_classic() +
    #   ggplot2::theme(title = ggplot2::element_text(size = 12),
    #                  text = ggplot2::element_text(size = 12)) +
    #   ggplot2::ylab(expression(lambda[2])) +
    #   ggplot2::xlab(expression(lambda[1])) +
    #   ggplot2::geom_point(mapping = ggplot2::aes(x = lam1,y = lam2),
    #                       colour = "black",shape = 16,size = 2.5)
    # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
    # ggplot2::geom_hline(data= param_abc, aes(yintercept = mu), colour = "grey50")

    p_emp <- ggplot() + theme_void()

    tiff(paste0("G:/results/project 2/tip_info/round4/secsse/cowplot_lam/param_",i,".tiff"),
         units="px", width=3000, height=2000,res = 300,compression="lzw")
    param_estimates <- cowplot::plot_grid(
      p_lam1,p_lam2,p_mu1,p_mu2,p_q12,p_q21,
      align = "hv", nrow = 3, ncol = 2
    )
    print(param_estimates)
    while (!is.null(dev.list()))  dev.off()
  }
}
