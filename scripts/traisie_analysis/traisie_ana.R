## formate traisie ABC results
for(test in c(1,3,5)){
  folder_path <- paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie1/traisie_ABC_test",test)
  files <- list.files(folder_path)
  param_data <- readr::read_csv2("data/traisie_ABC_test1.csv")
  param_data2<-param_data[rep(seq_len(nrow(param_data)), each=200),]

  lac_abc1 <- c()
  mu_abc1 <- c()
  gam_abc1 <- c()
  laa_abc1 <- c()
  lac_abc2 <- c()
  mu_abc2 <- c()
  gam_abc2 <- c()
  laa_abc2 <- c()
  trans_abc1 <- c()
  trans_abc2 <- c()
  n_iter <-c()
  n_iteration <- c()
  for(i in 1:100){
    file_to_load <- grep(paste0("traisie_ABC_test", test,"_param_set_",i,"_ss_0.RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      n_iteration[i] <- num_iter
      if(output$n_iter <= 2){
        lac_abc1 <- c(lac_abc1, rep(NA,200))
        mu_abc1 <- c(mu_abc1, rep(NA,200))
        gam_abc1 <- c(gam_abc1, rep(NA,200))
        laa_abc1 <- c(laa_abc1, rep(NA,200))
        lac_abc2 <- c(lac_abc2, rep(NA,200))
        mu_abc2 <- c(mu_abc2, rep(NA,200))
        gam_abc2 <- c(gam_abc2, rep(NA,200))
        laa_abc2 <- c(laa_abc2, rep(NA,200))
        trans_abc1 <- c(trans_abc1, rep(NA,200))
        trans_abc2 <- c(trans_abc2, rep(NA,200))
      } else{
        lac_abc1 <- c(lac_abc1, output$ABC[[num_iter-1]][,1])
        mu_abc1 <- c(mu_abc1, output$ABC[[num_iter-1]][,2])
        gam_abc1 <- c(gam_abc1, output$ABC[[num_iter-1]][,3])
        laa_abc1 <- c(laa_abc1, output$ABC[[num_iter-1]][,4])
        lac_abc2 <- c(lac_abc2, output$ABC[[num_iter-1]][,5])
        mu_abc2 <- c(mu_abc2, output$ABC[[num_iter-1]][,6])
        gam_abc2 <- c(gam_abc2, output$ABC[[num_iter-1]][,7])
        laa_abc2 <- c(laa_abc2, output$ABC[[num_iter-1]][,8])
        trans_abc1 <- c(trans_abc1, output$ABC[[num_iter-1]][,9])
        trans_abc2 <- c(trans_abc2, output$ABC[[num_iter-1]][,10])
      }
    } else {
      lac_abc1 <- c(lac_abc1, rep(NA,200))
      mu_abc1 <- c(mu_abc1, rep(NA,200))
      gam_abc1 <- c(gam_abc1, rep(NA,200))
      laa_abc1 <- c(laa_abc1, rep(NA,200))
      lac_abc2 <- c(lac_abc2, rep(NA,200))
      mu_abc2 <- c(mu_abc2, rep(NA,200))
      gam_abc2 <- c(gam_abc2, rep(NA,200))
      laa_abc2 <- c(laa_abc2, rep(NA,200))
      trans_abc1 <- c(trans_abc1, rep(NA,200))
      trans_abc2 <- c(trans_abc2, rep(NA,200))
    }
  }


  whole_df_ABC <- data.frame(param_data2,
                             # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                             lac_abc1,mu_abc1,gam_abc1,laa_abc1,
                             lac_abc2,mu_abc2,gam_abc2,laa_abc2,
                             trans_abc1,trans_abc2)


  #lac_abc,mu_abc,gam_abc,laa_abc,n_iter)
  save(whole_df_ABC,file = paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie1/whole_df_ABC_test",test,".RData"))
}

## plot ABC
# 1. plot estimations for each parameter set (single replicate)
library(ggplot2)
for(test in c(1,3,5)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie1/whole_df_ABC_test",test,".RData"))
  ## lac scenario, estimation: lac1 and lac2 (others are fixed)
  for(i in 1:100){
    param_abc <- whole_df_ABC[((i*200-199)):(i*200),]
    p_lac1 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,2)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_histogram(ggplot2::aes(x = lac_abc1),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.5, bins = 80) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("Cladogenesis state 1") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(lambda[1]^c))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)

    p_lac2 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,2)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_histogram(ggplot2::aes(x = lac_abc2),
                              fill = "red",colour = "red4",
                              alpha = 0.5, bins = 80) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("Cladogenesis state 2") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(lambda[2]^c))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = lac2), linetype = "dashed", size = 0.5)

    p_mu1 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,1)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_histogram(ggplot2::aes(x = mu_abc1),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.5, bins = 80) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("Extinction state 1") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(mu[1]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", size = 0.5)

    p_mu2 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,1)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_histogram(ggplot2::aes(x = mu_abc2),
                              fill = "red",colour = "red4",
                              alpha = 0.5, bins = 80) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("Extinction state 2") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(mu[2]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = mu2), linetype = "dashed", size = 0.5)

    p_gam1 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,0.06)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = gam_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_histogram(ggplot2::aes(x = gam_abc1),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.5, bins = 80) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("Colonization state 1") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(gamma[1]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed", size = 0.5)

    p_gam2 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,0.06)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = gam_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_histogram(ggplot2::aes(x = gam_abc2),
                              fill = "red",colour = "red4",
                              alpha = 0.5, bins = 80) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("Colonization state 2") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(gamma[2]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = gam2), linetype = "dashed", size = 0.5)

    p_laa1 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,0.5)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_histogram(ggplot2::aes(x = laa_abc1),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.5, bins = 80) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("Anagenesis state 1") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(lambda[1]^a))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.5)

    p_laa2 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,0.5)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_histogram(ggplot2::aes(x = laa_abc2),
                              fill = "red",colour = "red4",
                              alpha = 0.5, bins = 80) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("Anagenesis state 2") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(lambda[2]^a))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = laa2), linetype = "dashed", size = 0.5)

    # trasition rates
    p_q12 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,0.5)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_histogram(ggplot2::aes(x = trans_abc1),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.5, bins = 80) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("Transition 1 to 2") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(q[12]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.5)

    p_q21 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,0.5)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_histogram(ggplot2::aes(x = trans_abc2),
                              fill = "red",colour = "red4",
                              alpha = 0.5, bins = 80) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("Transition 2 to 1") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(q[21]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = laa2), linetype = "dashed", size = 0.5)


    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie1/plot_each_set/test",test,"_param_",i,".tiff"),
         units="px", width=4000, height=2000,res = 300,compression="lzw")
    param_estimates <- cowplot::plot_grid(
      p_lac1,p_lac2,p_mu1,p_mu2,
      p_gam1,p_gam2,p_laa1,p_laa2,
      align = "hv", nrow = 2, ncol = 4
    )
    print(param_estimates)
    while (!is.null(dev.list()))  dev.off()
  }
}



## plot net diversification rate and transitions
library(ggplot2)
for(test in c(1,3,5)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie1/whole_df_ABC_test",test,".RData"))
  for(i in 1:100){
    param_abc <- whole_df_ABC[((i*200-199)):(i*200),]
    p_net_div1 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,2)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_histogram(ggplot2::aes(x = abs(lac_abc1 - mu_abc1)),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.5, bins = 80) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("Net diversification state1") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(Net[1]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)

    p_net_div2 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,2)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_histogram(ggplot2::aes(x = abs(lac_abc2 - mu_abc2)),
                              fill = "red",colour = "red4",
                              alpha = 0.5, bins = 80) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("Net diversification state2") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(Net[2]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = lac2), linetype = "dashed", size = 0.5)

    # trasition rates
    p_q12 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,0.5)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_histogram(ggplot2::aes(x = trans_abc1),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.5, bins = 80) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("Transition 1 to 2") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(q[12]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.5)

    p_q21 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,0.5)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_abc),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_histogram(ggplot2::aes(x = trans_abc2),
                              fill = "red",colour = "red4",
                              alpha = 0.5, bins = 80) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("Transition 2 to 1") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(expression(q[21]))+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = laa2), linetype = "dashed", size = 0.5)


    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie1/net_div_q/test",test,"_param_",i,".tiff"),
         units="px", width=3000, height=2000,res = 300,compression="lzw")
    param_estimates <- cowplot::plot_grid(
      p_net_div1,p_net_div2,p_q12,p_q21,
      align = "hv", nrow = 2, ncol = 2
    )
    print(param_estimates)
    while (!is.null(dev.list()))  dev.off()
  }
}

## plot rate1 vs rate2
library(ggplot2)
for(test in c(1,3,5)){
  load(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie1/whole_df_ABC_test",test,".RData"))
  for(i in 1:100){
    param_abc <- whole_df_ABC[((i*200-199)):(i*200),]

    lac1_vs_lac2 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,2)+
      ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc1,y = lac_abc2),
                          colour = "grey30",shape = 16,alpha = 0.6) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("State 1 vs State 2") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab(expression(lambda[2]^c)) +
      ggplot2::xlab(expression(lambda[1]^c)) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = lac2),
                          colour = "black",shape = 16,size = 2.5)+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed",colour = "black")+
      ggplot2::geom_hline(data= param_abc, aes(yintercept = lac2),linetype = "dashed", colour = "black")

    mu1_vs_mu2 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,2)+
      ggplot2::geom_point(mapping = ggplot2::aes(x = mu_abc1,y = mu_abc2),
                          colour = "grey30",shape = 16,alpha = 0.6) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("State 1 vs State 2") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab(expression(mu[2])) +
      ggplot2::xlab(expression(mu[1])) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = mu,y = mu2),
                          colour = "black",shape = 16,size = 2.5)+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed",colour = "black")+
      ggplot2::geom_hline(data= param_abc, aes(yintercept = mu2),linetype = "dashed", colour = "black")

    gam1_vs_gam2 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,0.06)+
      ggplot2::geom_point(mapping = ggplot2::aes(x = gam_abc1,y = gam_abc2),
                          colour = "grey30",shape = 16,alpha = 0.6) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("State 1 vs State 2") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab(expression(gamma[2])) +
      ggplot2::xlab(expression(gamma[1])) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = gam,y = gam2),
                          colour = "black",shape = 16,size = 2.5)+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed",colour = "black")+
      ggplot2::geom_hline(data= param_abc, aes(yintercept = gam2),linetype = "dashed", colour = "black")

    laa1_vs_laa2 <- ggplot2::ggplot(data = param_abc) +
      ggplot2::theme_bw() +
      xlim(0,2)+
      ggplot2::geom_point(mapping = ggplot2::aes(x = laa_abc1,y = laa_abc2),
                          colour = "grey30",shape = 16,alpha = 0.6) +
      ggplot2::theme_classic() +
      ggplot2::ggtitle("State 1 vs State 2") +
      ggplot2::theme(title = ggplot2::element_text(size = 10),
                     text = ggplot2::element_text(size = 7),
                     plot.title = element_text(hjust = 0.5)) +
      ggplot2::ylab(expression(lambda[2]^a)) +
      ggplot2::xlab(expression(lambda[1]^a)) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = laa,y = laa2),
                          colour = "black",shape = 16,size = 2.5)+
      ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed",colour = "black")+
      ggplot2::geom_hline(data= param_abc, aes(yintercept = laa2),linetype = "dashed", colour = "black")


    tiff(paste0("D:/Onedrive-shu/OneDrive/project 2/results/round5/traisie1/plot_each_set/test",test,"_r1r2_param_",i,".tiff"),
         units="px", width=2000, height=2000,res = 300,compression="lzw")
    param_estimates <- cowplot::plot_grid(
      lac1_vs_lac2,mu1_vs_mu2,gam1_vs_gam2,laa1_vs_laa2,
      align = "hv", nrow = 2, ncol = 2
    )
    print(param_estimates)
    while (!is.null(dev.list()))  dev.off()
  }
}






