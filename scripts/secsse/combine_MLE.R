# combine MLE results into one
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/secsse_ABC.csv")
param_data2<-param_data[rep(seq_len(nrow(param_data)), each=100),]
# MLE_all<-round(MLE_all,5)
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE/MLE_secsse_ABC1.RData")
MLE_all1<-MLE_all
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE/MLE_secsse_ABC2.RData")
MLE_all2<-MLE_all
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE/MLE_secsse_ABC3.RData")
MLE_all3<-MLE_all
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE/MLE_secsse_ABC4.RData")
MLE_all4<-MLE_all
load("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE/MLE_secsse_ABC5.RData")
MLE_all5<-MLE_all

MLE_all <-c()
for (i in 1:27){
  MLE_all_add <- rbind(MLE_all1[(i*20-19):(i*20),],
                       MLE_all2[(i*20-19):(i*20),],
                       MLE_all3[(i*20-19):(i*20),],
                       MLE_all4[(i*20-19):(i*20),],
                       MLE_all5[(i*20-19):(i*20),])
  MLE_all<- rbind(MLE_all,MLE_all_add)
}
MLE_all <- data.frame(param_data2,MLE_all)
rownames(MLE_all) <- 1:2700

save(MLE_all,file = "G:/results/project 2/tip_info/round4/adap_secsse_new_space/combined_MLE.RData")


## plot MLE
library(ggplot2)
load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/combined_MLE.RData"))
for(i in 1:27){ #27/135
  # param_MLE <- whole_df_ABC[((i*5000-4999)):(i*5000),]  # for single scenario
  param_MLE <- MLE_all[((i*100-99)):(i*100),] #for single set  20/100
  if(!is.na(param_MLE[1,7])){
    p_lam1 <-ggplot2::ggplot(data = param_MLE) +
      ggplot2::theme_bw() +
      xlim(0,0.5)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_MLE),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(ggplot2::aes(x = lam1_MLE),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(lambda[1]))+
      ggplot2::geom_vline(data= param_MLE, aes(xintercept = lam1), linetype = "dashed", size = 0.5)
    # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
    #                     linetype = "dashed", size = 0.5,color = "red")

    p_lam2 <-ggplot2::ggplot(data = param_MLE) +
      ggplot2::theme_bw() +
      xlim(0,0.5)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_MLE),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(ggplot2::aes(x = lam2_MLE),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(lambda[2]))+
      ggplot2::geom_vline(data= param_MLE, aes(xintercept = lam2), linetype = "dashed", size = 0.5)
    # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
    #                     linetype = "dashed", size = 0.5,color = "red")

    p_mu1 <-ggplot2::ggplot(data = param_MLE) +
      ggplot2::theme_bw() +
      xlim(0,0.2)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_MLE),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = mu1_MLE),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(mu[1]))+
      ggplot2::geom_vline(data= param_MLE, aes(xintercept = mu1), linetype = "dashed", size = 0.5)

    p_mu2 <-ggplot2::ggplot(data = param_MLE) +
      ggplot2::theme_bw() +
      xlim(0,0.2)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_MLE),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = mu2_MLE),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(mu[2]))+
      ggplot2::geom_vline(data= param_MLE, aes(xintercept = mu2), linetype = "dashed", size = 0.5)

    p_q12 <-ggplot2::ggplot(data = param_MLE) +
      ggplot2::theme_bw() +
      xlim(0,0.5)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = gam_MLE),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.0005) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = q12_MLE),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(q[12]))+
      ggplot2::geom_vline(data= param_MLE, aes(xintercept = q12), linetype = "dashed", size = 0.5)


    p_q21 <-ggplot2::ggplot(data = param_MLE) +
      ggplot2::theme_bw() +
      xlim(0,0.5)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_MLE),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = q21_MLE),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(q[21]))+
      ggplot2::geom_vline(data= param_MLE, aes(xintercept = q21), linetype = "dashed", size = 0.5)

    p_emp <- ggplot() + theme_void()

    tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE/group/set_",i,".tiff"),
         units="px", width=3000, height=2000,res = 300,compression="lzw")
    param_estimates <- cowplot::plot_grid(
      p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
      align = "hv", nrow = 2, ncol = 3
    )
    print(param_estimates)
    while (!is.null(dev.list()))  dev.off()
  }
}

### plot initial values
library(ggplot2)
load(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/combined_MLE.RData"))
for(i in 1:2){ #27/135
  # param_MLE <- whole_df_ABC[((i*5000-4999)):(i*5000),]  # for single scenario
  param_MLE <- MLE_all[((i*100-99)):(i*100),] #for single set  20/100
  if(!is.na(param_MLE[1,7])){
    p_lam1 <-ggplot2::ggplot(data = param_MLE) +
      ggplot2::theme_bw() +
      xlim(0,0.5)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_MLE),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(ggplot2::aes(x = init_lam1),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(lambda[1]))+
      ggplot2::geom_vline(data= param_MLE, aes(xintercept = lam1), linetype = "dashed", size = 0.5)
    # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
    #                     linetype = "dashed", size = 0.5,color = "red")

    p_lam2 <-ggplot2::ggplot(data = param_MLE) +
      ggplot2::theme_bw() +
      xlim(0,0.5)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_MLE),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(ggplot2::aes(x = init_lam2),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(lambda[2]))+
      ggplot2::geom_vline(data= param_MLE, aes(xintercept = lam2), linetype = "dashed", size = 0.5)
    # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
    #                     linetype = "dashed", size = 0.5,color = "red")

    p_mu1 <-ggplot2::ggplot(data = param_MLE) +
      ggplot2::theme_bw() +
      xlim(0,0.2)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_MLE),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = init_mu1),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(mu[1]))+
      ggplot2::geom_vline(data= param_MLE, aes(xintercept = mu1), linetype = "dashed", size = 0.5)

    p_mu2 <-ggplot2::ggplot(data = param_MLE) +
      ggplot2::theme_bw() +
      xlim(0,0.2)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_MLE),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = init_mu2),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(mu[2]))+
      ggplot2::geom_vline(data= param_MLE, aes(xintercept = mu2), linetype = "dashed", size = 0.5)

    p_q12 <-ggplot2::ggplot(data = param_MLE) +
      ggplot2::theme_bw() +
      xlim(0,0.5)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = gam_MLE),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.0005) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = init_q12),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(q[12]))+
      ggplot2::geom_vline(data= param_MLE, aes(xintercept = q12), linetype = "dashed", size = 0.5)


    p_q21 <-ggplot2::ggplot(data = param_MLE) +
      ggplot2::theme_bw() +
      xlim(0,0.5)+
      # ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_MLE),
      #                       fill = "#009E73",colour = "#009E73",
      #                       alpha = 0.3, binwidth = 0.01) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = init_q21),
                            fill = "royalblue",colour = "blue3",
                            alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(title = ggplot2::element_text(size = 12),
                     text = ggplot2::element_text(size = 12)) +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(q[21]))+
      ggplot2::geom_vline(data= param_MLE, aes(xintercept = q21), linetype = "dashed", size = 0.5)

    p_emp <- ggplot() + theme_void()

    tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_new_space/MLE/initial/set_",i,".tiff"),
         units="px", width=3000, height=2000,res = 300,compression="lzw")
    param_estimates <- cowplot::plot_grid(
      p_lam1,p_mu1,p_q12,p_lam2,p_mu2,p_q21,
      align = "hv", nrow = 2, ncol = 3
    )
    print(param_estimates)
    while (!is.null(dev.list()))  dev.off()
  }
}



