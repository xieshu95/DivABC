#####
# folder_path <- "G:/results/project 2/tip_info/round4/kernel3/DAISIE_ABC_short2"
folder_path <- "G:/results/project 2/tip_info/round4/adap_daisie_pw/DAISIE_ABC_short"
files <- list.files(folder_path)
param_data <- readr::read_csv2("G:/R/Traisie-ABC/data/DAISIE_ABC_short.csv")

param_data2<-param_data[rep(seq_len(nrow(param_data)), each=200),]

for(n in c(0)){
  lac_abc <- c()
  mu_abc <- c()
  gam_abc <- c()
  laa_abc <- c()
  n_iter <-c()
  n_iteration <- c()
  for(i in 1:81){
    # if(i%%5 == 0){
    #   rep <- 5
    # } else {
    #   rep <- i%%5
    # }
    # param_set = (param_num-1)*5 + i
    file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", i,"_ss_",n,".RData"),  #,"_rep",rep
                         files,
                         value = TRUE,
                         fixed = TRUE)

    # abc <- NULL; rm(abc) # nolint ; hack around global var
    if (!identical(file_to_load, character())) {
      load(file.path(folder_path, file_to_load))
      num_iter <- output$n_iter
      n_iteration[i] <- num_iter
      if(output$n_iter <= 3){
        lac_abc <- c(lac_abc, rep(NA,200))
        mu_abc <- c(mu_abc, rep(NA,200))
        gam_abc <- c(gam_abc, rep(NA,200))
        laa_abc <- c(laa_abc, rep(NA,200))
      } else if (nrow(output$ABC[[output$n_iter]]) == 200) {
        lac_abc <- c(lac_abc, output$ABC[[num_iter]][,1])
        mu_abc <- c(mu_abc, output$ABC[[num_iter]][,2])
        gam_abc <- c(gam_abc, output$ABC[[num_iter]][,3])
        laa_abc <- c(laa_abc, output$ABC[[num_iter]][,4])
      } else {
        lac_abc <- c(lac_abc, output$ABC[[num_iter-1]][,1])
        mu_abc <- c(mu_abc, output$ABC[[num_iter-1]][,2])
        gam_abc <- c(gam_abc, output$ABC[[num_iter-1]][,3])
        laa_abc <- c(laa_abc, output$ABC[[num_iter-1]][,4])
      }
    } else {
      lac_abc <- c(lac_abc, rep(NA,200))
      mu_abc <- c(mu_abc, rep(NA,200))
      gam_abc <- c(gam_abc, rep(NA,200))
      laa_abc <- c(laa_abc, rep(NA,200))
    }
  }
  whole_df_ABC <- data.frame(param_data2,
                             # lac_mcmc,mu_mcmc,gam_mcmc,laa_mcmc,n_iter
                             lac_abc,mu_abc,gam_abc,laa_abc)
  save(whole_df_ABC,
       file = paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw/whole_df_ABC_ss_set",n,".RData"))

}

load(paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw/whole_df_ABC_ss_set",0,".RData"))
whole_df_ABC$net_div <- (whole_df_ABC$lac-whole_df_ABC$mu)
whole_df_ABC$net_div_ABC <- (whole_df_ABC$lac_abc-whole_df_ABC$mu_abc)

whole_df_ABC$ext_frac <- (whole_df_ABC$mu)/(whole_df_ABC$lac)
whole_df_ABC$ext_frac_ABC <- (whole_df_ABC$mu_abc)/(whole_df_ABC$lac_abc)
save(whole_df_ABC,file = paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw/delta_whole_df_ABC_ss_set",0,".RData"))

load(paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw/delta_whole_df_ABC_ss_set",0,".RData"))

#####
library(ggplot2)
load(paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw/delta_whole_df_ABC_ss_set",0,".RData"))
load("G:/results/project 2/tip_info/round4/adap_daisie_pw/whole_df_MCMC.RData")
load("G:/results/project 2/tip_info/round4/adap_daisie_pw/whole_df_MLE.RData")

for(n in c(0)){
  load(paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw/delta_whole_df_ABC_ss_set",n,".RData"))
  for(i in 1:81){
    param_abc <- whole_df_ABC[((i*200-199)):(i*200),]

    if(!is.na(param_abc[1,6])){
      p_lac <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,1)+
        # ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
        #                       fill = "#009E73",colour = "#009E73",
        #                       alpha = 0.3, binwidth = 0.01) +
        ggplot2::geom_density(ggplot2::aes(x = lac_abc),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.3) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(lambda^c))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)
      # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
      #                     linetype = "dashed", size = 0.5,color = "red")

      p_mu <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.5)+
        # ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_abc),
        #                       fill = "#009E73",colour = "#009E73",
        #                       alpha = 0.3, binwidth = 0.01) +
        ggplot2::geom_density(mapping = ggplot2::aes(x = mu_abc),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.3) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(mu))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", size = 0.5)

      p_gam <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.05)+
        # ggplot2::geom_histogram(mapping = ggplot2::aes(x = gam_abc),
        #                       fill = "#009E73",colour = "#009E73",
        #                       alpha = 0.3, binwidth = 0.0005) +
        ggplot2::geom_density(mapping = ggplot2::aes(x = gam_abc),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.3) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(gamma))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed", size = 0.5)


      p_laa <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,1.0)+
        # ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_abc),
        #                       fill = "#009E73",colour = "#009E73",
        #                       alpha = 0.3, binwidth = 0.01) +
        ggplot2::geom_density(mapping = ggplot2::aes(x = laa_abc),
                              fill = "royalblue",colour = "blue3",
                              alpha = 0.3) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(lambda^a))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.5)

      mu_vs_lac <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,1)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = mu_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(mu)) +
        ggplot2::xlab(expression(lambda^c)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = mu),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = mu), colour = "grey50")

      gam_vs_lac <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,1)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = gam_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(gamma)) +
        ggplot2::xlab(expression(lambda^c)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = gam),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = gam), colour = "grey50")

      laa_vs_lac <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,1)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = laa_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(lambda^a)) +
        ggplot2::xlab(expression(lambda^c)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = laa),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")

      gam_vs_mu <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.5)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = mu_abc,y = gam_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(gamma)) +
        ggplot2::xlab(expression(mu)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = mu,y = gam),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = gam), colour = "grey50")

      laa_vs_mu <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.5)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = mu_abc,y = laa_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(lambda^a)) +
        ggplot2::xlab(expression(mu)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = mu,y = laa),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")

      laa_vs_gam <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.05)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = gam_abc,y = laa_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(lambda^a)) +
        ggplot2::xlab(expression(gamma)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = gam,y = laa),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")

      p_emp <- ggplot() + theme_void()

      tiff(paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw/cowplots/ss",n,"_param_",i,".tiff"),
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

  }
}

#####
#cowplot only ABC (histgram)
library(ggplot2)
# load("G:/results/project 2/tip_info/round4/MLE.RData")
for(n in c(0)){
  load(paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw/delta_whole_df_ABC_ss_set",n,".RData"))
  for(i in 1:81){
    param_abc <- whole_df_ABC[((i*200-199)):(i*200),]

    if(!is.na(param_abc[1,6])){
      p_lac <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,1)+
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = lac_abc),
                                fill = "royalblue",
                                alpha = 0.9, binwidth = 0.04) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Frequency") +
        ggplot2::xlab(expression(lambda^c))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)
      # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
      #                     linetype = "dashed", size = 0.5,color = "red")

      p_mu <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.5)+
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = mu_abc),
                                fill = "royalblue",
                                alpha = 0.9, binwidth = 0.02) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Frequency") +
        ggplot2::xlab(expression(mu))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", size = 0.5)

      p_gam <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.03)+
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = gam_abc),
                                fill = "royalblue",
                                alpha = 0.9, binwidth = 0.0012) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Frequency") +
        ggplot2::xlab(expression(gamma))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed", size = 0.5)


      p_laa <-ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.75)+
        ggplot2::geom_histogram(mapping = ggplot2::aes(x = laa_abc),
                                fill = "royalblue",
                                alpha = 0.9, binwidth = 0.03) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab("Frequency") +
        ggplot2::xlab(expression(lambda^a))+
        ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.5)

      mu_vs_lac <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,1)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = mu_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(mu)) +
        ggplot2::xlab(expression(lambda^c)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = mu),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = mu), colour = "grey50")

      gam_vs_lac <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,1)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = gam_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(gamma)) +
        ggplot2::xlab(expression(lambda^c)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = gam),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = gam), colour = "grey50")

      laa_vs_lac <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,1)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac_abc,y = laa_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(lambda^a)) +
        ggplot2::xlab(expression(lambda^c)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = lac,y = laa),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")

      gam_vs_mu <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.5)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = mu_abc,y = gam_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(gamma)) +
        ggplot2::xlab(expression(mu)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = mu,y = gam),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = gam), colour = "grey50")

      laa_vs_mu <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.5)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = mu_abc,y = laa_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(lambda^a)) +
        ggplot2::xlab(expression(mu)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = mu,y = laa),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")

      laa_vs_gam <- ggplot2::ggplot(data = param_abc) +
        ggplot2::theme_bw() +
        xlim(0,0.03)+
        ggplot2::geom_point(mapping = ggplot2::aes(x = gam_abc,y = laa_abc),
                            colour = "royalblue",shape = 16,alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::theme(title = ggplot2::element_text(size = 12),
                       text = ggplot2::element_text(size = 12)) +
        ggplot2::ylab(expression(lambda^a)) +
        ggplot2::xlab(expression(gamma)) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = gam,y = laa),
                            colour = "black",shape = 16,size = 2.5)
      # ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), colour = "grey50") +
      # ggplot2::geom_hline(data= param_abc, aes(yintercept = laa), colour = "grey50")

      p_emp <- ggplot() + theme_void()

      tiff(paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw/cowplots_hist/ss",n,"_param_",i,".tiff"),
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
  }
}

#####
# AMM + net_diversification
library(ggplot2)
load(paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw/delta_whole_df_ABC_ss_set",0,".RData"))
load("G:/results/project 2/tip_info/round4/adap_daisie/whole_df_MCMC.RData")
load("G:/results/project 2/tip_info/round4/adap_daisie/whole_df_MLE.RData")
## get legend first
param_abc <- whole_df_ABC[1:10,]
param_mcmc <- whole_df_MCMC[1:10,]
param_mle <- whole_df_MLE[1:10,]

p_legend <-ggplot2::ggplot(data = param_abc) +
  ggplot2::theme_bw() +
  xlim(0,1)+
  ggplot2::geom_density(data = param_mcmc,
                        ggplot2::aes(x = lac_mcmc,fill = "MCMC"),colour = "red4",
                        alpha = 0.7) +
  ggplot2::geom_density(ggplot2::aes(x = lac_abc),
                        fill = "royalblue",colour = "blue3",
                        alpha = 0.7) +
  ggplot2::geom_density(data = param_mle,
                        ggplot2::aes(x = lac_MLE,fill = "MLE"),colour = "green4",
                        alpha = 0.5) +
  ggplot2::theme_classic() +
  ggplot2::ylab("Density") +
  ggplot2::scale_fill_manual(name = "Method",
                             values = c( "MCMC" = "#F7903D", "ABC" = "#4D85BD", "MLE" = "#59A95A"),
                             labels = c("MCMC", "ABC", "MLE"))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 15)) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 15)) +
  ggplot2::xlab(expression(lambda^c))+
  ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)


legend_all <- cowplot::get_legend(
  p_legend + theme(legend.box.margin = margin(0, 0, 0, 6))
)
color_values <-c("MCMC" = "#F7903D", "ABC" = "#4D85BD", "MLE" = "#59A95A")


for(i in 1:81){
  param_abc <- whole_df_ABC[((i*200-199)):(i*200),]
  param_mcmc <- whole_df_MCMC[((i*3001-199)):(i*3001),]
  param_mle <- whole_df_MLE[i,]

  # if(!is.na(param_abc[,7])){
  p_lac <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,1.2)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = lac_mcmc,fill = "MCMC"),
                            alpha = 0.7,binwidth = 0.04) +
    ggplot2::geom_histogram(ggplot2::aes(x = lac_abc,
                                         fill = "ABC"),
                            alpha = 0.7,binwidth = 0.045) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = lac_MLE),color = "#59A95A",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab(expression(lambda^c))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = lac), linetype = "dashed", size = 0.5)
  # ggplot2::geom_vline(data= MLE_all[i,], aes(xintercept = lac_MLE),
  #                     linetype = "dashed", size = 0.5,color = "red")



  p_mu <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,0.5)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = mu_mcmc,fill = "MCMC"),
                            alpha = 0.7,binwidth = 0.02) +
    ggplot2::geom_histogram(ggplot2::aes(x = mu_abc,
                                         fill = "ABC"),
                            alpha = 0.7,binwidth = 0.02) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = mu_MLE),color = "#59A95A",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab(expression(mu))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = mu), linetype = "dashed", size = 0.5)

  p_gam <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,0.05)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = gam_mcmc,fill = "MCMC"),
                            alpha = 0.7,binwidth = 0.002) +
    ggplot2::geom_histogram(ggplot2::aes(x = gam_abc,
                                         fill = "ABC"),
                            alpha = 0.7,binwidth = 0.002) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = gam_MLE),color = "#59A95A",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab(expression(gamma))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = gam), linetype = "dashed", size = 0.5)

  p_laa <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,1.0)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = laa_mcmc,fill = "MCMC"),
                            alpha = 0.7,binwidth = 0.04) +
    ggplot2::geom_histogram(ggplot2::aes(x = laa_abc,
                                         fill = "ABC"),
                            alpha = 0.7,binwidth = 0.04) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = laa_MLE),color = "#59A95A",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab(expression(lambda^a))+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = laa), linetype = "dashed", size = 0.5)

  p_net_div <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    xlim(0,1.0)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = net_div_mcmc,fill = "MCMC"),
                            alpha = 0.7,binwidth = 0.04) +
    ggplot2::geom_histogram(ggplot2::aes(x = net_div_ABC,fill = "ABC"),
                            alpha = 0.7,binwidth = 0.04) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = net_div_MLE),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 12),
                   text = ggplot2::element_text(size = 12)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab("Net diversification")+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = net_div), linetype = "dashed", size = 0.5)

  p_ext_frac <-ggplot2::ggplot(data = param_abc) +
    ggplot2::theme_bw() +
    # xlim(0,1.0)+
    ggplot2::geom_histogram(data = param_mcmc,
                            ggplot2::aes(x = ext_frac_MCMC,fill = "MCMC"),
                            alpha = 0.7) +
    ggplot2::geom_histogram(ggplot2::aes(x = ext_frac_ABC,fill = "ABC"),
                            alpha = 0.7) +
    ggplot2::geom_vline(data= param_mle,
                        aes(xintercept = ext_frac_MLE),colour = "green4",
                        linetype = "solid", size = 1)+
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 13),
                   text = ggplot2::element_text(size = 13)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab("Extinction fraction")+
    ggplot2::scale_fill_manual(name = "Method",
                               values = color_values,
                               labels = c("MCMC", "ABC", "MLE"))+
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(data= param_abc, aes(xintercept = ext_frac), linetype = "dashed", size = 0.5)



  p_emp <- ggplot() + theme_void()

  tiff(paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw/cowplot_AMM/AMM_hist_set_",i,".tiff"),
       units="px", width=3000, height=2000,res = 300,compression="lzw")
  param_estimates <- cowplot::plot_grid(
    p_lac,p_mu,p_net_div,p_gam,p_laa,p_ext_frac,
    align = "hv", nrow = 2, ncol = 3
  )
  param_est_final <- cowplot::plot_grid(param_estimates,legend_all,rel_widths = c(3, .4))
  print(param_est_final)
  while (!is.null(dev.list()))  dev.off()
  # }
}

#####
## plot the d(ss) for each generation
library(ggplot2)
folder_path <- "G:/results/project 2/tip_info/round4/adap_daisie_pw/DAISIE_ABC_short"
files <- list.files(folder_path)
for(set in 11:81){
  message("set", set)
  file_to_load <- grep(paste0("DAISIE_ABC_short_param_set_", set,"_ss_0.RData"),  #,"_rep",rep
                       files,
                       value = TRUE,
                       fixed = TRUE)

  # abc <- NULL; rm(abc) # nolint ; hack around global var
  if (!identical(file_to_load, character())) {
    load(file.path(folder_path, file_to_load))

    ss_dist<-c()
    n_gene <- length(output$ss_diff_list)
    if(nrow(output$ss_diff_list[[n_gene]]) < 200){
      n_gene <- n_gene - 1
    }
    for(i in 1:n_gene){
      ss_dist <- rbind(ss_dist,output$ss_diff_list[[i]])
    }


    colnames(ss_dist) <- c("Anagenesis","Cladogenesis","Nonendemic",
                           "pwNLTT","pwCS","pwCT")
    rownames(ss_dist) <- 1:nrow(ss_dist)
    ss_dist <- as.data.frame(ss_dist)
    ss_dist$generation <- as.factor(rep(1:n_gene, each = 200))

    g1 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Anagenesis)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g1)

    g2 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Cladogenesis)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g2)

    g3 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = Nonendemic)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g3)

    g4 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = pwNLTT)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g4)

    g5 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = pwCS)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g5)

    g6 <- ggplot2::ggplot(ss_dist, aes(x = generation,y = pwCT)) +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot()
    # print(g6)
    tiff(paste0("G:/results/project 2/tip_info/round4/adap_daisie_pw/dss/ss_0_param_",set,".tiff"),
         units="px", width=4000, height=2000,res = 300,compression="lzw")
    dss <- cowplot::plot_grid(
      g1,g2,g3,g4,g5,g6,
      align = "hv", nrow = 2, ncol = 3
    )
    print(dss)
    while (!is.null(dev.list()))  dev.off()
  }
}

