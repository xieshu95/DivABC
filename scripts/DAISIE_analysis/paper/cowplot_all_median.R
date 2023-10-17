## cowplot
library(ggplot2)
load(paste0("daisie_high_prior/obs_ss_long_with_pars_DI.RData"))

load(paste0("daisie_high_prior/delta_whole_df_ABC_ss_set",0,".RData"))
whole_df_ABC$Method = "ABC All"
whole_df_ABC_s0 = whole_df_ABC
whole_df_ABC_s0$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s0$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s0$set <- rep(1:16, each = 5000)
whole_df_ABC_s0$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s0$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)



load(paste0("daisie_high_prior/delta_whole_df_ABC_ss_set",1,".RData"))
whole_df_ABC$Method = "ABC Phylogenetic"
whole_df_ABC_s1 = whole_df_ABC
whole_df_ABC_s1$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s1$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s1$set <- rep(1:16, each = 5000)
whole_df_ABC_s1$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s1$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)

load(paste0("daisie_high_prior/delta_whole_df_ABC_ss_set",2,".RData"))
whole_df_ABC$Method = "ABC Diversity"
whole_df_ABC_s2 = whole_df_ABC
whole_df_ABC_s2$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s2$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s2$set <- rep(1:16, each = 5000)
whole_df_ABC_s2$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s2$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)

load(paste0("daisie_high_prior/delta_whole_df_ABC_ss_set",3,".RData"))
whole_df_ABC$Method = "ABC NLTT"
whole_df_ABC_s3 = whole_df_ABC
whole_df_ABC_s3$total <- rep(rep(pars_ss$total, each = 500), 1)
whole_df_ABC_s3$rep <- rep(rep(1:10, each = 500), 16)
whole_df_ABC_s3$set <- rep(1:16, each = 5000)
whole_df_ABC_s3$num_clade <- rep(rep(pars_ss$num.clade, each = 500), 1)
whole_df_ABC_s3$largest_clade <- rep(rep(pars_ss$largest.clade, each = 500), 1)


whole_df_ABC <- rbind(whole_df_ABC_s0,
                      whole_df_ABC_s1,
                      whole_df_ABC_s2,
                      whole_df_ABC_s3) #whole_df_ABC_20


whole_df_ABC$dlac <- whole_df_ABC$lac_abc - whole_df_ABC$lac
whole_df_ABC$dmu <- whole_df_ABC$mu_abc - whole_df_ABC$mu
whole_df_ABC$dgam <- whole_df_ABC$gam_abc - whole_df_ABC$gam
whole_df_ABC$dlaa <- whole_df_ABC$laa_abc - whole_df_ABC$laa
whole_df_ABC$dnet_div <- whole_df_ABC$net_div_ABC - whole_df_ABC$net_div
whole_df_ABC$dext_frac <- whole_df_ABC$ext_frac_ABC - whole_df_ABC$ext_frac
# whole_df_ABC$total <- rep(rep(pars_ss$total, each = 400), 1) # 400,5
df <- whole_df_ABC
n <- 500
ABC_median <-aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]
ABC_median$Method <- rep(c("ABC All","ABC Phylogenetic","ABC Diversity","ABC NLTT"),each = 160) #,

save(whole_df_ABC,file =
       paste0("daisie_high_prior/comb_ss_ABC.RData"))
save(ABC_median,file =
       paste0("daisie_high_prior/comb_ss_ABC_median.RData"))


load(paste0("daisie_high_prior/delta_whole_df_MCMC.RData"))
whole_df_MCMC$Method = "MCMC"
whole_df_MCMC$total <- rep(rep(pars_ss$total, each = 2501), 1)
whole_df_MCMC$rep <- rep(rep(1:10, each = 2501), 16)
whole_df_MCMC$set <- rep(1:16, each = 25010)
whole_df_MCMC$num_clade <- rep(rep(pars_ss$num.clade, each = 2501), 1)
whole_df_MCMC$largest_clade <- rep(rep(pars_ss$largest.clade, each = 2501), 1)
whole_df_MCMC$dlac <- whole_df_MCMC$lac_mcmc - whole_df_MCMC$lac
whole_df_MCMC$dmu <- whole_df_MCMC$mu_mcmc - whole_df_MCMC$mu
whole_df_MCMC$dgam <- whole_df_MCMC$gam_mcmc - whole_df_MCMC$gam
whole_df_MCMC$dlaa <- whole_df_MCMC$laa_mcmc - whole_df_MCMC$laa
whole_df_MCMC$dnet_div <- whole_df_MCMC$net_div_mcmc - whole_df_MCMC$net_div
whole_df_MCMC$dext_frac <- whole_df_MCMC$ext_frac_MCMC - whole_df_MCMC$ext_frac

df<-whole_df_MCMC
n <- 2501
MCMC_median <- aggregate(df, list(rep(1:(nrow(df) %/% n + 1), each = n, len = nrow(df))), median)[-1]

save(whole_df_MCMC,file =
       paste0("daisie_high_prior/comb_ss_MCMC.RData"))
save(MCMC_median,file =
       paste0("daisie_high_prior/comb_ss_MCMC_median.RData"))

load("D:/Onedrive-shu/OneDrive/project 2/results/round5/daisie/daisie_new_space6/whole_df_MLE_DI.RData")
whole_df_MLE$Method = "MLE"
whole_df_MLE$total <- rep(rep(pars_ss$total, each = 1), 1)
whole_df_MLE$rep <- rep(rep(1:10, each = 1), 16)
whole_df_MLE$set <- rep(1:16, each = 10)
whole_df_MLE$num_clade <- rep(rep(pars_ss$num.clade, each = 1), 1)
whole_df_MLE$largest_clade <- rep(rep(pars_ss$largest.clade, each = 1), 1)
whole_df_MLE$dlac <- whole_df_MLE$lac_MLE - whole_df_MLE$lac
whole_df_MLE$dmu <- whole_df_MLE$mu_MLE - whole_df_MLE$mu
whole_df_MLE$dgam <- whole_df_MLE$gam_MLE - whole_df_MLE$gam
whole_df_MLE$dlaa <- whole_df_MLE$laa_MLE - whole_df_MLE$laa
whole_df_MLE$dnet_div <- whole_df_MLE$net_div_MLE - whole_df_MLE$net_div
whole_df_MLE$dext_frac <- whole_df_MLE$ext_frac_MLE - whole_df_MLE$ext_frac

save(whole_df_MLE,file =
       paste0("daisie_high_prior/comb_ss_MLE.RData"))

whole_df_all <- rbind(ABC_median[,c(1:5,10,12,14:25)],
                      MCMC_median[,c(1:5,10,12,14:25)],
                      whole_df_MLE[,c(1:5,10,12,14:25)])


lac_names <- c(
  `0.4` = 'lambda^c~"="~0.4',
  `0.7` = 'lambda^c~"="~0.7'
)

mu_names <- c(
  `0` = 'mu~"="~0',
  `0.3` = 'mu~"="~0.3'
)

gam_names <- c(
  `0.003` = 'gamma~"="~0.003',
  `0.009` = 'gamma~"="~0.009'
)

laa_names <- c(
  `0.1` = 'lambda^a~"="~0.1',
  `1` = 'lambda^a~"="~1.0'
)


library(ggplot2)
load(paste0("daisie_high_prior/comb_ss_ABC.RData"))
load(paste0("daisie_high_prior/comb_ss_MCMC.RData"))
load(paste0("daisie_high_prior/comb_ss_MLE.RData"))

load(paste0("daisie_high_prior/comb_ss_ABC_median.RData"))
load(paste0("daisie_high_prior/comb_ss_MCMC_median.RData"))


# for(i in 1:16){
  # param_abc <- whole_df_ABC[which(whole_df_ABC$set == i & whole_df_ABC$rep == 1 & whole_df_ABC$Method == "ABC NLTT"),]
  # param_abc2 <- whole_df_ABC[which(whole_df_ABC$set == i & whole_df_ABC$rep == 1 & whole_df_ABC$Method == "ABC Diversity"),]
  # param_mcmc <- whole_df_MCMC[which(whole_df_MCMC$set == i & whole_df_MCMC$rep == 1),]
  # param_mle <- whole_df_MLE[which(whole_df_MLE$set == i & whole_df_MLE$rep == 1),]

  # param_abc <- whole_df_ABC[which(whole_df_ABC$set == i  & whole_df_ABC$Method == "ABC NLTT"),]
  # param_abc2 <- whole_df_ABC[which(whole_df_ABC$set == i & whole_df_ABC$Method == "ABC Diversity"),]
  # param_mcmc <- whole_df_MCMC[which(whole_df_MCMC$set == i),]
  # param_mle <- whole_df_MLE[which(whole_df_MLE$set == i),]

  param_abc <- whole_df_ABC[which(whole_df_ABC$Method == "ABC NLTT"),]
  param_abc2 <- whole_df_ABC[which(whole_df_ABC$Method == "ABC Diversity"),]
  param_mcmc <- whole_df_MCMC
  param_mle <- whole_df_MLE


  whole_df<- rbind(param_abc[,c(1:5,10,12,14:25)],
                   param_abc2[,c(1:5,10,12,14:25)],
                   param_mcmc[,c(1:5,10,12,14:25)])
                   # param_mle[,c(1:5,10,12,14:25)])

  whole_df2<- rbind(param_abc[,c(1:5,10,12,14:25)],
                   param_abc2[,c(1:5,10,12,14:25)])

  # param_abc_median <- ABC_median[which(ABC_median$set == i  & ABC_median$Method == "ABC NLTT"),]
  # param_abc2_median <- ABC_median[which(ABC_median$set == i & ABC_median$Method == "ABC Diversity"),]
  # param_mcmc_median <- MCMC_median[which(MCMC_median$set == i),]

  param_abc_median <- ABC_median[which(ABC_median$Method == "ABC NLTT"),]
  param_abc2_median <- ABC_median[which(ABC_median$Method == "ABC Diversity"),]
  param_mcmc_median <- MCMC_median

  whole_df_median1<- rbind(param_abc_median[,c(1:5,10,12,14:25)],
                   param_abc2_median[,c(1:5,10,12,14:25)],
                   param_mcmc_median[,c(1:5,10,12,14:25)])

  whole_df_median<- rbind(param_abc_median[,c(1:5,10,12,14:25)],
                          param_abc2_median[,c(1:5,10,12,14:25)],
                          param_mcmc_median[,c(1:5,10,12,14:25)],
                          param_mle[,c(1:5,10,12,14:25)])


  p_lac <-ggplot2::ggplot(data = whole_df,mapping = aes(x = dlac,color = Method, fill = Method,size = Method)) + #y = abs(dnet_div)
    ggplot2::geom_density(alpha = 0.3,adjust = 2) + #,adjust = 2
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::xlim(-1,1.5)+ #(0,1.6)
    # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
    ggplot2::scale_colour_manual("Method",values = c("orange","red4","darkgreen"))+ #,"blue3"
    ggplot2::scale_fill_manual("Method",values = c("#ffc425","red2","#8CC269"))+ #,"#4479E4"
    ggplot2::scale_size_manual(values = c(1,1,1,1))+
    ggplot2::theme(title = ggplot2::element_text(size = 15,colour = "black"),
                   text = ggplot2::element_text(size = 15,colour = "black"),
                   strip.text = element_text(size = 15,colour = "black")) +
    ggplot2::xlab(expression(Delta~lambda^c))+
    ggplot2::ylab(expression(Density))+
    # ggplot2::geom_vline(data= param_mle,aes(xintercept = median(dlac)),color = "#4479E4",linetype = "solid", size = 1)+
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)

  p_mu <-ggplot2::ggplot(data = whole_df_median1,mapping = aes(x = dmu,color = Method, fill = Method,size = Method)) + #y = abs(dnet_div)
    ggplot2::geom_density(alpha = 0.3,adjust = 1.5) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::xlim(-0.5,1.5)+ #(0,1.6)
    # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
    ggplot2::scale_colour_manual("Method",values = c("orange","red4","darkgreen"))+ #,"blue3"
    ggplot2::scale_fill_manual("Method",values = c("#ffc425","red2","#8CC269"))+ #,"#4479E4"
    ggplot2::scale_size_manual(values = c(1,1,1,1))+
    ggplot2::theme(title = ggplot2::element_text(size = 15,colour = "black"),
                   text = ggplot2::element_text(size = 15,colour = "black"),
                   strip.text = element_text(size = 15,colour = "black")) +
    ggplot2::xlab(expression(Delta~mu))+
    ggplot2::ylab(expression(Density))+
    # ggplot2::geom_vline(data= param_mle,aes(xintercept = median(dmu)),color = "#4479E4",linetype = "solid", size = 1)+
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)


  p_gam <-ggplot2::ggplot(data = whole_df_median1,mapping = aes(x = dgam,color = Method, fill = Method,size = Method)) + #y = abs(dnet_div)
    ggplot2::geom_density(alpha = 0.3,adjust = 1.5) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::xlim(-0.01,0.017)+ #(0,1.6)
    # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
    ggplot2::scale_colour_manual("Method",values = c("orange","red4","darkgreen"))+ #,"blue3"
    ggplot2::scale_fill_manual("Method",values = c("#ffc425","red2","#8CC269"))+ #,"#4479E4"
    ggplot2::scale_size_manual(values = c(1,1,1,1))+
    ggplot2::theme(title = ggplot2::element_text(size = 15,colour = "black"),
                   text = ggplot2::element_text(size = 15,colour = "black"),
                   strip.text = element_text(size = 15,colour = "black")) +
    ggplot2::xlab(expression(Delta~gamma))+
    ggplot2::ylab(expression(Density))+
    # ggplot2::geom_vline(data= param_mle,aes(xintercept = median(dgam)),color = "#4479E4",linetype = "solid", size = 1)+
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)

  p_laa <-ggplot2::ggplot(data = whole_df_median1,mapping = aes(x = dlaa,color = Method, fill = Method,size = Method)) + #y = abs(dnet_div)
    ggplot2::geom_density(alpha = 0.3,adjust = 1.5) +
    ggplot2::theme_bw() +
    ggplot2::theme_classic() +
    ggplot2::xlim(-1.5,2.5)+ #(0,1.6)
    # ggplot2::stat_smooth(method = "lm", se = F,alpha = 0.1)+
    ggplot2::scale_colour_manual("Method",values = c("orange","red4","darkgreen"))+ #,"blue3"
    ggplot2::scale_fill_manual("Method",values = c("#ffc425","red2","#8CC269"))+ #,"#4479E4"
    ggplot2::scale_size_manual(values = c(1,1,1,1))+
    ggplot2::theme(title = ggplot2::element_text(size = 15,colour = "black"),
                   text = ggplot2::element_text(size = 15,colour = "black"),
                   strip.text = element_text(size = 15,colour = "black")) +
    ggplot2::xlab(expression(Delta~lambda^a))+
    ggplot2::ylab(expression(Density))+
    # ggplot2::geom_vline(data= param_mle,aes(xintercept = median(dlaa)),color = "#4479E4",linetype = "solid", size = 1)+
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.5)

  lac_vs_mu <- ggplot2::ggplot(data = whole_df_median,mapping = ggplot2::aes(x = dlac,y = dmu,color = Method,fill = Method, shape = Method)) +
    ggplot2::theme_bw() +
    xlim(-1,1.5)+
    ylim(-0.5,1.5)+
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::scale_colour_manual("Method",values = c("orange","red4","darkgreen","#4479E4"))+
    ggplot2::scale_fill_manual("Method",values = c("orange","red2","darkgreen","#4479E4"))+
    scale_shape_manual(values=c(15,19,23,17))+
    ggplot2::xlab(expression(Delta~lambda^c)) +
    ggplot2::ylab(expression(Delta~mu)) +
    # ggplot2::geom_point(data = param_mle, mapping = ggplot2::aes(x = median(dlac),y = median(dmu)),
    #                     colour = "#4479E4",shape = 16,size = 2.5)+
    # ggplot2::geom_point(data = param_mcmc, mapping = ggplot2::aes(x = median(dlac),y = median(dmu)),
    #                     colour = "darkgreen",shape = 16,size = 2.5)
  ggplot2::geom_vline(data= param_abc, aes(xintercept = 0), colour = "grey50", linetype = "dashed") +
  ggplot2::geom_hline(data= param_abc, aes(yintercept = 0), colour = "grey50", linetype = "dashed")

  lac_vs_gam <- ggplot2::ggplot(data = whole_df_median,mapping = ggplot2::aes(x = dlac,y = dgam,color = Method,fill = Method, shape = Method)) +
    ggplot2::theme_bw() +
    xlim(-1,1.5)+
    ylim(-0.01,0.017)+
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::scale_colour_manual("Method",values = c("orange","red4","darkgreen","#4479E4"))+
    ggplot2::scale_fill_manual("Method",values = c("orange","red2","darkgreen","#4479E4"))+
    scale_shape_manual(values=c(15,19,23,17))+
    ggplot2::xlab(expression(Delta~lambda^c)) +
    ggplot2::ylab(expression(Delta~gamma)) +
    # ggplot2::geom_point(data = param_mle, mapping = ggplot2::aes(x = median(dlac),y = median(dmu)),
    #                     colour = "#4479E4",shape = 16,size = 2.5)+
    # ggplot2::geom_point(data = param_mcmc, mapping = ggplot2::aes(x = median(dlac),y = median(dmu)),
    #                     colour = "darkgreen",shape = 16,size = 2.5)
    ggplot2::geom_vline(data= param_abc, aes(xintercept = 0), colour = "grey50", linetype = "dashed") +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = 0), colour = "grey50", linetype = "dashed")

  lac_vs_laa <- ggplot2::ggplot(data = whole_df_median,mapping = ggplot2::aes(x = dlac,y = dlaa,color = Method,fill = Method, shape = Method)) +
    ggplot2::theme_bw() +
    xlim(-1,1.5)+
    ylim(-1.5,2.5)+
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::scale_colour_manual("Method",values = c("orange","red4","darkgreen","#4479E4"))+
    ggplot2::scale_fill_manual("Method",values = c("orange","red2","darkgreen","#4479E4"))+
    scale_shape_manual(values=c(15,19,23,17))+
    ggplot2::xlab(expression(Delta~lambda^c)) +
    ggplot2::ylab(expression(Delta~lambda^a)) +
    # ggplot2::geom_point(data = param_mle, mapping = ggplot2::aes(x = median(dlac),y = median(dmu)),
    #                     colour = "#4479E4",shape = 16,size = 2.5)+
    # ggplot2::geom_point(data = param_mcmc, mapping = ggplot2::aes(x = median(dlac),y = median(dmu)),
    #                     colour = "darkgreen",shape = 16,size = 2.5)
    ggplot2::geom_vline(data= param_abc, aes(xintercept = 0), colour = "grey50", linetype = "dashed") +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = 0), colour = "grey50", linetype = "dashed")

  mu_vs_gam <- ggplot2::ggplot(data = whole_df_median,mapping = ggplot2::aes(x = dmu,y = dgam,color = Method,fill = Method, shape = Method)) +
    ggplot2::theme_bw() +
    xlim(-0.5,1.5)+
    ylim(-0.01,0.017)+
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::scale_colour_manual("Method",values = c("orange","red4","darkgreen","#4479E4"))+
    ggplot2::scale_fill_manual("Method",values = c("orange","red2","darkgreen","#4479E4"))+
    scale_shape_manual(values=c(15,19,23,17))+
    ggplot2::xlab(expression(Delta~mu)) +
    ggplot2::ylab(expression(Delta~gamma)) +
    # ggplot2::geom_point(data = param_mle, mapping = ggplot2::aes(x = median(dlac),y = median(dmu)),
    #                     colour = "#4479E4",shape = 16,size = 2.5)+
    # ggplot2::geom_point(data = param_mcmc, mapping = ggplot2::aes(x = median(dlac),y = median(dmu)),
    #                     colour = "darkgreen",shape = 16,size = 2.5)
    ggplot2::geom_vline(data= param_abc, aes(xintercept = 0), colour = "grey50", linetype = "dashed") +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = 0), colour = "grey50", linetype = "dashed")

  mu_vs_laa <- ggplot2::ggplot(data = whole_df_median,mapping = ggplot2::aes(x = dmu,y = dlaa,color = Method,fill = Method, shape = Method)) +
    ggplot2::theme_bw() +
    xlim(-0.5,1.5)+
    ylim(-1.5,2.5)+
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::scale_colour_manual("Method",values = c("orange","red4","darkgreen","#4479E4"))+
    ggplot2::scale_fill_manual("Method",values = c("orange","red2","darkgreen","#4479E4"))+
    scale_shape_manual(values=c(15,19,23,17))+
    ggplot2::xlab(expression(Delta~mu)) +
    ggplot2::ylab(expression(Delta~lambda^a)) +
    # ggplot2::geom_point(data = param_mle, mapping = ggplot2::aes(x = median(dlac),y = median(dmu)),
    #                     colour = "#4479E4",shape = 16,size = 2.5)+
    # ggplot2::geom_point(data = param_mcmc, mapping = ggplot2::aes(x = median(dlac),y = median(dmu)),
    #                     colour = "darkgreen",shape = 16,size = 2.5)
    ggplot2::geom_vline(data= param_abc, aes(xintercept = 0), colour = "grey50", linetype = "dashed") +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = 0), colour = "grey50", linetype = "dashed")

  gam_vs_laa <- ggplot2::ggplot(data = whole_df_median,mapping = ggplot2::aes(x = dgam,y = dlaa,color = Method,fill = Method, shape = Method)) +
    ggplot2::theme_bw() +
    xlim(-0.01,0.017)+
    ylim(-1.5,2.5)+
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::scale_colour_manual("Method",values = c("orange","red4","darkgreen","#4479E4"))+
    ggplot2::scale_fill_manual("Method",values = c("orange","red2","darkgreen","#4479E4"))+
    scale_shape_manual(values=c(15,19,23,17))+
    ggplot2::xlab(expression(Delta~gamma)) +
    ggplot2::ylab(expression(Delta~lambda^a)) +
    # ggplot2::geom_point(data = param_mle, mapping = ggplot2::aes(x = median(dlac),y = median(dmu)),
    #                     colour = "#4479E4",shape = 16,size = 2.5)+
    # ggplot2::geom_point(data = param_mcmc, mapping = ggplot2::aes(x = median(dlac),y = median(dmu)),
    #                     colour = "darkgreen",shape = 16,size = 2.5)
    ggplot2::geom_vline(data= param_abc, aes(xintercept = 0), colour = "grey50", linetype = "dashed") +
    ggplot2::geom_hline(data= param_abc, aes(yintercept = 0), colour = "grey50", linetype = "dashed")

  p_emp <- ggplot() + theme_void()

  get_legend <- ggplot2::ggplot(data = whole_df_median,mapping = ggplot2::aes(x = dgam,y = dlaa,color = Method,fill = Method, shape = Method)) +
    ggplot2::theme_bw() +
    xlim(-0.01,0.017)+
    ylim(-1.5,2.5)+
    ggplot2::geom_point(alpha = 1) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 15),
                   text = ggplot2::element_text(size = 15)) +
    ggplot2::scale_colour_manual("Method",values = c("orange","red4","darkgreen","#4479E4"))+
    ggplot2::scale_fill_manual("Method",values = c("orange","red2","darkgreen","#4479E4"))+
    scale_shape_manual(values=c(15,19,23,17))


  tiff(paste0("daisie_high_prior/paper/cowplot_median2.tiff"),
       units="px", width=5000, height=3500,res = 400,compression="lzw")
  legend_all <- cowplot::get_legend(
    get_legend + theme(legend.box.margin = margin(0, 0, 0, 6))
  )
  param_estimates <- cowplot::plot_grid(
    p_lac+ggplot2::theme(legend.position = "none"),
    p_emp, p_emp, p_emp,
    lac_vs_mu+ggplot2::theme(legend.position = "none"),
    p_mu+ ggplot2::theme(legend.position = "none"), p_emp, legend_all,
    lac_vs_gam+ggplot2::theme(legend.position = "none"),
    mu_vs_gam+ggplot2::theme(legend.position = "none"),
    p_gam+ggplot2::theme(legend.position = "none"),p_emp,
    lac_vs_laa+ggplot2::theme(legend.position = "none"),
    mu_vs_laa+ggplot2::theme(legend.position = "none"),
    gam_vs_laa+ggplot2::theme(legend.position = "none"),
    p_laa+ggplot2::theme(legend.position = "none"),
    align = "hv", nrow = 4, ncol = 4
  )

  # param_est_final <- cowplot::plot_grid(param_estimates,legend_all,rel_widths = c(3, .4))
  print(param_estimates)
  while (!is.null(dev.list()))  dev.off()
  # }
# }