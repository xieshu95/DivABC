# treck the likelihood trace with different mu
calc_loglik_secsse <- function(params, datalist) {
  pars <- secsse::id_paramPos(traits = datalist$examTraits,num_concealed_states = 2)
  pars[[1]][] <- c(params[1],params[2],params[1],params[2])
  pars[[2]][] <- c(params[3],params[4],params[3],params[4])
  masterBlock <- matrix(c(params[5],params[6]),
                        ncol=2,nrow=2,byrow=TRUE)
  diag(masterBlock) <- NA
  q <-secsse::q_doubletrans(c(1,2),masterBlock,diff.conceal=F)
  q[1,3]<- q[2,4] <- q[3,1] <- q[4,2] <- 0
  pars[[3]][] <- q
  log_lik <- secsse::secsse_loglik(
    parameter = pars,
    phy = datalist$phy,
    traits = datalist$examTraits,
    num_concealed_states = 2,
    sampling_fraction = c(1,1),
    cond = "proper_cond"
  )
  return(log_lik)
}

## run
param_space <- readr::read_csv2("data/secsse_ABC_test5.csv")
load(paste0("scripts/loglik_test/obs_ss_test5.RData"))
load(paste0("scripts/loglik_test/whole_df_MLE5.RData"))

## 3D mu1_mu2_loglik
for(i in 1:100) {
  message("set",i)
  set.seed(i)
  obs_sim_pars <- param_space[i,]
  obs_sim <- get_secsse_sim_create_obs(parameters = as.numeric(obs_sim_pars),
                                       K = Inf,
                                       replicates = 1)
  startingpoint <- DDD::bd_ML(brts = ape::branching.times(obs_sim[[1]]$phy))
  ml_pars <- as.numeric(whole_df_MLE[i,8:13])
  loglik <- c() # fix mu2 only change mu1
  mu1 <- seq(0,1,0.025)
  mu2 <- seq(0,1,0.025)
  mu_comb <- data.frame(expand.grid(mu1,mu2))
  colnames(mu_comb) <-c("mu1","mu2")
  for(n in 1:nrow(mu_comb)){
    pars <- c(ml_pars[1],ml_pars[2],mu_comb[n,1],mu_comb[n,2],ml_pars[5],ml_pars[6])
    loglik[n] <- calc_loglik_secsse(pars,obs_sim[[1]])
  }
  pars_ll <- data.frame(mu_comb,loglik)
  # save(pars_ll,file = paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3_new3/loglik_trace/test5/pars_loglik",i,".RData"))

}

plot(x = pars_ll1[,1],y = pars_ll1[,2])

library(reshape2)
plot_matrix <- t(acast(pars_ll, mu1~mu2, value.var="loglik1"))
plot_matrix

persp(x = as.numeric(colnames(plot_matrix)),
      y = as.numeric(rownames(plot_matrix)),
      z = plot_matrix,
      xlab = "mu1",
      ylab = "mu2",
      zlab = "loglik",
      ticktype ='detailed',
      theta = 120,
      phi = 40,
      col = "red", shade = 0.3)

tiff(paste0("G:/results/project 2/tip_info/round4/adap_secsse_test3_new3/loglik_trace/test5/pars_loglik",i,".tiff"),
     units="px", width=2000, height=1500,res = 300,compression="lzw")
params <- cowplot::plot_grid(
  p_lam1+ggplot2::theme(legend.position = "none"),
  p_mu1+ggplot2::theme(legend.position = "none"),
  p_q12+ggplot2::theme(legend.position = "none"),
  p_lam2+ggplot2::theme(legend.position = "none"),
  p_mu2+ggplot2::theme(legend.position = "none"),
  p_q21+ggplot2::theme(legend.position = "none"),
  align = "hv", nrow = 2, ncol = 3
)
legend <- cowplot::get_legend(
  p_lam1 + theme(legend.box.margin = margin(0, 0, 0, 6))
)
param_estimates <- cowplot::plot_grid(params,legend,
                                      rel_widths = c(3,0.4)
)
print(param_estimates)
while (!is.null(dev.list()))  dev.off()



library(plotly)
plot_ly(
  x = as.numeric(colnames(plot_matrix)),
  y = as.numeric(rownames(plot_matrix)),
  z = plot_matrix
) %>%
  add_surface() %>%
  layout(
    title = "",
    scene = list(
      xaxis = list(type = "log", title = "Total observations"),
      yaxis = list(type = "log", title = "Firm size"),
      zaxis = list(title = "Median"),
      camera = list(eye = list(x = 1.95, y = -1.25, z = 1.25))
    ))

library("scatterplot3d")

p1 <- pars_ll[which(pars_ll$mu1 == 0.5),]
p1
plot(x = p1[,2],y = p1[,3])
