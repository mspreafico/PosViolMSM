###############################################
#     Simulation study using Algorithm II     #
#---------------------------------------------#
#     Investigating various scenarios         #
#             (see Section 4.2.1)             #
###############################################
source("functions/algorithm_II.R")
source("functions/mc_simII_functions.R")

# Determine possible rule-threshold values
set.seed(12345)
df.no.viol = sim.algorithmII(pi.prop = 10, tau = -Inf, n = 100000)
valuesQ = quantile(df.no.viol$L, probs = c(0.8,0.9,0.95,0.99,1))
round(valuesQ, 2)
hist(df.no.viol$L)

# Set B and rule-threshold values
nsim = 1000
thresholds = c(1,1.5,2,3,7,10)
time_points = seq(0,5,0.1)


##################################################################################
##################
# Scenarios II.1 #
##################
n_sizes = c(100,300,500,800,1000)
cum_coef_est = NULL
surv_est = NULL

set.seed(30042007)
for(nn in n_sizes){
  print(paste0('Sample size: ', nn))
  pb = txtProgressBar(min = 0, max = length(thresholds), style = 3, width = 50, char = "=")
  for(tt in 1:length(thresholds)){
    result = mc.sim.algII(B = nsim, pi.compliance = 0, tau.rule = thresholds[tt],
                          n.size = nn, trunc = FALSE, t.hor = time_points)
    cum_coef_est = rbind.data.frame(cum_coef_est, result$cum_coefs)
    surv_est = rbind.data.frame(surv_est, result$survivals)
    setTxtProgressBar(pb, tt)
  }
  close(pb)
}

save(cum_coef_est, file='results/algorithmII/s1_coefs.Rdata')
save(surv_est, file='results/algorithmII/s1_survivals.Rdata')


##################################################################################
##################
# Scenarios II.2 #
##################
trunc_vec = c(FALSE,TRUE,TRUE,TRUE)
perc_matrix = matrix(c(NA,NA,1,99,5,95,10,90), nrow=4, byrow=T)
cum_coef_est = NULL
surv_est = NULL

set.seed(30042007)
for(i in 1:length(trunc_vec)){
  print(paste0('Weight truncation: ',perc_matrix[i,1],'-',perc_matrix[i,2]))
  pb = txtProgressBar(min = 0, max = length(thresholds), style = 3, width = 50, char = "=")
  for(tt in 1:length(thresholds)){
    result = mc.sim.algII(B = nsim, pi.compliance = 0, tau.rule = thresholds[tt],
                          n.size = 1000, trunc = trunc_vec[i],
                          trunc.percentiles = as.vector(perc_matrix[i,]),
                          t.hor = time_points)
    cum_coef_est = rbind.data.frame(cum_coef_est, result$cum_coefs)
    surv_est = rbind.data.frame(surv_est, result$survivals)
    setTxtProgressBar(pb, tt)
  }
  close(pb)
}

save(cum_coef_est, file='results/algorithmII/s2_coefs.Rdata')
save(surv_est, file='results/algorithmII/s2_survivals.Rdata')



##################################################################################
##################
# Scenarios II.3 #
##################
pi_values = c(0.1,0.3,0.5,0.8,1)
cum_coef_est = NULL
surv_est = NULL

set.seed(30042007)
for(pp in pi_values){
  print(paste0('Compliance threshold: ', pp))
  pb = txtProgressBar(min = 0, max = length(thresholds), style = 3, width = 50, char = "=")
  for(tt in 1:length(thresholds)){
    for(i in c(FALSE,TRUE)){
      result = mc.sim.algII(B = nsim, pi.compliance = pp, tau.rule = thresholds[tt],
                            n.size = 1000, trunc = i, t.hor = time_points)
      cum_coef_est = rbind.data.frame(cum_coef_est, result$cum_coefs)
      surv_est = rbind.data.frame(surv_est, result$survivals)
      setTxtProgressBar(pb, tt)
    }
  }
  close(pb)
}

save(cum_coef_est, file='results/algorithmII/s3_coefs.Rdata')
save(surv_est, file='results/algorithmII/s3_survivals.Rdata')

