###############################################
#     Simulation study using Algorithm I      #
#---------------------------------------------#
#     Investigating various scenarios         #
#             (see Section 4.1.1)             #
###############################################
source("functions/algorithm_I.R")
source("functions/mc_simI_functions.R")

nsim = 1000

##################################################################################
#################
# Scenarios I.1 #
#################
thresholds = c(0,100,200,300,400,500)
n_sizes = c(100,300,500,800,1000)
coef_est = NULL

set.seed(30042007)
for(nn in n_sizes){
  print(paste0('Sample size: ', nn))
  pb = txtProgressBar(min = 0, max = length(thresholds), style = 3, width = 50, char = "=")
  for(tt in 1:length(thresholds)){
    result = mc.sim.algI(B = nsim, pi.compliance = 0, tau.rule = thresholds[tt],
                         n.size = nn, trunc = FALSE)
    coef_est = rbind.data.frame(coef_est,result)
    setTxtProgressBar(pb, tt)
  }
  close(pb)
}

save(coef_est, file='results/algorithmI/s1_coefs.Rdata')

# Check glm convergence
coef_est = repeat.nonconverged.fit(coef_est)


##################################################################################
#################
# Scenarios I.2 #
#################
thresholds = c(0,100,200,300,400,500)
trunc_vec = c(FALSE,TRUE,TRUE,TRUE)
perc_matrix = matrix(c(NA,NA,1,99,5,95,10,90), nrow=4, byrow=T)
coef_est = NULL

set.seed(30042007)
for(i in 1:length(trunc_vec)){
  print(paste0('Weight truncation: ',perc_matrix[i,1],'-',perc_matrix[i,2]))
  pb = txtProgressBar(min = 0, max = length(thresholds), style = 3, width = 50, char = "=")
  for(tt in 1:length(thresholds)){
    result = mc.sim.algI(B = nsim, pi.compliance = 0, tau.rule = thresholds[tt], 
                         n.size = 1000, trunc = trunc_vec[i],
                         trunc.percentiles = as.vector(perc_matrix[i,]))
    coef_est = rbind.data.frame(coef_est,result)
    setTxtProgressBar(pb, tt)
  }
  close(pb)
}

save(coef_est, file='results/algorithmI/s2_coefs.Rdata')

# Check glm convergence
coef_est = repeat.nonconverged.fit(coef_est)


##################################################################################
#################
# Scenarios I.3 #
#################
thresholds = c(0,100,200,300,400,500)
pi_values = c(0.1,0.3,0.5,0.8,1)
coef_est = NULL

set.seed(30042007)
for(pp in pi_values){
  print(paste0('Compliance threshold: ', pp))
  pb = txtProgressBar(min = 0, max = length(thresholds), style = 3, width = 50, char = "=")
  for(tt in 1:length(thresholds)){
    for(i in c(FALSE,TRUE)){
      result = mc.sim.algI(B = nsim, pi.compliance = pp, tau.rule = thresholds[tt],
                              n.size = 1000, trunc=i)
      coef_est = rbind.data.frame(coef_est,result)
      setTxtProgressBar(pb, tt)
    }
  }
  close(pb)
}

save(coef_est, file='results/algorithmI/s3_coefs.Rdata')

# Check glm convergence
coef_est = repeat.nonconverged.fit(coef_est)
save(coef_est, file='results/algorithmI/s3_coefs_converged.Rdata')

