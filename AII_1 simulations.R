###############################################
#     Simulation study using Algorithm II     #
#---------------------------------------------#
#     Investigating various scenarios         #
#             (see Section 5.2.1)             #
###############################################
library(foreach)
library(doParallel)
# Register all the cores
getDoParWorkers()
n.cores = 5
cl <- makeCluster(n.cores)
registerDoParallel(cl)

# Determine possible rule-threshold values
source("functions/algorithm_II.R")
set.seed(12345)
df.no.viol = sim.algorithmII(pi.prop = 10, tau = -Inf, n = 100000)
valuesQ = quantile(df.no.viol$L, probs = c(0.8,0.9,0.95,0.99,1))
round(valuesQ, 2)
hist(df.no.viol$L)

# Sample sizes
n_sizes = c(50,100,250,500,1000)

foreach(j=1:n.cores) %dopar% {
  # load
  source("functions/algorithm_II.R")
  source("functions/mc_simII_functions.R")
  # Set B and rule-threshold values
  nsim = 1000
  thresholds = c(1,1.5,2,3,7,10)
  time_points = seq(0,5,0.1)
  pi_values = c(0,0.05,0.1,0.3,0.5,0.8,1)
  trunc_vec = c(FALSE,TRUE,TRUE,TRUE)
  perc_matrix = matrix(c(NA,NA,1,99,5,95,10,90), nrow=4, byrow=T)
  
  cum_coef_est = NULL
  surv_est = NULL
  set.seed(30042007)
  for(i in 1:length(trunc_vec)){
    for(pp in pi_values){
      for(tt in thresholds){
        result = mc.sim.algII(B = nsim, pi.compliance = pp, tau.rule = tt,
                              n.size = n_sizes[j], trunc = trunc_vec[i],
                              trunc.percentiles = as.vector(perc_matrix[i,]),
                              t.hor = time_points)
        cum_coef_est = rbind.data.frame(cum_coef_est, result$cum_coefs)
        surv_est = rbind.data.frame(surv_est, result$survivals)
      }
    }
  }
  save(cum_coef_est, file=paste0("results/algorithmII/sim_coefs_n",n_sizes[j],".Rdata"))
  save(surv_est, file=paste0("results/algorithmII/survival_est_n",n_sizes[j],".Rdata"))
  print(paste0("SAVED: files for sample size ", n_sizes[j]))
}

getDoParWorkers()
stopCluster(cl)

