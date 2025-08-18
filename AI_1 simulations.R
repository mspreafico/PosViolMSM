###############################################
#     Simulation study using Algorithm I      #
#---------------------------------------------#
#     Investigating various scenarios         #
#             (see Section 4.1.1)             #
###############################################
library(foreach)
library(doParallel)
# Register all the cores
getDoParWorkers()
n.cores = 5
cl <- makeCluster(n.cores)
registerDoParallel(cl)

n_sizes = c(50,100,250,500,1000)

foreach(j=1:n.cores) %dopar% {
  # load
  source("functions/algorithm_I.R")
  source("functions/mc_simI_functions.R")
  # param
  nsim = 1000
  thresholds = c(0,100,200,300,400,500)
  pi_values = c(0,0.05,0.1,0.3,0.5,0.8,1)
  trunc_vec = c(FALSE,TRUE,TRUE,TRUE)
  perc_matrix = matrix(c(NA,NA,1,99,5,95,10,90), nrow=4, byrow=T)
  
  coef_est = NULL
  ipw_weights = NULL
  set.seed(30042007)
  for(i in 1:length(trunc_vec)){
    for(pp in pi_values){
      for(tt in thresholds){
        result = mc.sim.algI(B = nsim, pi.compliance = pp, tau.rule = tt, 
                             n.size = n_sizes[j], trunc = trunc_vec[i],
                             trunc.percentiles = as.vector(perc_matrix[i,]))
        coef_est = rbind.data.frame(coef_est, result$coef_est)
        ipw_weights = rbind.data.frame(ipw_weights, result$weights)
      }
    }
  }
  save(ipw_weights, file=paste0("results/algorithmI/ipw_summary_n",n_sizes[j],".Rdata"))
  save(coef_est,file=paste0("results/algorithmI/sim_coefs_n",n_sizes[j],".Rdata"))
  print(paste0("SAVED: files for sample size ", n_sizes[j]))
}


getDoParWorkers()
stopCluster(cl)
