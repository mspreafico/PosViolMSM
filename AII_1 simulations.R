###############################################
#     Simulation study using Algorithm II     #
#---------------------------------------------#
#     Investigating various scenarios         #
#             (see Section 5.2.1)             #
###############################################
library(foreach)
library(doParallel)

# Determine possible rule-threshold values
source("functions/algorithm_II.R")
set.seed(12345)
df.no.viol = sim.algorithmII(pi.prop = 10, tau = -Inf, n = 100000)
valuesQ = quantile(df.no.viol$L, probs = c(0.8,0.9,0.95,0.99,1))
round(valuesQ, 2)
hist(df.no.viol$L)

run_simulation_studyII <- function(n.cores = 5, n.sizes = c(50,100,250,500,1000), n.sim = 1000) {
  
  # Warn if number of cores does not match number of sample sizes
  if (n.cores > length(n.sizes)) {
    stop(
      "Number of cores (", n.cores, 
      ") is greater than the number of sample sizes (", length(n.sizes), ").",
      " Some cores will be idle. Please set n.cores to ", length(n.sizes),"."
    )
  } else if (n.cores < length(n.sizes)) {
    message(
      "Warning: Number of cores (", n.cores, 
      ") is less than the number of sample sizes (", length(n.sizes), ").",
      " Some sample sizes will not be processed in parallel and will need to be run by",
      " calling the function again for the missing values. See ReadMe.txt for further details."
    )
  }
  
  # Register n.cores cores for parallelization 
  getDoParWorkers()
  cl <- makeCluster(n.cores)
  registerDoParallel(cl)
  # Stop cluster when the script/function exits
  on.exit(stopCluster(cl), add = TRUE)
  
  start_time = Sys.time()
  
  foreach(j=1:n.cores) %dopar% {
    # load
    source("functions/algorithm_II.R")
    source("functions/mc_simII_functions.R")
    # Set rule-threshold values
    thresholds = c(1,1.5,2,3,7,10)
    time_points = seq(0,5,0.1)
    pi_values = c(0,0.05,0.1,0.3,0.5,0.8,1)
    trunc_vec = c(FALSE,TRUE,TRUE,TRUE)
    perc_matrix = matrix(c(NA,NA,1,99,5,95,10,90), nrow=4, byrow=T)
    
    ipw_weights = NULL
    cum_coef_est = NULL
    surv_est = NULL
    set.seed(30042007)
    for(i in 1:length(trunc_vec)){
      for(pp in pi_values){
        for(tt in thresholds){
          print(paste0(i,'-',pp,'-',tt))
          result = mc.sim.algII(B = n.sim, pi.compliance = pp, tau.rule = tt,
                                n.size = n.sizes[j], trunc = trunc_vec[i],
                                trunc.percentiles = as.vector(perc_matrix[i,]),
                                t.hor = time_points)
          ipw_weights = rbind.data.frame(ipw_weights, result$weights)
          cum_coef_est = rbind.data.frame(cum_coef_est, result$cum_coefs)
          surv_est = rbind.data.frame(surv_est, result$survivals)
        }
      }
    }
    save(ipw_weights, file=paste0("results/algorithmII/ipw_summary_n",n.sizes[j],".Rdata"))
    save(cum_coef_est, file=paste0("results/algorithmII/sim_coefs_n",n.sizes[j],".Rdata"))
    save(surv_est, file=paste0("results/algorithmII/survival_est_n",n.sizes[j],".Rdata"))
  
  }
  
  end_time = Sys.time()
  message(paste0("\nAll ",n.sim," simulations finished successfully for sample size = ",n.sizes[1:n.cores]))
  message("Running time: ")
  print(end_time - start_time)

}

# See ReadMe.txt if your machine has fewer than 5 cores
run_simulation_studyII(n.cores=5, n.sizes=c(50,100,250,500,1000), n.sim=1000)
