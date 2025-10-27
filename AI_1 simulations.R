###############################################
#     Simulation study using Algorithm I      #
#---------------------------------------------#
#     Investigating various scenarios         #
#             (see Section 4.1.1)             #
###############################################
library(foreach)
library(doParallel)

run_simulation_studyI <- function(n.cores = 5, n.sizes = c(50,100,250,500,1000), n.sim = 1000) {
  
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
    source("functions/algorithm_I.R")
    source("functions/mc_simI_functions.R")
    # param
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
          result = mc.sim.algI(B = n.sim, pi.compliance = pp, tau.rule = tt, 
                               n.size = n.sizes[j], trunc = trunc_vec[i],
                               trunc.percentiles = as.vector(perc_matrix[i,]))
          coef_est = rbind.data.frame(coef_est, result$coef_est)
          ipw_weights = rbind.data.frame(ipw_weights, result$weights)
        }
      }
    }
    save(ipw_weights, file=paste0("results/algorithmI/ipw_summary_n",n.sizes[j],".Rdata"))
    save(coef_est,file=paste0("results/algorithmI/sim_coefs_n",n.sizes[j],".Rdata"))
  }
  
  end_time = Sys.time()
  message(paste0("\nAll ",n.sim," simulations finished successfully for sample size = ",n.sizes[1:n.cores]))
  message("Running time: ")
  print(end_time - start_time)
  
}

# See ReadMe.txt if your machine has fewer than 5 cores
run_simulation_studyI(n.cores=5, n.sizes=c(50,100,250,500,1000), n.sim=1000)
