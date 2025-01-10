#############################################
# Functions to evaluate the results for the #
#   scenarios simulated using Algorithm I   #
#############################################
library(data.table)
library(tidyr)

# Define inverse logit (expit) function
expit <- function(x) return(exp(x)/(1 + exp(x)))

# Check estreme results (estimation fail)
est.fail <- function(data, var.names = c('WT','pi','tau','n')){
  
  data = data.table(data)
  data[, fail := FALSE]
  data[abs(gamma0)>1e10 | abs(gammaA1)>1e10 | 
         abs(gammaA2)>1e10 | abs(gammaA3)>1e10, fail := TRUE]
  data[, deleted := sum(fail), by=var.names]
  
  return(data)
  
}


# Compute performance measures for a given regression coefficient
eval.resultsI <- function(data, coef.name, true, var.names = c('WT','pi','tau','n')){
  
  data = data.table(data)
  # Extreme cases
  data = est.fail(data, var.names)
  
  B = max(data$rep_b)
  keep = c('rep_b',var.names,coef.name,'deleted')
  result = data[fail==FALSE, ..keep]
  result[, nsim := B - deleted]
  result[, error := get(coef.name) - true]
  result[, meanG := mean(get(coef.name)), by=var.names]
  result[, empSE := sd(get(coef.name)), by=var.names]
  result[, Bias := sum(error)/(nsim), by=var.names]
  result[, Bias_MCse := empSE/sqrt(nsim), by=var.names]
  result[, MSE := sum((error)^2)/nsim, by=var.names]
  result[, MSE_MCse := sqrt(sum(((error)^2-MSE)^2)/(nsim*(nsim-1))), by=var.names]
  result[, rel_Bias := Bias/true, by=var.names]
  result[, RMSE := sqrt(MSE), by=var.names]
  
  keep2 = c(var.names,'deleted','meanG','empSE','Bias','Bias_MCse','MSE','MSE_MCse','rel_Bias','RMSE')
  eval = result[, ..keep2]
  eval = eval[!duplicated(eval)]
  
  keep3 = c('rep_b',var.names,coef.name,'error')
  
  return(list('errors' = result[, ..keep3], 'eval' = eval))
}


# Compute marginal survival probabilities: always treated vs never treated
marginal.surv.probI <- function(data, times, var.names = c('WT','pi','tau','n')){
  
  data = data.table(data)
  B = max(data$rep_b)
  # Extreme cases
  data = est.fail(data, var.names)
  data[, nsim := B - deleted, by=var.names]
  data[, deleted := NULL]

  marg.surv = data.table(crossing(data[fail==FALSE], time = times))
  marg.surv[, haz0 := expit(gamma0+gammaA1*time)]
  marg.surv[, haz1 := expit(gamma0+gammaA2+gammaA3*time)]
  var.rep.names = c('rep_b',var.names)
  marg.surv[, surv0 := cumprod(1-haz0), by=var.rep.names]
  marg.surv[, surv1 := cumprod(1-haz1), by=var.rep.names]
  
  keep = c(var.rep.names,'time','haz0','haz1','surv0','surv1','nsim')
  
  return(marg.surv[, ..keep])
}


# Performance marginal survival probabilities: always treated vs never treated
eval.marginal.surv.probI <- function(data.surv, var.names = c('WT','pi','tau','n'), 
                                     true.surv){
  
  var.time.names = c(var.names, 'time')
  keep = c('rep_b',var.time.names,'surv0','surv1','nsim')
  result = merge(data.surv[, ..keep], true.surv, by='time')
  
  result[, avg_surv0 := mean(surv0), by=var.time.names]
  result[, avg_surv1 := mean(surv1), by=var.time.names]
  result[, empSE_surv0 := sd(surv0), by=var.time.names]
  result[, empSE_surv1 := sd(surv1), by=var.time.names]
  
  result[, error_surv0 := surv0 - true_surv0]
  result[, Bias_surv0 := sum(error_surv0)/nsim, by=var.time.names]
  result[, Bias_surv0_MCse := empSE_surv0/sqrt(nsim), by=var.time.names]
  result[, MSE_surv0 := sum((error_surv0)^2)/nsim, by=var.time.names]
  result[, MSE_surv0_MCse := sqrt(sum(((error_surv0)^2-MSE_surv0)^2)/(nsim*(nsim-1))), by=var.time.names]
  
  result[, error_surv1 := surv1 - true_surv1]
  result[, Bias_surv1 := sum(error_surv1)/nsim, by=var.time.names]
  result[, Bias_surv1_MCse := empSE_surv1/sqrt(nsim), by=var.time.names]
  result[, MSE_surv1 := sum((error_surv1)^2)/nsim, by=var.time.names]
  result[, MSE_surv1_MCse := sqrt(sum(((error_surv1)^2-MSE_surv1)^2)/(nsim*(nsim-1))), by=var.time.names]
  
  keep2 = names(result)[-which(names(result) %in% 
                                 c("rep_b","surv0","surv1",
                                   "true_haz0","true_haz1","true_surv0","true_surv1",
                                   "error_surv0","error_surv1"))]
  eval = result[, ..keep2]
  eval = eval[!duplicated(eval)]
  setorderv(eval,c(var.names, 'time'))
  
  keep3 = c('rep_b',var.time.names,"surv0","true_surv0","error_surv0",
            "surv1","true_surv1","error_surv1")
  errors = result[, ..keep3]
  setorderv(errors, c('rep_b',var.names, 'time'))
  
  return(list('errors' = errors, 'eval' = eval))
}


# Compute true survival probabilities
true.survival.probI <- function(times, true_gam = c(-3,0.05,-1.5,0.05)){
  
  true.surv = data.table('time'=times)
  
  true.surv[, true_haz0 := expit(true_gam[1]+true_gam[2]*time)]
  true.surv[, true_haz1 := expit(true_gam[1]+true_gam[3]+true_gam[4]*time)]
  true.surv[, true_surv0 := cumprod(1-true_haz0)]
  true.surv[, true_surv1 := cumprod(1-true_haz1)]
  
  return(true.surv)
}
