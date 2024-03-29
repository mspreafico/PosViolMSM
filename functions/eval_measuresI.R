#############################################
# Functions to evaluate the results for the #
#   scenarios simulated using Algorithm I   #
#############################################
library(data.table)
library(tidyr)

# Define inverse logit (expit) function
expit <- function(x) return(exp(x)/(1 + exp(x)))


# Compute performance measures for a given regression coefficient
eval.resultsI <- function(data, coef.name, true, var.names = c('WT','pi','tau','n')){
  
  data = data.table(data)
  B = max(data$rep_b)
  keep = c('rep_b',var.names,coef.name)
  result = data[, ..keep]
  result[, error := get(coef.name) - true]
  result[, meanG := mean(get(coef.name)), by=var.names]
  result[, empSE := sd(get(coef.name)), by=var.names]
  result[, Bias := sum(error)/B, by=var.names]
  result[, Bias_MCse := empSE/sqrt(B), by=var.names]
  result[, MSE := sum((error)^2)/B, by=var.names]
  result[, MSE_MCse := sqrt(sum(((error)^2-MSE)^2)/(B*(B-1))), by=var.names]
  
  keep2 = c(var.names,'meanG','empSE','Bias','Bias_MCse','MSE','MSE_MCse')
  eval = result[, ..keep2]
  eval = eval[!duplicated(eval)]

  return(list('errors' = result[,1:(length(keep)+1)], 'eval' = eval))
}


# Compute marginal survival probabilities: always treated vs never treated
marginal.surv.probI <- function(data, times, var.names = c('WT','pi','tau','n')){
  
  data = data.table(data)
  marg.surv = data.table(crossing(data, time = times))
  marg.surv[, haz0 := expit(gamma0+gammaA1*time)]
  marg.surv[, haz1 := expit(gamma0+gammaA2+gammaA3*time)]
  var.rep.names = c('rep_b',var.names)
  marg.surv[, surv0 := cumprod(1-haz0), by=var.rep.names]
  marg.surv[, surv1 := cumprod(1-haz1), by=var.rep.names]
  
  return(marg.surv[,c(1:5,10:14)])
}


# Performance marginal survival probabilities: always treated vs never treated
eval.marginal.surv.probI <- function(data.surv, var.names = c('WT','pi','tau','n'), 
                                     true.surv){
  
  B = max(data.surv$rep_b)
  result = merge(data.surv[,c(1:6,9:10)], true.surv, by='time')
  var.time.names = c(var.names, 'time')
  result[, avg_surv0 := mean(surv0), by=var.time.names]
  result[, avg_surv1 := mean(surv1), by=var.time.names]
  result[, empSE_surv0 := sd(surv0), by=var.time.names]
  result[, empSE_surv1 := sd(surv1), by=var.time.names]
  
  result[, error_surv0 := surv0 - true_surv0]
  result[, Bias_surv0 := sum(error_surv0)/B, by=var.time.names]
  result[, Bias_surv0_MCse := empSE_surv0/sqrt(B), by=var.time.names]
  result[, MSE_surv0 := sum((error_surv0)^2)/B, by=var.time.names]
  result[, MSE_surv0_MCse := sqrt(sum(((error_surv0)^2-MSE_surv0)^2)/(B*(B-1))), by=var.time.names]
  
  result[, error_surv1 := surv1 - true_surv1]
  result[, Bias_surv1 := sum(error_surv1)/B, by=var.time.names]
  result[, Bias_surv1_MCse := empSE_surv1/sqrt(B), by=var.time.names]
  result[, MSE_surv1 := sum((error_surv1)^2)/B, by=var.time.names]
  result[, MSE_surv1_MCse := sqrt(sum(((error_surv1)^2-MSE_surv1)^2)/(B*(B-1))), by=var.time.names]
  
  
  eval = result[, c(3:6,1,13:16,18:21,23:26)]
  eval = eval[!duplicated(eval)]
  setorderv(eval,c(var.names, 'time'))
  
  errors = result[,c(2:6,1,7,11,17,8,12,22)]
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
