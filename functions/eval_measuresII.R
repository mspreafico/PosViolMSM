#############################################
# Functions to evaluate the results for the #
#   scenarios simulated using Algorithm II  #
#############################################
library(data.table)
library(tidyr)

# Compute performance measures for a given cumulative regression coefficient
eval.resultsII <- function(data, coef.name, true.df, var.names = c('WT','pi','tau','n','time')){
  
  data = data.table(data)
  B = max(data$rep_b)
  keep = c('rep_b',var.names,coef.name)
  result = data[!is.na(get(coef.name)), ..keep]
  result = merge(result, true.df[Ccoef==coef.name, .(time,true)])
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
  
  return(list('errors' = result[,1:(length(keep)+2)], 'eval' = eval))
}


# Performance marginal survival probabilities: always treated vs never treated
eval.marginal.surv.probII <- function(data.surv, var.names = c('WT','pi','tau','n'), 
                                     true.surv){
  
  B = max(data.surv$rep_b)
  result = merge(data.surv, true.surv, by='time')
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
  
  
  eval = result[, c(3:6,1,11:14,16:19,21:24)]
  eval = eval[!duplicated(eval)]
  setorderv(eval,c(var.names, 'time'))
  
  errors = result[,c(2:6,1,7,9,15,8,10,20)]
  setorderv(errors, c('rep_b',var.names, 'time'))
  
  return(list('errors' = errors, 'eval' = eval))
}
