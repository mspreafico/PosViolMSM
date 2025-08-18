################################################
#         Functions to estimate IPTW in        #
# longitudinal data simulated from Algorithm I #
################################################

# Get IPTW weights
get.std.weightsI <- function(data, kappa = 5, trunc = FALSE, percentiles = c(1,99)) {
  # data: data simulated from one of three above functions
  # kappa: time interval between which exposure and confounder can be updated.
  # Model for denominator (includes L)
  den <- glm(A ~ visit + L, family=binomial, subset=(visit %% kappa == 0 & A_1==0), data=data)
  treat.prob.den <- rep(1, dim(data)[1])
  treat.prob.den[data$visit %% kappa == 0 & data$A_1==0] <- ifelse(data$A[data$visit %% kappa == 0 & data$A_1==0]==1, 
                                                                   predict(den, type='response'), 
                                                                   1-predict(den, type='response'))
  # Model for numerator (no L)
  num <- glm(A ~ visit, family=binomial, subset=(visit %% kappa == 0 & A_1==0), data=data) 
  treat.prob.num <- rep(1, dim(data)[1])
  treat.prob.num[data$visit %% kappa == 0 & data$A_1==0] <- ifelse(data$A[data$visit %% kappa == 0 & data$A_1==0]==1, 
                                                                   predict(num, type='response'), 
                                                                   1-predict(num, type='response'))
  # Stabilized weight under time-dependent treatment (cumulative)
  treat.weights <- unlist(tapply(treat.prob.num/treat.prob.den, data$id, cumprod))
  # Add the stabilized weight to the dataset
  data.weights = cbind(data, sw=treat.weights)
  # Truncation
  if(trunc==TRUE){ data.weights$sw <- trunc.sw(data.weights$sw, percentiles = percentiles) }
  
  return(data.frame(data.weights))
}

# Weight truncation
trunc.sw <- function(weights, percentiles) {
  lowerQ = quantile(weights, percentiles[1]/100)
  upperQ = quantile(weights, percentiles[2]/100)
  #Truncation
  weights = ifelse(weights < lowerQ, lowerQ, weights)
  weights = ifelse(weights > upperQ, upperQ, weights)
  return(weights)
}

# Mean-Min-Max of IPTW weights
eval.ipwI <- function(df.weights){
  
  df.weights[, mean_sw := mean(sw), by = c('visit')]
  df.weights[, sd_sw := sd(sw), by = c('visit')]
  df.weights[, min_sw := min(sw), by = c('visit')]
  df.weights[, max_sw := max(sw), by = c('visit')]
  
  df.weights[, id := NULL]
  df.weights[, sw := NULL]
  df.weights = df.weights[!duplicated(df.weights)]
  
  return(df.weights)
}