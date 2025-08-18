#################################################
#         Functions to estimate IPTW in         #
# longitudinal data simulated from Algorithm II #
#################################################

# Get IPTW weights
get.std.weightsII <- function(df.long, trunc = FALSE, percentiles = c(1,99)) {
  # df.long: data simulated in long format
  
  # Model for denominator (includes L)
  wt.mod = glm(A ~ L + Alag1, family="binomial", data=df.long)
  pred.wt = predict(wt.mod, type = "response")
  df.long$wt = ifelse(df.long$A==1, pred.wt, 1-pred.wt)
  df.long$wt.cum = ave(df.long$wt, df.long$id, FUN=cumprod)
  
  # Model for numerator (no L)
  wt.mod.num = glm(A ~ Alag1, family="binomial", data=df.long)
  pred.wt.num = predict(wt.mod.num, type = "response")
  df.long$wt.num = ifelse(df.long$A==1, pred.wt.num, 1-pred.wt.num)
  df.long$wt.cum.num = ave(df.long$wt.num, df.long$id, FUN=cumprod)
  
  # Stabilized weights
  df.long$sw = df.long$wt.cum.num/df.long$wt.cum
  # Truncated Stabilized weight
  if(trunc==TRUE){ df.long$sw <- trunc.sw(df.long$sw, percentiles = percentiles) }
  
  return(df.long)
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
eval.ipwII <- function(df.weights){
  
  df.weights[, mean_sw := mean(sw), by = c('time')]
  df.weights[, sd_sw := sd(sw), by = c('time')]
  df.weights[, min_sw := min(sw), by = c('time')]
  df.weights[, max_sw := max(sw), by = c('time')]
  
  df.weights[, id := NULL]
  df.weights[, sw := NULL]
  df.weights = df.weights[!duplicated(df.weights)]
  
  return(df.weights)
}
