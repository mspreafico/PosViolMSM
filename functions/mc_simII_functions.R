#################################################
#  Functions to estimate the Aalen-MSMs using   #
# longitudinal data simulated from Algorithm II #
#################################################
library(timereg)

# Weight truncation
trunc.sw <- function(weights, percentiles) {
  lowerQ = quantile(weights, percentiles[1]/100)
  upperQ = quantile(weights, percentiles[2]/100)
  #Truncation
  weights = ifelse(weights < lowerQ, lowerQ, weights)
  weights = ifelse(weights > upperQ, upperQ, weights)
  return(weights)
}

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
  # Add the stabilized weight to the dataset
  
  return(df.long)
}

# Fit Aalen's marginal structural models
aalen.MSM <- function(df.long, K){
  
  ah.MSM = list()
  formula.t = 'Surv(time,time.stop,event)~A'
  for(t in 1:K){
    ah.tmp = aalen(as.formula(formula.t), data=df.long[df.long$time==(t-1),], 
                   n.sim=0, weights=df.long[df.long$time==(t-1),]$sw)
    ah.MSM[[t]] = ah.tmp
    formula.t = paste0(formula.t,'+Alag',t)
  }
  
  return(ah.MSM)
}


# Create a step function for each estimated cumulative coefficient, 
# enabling us to obtain the estimated cumulative coefficient at any time 
ah.step.functions <- function(ah.MSM){
  K = length(ah.MSM)
  ah.stepfun = NULL
  
  time = c(ah.MSM[[1]]$cum[,1])
  maxrow = c(dim(ah.MSM[[1]]$cum)[1])
  sum.maxrow0 = c(ah.MSM[[1]]$cum[maxrow[1],2], ah.MSM[[1]]$cum[maxrow[1],3])
  for(t in 2:K){
    time = c(time, ah.MSM[[t]]$cum[-1,1])
    maxrow = c(maxrow, dim(ah.MSM[[t]]$cum)[1])
    sum.maxrow0 = c(sum.maxrow0, ah.MSM[[t]]$cum[maxrow[t],t+2])
  }
  
  for(m in 1:2){
    ah.cum = c(0, ah.MSM[[1]]$cum[,m+1])
    for(t in 2:K){
      ah.cum = c(ah.cum, sum.maxrow0[m] + ah.MSM[[t]]$cum[-1,m+1])
      sum.maxrow0[m] = sum.maxrow0[m] + ah.MSM[[t]]$cum[maxrow[t],m+1]
    }
    ah.stepfun[[m]] = stepfun(time, ah.cum)
    ah.cum = NULL
  }
  
  Nmod = length(sum.maxrow0)
  ah.cum0 = c(0, rep(0,maxrow[1]))
  for(m in 3:(Nmod-1)){
    ah.cum = c(ah.cum0, ah.MSM[[m-1]]$cum[-1,m+1])
    for(t in m:K){
      ah.cum = c(ah.cum, sum.maxrow0[m] + ah.MSM[[t]]$cum[-1,m+1])
      sum.maxrow0[m] = sum.maxrow0[m] + ah.MSM[[t]]$cum[maxrow[t],m+1]
    }
    ah.stepfun[[m]] = stepfun(time, ah.cum)
    ah.cum = NULL
    ah.cum0 = c(ah.cum0, rep(0,maxrow[m-1]-1))
  }
  
  ah.stepfun[[Nmod]] = stepfun(time, c(ah.cum0, ah.MSM[[Nmod-1]]$cum[-1,Nmod+1]))
  
  return(ah.stepfun)
}

# Estimated cumulative coefficients from the Aalen-MSM at times t.hor
cum.coef.aalen <- function(ah.MSM, t.hor){
  
  ah.stepfun = ah.step.functions(ah.MSM)
  
  cum.coefs = NULL
  for(k in 1:length(ah.stepfun)){
    cum.coefs[[k]] = ah.stepfun[[k]](t.hor)
  }
  
  return(list('time'=t.hor, 'cum_coefs'=cum.coefs))
}

# Extract cumulative coefs at t=1,...,K
extract.cum.coef <- function(cum.coef.list, K){
  
  cum.coefs = cum.coef.list$cum_coefs
  t.hor = cum.coef.list$time
  
  pos.t = which(t.hor %in% seq(1,K))
  Nt = length(pos.t)
  sel.cum.coefs = matrix(nrow=K+1, ncol=Nt)
  colnames(sel.cum.coefs) = paste0('t',seq(1,K))
  rownames(sel.cum.coefs) = c('C0',paste0('CA',0:(K-1)))
  sel.cum.coefs[1,] = cum.coefs[[1]][pos.t]
  for(i in 1:K){
    sel.cum.coefs[i+1,i:Nt]  = cum.coefs[[i+1]][pos.t[i:Nt]]
  }
  return(sel.cum.coefs)
}

# Compute marginal survival probabilities at times t.hor in the 'always treated' (surv1) 
# and in the 'never treated' (surv0)
marginal.surv.probII <- function(ah.MSM, t.hor){
  
  ah.stepfun = ah.step.functions(ah.MSM)
  eta1 = 0
  for(k in 1:length(ah.stepfun)){
    eta1 = eta1 + ah.stepfun[[k]](t.hor)
  }
  surv1 = exp(-(eta1))
  surv0 = exp(-(ah.stepfun[[1]](t.hor)))
  
  surv.df = data.frame('time'=t.hor, 'surv0'=surv0, 'surv1'=surv1)
  
  return(surv.df)
}


##############################################################################
# MC functions
mc.sim.algII <- function(B, pi.compliance, tau.rule, n.size, 
                         K = 5, alpha_cond = c(0.7, -0.2, 0.05, 0.05), 
                         theta_cond = c(-2, 0.5, 1),
                         trunc = TRUE, trunc.percentiles = c(1,99), t.hor = NULL){
  
  if(is.null(t.hor)){ t.hor=seq(0,K,0.01) }
  
  if(trunc){
    setting = data.frame('WT' = paste0(trunc.percentiles[1],'-',trunc.percentiles[2]),
                         'pi' = pi.compliance, 'tau' = tau.rule, 'n' = n.size)
  }else{
    setting = data.frame('WT' = 'NoWT', 'pi' = pi.compliance, 'tau' = tau.rule, 'n' = n.size)
  }
  settingK = setting[rep(seq_len(nrow(setting)), each = K), ]
  settingHOR = setting[rep(seq_len(nrow(setting)), each = length(t.hor)), ]
  
  est.cum.coefs = NULL
  surv.est = NULL
  for(b in 1:B){
    tmp = NULL
    tmp_surv = NULL
    df = sim.algorithmII(pi.prop = pi.compliance, tau = tau.rule, n = n.size, 
                         K, alphas = alpha_cond, thetas = theta_cond)
    df.sw = get.std.weightsII(df, trunc, percentiles=trunc.percentiles) 
    MSMmodel = aalen.MSM(df.sw, K)
    # Cumulative coefficients
    cum.coefs = cum.coef.aalen(MSMmodel, t.hor)
    M = extract.cum.coef(cum.coefs, K)
    tmp = data.frame('rep_b' = rep(b,K), settingK, 'time' = c(1:K), t(M))
    est.cum.coefs = rbind.data.frame(est.cum.coefs, tmp)
    # Survival curves
    surv.df = marginal.surv.probII(MSMmodel, t.hor)
    tmp.surv = data.frame('rep_b' = rep(b,length(t.hor)), settingHOR, surv.df)
    surv.est = rbind.data.frame(surv.est, tmp.surv)
  }
  rownames(est.cum.coefs) = 1:(B*K)
  rownames(surv.est) = 1:(B*length(t.hor))
  
  return(list('cum_coefs' = data.table(est.cum.coefs),
              'survivals' = data.table(surv.est)) )
}

