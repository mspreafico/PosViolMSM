################################################
#  Functions to estimate the logit-MSMs using  #
# longitudinal data simulated from Algorithm I #
################################################
source('functions/algorithm_I.R')
source('functions/iptw_I.R')

# MC functions
mc.sim.algI <- function(B, pi.compliance, tau.rule, n.size, 
                        K = 40, kappa = 5, gam_true = c(-3,0.05,-1.5,0.1), 
                        theta_cond = c(-0.405,0.0205,-0.00405),
                        trunc = TRUE, trunc.percentiles = c(1,99)){
  
  gammas = NULL
  if(trunc){
    setting = data.frame('WT' = paste0(trunc.percentiles[1],'-',trunc.percentiles[2]),
                       'pi' = pi.compliance, 'tau' = tau.rule, 'n' = n.size)
  }else{
    setting = data.frame('WT' = 'NoWT', 'pi' = pi.compliance, 'tau' = tau.rule, 'n' = n.size)
  }
  
  gamma = NULL
  ipw.weights = NULL
  for(b in 1:B){
    tmp = NULL
    tmp.ipw = NULL
    df = sim.algorithmI(pi.prop = pi.compliance, tau = tau.rule, n = n.size, 
                        K, kappa, gam = gam_true, theta = theta_cond)
    # Weights
    df_sw <- get.std.weightsI(df, kappa, trunc, percentiles=trunc.percentiles) 
    df_sw_kappa = df_sw[df_sw$visit %% kappa == 0, which(colnames(df_sw) %in% c('id','visit','sw'))]
    tmp.ipw = eval.ipwI(data.table(df_sw_kappa))
    tmp.ipw = data.frame('rep_b' = rep(b, dim(tmp.ipw)[1]), 
                         setting[rep(seq_len(nrow(setting)), each = dim(tmp.ipw)[1]), ], tmp.ipw)
    ipw.weights = rbind.data.frame(ipw.weights, tmp.ipw)
    # Coefficients
    est_coefs <- coef(glm(Y ~ d1 + A + d3, family=quasibinomial, data=df_sw, weights=sw))
    tmp = data.frame('rep_b' = b, setting, 'gamma0'= est_coefs[1],
                     'gammaA1'= est_coefs[2],'gammaA2'= est_coefs[3],'gammaA3'= est_coefs[4])
    gammas = rbind.data.frame(gammas, tmp)
  }
  
  rownames(gammas) = 1:B
  rownames(ipw.weights) = 1:dim(ipw.weights)[1]
  
  return(list('coef_est' = gammas,
              'weights' = ipw.weights))
}


# Repeat simulations for non converged models
repeat.nonconverged.fit <- function(data, lim=10^6, K=40, kappa=5, 
                                    gam_true = c(-3,0.05,-1.5,0.1), 
                                    theta_cond = c(-0.405,0.0205,-0.00405)){
  
  data = data.table(data)
  issues = data[abs(gamma0)>lim | abs(gammaA2)>lim | abs(gammaA3)>lim | abs(gammaA1)>lim]
  Npb = dim(issues)[1]
  
  while(Npb>0){
    print(paste0('Re-fitting ', Npb,' models that did not converge'))
    data = data[!issues, on=.(rep_b,WT,pi,tau,n)]
    for(i in 1:Npb){
      df = sim.algorithmI(pi.prop = issues[i,]$pi, tau = issues[i,]$tau, n = issues[i,]$n,
                          K, kappa, gam = gam_true, theta = theta_cond)
      if(issues[i,]$WT=='NoWT'){
        df_sw <- get.std.weightsI(df, kappa, trunc=FALSE) 
      }else{
        trunc.percentiles = as.numeric(str_split(issues[i,]$WT, "-", n = 2)[[1]])/100
        df_sw <- get.std.weightsI(df, kappa, trunc=TRUE, percentiles=trunc.percentiles)
      }
      est_coefs <- coef(glm(Y ~ d1 + A + d3, family=quasibinomial, data=df_sw, weights=sw))
      tmp = data.frame(issues[i,.(rep_b,WT,pi,tau,n)], 'gamma0'= est_coefs[1],
                       'gammaA1'= est_coefs[2],'gammaA2'= est_coefs[3],'gammaA3'= est_coefs[4])
      data = rbind.data.frame(data, tmp)
    }
    issues = data[abs(gamma0)>lim | abs(gammaA2)>lim | abs(gammaA3)>lim | abs(gammaA1)>lim]
    Npb = dim(issues)[1]
  }
  
  print('All models converged')
  
  return(data)
  
}
