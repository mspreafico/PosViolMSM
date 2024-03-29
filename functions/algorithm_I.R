#######################################
#     Algorithm I - Section 3.2.2     #
#######################################
library(data.table)

# Define inverse logit (expit) function
expit <- function(x) return(exp(x)/(1 + exp(x)))

# Algorithm I
sim.algorithmI <- function(pi.prop, tau, n, 
                      K = 40, kappa = 5, gam = c(-3,0.05,-1.5,0.1), 
                      theta = c(-0.405,0.0205,-0.00405)){
  sim_df = data.table(
    id = seq(1:n),
    visit = rep(0,n),
    A_1 = rep(0,n),
    U0 = runif(n),
    Y = rep(0,n),
    P = runif(n)
  )
  sim_df[, U := U0]
  sim_df[, L := qgamma(U0, shape=3, scale=154)]
  sim_df$L = sim_df$L + rnorm(n, 0, 20)
  sim_df[ L < 0, L := 0]
  # Set A
  sim_df[, pAt := expit(theta[1] + theta[3] * (L - 500)) ]
  sim_df$A = rbinom(n, size = 1, prob = sim_df$pAt)
  sim_df[, pAt := NULL]
  # Near-positivity violations by thresholding
  sim_df[ P >= pi.prop & L < tau, A := 1] 
  # Set time of starting treatment to 0 or K+1 initially
  sim_df[, Kstar := (1-A)*(K+1)]
  
  # Initial value of hazard and survival
  sim_df[, hazard := expit(gam[1] + gam[3] * A)]
  sim_df[, surv := 1 - hazard]
  sim_df[ surv <= (1 - U0), Y := 1]
  # utils for-loop
  varnames = colnames(sim_df)
  risk.set = unique(sim_df[visit==0 & Y==0]$id)
  last_kappa = 0
  
  for(t in 1:K){
    
    # Save in tmp values at k-1 and k-kappa
    tmp = sim_df[ id %in% risk.set & visit==(t-1)]
    tmp = subset(tmp, select = -c(A_1, hazard))
    tmp[, visit := t]
    rename = which(colnames(tmp) %in% c('U','L','A','surv'))
    colnames(tmp)[rename] = paste0(colnames(tmp)[rename],'_1')
    tmp$A_1_kappa = sim_df[ id %in% risk.set & visit==last_kappa ]$A_1
    tmp$A_kappa = sim_df[ id %in% risk.set & visit==last_kappa ]$A
    
    
    # U_{t,i} = min(1, max(0, U_{t-1,i} + rnorm(1,0,0.05))) 
    tmp$delta = rnorm(length(risk.set),0,0.05)
    tmp[, U := min(1, max(0, U_1 + delta)), by=id]
    
    if (t %% kappa != 0) {
      tmp[, L := L_1]
      tmp[, A := A_1]
    }else{
      tmp$eps = rnorm(length(risk.set), 100*(tmp$U - 2), 50)
      #  L_{t,i} <- max(L_{t-1,i} + 150*A{t-k,i}*(1 - A_{t-k-1,i}) + eps, 0)
      tmp[, L := max(L_1 + 150*A_kappa*(1 - A_1_kappa) + eps, 0), by = id]
      # Stochastic exposure assignment
      tmp[, pAt := expit(theta[1] + theta[2] * t + theta[3] * (L - 500)) ]
      tmp$A = rbinom(length(risk.set), size = 1, prob = tmp$pAt)
      # Deterministic exposure assignment
      tmp[ (P >= pi.prop & L < tau) | A_1==1, A := 1] 
      last_kappa = t # new check-up
    }
    
    tmp[A_1==0 & A == 1, Kstar := t]
    # Compute the hazard
    tmp[, hazard := 1/(1 + exp(-(gam[1] + ((1 - A)*t + A*Kstar)*gam[2] + 
                                   gam[3]*A + gam[4]*A*(t-Kstar))))]
    # Generate survival
    tmp[, surv := surv_1*(1-hazard)]
    # Outcome
    tmp[ surv <= (1-U0), Y := 1]
    risk.set = unique(tmp[visit==t & Y==0]$id)
    
    sim_df = data.table(rbind.data.frame(sim_df, tmp[,..varnames]))
  }
  
  # Make variables for regression
  sim_df[, d1 := (1-A) * visit + A * Kstar ]
  sim_df[, d3 := A * (visit - Kstar)]
  
  # put all variables in one dataframe
  sim_df = sim_df[, .(id, visit, Y, A, A_1, L, d1, d3, hazard, surv)]
  setorder(sim_df, cols = "id", "visit")
  
  # return dataframe
  return(sim_df) 
  
}

