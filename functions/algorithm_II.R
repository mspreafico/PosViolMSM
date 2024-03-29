########################################
#     Algorithm II - Section 3.3.2     #
########################################
library(data.table)

# Define inverse logit (expit) function
expit <- function(x) return(exp(x)/(1 + exp(x)))

# Algorithm II
sim.algorithmII <- function(pi.prop, tau, n, K = 5, # K is the number of visits, including baseline
                      alphas = c(0.7, -0.2, 0.05, 0.05), 
                      thetas = c(-2, 0.5, 1)){
  
  # Subjects propensity
  P = runif(n)
  index.P = which(P >= pi.prop)
  
  # Individual frailty term
  U = rnorm(n,0,0.1)
  
  A = matrix(nrow=n,ncol=K)
  L = matrix(nrow=n,ncol=K)
  # Baseline: t=0
  L[,1] = rnorm(n,0+U,1)
  A[,1] = rbinom(n,1,expit(thetas[1]+thetas[2]*L[,1]))
  A[intersect(which(L[,1]>tau),index.P),1] = 1
  
  # Visits: t = 1,...,K-1
  for(k in 2:K){
    L[,k] = rnorm(n, 0.8*L[,k-1]-A[,k-1]+0.1*(k-1)+U, 1)
    A[,k] = rbinom(n,1,expit(thetas[1]+thetas[2]*L[,k]+thetas[3]*A[,k-1]))
    A[intersect(which(L[,k]>tau),index.P),k] = 1
  }
  
  T.obs=rep(NA,n)
  sum.haz.neg=0
  for(t in 1:K){
    V = runif(n,0,1)
    haz = alphas[1] + alphas[2]*A[,t] + alphas[3]*L[,t] + alphas[4]*U
    Delta = -log(V)/haz
    T.obs = ifelse(is.na(T.obs) & Delta<1 & haz>0,t-1+Delta, T.obs)
    sum.haz.neg = sum(sum.haz.neg,(haz<0),na.rm=T)
  }
  Y.obs = ifelse(is.na(T.obs),0,1)
  T.obs = ifelse(is.na(T.obs),K,T.obs)
  
  #------------------------------------------------------------------
  #Reshape into long format
  
  L.dat = as.data.frame(L)
  names(L.dat) = paste0("L.",0:(K-1))
  
  A.dat = as.data.frame(A)
  names(A.dat) = paste0("A.",0:(K-1))
  
  A0 = NULL
  dat = data.frame(id=1:n,T.obs,Y.obs,U,L.dat,A.dat)
  varying.names = c(paste0("A.",0:(K-1)),paste0("L.",0:(K-1)))
  for(t in 1:(K-1)){
    A0 = cbind(A0,rep(0,n))
    tmp.dat = as.data.frame(cbind(A0,A.dat[,1:(K-t)]))
    tmp.names = paste0("Alag",t,".",0:(K-1))
    varying.names = c(varying.names,tmp.names)
    names(tmp.dat) =  tmp.names
    dat = cbind.data.frame(dat,tmp.dat)
  }
  
  sim_df = reshape(data = dat, varying=varying.names, direction="long", idvar="id")
  # Order by id and visits
  sim_df = sim_df[order(sim_df$id,sim_df$time),]
  # Create t.stop
  sim_df$time.stop = sim_df$time+1
  # Delete observations < T.obs & fix timing
  sim_df = sim_df[sim_df$time<sim_df$T.obs,]
  sim_df$time.stop = ifelse(sim_df$time.stop>sim_df$T.obs, sim_df$T.obs, sim_df$time.stop)
  # Identify events
  sim_df$event = ifelse(sim_df$time.stop==sim_df$T.obs & sim_df$Y.obs==1,1,0)
  
  return(sim_df)
  
}
  
  