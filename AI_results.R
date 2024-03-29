###############################################
#     Simulation study using Algorithm I      #
#---------------------------------------------#
#                  Results                    #
#      (see Sections 4.1.2 and 4.1.3)         #
###############################################
source("functions/eval_measuresI.R")


###############
# True values #
###############
# True values of MSM coefficients in Equation (11)
gammas_true = c(-3, 0.05, -1.5, 0.1)
# Compute true survival probabilities for never treated and always treated
time_points = seq(0,40,1)
true_surv = true.survival.probI(times = time_points, true_gam = gammas_true)
#save(true_surv, file='results/algorithmI/true_survivals.Rdata')


##################################################################################
#################
# Scenarios I.1 #
#################
rm(list=setdiff(ls(), c('gammas_true','time_points','true_surv')))
source("functions/eval_measuresI.R")
load('results/algorithmI/s1_coefs.Rdata')

## Regression coefficients
##------------------------
# Evaluating performance
gamma0 = eval.resultsI(data=coef_est, coef.name='gamma0', true = gammas_true[1])
gammaA1 = eval.resultsI(data=coef_est, coef.name='gammaA1', true = gammas_true[2])
gammaA2 = eval.resultsI(data=coef_est, coef.name='gammaA2', true = gammas_true[3])
gammaA3 = eval.resultsI(data=coef_est, coef.name='gammaA3', true = gammas_true[4])

performances = list('gamma0' = gamma0, 'gammaA1'= gammaA1,
                    'gammaA2'= gammaA2, 'gammaA3'= gammaA3)
save(performances, file='results/algorithmI/s1_coefs_perfs.Rdata')

# Values shown in Figure 2
performances$gamma0$eval[,.(n,WT,pi,tau,Bias,empSE)]
performances$gammaA2$eval[,.(n,WT,pi,tau,Bias,empSE)]
performances$gammaA1$eval[,.(n,WT,pi,tau,Bias,empSE)]
performances$gammaA3$eval[,.(n,WT,pi,tau,Bias,empSE)]


## Counterfactual survival probabilities
##----------------------------------------
# Compute never treated and always treated curves for each repetition
surv_est = marginal.surv.probI(data = coef_est, times = time_points)
save(surv_est, file='results/algorithmI/s1_survivals.Rdata')

# Compute mean survival probabilities for never treated and always treated 
# with relative performance
surv_perfs = eval.marginal.surv.probI(data.surv = surv_est, true.surv = true_surv)
save(surv_perfs, file='results/algorithmI/s1_surv_perfs.Rdata')

# Values shown in Figure 4 - Scenarios I.1
surv_perfs$eval[,.(n,WT,pi,tau,time,avg_surv0,avg_surv1)]
# True counterfactual survival probabilities (orange curves)
true_surv


##################################################################################
#################
# Scenarios I.2 #
#################
rm(list=setdiff(ls(), c('gammas_true','time_points','true_surv')))
source("functions/eval_measuresI.R")
load('results/algorithmI/s2_coefs.Rdata')

## Regression coefficients
##------------------------
# Evaluating performance
gamma0 = eval.resultsI(data=coef_est, coef.name='gamma0', true = gammas_true[1])
gammaA1 = eval.resultsI(data=coef_est, coef.name='gammaA1', true = gammas_true[2])
gammaA2 = eval.resultsI(data=coef_est, coef.name='gammaA2', true = gammas_true[3])
gammaA3 = eval.resultsI(data=coef_est, coef.name='gammaA3', true = gammas_true[4])

performances = list('gamma0' = gamma0, 'gammaA1'= gammaA1,
                    'gammaA2'= gammaA2, 'gammaA3'= gammaA3)
save(performances, file='results/algorithmI/s2_coefs_perfs.Rdata')

# Values shown in Figure 3
performances$gamma0$eval[,.(n,WT,pi,tau,Bias,empSE)]
performances$gammaA2$eval[,.(n,WT,pi,tau,Bias,empSE)]
performances$gammaA1$eval[,.(n,WT,pi,tau,Bias,empSE)]
performances$gammaA3$eval[,.(n,WT,pi,tau,Bias,empSE)]


## Counterfactual survival probabilities
##----------------------------------------
# Compute never treated and always treated curves for each repetition
surv_est = marginal.surv.probI(data = coef_est, times = time_points)
save(surv_est, file='results/algorithmI/s2_survivals.Rdata')

# Compute mean survival probabilities for never treated and always treated 
# with relative performance
surv_perfs = eval.marginal.surv.probI(data.surv = surv_est, true.surv = true_surv)
save(surv_perfs, file='results/algorithmI/s2_surv_perfs.Rdata')

# Values shown in Figure 4 - Scenarios I.2
surv_perfs$eval[,.(n,WT,pi,tau,time,avg_surv0,avg_surv1)]
# True counterfactual survival probabilities (orange curves)
true_surv


##################################################################################
#################
# Scenarios I.3 #
#################
rm(list=setdiff(ls(), c('gammas_true','time_points','true_surv')))
source("functions/eval_measuresI.R")
load('results/algorithmI/s3_coefs_converged.Rdata')

## Regression coefficients
##------------------------
# Evaluating performance
gamma0 = eval.resultsI(data=coef_est, coef.name='gamma0', true = gammas_true[1])
gammaA1 = eval.resultsI(data=coef_est, coef.name='gammaA1', true = gammas_true[2])
gammaA2 = eval.resultsI(data=coef_est, coef.name='gammaA2', true = gammas_true[3])
gammaA3 = eval.resultsI(data=coef_est, coef.name='gammaA3', true = gammas_true[4])

performances = list('gamma0' = gamma0, 'gammaA1'= gammaA1,
                    'gammaA2'= gammaA2, 'gammaA3'= gammaA3)
save(performances, file='results/algorithmI/s3_coefs_perfs.Rdata')

# Values shown in Figure 4
performances$gamma0$eval[,.(n,WT,pi,tau,Bias,empSE)]
performances$gammaA2$eval[,.(n,WT,pi,tau,Bias,empSE)]
performances$gammaA1$eval[,.(n,WT,pi,tau,Bias,empSE)]
performances$gammaA3$eval[,.(n,WT,pi,tau,Bias,empSE)]


## Counterfactual survival probabilities
##----------------------------------------
# Compute never treated and always treated curves for each repetition
surv_est = marginal.surv.probI(data = coef_est, times = time_points)
save(surv_est, file='results/algorithmI/s3_survivals.Rdata')

# Compute mean survival probabilities for never treated and always treated 
# with relative performance
surv_perfs = eval.marginal.surv.probI(data.surv = surv_est, true.surv = true_surv)
save(surv_perfs, file='results/algorithmI/s3_surv_perfs.Rdata')

# Values shown in Figure 4 - Scenarios I.3
surv_perfs$eval[,.(n,WT,pi,tau,time,avg_surv0,avg_surv1)]
# True counterfactual survival probabilities (orange curves)
true_surv

