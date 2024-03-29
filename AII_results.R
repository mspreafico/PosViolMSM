###############################################
#     Simulation study using Algorithm II     #
#---------------------------------------------#
#                  Results                    #
#      (see Sections 4.2.2 and 4.2.3)         #
###############################################

###############
# True values #
###############
# Retrieved from Keogh et al. (2021), Table 1
load('results/algorithmII/true_cum_coefs.Rdata')
Ccoefs_true
# Retrieved from Keogh et al. (2021), Table 2
load('results/algorithmII/true_survivals.Rdata')
true_surv


##################################################################################
##################
# Scenarios II.1 #
##################
rm(list=ls())
source("functions/eval_measuresII.R")
load('results/algorithmII/true_cum_coefs.Rdata')
load('results/algorithmII/s1_coefs.Rdata')

## Cumulative regression coefficients
##------------------------------------
# Evaluating performance
C0 = eval.resultsII(data=cum_coef_est, coef.name='C0', true.df = Ccoefs_true)
CA0 = eval.resultsII(data=cum_coef_est, coef.name='CA0', true.df = Ccoefs_true)
CA1 = eval.resultsII(data=cum_coef_est, coef.name='CA1', true.df = Ccoefs_true)
CA2 = eval.resultsII(data=cum_coef_est, coef.name='CA2', true.df = Ccoefs_true)
CA3 = eval.resultsII(data=cum_coef_est, coef.name='CA3', true.df = Ccoefs_true)
CA4 = eval.resultsII(data=cum_coef_est, coef.name='CA4', true.df = Ccoefs_true)

performances = list('C0' = C0, 'CA0'= CA0, 'CA1'= CA1,
                    'CA2'= CA2, 'CA3'= CA3, 'CA4'= CA4)
save(performances, file='results/algorithmII/s1_coefs_perfs.Rdata')

# Values shown in Figure 6  - Scenarios II.1
performances$CA0$eval[,.(n,WT,pi,tau,time,Bias)]
# Values shown in Figure 7  - Scenarios II.1
performances$CA0$eval[,.(n,WT,pi,tau,time,empSE)]


## Counterfactual survival probabilities
##----------------------------------------
# Compute mean survival probabilities for never treated and always treated 
# at time t=0,1,2,3,4,5 with relative performance
load('results/algorithmII/true_survivals.Rdata')
load('results/algorithmII/s1_survivals.Rdata')
surv_perfs = eval.marginal.surv.probII(data.surv = surv_est[time %in% c(0:5)], 
                                       true.surv = true_surv)
save(surv_perfs, file='results/algorithmII/s1_surv_perfs.Rdata')

# Values shown in Figure 8 - Scenarios II.1
surv_perfs$eval[,.(n,WT,pi,tau,time,avg_surv0,avg_surv1)]
# True counterfactual survival probabilities (green curves)
true_surv


##################################################################################
##################
# Scenarios II.2 #
##################
rm(list=ls())
source("functions/eval_measuresII.R")
load('results/algorithmII/true_cum_coefs.Rdata')
load('results/algorithmII/s2_coefs.Rdata')

## Cumulative regression coefficients
##------------------------------------
# Evaluating performance
C0 = eval.resultsII(data=cum_coef_est, coef.name='C0', true.df = Ccoefs_true)
CA0 = eval.resultsII(data=cum_coef_est, coef.name='CA0', true.df = Ccoefs_true)
CA1 = eval.resultsII(data=cum_coef_est, coef.name='CA1', true.df = Ccoefs_true)
CA2 = eval.resultsII(data=cum_coef_est, coef.name='CA2', true.df = Ccoefs_true)
CA3 = eval.resultsII(data=cum_coef_est, coef.name='CA3', true.df = Ccoefs_true)
CA4 = eval.resultsII(data=cum_coef_est, coef.name='CA4', true.df = Ccoefs_true)

performances = list('C0' = C0, 'CA0'= CA0, 'CA1'= CA1,
                    'CA2'= CA2, 'CA3'= CA3, 'CA4'= CA4)
save(performances, file='results/algorithmII/s2_coefs_perfs.Rdata')

# Values shown in Figure 6  - Scenarios II.2
performances$CA0$eval[,.(n,WT,pi,tau,time,Bias)]
# Values shown in Figure 7  - Scenarios II.2
performances$CA0$eval[,.(n,WT,pi,tau,time,empSE)]

## Counterfactual survival probabilities
##----------------------------------------
# Compute mean survival probabilities for never treated and always treated 
# at time t=0,1,2,3,4,5 with relative performanceload('results/algorithmII/true_survivals.Rdata')
load('results/algorithmII/s2_survivals.Rdata')
surv_perfs = eval.marginal.surv.probII(data.surv = surv_est[time %in% c(0:5)], 
                                       true.surv = true_surv)
save(surv_perfs, file='results/algorithmII/s2_surv_perfs.Rdata')

# Values shown in Figure 8 - Scenarios II.2
surv_perfs$eval[,.(n,WT,pi,tau,time,avg_surv0,avg_surv1)]
# True counterfactual survival probabilities (green curves)
true_surv


##################################################################################
##################
# Scenarios II.3 #
##################
rm(list=setdiff(ls(), c('time_points')))
source("functions/eval_measuresII.R")
load('results/algorithmII/true_cum_coefs.Rdata')
load('results/algorithmII/s3_coefs.Rdata')

## Cumulative regression coefficients
##------------------------------------
# Evaluating performance
C0 = eval.resultsII(data=cum_coef_est, coef.name='C0', true.df = Ccoefs_true)
CA0 = eval.resultsII(data=cum_coef_est, coef.name='CA0', true.df = Ccoefs_true)
CA1 = eval.resultsII(data=cum_coef_est, coef.name='CA1', true.df = Ccoefs_true)
CA2 = eval.resultsII(data=cum_coef_est, coef.name='CA2', true.df = Ccoefs_true)
CA3 = eval.resultsII(data=cum_coef_est, coef.name='CA3', true.df = Ccoefs_true)
CA4 = eval.resultsII(data=cum_coef_est, coef.name='CA4', true.df = Ccoefs_true)

performances = list('C0' = C0, 'CA0'= CA0, 'CA1'= CA1,
                    'CA2'= CA2, 'CA3'= CA3, 'CA4'= CA4)
save(performances, file='results/algorithmII/s3_coefs_perfs.Rdata')

# Values shown in Figure 6  - Scenarios II.3
performances$CA0$eval[,.(n,WT,pi,tau,time,Bias)]
# Values shown in Figure 7  - Scenarios II.3
performances$CA0$eval[,.(n,WT,pi,tau,time,empSE)]


## Counterfactual survival probabilities
##----------------------------------------
# Compute mean survival probabilities for never treated and always treated 
# at time t=0,1,2,3,4,5 with relative performance
load('results/algorithmII/true_survivals.Rdata')
load('results/algorithmII/s3_survivals.Rdata')
surv_perfs = eval.marginal.surv.probII(data.surv = surv_est[time %in% c(0:5)], 
                                       true.surv = true_surv)
save(surv_perfs, file='results/algorithmII/s3_surv_perfs.Rdata')

# Values shown in Figure 8 - Scenarios II.3
surv_perfs$eval[,.(n,WT,pi,tau,time,avg_surv0,avg_surv1)]
# True counterfactual survival probabilities (green curves)
true_surv

