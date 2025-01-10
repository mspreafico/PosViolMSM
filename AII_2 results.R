#####################################################
#       Simulation study using Algorithm II         #
#---------------------------------------------------#
#                      Results                      #
# (see Section 5.2.2 and Supplementary Material S2) #
#####################################################

rm(list=ls())
source("functions/eval_measuresII.R")

## Cumulative regression coefficients 
##------------------------------------

# Retrieved true values from Keogh et al. (2021), Table 1
load('results/algorithmII/true_cum_coefs.Rdata')
Ccoefs_true

# Load all simulation results
all_coef_est = NULL
load('results/algorithmII/sim_coefs_n50.Rdata')
all_coef_est = rbind(all_coef_est, cum_coef_est)
load('results/algorithmII/sim_coefs_n100.Rdata')
all_coef_est = rbind(all_coef_est, cum_coef_est)
load('results/algorithmII/sim_coefs_n250.Rdata')
all_coef_est = rbind(all_coef_est, cum_coef_est)
load('results/algorithmII/sim_coefs_n500.Rdata')
all_coef_est = rbind(all_coef_est, cum_coef_est)
load('results/algorithmII/sim_coefs_n1000.Rdata')
all_coef_est = rbind(all_coef_est, cum_coef_est)
table(all_coef_est$n)

# Evaluating performance
C0 = eval.resultsII(data=all_coef_est, coef.name='C0', true.df = Ccoefs_true)
CA0 = eval.resultsII(data=all_coef_est, coef.name='CA0', true.df = Ccoefs_true)
CA1 = eval.resultsII(data=all_coef_est, coef.name='CA1', true.df = Ccoefs_true)
CA2 = eval.resultsII(data=all_coef_est, coef.name='CA2', true.df = Ccoefs_true)
CA3 = eval.resultsII(data=all_coef_est, coef.name='CA3', true.df = Ccoefs_true)
CA4 = eval.resultsII(data=all_coef_est, coef.name='CA4', true.df = Ccoefs_true)

performances = list('C0' = C0, 'CA0'= CA0, 'CA1'= CA1,
                    'CA2'= CA2, 'CA3'= CA3, 'CA4'= CA4)
save(performances, file='results/algorithmII/coefs_perfs.Rdata')


## Marginal survival probabilities
##----------------------------------------
# Retrieved true values from Keogh et al. (2021), Table 2
load('results/algorithmII/true_survivals.Rdata')
true_surv

# Load all simulation survivals
all_surv_est = NULL
load('results/algorithmII/survival_est_n50.Rdata')
all_surv_est = rbind(all_surv_est, surv_est[time %in% c(0:5)])
load('results/algorithmII/survival_est_n100.Rdata')
all_surv_est = rbind(all_surv_est, surv_est[time %in% c(0:5)])
load('results/algorithmII/survival_est_n250.Rdata')
all_surv_est = rbind(all_surv_est, surv_est[time %in% c(0:5)])
load('results/algorithmII/survival_est_n500.Rdata')
all_surv_est = rbind(all_surv_est, surv_est[time %in% c(0:5)])
load('results/algorithmII/survival_est_n1000.Rdata')
all_surv_est = rbind(all_surv_est, surv_est[time %in% c(0:5)])
table(all_surv_est$n)

# Compute mean survival probabilities for never treated and always treated 
# at time t=0,1,2,3,4,5 with relative performance
load('results/algorithmII/true_survivals.Rdata')
surv_perfs = eval.marginal.surv.probII(data.surv = all_surv_est, 
                                       true.surv = true_surv)
save(surv_perfs, file='results/algorithmII/surv_perfs.Rdata')


