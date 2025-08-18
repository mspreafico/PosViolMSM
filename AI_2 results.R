#####################################################
#       Simulation study using Algorithm I          #
#---------------------------------------------------#
#                      Results                      #
#   (see Section 4 and Supplementary Material S1)   #
#####################################################
source("functions/eval_measuresI.R")

# Load all simulation results
all_coef_est = NULL
load('results/algorithmI/sim_coefs_n50.Rdata')
all_coef_est = rbind(all_coef_est, coef_est)
load('results/algorithmI/sim_coefs_n100.Rdata')
all_coef_est = rbind(all_coef_est, coef_est)
load('results/algorithmI/sim_coefs_n250.Rdata')
all_coef_est = rbind(all_coef_est, coef_est)
load('results/algorithmI/sim_coefs_n500.Rdata')
all_coef_est = rbind(all_coef_est, coef_est)
load('results/algorithmI/sim_coefs_n1000.Rdata')
all_coef_est = rbind(all_coef_est, coef_est)
table(all_coef_est$n)


###############
# True values #
###############
# True values of MSM coefficients in Equation (11)
gammas_true = c(-3, 0.05, -1.5, 0.1)
# Compute true survival probabilities for never treated and always treated
time_points = seq(0,40,1)
true_surv = true.survival.probI(times = time_points, true_gam = gammas_true)
#save(true_surv, file='results/algorithmI/true_survivals.Rdata')

## Regression coefficients
##------------------------
# Evaluating performance
gamma0 = eval.resultsI(data=all_coef_est, coef.name='gamma0', true = gammas_true[1])
gammaA1 = eval.resultsI(data=all_coef_est, coef.name='gammaA1', true = gammas_true[2])
gammaA2 = eval.resultsI(data=all_coef_est, coef.name='gammaA2', true = gammas_true[3])
gammaA3 = eval.resultsI(data=all_coef_est, coef.name='gammaA3', true = gammas_true[4])

performances = list('gamma0' = gamma0, 'gammaA1'= gammaA1,
                    'gammaA2'= gammaA2, 'gammaA3'= gammaA3)
save(performances, file='results/algorithmI/coefs_perfs.Rdata')


## Counterfactual survival probabilities
##----------------------------------------
# Compute never treated and always treated curves for each repetition
surv_est = marginal.surv.probI(data = all_coef_est, times = time_points)
save(surv_est, file='results/algorithmI/survival_est.Rdata')

# Compute mean survival probabilities for never treated and always treated 
# with relative performance
surv_perfs = eval.marginal.surv.probI(data.surv = surv_est, true.surv = true_surv)
save(surv_perfs, file='results/algorithmI/surv_perfs.Rdata')





