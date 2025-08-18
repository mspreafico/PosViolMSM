##################################################################
#              Simulation study using Algorithm I                #
#----------------------------------------------------------------#
# Focused examination of weight truncation in selected scenarios #
#                      (see Section 4.2.3)                       #
##################################################################
source("functions/specific_plots_simI.R")

# Parameters of the three specific scenarios
nn = 500
pp = 0.1
tt = 400
nowt = 'NoWT'
wt = '1-99'
wt5 = '5-95'


## IPTW Weights
##------------------------------------
load(paste0('results/algorithmI/ipw_summary_n',nn,'.Rdata'))
df_ipw = data.table(ipw_weights)
df_ipw = df_ipw[pi==pp & n==nn & tau==tt & WT %in% c(nowt,wt,wt5)]
df_ipw$WT = as.factor(df_ipw$WT)
df_ipw$WT <- relevel(df_ipw$WT, ref = "NoWT")

df_ipw[, log_mean_sw := log(mean_sw)]
df_ipw[, log_max_sw := log(max_sw)]
df_ipw[, log_min_sw := log(min_sw)]


p_mean = plot.ipwI.wt(df_ipw, y.var='log_mean_sw', ylim = c(-3.45,8.2),
                      title='Log-transformed Mean IPTW values over time',
                      ylab=expression(paste('Logarithm of ', mean[i]~hat(sw)[i]^b~(t))) )

p_max = plot.ipwI.wt(df_ipw, y.var='log_max_sw', ylim = c(-3.45,8.2),
                     title='Log-transformed Max IPTW values over time',
                     ylab=expression(paste('Logarithm of ', max[i]~hat(sw)[i]^b~(t))) )
p_max = p_max + geom_hline(yintercept = 3, color = "red", linetype = "dashed") 

p_min = plot.ipwI.wt(df_ipw, y.var='log_min_sw', ylim = c(-6,0),
                     title='Log-transformed Min IPTW values over time',
                     ylab=expression(paste('Logarithm of ', min[i]~hat(sw)[i]^b~(t))) )
p_min = p_min + geom_hline(yintercept = -5, color = "red", linetype = "dashed")


#### FIGURE 4
dev.new()
ggarrange(p_mean,p_max,p_min,
          ncol=3, nrow=1, common.legend = TRUE, legend = 'bottom', align='hv')



## Regression coefficients 
##------------------------------------
error = T
load(paste0('results/algorithmI/sim_coefs_n',nn,'.Rdata'))
df = data.table(coef_est)
df = df[pi==pp & n==nn & tau==tt & WT %in% c(nowt,wt,wt5)]

if(error){
  gammas_true = c(-3, 0.05, -1.5, 0.1)
  df[, gamma0 := gamma0-gammas_true[1]]
  df[, gammaA1 := gammaA1-gammas_true[2]]
  df[, gammaA2 := gammaA2-gammas_true[3]]
  df[, gammaA3 := gammaA3-gammas_true[4]]
}

long.df <- melt(df[,.(rep_b,WT,gamma0,gammaA1,gammaA2,gammaA3)], 
                id.vars = c("rep_b","WT"), variable.name = "Coefficient", value.name = 'Value')
long.df$WT = as.factor(long.df$WT)
long.df$WT <- relevel(long.df$WT, ref = "NoWT")

p02 = plot.coefI.wt(long.df[Coefficient %in% c('gamma0','gammaA2')])
p13 = plot.coefI.wt(long.df[Coefficient %in% c('gammaA1','gammaA3')])

### FIGURE 5
dev.new()
ggarrange(p02,p13,
          ncol=2, nrow=1, common.legend = TRUE, legend = 'bottom', align='hv')


# Performances 
#--------------
load('results/algorithmI/coefs_perfs.Rdata')

t0 = performances$gamma0$eval[pi==pp & n==nn & tau==tt & WT %in% c(nowt,wt,wt5)]
t0$Coef = 'gamma0'
t0$true = -3
tA1 = performances$gammaA1$eval[pi==pp & n==nn & tau==tt & WT %in% c(nowt,wt,wt5)]
tA1$Coef = 'gammaA1'
tA1$true = 0.05
tA2 = performances$gammaA2$eval[pi==pp & n==nn & tau==tt & WT %in% c(nowt,wt,wt5)]
tA2$Coef = 'gammaA2'
tA2$true = -1.5
tA3 = performances$gammaA3$eval[pi==pp & n==nn & tau==tt & WT %in% c(nowt,wt,wt5)]
tA3$Coef = 'gammaA3'
tA3$true = 0.1

### TABLE 1
table.1 = rbind(t0,tA1,tA2,tA3)[,.(Coef,true,WT,meanG,Bias,empSE,RMSE)]
table.1


## Marginal survival curves
##------------------------------------
load('results/algorithmI/survival_est.Rdata')
load('results/algorithmI/surv_perfs.Rdata')
load('results/algorithmI/true_survivals.Rdata')

# NO weight truncation
colore = '#CC0066'
p0 = plot.mc.survI(df_surv_est = surv_est[pi==pp & n==nn & tau==tt & WT==nowt], 
                   df_surv_perf = surv_perfs$eval[pi==pp & n==nn & tau==tt & WT==nowt], 
                   df_true_surv = true_surv,
                   title = 'Never treated | No WT',
                   regimen = 'never', tau_color = colore)
p1 = plot.mc.survI(df_surv_est = surv_est[pi==pp & n==nn & tau==tt & WT==nowt], 
                   df_surv_perf = surv_perfs$eval[pi==pp & n==nn & tau==tt & WT==nowt], 
                   df_true_surv = true_surv,
                   title = 'Always treated | No WT',
                   regimen = 'always', tau_color = colore)

# Weight truncation 1-99
colore = '#FFCC33'
p0.wt1 = plot.mc.survI(df_surv_est = surv_est[pi==pp & n==nn & tau==tt & WT==wt], 
                       df_surv_perf = surv_perfs$eval[pi==pp & n==nn & tau==tt & WT==wt], 
                       df_true_surv = true_surv,
                       title = 'Never treated | WT: 1-99',
                       regimen = 'never', tau_color = colore)
p1.wt1 = plot.mc.survI(df_surv_est = surv_est[pi==pp & n==nn & tau==tt & WT==wt], 
                       df_surv_perf = surv_perfs$eval[pi==pp & n==nn & tau==tt & WT==wt], 
                       df_true_surv = true_surv,
                       title = 'Always treated | WT: 1-99',
                       regimen = 'always', tau_color = colore)

# Weight truncation 5-95
colore = '#619CFF'
p0.wt5 = plot.mc.survI(df_surv_est = surv_est[pi==pp & n==nn & tau==tt & WT==wt5], 
                       df_surv_perf = surv_perfs$eval[pi==pp & n==nn & tau==tt & WT==wt5], 
                       df_true_surv = true_surv,
                       title = 'Never treated | WT: 5-95',
                       regimen = 'never', tau_color = colore)
p1.wt5 = plot.mc.survI(df_surv_est = surv_est[pi==pp & n==nn & tau==tt & WT==wt5], 
                       df_surv_perf = surv_perfs$eval[pi==pp & n==nn & tau==tt & WT==wt5], 
                       df_true_surv = true_surv,
                       title = 'Always treated | WT: 5-95',
                       regimen = 'always', tau_color = colore)

### FIGURE 6
dev.new()
ggarrange(p0,p0.wt1,p0.wt5,p1,p1.wt1,p1.wt5,
          ncol=3, nrow=2, common.legend = TRUE, legend = 'none', align='hv')



