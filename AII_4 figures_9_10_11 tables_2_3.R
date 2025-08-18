##################################################################
#              Simulation study using Algorithm II              #
#----------------------------------------------------------------#
# Focused examination of weight truncation in selected scenarios #
#                      (see Section 5.2.3)                       #
##################################################################
source("functions/specific_plots_simII.R")

# Parameters of the three specific scenarios
nn = 500
pp = 0.05
tt = 1
nowt = 'NoWT'
wt = '1-99'
wt5 = '5-95'


## IPTW Weights
##------------------------------------
load(paste0('results/algorithmII/ipw_summary_n',nn,'.Rdata'))
df_ipw = data.table(ipw_weights)
df_ipw = df_ipw[pi==pp & n==nn & tau==tt & WT %in% c(nowt,wt,wt5)]
df_ipw$WT = as.factor(df_ipw$WT)
df_ipw$WT <- relevel(df_ipw$WT, ref = "NoWT")

df_ipw[, log_mean_sw := log(mean_sw)]
df_ipw[, log_max_sw := log(max_sw)]
df_ipw[, log_min_sw := log(min_sw)]

p_mean = plot.ipwII.wt(df_ipw, y.var='log_mean_sw', ylim = c(-2,15.5), #y_range,
                       title='Log-transformed Mean IPTW values over time',
                       ylab=expression(paste('Logarithm of ', mean[i]~hat(sw)[i]^b~(t))) )

p_max = plot.ipwII.wt(df_ipw, y.var='log_max_sw', ylim = c(-2,15.5), #y_range,
                      title='Log-transformed Max IPTW values over time',
                      ylab=expression(paste('Logarithm of ', max[i]~hat(sw)[i]^b~(t))) )
p_max = p_max + geom_hline(yintercept = 3, color = "red", linetype = "dashed") 

p_min = plot.ipwII.wt(df_ipw, y.var='log_min_sw', ylim = c(-7.5,2.5), #y_range,
                      title='Log-transformed Min IPTW values over time',
                      ylab=expression(paste('Logarithm of ', min[i]~hat(sw)[i]^b~(t))) )
p_min = p_min + geom_hline(yintercept = -5, color = "red", linetype = "dashed")

### FIGURE 9
dev.new()
ggarrange(p_mean,p_max,p_min,
          ncol=3, nrow=1, common.legend = TRUE, legend = 'bottom', align='hv')



## Cumulative coefficients 
##------------------------------------
error = T
load("results/algorithmII/true_cum_coefs.Rdata")
load(paste0('results/algorithmII/sim_coefs_n',nn,'.Rdata'))
df = data.table(cum_coef_est)
df = df[pi==pp & n==nn & tau==tt & WT %in% c(nowt,wt,wt5)]

long.df <- melt(df[,.(rep_b,WT,time,C0,CA0,CA1,CA2,CA3,CA4)], 
                id.vars = c("rep_b","WT","time"), variable.name = "Ccoef", value.name = 'Value')
long.df$WT = as.factor(long.df$WT)
long.df$WT <- relevel(long.df$WT, ref = "NoWT")
if(error){
  long.df = merge(long.df, Ccoefs_true, by=c('Ccoef','time'))
  long.df[, Value := Value - true]
}

pall = plot.coefII.wt(long.df, size.value = 0.4)
dev.new()
pall

# Performances 
#--------------
load('results/algorithmII/coefs_perfs.Rdata')

t0 = performances$C0$eval[pi==pp & n==nn & tau==tt & WT %in% c(nowt,wt,wt5)]
t0 = merge(t0, Ccoefs_true[Ccoef=='C0'], by = 'time')
tA0 = performances$CA0$eval[pi==pp & n==nn & tau==tt & WT %in% c(nowt,wt,wt5)]
tA0 = merge(tA0, Ccoefs_true[Ccoef=='CA0'], by = 'time')
tA1 = performances$CA1$eval[pi==pp & n==nn & tau==tt & WT %in% c(nowt,wt,wt5)]
tA1 = merge(tA1, Ccoefs_true[Ccoef=='CA1'], by = 'time')
tA2 = performances$CA2$eval[pi==pp & n==nn & tau==tt & WT %in% c(nowt,wt,wt5)]
tA2 = merge(tA2, Ccoefs_true[Ccoef=='CA2'], by = 'time')
tA3 = performances$CA3$eval[pi==pp & n==nn & tau==tt & WT %in% c(nowt,wt,wt5)]
tA3 = merge(tA3, Ccoefs_true[Ccoef=='CA3'], by = 'time')
tA4 = performances$CA4$eval[pi==pp & n==nn & tau==tt & WT %in% c(nowt,wt,wt5)]
tA4 = merge(tA4, Ccoefs_true[Ccoef=='CA4'], by = 'time')

### TABLE 2
table.2 = rbind(t0,tA0)[,.(Ccoef,time,true,WT,meanG,Bias,empSE,RMSE)]
table.2

### TABLE 3
table.3 = rbind(tA1,tA2,tA3,tA4)[,.(Ccoef,time,true,WT,meanG, Bias,empSE,RMSE)]
table.3



## Marginal survival curves
##------------------------------------
load(paste0('results/algorithmII/survival_est_n',nn,'.Rdata'))
#load('results/algorithmII/surv_perfs.Rdata')
load('results/algorithmII/surv_perfs_avg_allt.Rdata')
load('results/algorithmII/true_survivals.Rdata')

# NO weight truncation
colore = '#CC0066'
p0 = plot.mc.survII(df_surv_est = surv_est[pi==pp & n==nn & tau==tt & WT==nowt], 
                    df_surv_perf = surv_perfs$eval[pi==pp & n==nn & tau==tt & WT==nowt], 
                    df_true_surv = true_surv,
                    title = 'Never treated | No WT',
                    regimen = 'never', tau_color = colore)
p1 = plot.mc.survII(df_surv_est = surv_est[pi==pp & n==nn & tau==tt & WT==nowt], 
                    df_surv_perf = surv_perfs$eval[pi==pp & n==nn & tau==tt & WT==nowt], 
                    df_true_surv = true_surv,
                    title = 'Always treated | No WT',
                    regimen = 'always', tau_color = colore)
p1 = p1 + coord_cartesian(ylim = c(0, 1))

# Weight truncation 1-99
colore = '#FFCC33'
p0.wt1 = plot.mc.survII(df_surv_est = surv_est[pi==pp & n==nn & tau==tt & WT==wt], 
                        df_surv_perf = surv_perfs$eval[pi==pp & n==nn & tau==tt & WT==wt], 
                        df_true_surv = true_surv,
                        title = 'Never treated | WT: 1-99',
                        regimen = 'never', tau_color = colore)
p1.wt1 = plot.mc.survII(df_surv_est = surv_est[pi==pp & n==nn & tau==tt & WT==wt], 
                        df_surv_perf = surv_perfs$eval[pi==pp & n==nn & tau==tt & WT==wt], 
                        df_true_surv = true_surv,
                        title = 'Always treated | WT: 1-99',
                        regimen = 'always', tau_color = colore)
p1.wt1 = p1.wt1 + coord_cartesian(ylim = c(0, 1))

# Weight truncation 5-95
colore = '#619CFF'
p0.wt5 = plot.mc.survII(df_surv_est = surv_est[pi==pp & n==nn & tau==tt & WT==wt5], 
                        df_surv_perf = surv_perfs$eval[pi==pp & n==nn & tau==tt & WT==wt5], 
                        df_true_surv = true_surv,
                        title = 'Never treated | WT: 5-95',
                        regimen = 'never', tau_color = colore)
p1.wt5 = plot.mc.survII(df_surv_est = surv_est[pi==pp & n==nn & tau==tt & WT==wt5], 
                        df_surv_perf = surv_perfs$eval[pi==pp & n==nn & tau==tt & WT==wt5], 
                        df_true_surv = true_surv,
                        title = 'Always treated | WT: 5-95',
                        regimen = 'always', tau_color = colore)
p1.wt5 = p1.wt5 + coord_cartesian(ylim = c(0, 1))

dev.new()
ggarrange(p0,p0.wt1,p0.wt5,p1,p1.wt1,p1.wt5,
          ncol=3, nrow=2, common.legend = TRUE, legend = 'none', align='hv')

