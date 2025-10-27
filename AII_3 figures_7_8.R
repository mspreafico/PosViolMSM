library(data.table)
library(ggplot2)
library(ggpubr)

source("functions/plots_simII.R")

#--------------------------#
# Counterfactual Survivals #
#--------------------------#
load('results/algorithmII/surv_perfs.Rdata')
#load('results/algorithmII/surv_perfs_avg_allt.Rdata')
load('results/algorithmII/true_survivals.Rdata')

n_sizes = c(50,100,250,500,1000)
pis = c(0,0.05,0.1,0.3,0.5,0.8,1)

#No WT
nowt = list()
for(nn in 1:length(n_sizes)){
  nowt[[nn]] <- list()
  for(pp in 1:length(pis)){
    current.title = substitute(paste(n == number1,', ',pi == number2),
                               list(number1 = n_sizes[nn], number2 = pis[pp]))
    nowt[[nn]][[pp]] = plot.survII(data = surv_perfs$eval[n==n_sizes[nn] & pi==pis[pp] & WT=='NoWT'],
                                  title = current.title, title.rel = 0.95)
  }
}

#### FIGURE 7
dev.new()
nn1=1 #first sample size index 1
nn2=2 #second sample size index 2
plot.n = ggarrange(nowt[[nn1]][[2]]+rremove("ylab") + coord_cartesian(ylim = c(0, 1)),
                   nowt[[nn1]][[3]]+rremove("ylab") + coord_cartesian(ylim = c(0, 1)), 
                   nowt[[nn1]][[4]]+rremove("ylab") + coord_cartesian(ylim = c(0, 1)),
                   nowt[[nn1]][[5]]+rremove("ylab") + coord_cartesian(ylim = c(0, 1)),
                   nowt[[nn1]][[6]]+rremove("ylab") + coord_cartesian(ylim = c(0, 1)), 
                   nowt[[nn1]][[7]]+rremove("ylab") + coord_cartesian(ylim = c(0, 1)),
                   nowt[[nn2]][[2]]+rremove("ylab") + coord_cartesian(ylim = c(0, 1)),
                   nowt[[nn2]][[3]]+rremove("ylab") + coord_cartesian(ylim = c(0, 1)),
                   nowt[[nn2]][[4]]+rremove("ylab") + coord_cartesian(ylim = c(0, 1)),
                   nowt[[nn2]][[5]]+rremove("ylab") + coord_cartesian(ylim = c(0, 1)),
                   nowt[[nn2]][[6]]+rremove("ylab") + coord_cartesian(ylim = c(0, 1)),
                   nowt[[nn2]][[7]]+rremove("ylab") + coord_cartesian(ylim = c(0, 1)),
                   ncol=6, nrow=2, common.legend = TRUE, legend = 'none', align='hv')
annotate_figure(plot.n, left = text_grob("Marginal Survival Probability                                       Marginal Survival Probability ", 
                                         size=10, rot = 90, vjust = 0.9))
# Edit nn1 and nn2 for different sample sizes


#### FIGURE 8
#No WT
nowt2 = list()
for(nn in 1:length(n_sizes)){
  nowt2[[nn]] <- list()
  for(pp in 1:length(pis)){
    current.title = substitute(paste('No WT, ',pi == number2),
                               list(number1 = n_sizes[nn], number2 = pis[pp]))
    nowt2[[nn]][[pp]] = plot.survII(data = surv_perfs$eval[n==n_sizes[nn] & pi==pis[pp] & WT=='NoWT'],
                                   title = current.title, title.rel = 0.95)
  }
}


#WT 1-99
wt1 = list()
for(nn in 1:length(n_sizes)){
  wt1[[nn]] <- list()
  for(pp in 1:length(pis)){
    current.title = substitute(paste('WT: 1-99, ',pi == number2),
                               list(number1 = n_sizes[nn], number2 = pis[pp]))
    wt1[[nn]][[pp]] = plot.survII(data = surv_perfs$eval[n==n_sizes[nn] & pi==pis[pp] & WT=='1-99'],
                                 title = current.title, title.rel = 0.95)
  }
}

#WT 5-95
wt5 = list()
for(nn in 1:length(n_sizes)){
  wt5[[nn]] <- list()
  for(pp in 1:length(pis)){
    current.title = substitute(paste('WT: 5-95, ',pi == number2),
                               list(number1 = n_sizes[nn], number2 = pis[pp]))
    wt5[[nn]][[pp]] = plot.survII(data = surv_perfs$eval[n==n_sizes[nn] & pi==pis[pp] & WT=='5-95'],
                                 title = current.title,  title.rel = 0.95)
  }
}

#WT 10-90
wt10 = list()
for(nn in 1:length(n_sizes)){
  wt10[[nn]] <- list()
  for(pp in 1:length(pis)){
    current.title = substitute(paste('WT: 10-90, ',pi == number2),
                               list(number1 = n_sizes[nn], number2 = pis[pp]))
    wt10[[nn]][[pp]] = plot.survII(data = surv_perfs$eval[n==n_sizes[nn] & pi==pis[pp] & WT=='10-90'],
                                  title = current.title,  title.rel = 0.95)
  }
}



nn=5 #sample size index
dev.new()
plot.wt.a = ggarrange(nowt2[[nn]][[2]]+rremove("ylab"), 
                      nowt2[[nn]][[3]]+rremove("ylab"), 
                      nowt2[[nn]][[4]]+rremove("ylab"), 
                      nowt2[[nn]][[5]]+rremove("ylab"),
                      nowt2[[nn]][[6]]+rremove("ylab"), 
                      nowt2[[nn]][[7]]+rremove("ylab"),
                      wt1[[nn]][[2]]+rremove("ylab"),
                      wt1[[nn]][[3]]+rremove("ylab"), 
                      wt1[[nn]][[4]]+rremove("ylab"),
                      wt1[[nn]][[5]]+rremove("ylab"),
                      wt1[[nn]][[6]]+rremove("ylab"),
                      wt1[[nn]][[7]]+rremove("ylab"), 
                      ncol=6, nrow=2, common.legend = TRUE, legend = 'none', align='hv')
annotate_figure(plot.wt.a, left = text_grob("Marginal Survival Probability                                       Marginal Survival Probability ", 
                                            size=10, rot = 90, vjust = 0.9))

dev.new()
plot.wt.b = ggarrange(wt5[[nn]][[2]]+rremove("ylab"), 
                      wt5[[nn]][[3]]+rremove("ylab"), 
                      wt5[[nn]][[4]]+rremove("ylab"), 
                      wt5[[nn]][[5]]+rremove("ylab"),
                      wt5[[nn]][[6]]+rremove("ylab"), 
                      wt5[[nn]][[7]]+rremove("ylab"),
                      wt10[[nn]][[2]]+rremove("ylab"),
                      wt10[[nn]][[3]]+rremove("ylab"), 
                      wt10[[nn]][[4]]+rremove("ylab"),
                      wt10[[nn]][[5]]+rremove("ylab"),
                      wt10[[nn]][[6]]+rremove("ylab"),
                      wt10[[nn]][[7]]+rremove("ylab"), 
                      ncol=6, nrow=2, common.legend = TRUE, legend = 'none', align='hv')
annotate_figure(plot.wt.b, left = text_grob("Marginal Survival Probability                                       Marginal Survival Probability ", 
                                            size=10, rot = 90, vjust = 0.9))
