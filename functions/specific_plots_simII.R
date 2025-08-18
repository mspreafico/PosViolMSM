library(ggplot2)
library(ggpubr)
library(data.table)

# Plot functions for Figures 9-10-11
plot.ipwII.wt <- function(df_ipw, y.var, visit.times = c(1:5),
                          title='Add title or set to NULL',
                          ylab=NULL, ylim=NULL,
                          colori = c('#CC0066','#FFCC33','#619CFF'),
                          axis.text.rel = 0.7, axis.title.rel = 0.8,
                          title.rel = 0.9, legend.rel = 0.9){
  if(is.null(y.var)){
    ylab = y.var
  }
  if(is.null(ylim)){
    ylim = range(df_ipw[,get(y.var)])
    ylim[1] = ifelse(ylim[1]>0,0,ylim[1])
    ylim[2] = ifelse(ylim[2]<0,0,ylim[2])
  }
  p = ggplot(df_ipw[time %in% visit.times], 
             aes(x=factor(time), y=get(y.var), fill=WT)) +
    geom_boxplot(size=0.4) +
    scale_y_continuous(limits=ylim) +
    scale_fill_manual(values=colori) +
    labs(fill = 'Weight Truncation') + #values = tau.vec.col, 
    xlab('Time t') + 
    geom_hline(yintercept=0, linetype="dashed", color = "gray40") +
    ylab(ylab) +
    ggtitle(title)+
    theme_light() +
    theme(axis.text=element_text(size=rel(axis.text.rel)),
          axis.title = element_text(size=rel(title.rel)),
          plot.title = element_text(face="bold", size=rel(title.rel)), 
          legend.text = element_text(size=rel(legend.rel)), legend.title = element_text(size=rel(legend.rel)))
  
  return(p)
}

plot.coefII.wt <- function(long.df, visit.times = c(1:5),
                           colori = c('#CC0066','#FFCC33','#619CFF'),
                           axis.text.rel = 0.7, axis.title.rel = 0.8,
                           title.rel = 0.9, legend.rel = 0.9, size.value = 0.5){
  Ccoef_names <- list(
    'C0'=expression(hat(C)[0]^"b"*(t) - C[0]^"*"*(t)),
    'CA0'=expression(hat(C)[A0]^"b"*(t) - C[A0]^"*"*(t)),
    'CA1'=expression(hat(C)[A1]^"b"*(t) - C[A1]^"*"*(t)),
    'CA2'=expression(hat(C)[A2]^"b"*(t) - C[A2]^"*"*(t)),
    'CA3'=expression(hat(C)[A3]^"b"*(t) - C[A3]^"*"*(t)),
    'CA4'=expression(hat(C)[A4]^"b"*(t) - C[A4]^"*"*(t))
  )
  
  Ccoefs_labeller <- function(variable,value){
    return(Ccoef_names[value])
  }
  
  p = ggplot(long.df[time %in% visit.times], 
             aes(x=factor(time), y=Value, fill=WT)) +
    geom_hline(yintercept=0, linetype="dashed", color = "red") +
    geom_boxplot(size = size.value) +
    labs(fill = 'Weight Truncation') +
    scale_fill_manual(values=colori) +
    facet_grid(. ~ Ccoef, labeller=Ccoefs_labeller) +
    stat_summary(data = long.df[long.df$time %in% visit.times, ], aes(x=factor(time), group=WT),
                 fun = mean, geom = "point", shape = 23, size = 1, color = "black",
                 fill = "white", position = position_dodge(width = 0.75) ) +    
    xlab('Time t') + 
    ylab('Estimation Error') +
    theme_light() +
    theme(legend.position = 'bottom',strip.background = element_rect(fill="gray25"),
          axis.text=element_text(size=rel(axis.text.rel)),
          axis.title = element_text(size=rel(title.rel)),
          plot.title = element_text(face="bold", size=rel(title.rel)), 
          legend.text = element_text(size=rel(legend.rel)), legend.title = element_text(size=rel(legend.rel)))
  
  return(p)
}

plot.mc.survII <- function(df_surv_est, df_surv_perfs, df_true_surv,
                           title='Add title or set to NULL',
                           regimen = NULL, tau_color = 'gray10',
                           axis.text.rel = 0.7, axis.title.rel = 0.8,
                           title.rel = 0.9, legend.rel = 0.9){
  
  if(regimen=='always'){
    var.surv = 'surv1'; avg.surv = 'avg_surv1'; true.surv = 'true_surv1'
    line.type = 'solid'
  }else if(regimen=='never'){
    var.surv = 'surv0'; avg.surv = 'avg_surv0'; true.surv = 'true_surv0'
    line.type = 'dashed'
  }else{
    stop('Please specify regimen as always or never (treated)')
  }
  
  p = ggplot(df_surv_est, aes(x=time, y=get(var.surv), group=factor(rep_b))) +
    geom_line(alpha=0.2, linetype=line.type,color='gray') +
    geom_line(data=df_true_surv, aes(x=time, y=get(true.surv), group=NULL), 
              linetype=line.type, color='forestgreen', size=0.9) +
    geom_line(data=df_surv_perfs, aes(x=time, y=get(avg.surv), group=NULL), 
              linetype=line.type, color=tau_color, size=0.9) +
    ylab('Marginal Survival Probability') +
    xlab('Time') +
    ggtitle(title) +
    theme_light() +
    theme(axis.text=element_text(size=rel(axis.text.rel)),axis.title=element_text(size=rel(axis.title.rel)),
          plot.title = element_text(face="bold", size=rel(title.rel)), 
          legend.text = element_text(size=rel(legend.rel)), legend.title = element_text(size=rel(legend.rel)))
  
  return(p)
}
