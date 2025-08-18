library(ggplot2)
library(ggpubr)
library(data.table)

# Plot functions for Figures 4-5-6
plot.ipwI.wt <- function(df_ipw, y.var, visit.times = c(0,10,20,30,40),
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
  p = ggplot(df_ipw[visit %in% visit.times], 
             aes(x=factor(visit), y=get(y.var), fill=WT)) +
    geom_boxplot(size=0.4) +
    scale_y_continuous(limits=ylim) +
    scale_fill_manual(values=colori) +
    labs(fill = 'Weight Truncation') + #values = tau.vec.col, 
    xlab('Time-visit k') + 
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

plot.coefI.wt <- function(long.df, colori = c('#CC0066','#FFCC33','#619CFF'),
                          axis.text.rel = 0.7, axis.title.rel = 0.8,
                          title.rel = 0.9, legend.rel = 0.9){
  
  gammas_names <- list(
    'gamma0'=expression(hat(tilde(gamma))[0]^"b" - tilde(gamma)[0]^"*"),
    'gammaA1'=expression(hat(tilde(gamma))[A1]^"b" - tilde(gamma)[A1]^"*"),
    'gammaA2'=expression(hat(tilde(gamma))[A2]^"b" - tilde(gamma)[A2]^"*"),
    'gammaA3'=expression(hat(tilde(gamma))[A3]^"b" - tilde(gamma)[A3]^"*")
  )
  
  gammas_labeller <- function(variable,value){
    return(gammas_names[value])
  }
  
  hline.df <- data.table(data.frame(
    Coefficient = c("gamma0","gammaA1","gammaA2","gammaA3"),
    yintercept = rep(0,4) #c(-3, 0.05, -1.5, 0.1)
  ))
  
  p = ggplot(long.df, aes(x=WT, y=Value, fill=WT)) +
    geom_boxplot(size=0.4) +
    labs(fill = ' ') +
    scale_fill_manual(values=colori) +
    facet_grid(. ~ Coefficient, labeller=gammas_labeller) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 2, color = "black", fill = "white") +
    geom_hline(data = hline.df[Coefficient %in% unique(long.df$Coefficient)], 
               aes(yintercept = yintercept), 
               color = "red", linetype = "dashed") +
    theme_light() + 
    xlab('Weight Truncation') + 
    ylab('Estimation Error') +
    theme(strip.background = element_rect(fill="gray25"),
          axis.text=element_text(size=rel(axis.text.rel)),
          axis.title = element_text(size=rel(title.rel)),
          plot.title = element_text(face="bold", size=rel(title.rel)), 
          legend.text = element_text(size=rel(legend.rel)), legend.title = element_text(size=rel(legend.rel)))
  
  return(p)
}

plot.mc.survI <- function(df_surv_est, df_surv_perfs, df_true_surv,
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
              linetype=line.type, color='#FF6633', size=0.9) +
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
