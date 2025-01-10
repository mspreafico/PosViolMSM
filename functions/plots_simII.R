library(ggplot2)


relevelWT = function(data){
  data[WT!='NoWT', WT := paste0('WT:',WT)]
  data$WT = factor(data$WT,  level=c('NoWT','WT:1-99','WT:5-95','WT:10-90'))
  return(data)
}

plotII.xtime.wt <- function(data, y.var, 
                            tau.colors = c('#660099','#9900CC','#CC00CC','#FF66FF','#FF99FF','#FFCCFF'), 
                            title=NULL, bks = c(1:5), y.lim=NULL,                          
                            axis.text.rel = 0.7, axis.title.rel = 0.8,
                            title.rel = 1, legend.rel = 1){
  
  data = relevelWT(data)
  data[, group := .GRP, by = c("WT","tau")]
  
  if(is.null(y.lim)){
    y.lim = range(data[,get(y.var)])
  }
  if(y.lim[2]<0){y.lim[2]=0}
  if(y.lim[1]>0){y.lim[1]=0}
  p = ggplot(data, aes(x=time, y=get(y.var), group=as.factor(group), colour=as.factor(tau))) +
    geom_point(size=2) +
    geom_line(aes(linetype=as.factor(WT))) +
    scale_linetype_manual(values=c('solid','longdash','dotdash','dotted'), name = 'WT strategy') + 
    scale_colour_manual(name = expression(tau), values=tau.colors)+
    scale_x_continuous(limits=c(1,5)) +
    scale_y_continuous(limits=y.lim) +
    xlab('Time') +
    ylab(y.var) +
    ggtitle(title) +
    theme_light() +
    theme(axis.text=element_text(size=rel(axis.text.rel)),
          axis.title.y = element_text(face = "bold", size=rel(title.rel)),
          axis.title.x = element_text(size=rel(axis.title.rel)),
          plot.title = element_text(face="bold", size=rel(title.rel)), 
          legend.text = element_text(size=rel(legend.rel)), 
          legend.title = element_text(size=rel(legend.rel)))
  
  if(y.var=='Bias'){
    p = p + geom_hline(yintercept = 0, color='gray45', linetype='solid', alpha=0.6)
  }
  return(p)
}




# MEAN COUNTERFACTUAL SURVIVAL PLOT
plot.survII <- function(data, title='Add title or set to NULL',
                        taus_colors = c('#660099','#9900CC','#CC00CC','#FF66FF','#FF99FF','#FFCCFF'),
                        true_color='forestgreen',
                        axis.text.rel = 0.7, axis.title.rel = 0.8,
                        title.rel = 0.9, legend.rel = 0.8){
  
  p = ggplot(data, aes(x=time, y=avg_surv0, group=tau, color=as.factor(tau), linetype='avg_surv0')) +
    geom_line() +
    geom_line(aes(x=time, y=avg_surv1, group=tau, color=as.factor(tau), linetype='avg_surv1')) +
    scale_color_manual(values=taus_colors, name = expression(tau)) +
    geom_line(data=true_surv, aes(x=time, y=true_surv0, group=NULL), 
              linetype='dashed', color=true_color) +
    geom_line(data=true_surv, aes(x=time, y=true_surv1, group=NULL), 
              linetype='solid', color=true_color) +
    ylab('Survival Probability') +
    xlab('Time') +
    ggtitle(title) +
    theme_light() +
    scale_linetype_manual(name = "Regimen", values = c("solid", "dashed"),
                          breaks = c('avg_surv1', 'avg_surv0'),
                          labels = c("Always treated", "Never treated")) +
    theme(axis.text=element_text(size=rel(axis.text.rel)),axis.title=element_text(size=rel(axis.title.rel)),
          plot.title = element_text(face="bold", size=rel(title.rel)), 
          legend.text = element_text(size=rel(legend.rel)), legend.title = element_text(size=rel(legend.rel)))
  
  return(p)
}


