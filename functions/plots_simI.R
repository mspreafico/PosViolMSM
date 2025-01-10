library(data.table)
library(ggplot2)
library(ggpubr)

relevelWT = function(data){
  data[WT!='NoWT', WT := paste0('WT:',WT)]
  data$WT = factor(data$WT,  level=c('NoWT','WT:1-99','WT:5-95','WT:10-90'))
  return(data)
}


plot.evalI.pi <- function(data, y.var, title='Add title or set to NULL',
                          taus_colors = c('gray60','#66CCFF','#0099FF','#3366CC','#0033CC','navy'),
                          axis.text.rel = 0.7, axis.title.rel = 0.8,
                          title.rel = 1, legend.rel = 1){
  
  data = relevelWT(data)
  data[, group := .GRP, by = c("WT","tau")]
  
  pis = unique(data$pi)
  group_name = NULL
  for(i in 1:length(pis)){ group_name = c(group_name, bquote(pi==.(pis[i]))) }
  
  p = ggplot(data = data, aes(x=as.factor(pi), y=get(y.var), group=factor(group), color=factor(tau)))+
    geom_point(size=2) +
    geom_line(aes(linetype=WT)) +
    scale_linetype_manual(values=c('solid','longdash','dotdash','dotted'), name = 'WT strategy') + 
    scale_color_manual(values=taus_colors, name = expression(tau)) + 
    scale_fill_manual(values=taus_colors, name = expression(tau)) + 
    scale_x_discrete(labels=group_name) +
    xlab(NULL) + 
    ylab(y.var) +
    ggtitle(title)+
    theme_light() +
    theme(axis.text=element_text(size=rel(axis.text.rel)),
          axis.title.y = element_text(face = "bold", size=rel(title.rel)),
          axis.title.x = element_text(size=14),
          plot.title = element_text(face="bold", size=rel(title.rel)), 
          legend.text = element_text(size=rel(legend.rel)), legend.title = element_text(size=rel(legend.rel)))
  
  if(y.var=='Bias'){
    p = p + geom_hline(yintercept = 0, color='gray20', linetype='solid', alpha=0.6)
  }
  return(p)
}

# MEAN COUNTERFACTUAL SURVIVAL PLOT
plot.survI <- function(data, title='Add title or set to NULL',
                       taus_colors = c('gray60','#66CCFF','#0099FF','#3366CC','#0033CC','navy'),
                       true_color='#FF6633',
                       axis.text.rel = 0.7, axis.title.rel = 0.8,
                       title.rel = 0.9, legend.rel = 0.9){
  
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
