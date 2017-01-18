giniauc <- function(df,x,y,plotroc=FALSE){
  library("ggplot2")
  library("sqldf")
  
  options(warn=-1)
  
  df <- df[,c(x,y)]
  colnames(df) <- c("x","y")
  
  groups <- length(unique(df$x))
  if (groups>100) {
    groups <- 100
  }
  
  df$xrank <- ceiling(groups*rank(df$x, ties.method= "min")/nrow(df)) 
  gc.graphdata <- sqldf(paste("select xrank, count(*) as n,
                              avg(x) as avg_x, avg(y) as avg_target, sum(y) as sum_target, 
                              1.0*sum(y)/target_total as captured_share,
                              avg(y)/avg_target_total as lift
                              from df
                              ,(select sum(y) as target_total, avg(y) as avg_target_total from df) 
                              group by xrank, target_total order by xrank desc"))
  
  for (i in 1:nrow(gc.graphdata)) {
    #gc.graphdata[i,"x"] <- 1.0*i/nrow(gc.graphdata)
    if(i==1) {
      gc.graphdata[i,"captured_share_acc"] <- gc.graphdata[i,"captured_share"]
      gc.graphdata[i,"n_acc"] <- gc.graphdata[i,"n"] 
      gc.graphdata[i,"x"] <- 1.0*gc.graphdata[i,"n_acc"]/sum(gc.graphdata[,"n"])
      gc.graphdata[i,"area"] <- 0.5*gc.graphdata[i,"captured_share_acc"]*gc.graphdata[i,"x"]
    } else {   
      gc.graphdata[i,"captured_share_acc"] <- gc.graphdata[i-1,"captured_share_acc"] + gc.graphdata[i,"captured_share"]
      gc.graphdata[i,"n_acc"] <- (gc.graphdata[i-1,"n_acc"]+gc.graphdata[i,"n"])   
      gc.graphdata[i,"x"] <- 1.0*gc.graphdata[i,"n_acc"]/sum(gc.graphdata[,"n"])
      gc.graphdata[i,"area"] <- gc.graphdata[i-1,"captured_share_acc"]*(gc.graphdata[i,"x"]-gc.graphdata[i-1,"x"]) +  0.5*(gc.graphdata[i,"captured_share_acc"]-gc.graphdata[i-1,"captured_share_acc"])*(gc.graphdata[i,"x"]-gc.graphdata[i-1,"x"])
    }
  }
  gc.graphdata[,"lift_acc"] <- gc.graphdata[,"captured_share_acc"]/gc.graphdata[,"x"]
  
  gc.auc <- sum(gc.graphdata$area)
  gc.gini <- (gc.auc-0.5)/(0.5-mean(df$y)/2)  
  
  if (plotroc){
    gc.graphdata[nrow(gc.graphdata)+1,"x"] <- 0
    gc.graphdata[nrow(gc.graphdata),"captured_share_acc"] <- 0
    
    gc.giniplot <- ggplot(data=gc.graphdata, aes(x=x)) +
      geom_line(aes(y=captured_share_acc), color="red") +
      geom_line(aes(y=x)) +
      expand_limits(y=0) +
      xlab("Score rank (desc)") + ylab("Captured response (accumulated)") +
      ggtitle(paste("GINI (captured response). gini=",format(gc.gini,digits=3), "; AUC=",format(gc.auc,digits=3), sep=""))
    print(gc.giniplot)
    
    gc.liftplot <- ggplot(gc.graphdata, aes(x, y = value, color = Graphs)) + 
      geom_line(aes(y = lift, col = "Lift")) + 
      geom_line(aes(y = lift_acc, col = "AccLift"))  +
      xlab("Score rank (desc)") + ylab("Lift") +
      ggtitle(paste("Lift. gini=",format(gc.gini,digits=3), "; AUC=",format(gc.auc,digits=3), sep="")) +
      scale_colour_manual(values=c("grey", "blue"))
    print(gc.liftplot)
  }
  
  #  options(warn=0)
  
  outs <-c(gc.auc, gc.gini)
  names(outs) <- c("auc", "gini")
  return(outs)
}