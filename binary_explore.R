
colnames(df)
df <- conzoom
y <- "MEMBER"
x <- "VEJKODE"
pdf <- FALSE
binary_explore(conzoom,"MEMBER",pdf=FALSE,pdfname="c:/temp/testpdf.pdf")
giniauc(df,x,y,plotroc=TRUE)["gini"]


binary_explore <- function(df, y, pdf=FALSE, pdfname=NULL){
  library(reshape2)
  library(ggplot2)
  library(sqldf)
  
  options(sqldf.driver = "SQLite") # sqldf sql driver points back to SQLite for local data handling
  
  if(pdf==TRUE) {
    #pdf(pdfname,width=20)
    pdf(pdfname)
  }
  df0 <- df

  inputvars <- colnames(df)[c(!colnames(df) %in% y)]
  for (x in inputvars){
    go <- ifelse(is.numeric(df$x),TRUE,ifelse(length(unique(df[,x]))<40,TRUE,FALSE))
    if (go==FALSE) inputvars <- inputvars[!inputvars %in% x]
  }

  k<-0
  for (x in inputvars){
    k<-k+1
    print(paste("working...",k,"/",length(inputvars),":",x))
    df <- na.omit(df0[, c(x, y)])
    colnames(df) <- c("x","y")

    if (is.character(df$x)) df$x <- as.factor(df$x)
    
      mod <- glm(y~x,data=df,family=binomial())
      met <- gini_curve(df,mod,"y=1",plotroc=FALSE)
      
      ginimet <- met["gini"]
      auc <- met["auc"]
      #unique(df2$x)
      
      if (is.factor(df[,c("x")])) { # factor
        if (length(unique(df$x))>25) {
          
          df2 <- sqldf(paste("select xCenter2, sum(dist) as dist, total, 1.0*sum(y_sum)/sum(dist) as y_share
          from (
          select case when count(*)< n*0.03 then 'Other' else x end as xCenter2, count(*) as dist, n as total, sum(y) as y_sum
                               from df, (select count(*) as n from df) a 
                               group by x) z group by xCenter2 order by 1.0*y_sum/sum(dist) "))

        } else {
          df2 <- sqldf(paste("select x as xCenter2, count(*) as dist, n as total, avg(y) as y_share
                             from df, (select count(*) as n from df) a 
                             group by x order by avg(y)"))
          
        }
        df2$share <- df2$dist/df2$total
        
        barwidth <- 9/length(unique(df$x))
        
        plot <- ggplot(data=df2, aes(x=xCenter2, y=share)) + #fill=y_share)) +
          geom_bar(colour="grey", fill='lightgrey', stat="identity", width=barwidth) +
          coord_flip() + 
          geom_line(aes(y=y_share, x=xCenter2,group=1), size=2, stat = "identity", position = "identity", colour="green") +
          xlab(paste(x)) + ylab("Share [0;1]") +
          ggtitle(paste("Share of '",y,"' (gini=",format(ginimet, digits=3),"(in glm model))\n - by '",x,"' \n(incl. distribution of '",x,"')",sep="")) +
          theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"),
                axis.text.x=element_text(size=15),
                axis.title.x = element_text(size=14),
                axis.title.y = element_text(size=14)
          )
        print(plot)
      } else if (is.numeric(df[,c("x")])) { # numeric
        
        barwidth <- (max(df[,c("x")])-min(df[,c("x")]))*0.75/14
        df[,c("x")] <- as.numeric(df[,c("x")])
        
        if (length(unique(df$x))>12) {
            df$xmin <- ave(df$x, cut(df$x,12), FUN=min)
            df$xmax <- ave(df$x, cut(df$x,12), FUN=max)
            df$xCenter <- ave(df$x, cut(df$x,12), FUN=median)
            df2 <-   sqldf(paste("select xCenter, xmin+(xmax-xmin)/2 as xCenter2, avg(x) as avg_x, count(*) as dist, n as total, avg(y) as y_share 
                                 from df, (select count(*) as n from df) a 
                                 group by xCenter, xmin, xmax order by xCenter"))
        } else {
            df2 <- sqldf(paste("select x as xCenter2, avg(x) as avg_x, count(*) as dist, n as total, avg(y) as y_share
                               from df, (select count(*) as n from df) a 
                                 group by x order by x"))
        }
        df2$share <- df2$dist/df2$total
        
        plot <- ggplot(data=df2, aes(x=xCenter2, y=share)) + #fill=y_share)) +
          geom_bar(colour="grey", fill='lightgrey', stat="identity", width=barwidth) +
          geom_smooth(se=FALSE, method='loess', linetype='dotted') +
          geom_line(aes(y=y_share), size=2, stat = "identity", position = "identity", colour="green") +
          xlab(paste(x)) + ylab("Share [0;1]") +
          ggtitle(paste("Share of '",y,"' (gini=",format(ginimet, digits=3),"(in glm model))\n - by '",x,"' \n(incl. distribution of '",x,"')",sep="")) +
          theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"),
                axis.text.x=element_text(size=15),
                axis.title.x = element_text(size=14),
                axis.title.y = element_text(size=14)
          )
        print(plot)
        
      } else {
        warning <- "Error..."
      }
  }

  if(pdf==TRUE) {
    dev.off()
  }

}


