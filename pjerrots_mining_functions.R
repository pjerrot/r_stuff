# Functions:

#giniauc : gini and auc
#samp : sample df
#calc.r2 : calc r2
#rpart_to_sql
#gini_curve : old gini calc
#glm_to_sql 
#graph_num_grouped : graphs var against target
#graph_categorical_grouped : graphs var against target
#str_replace : replaces str pattern in string
#starsplit : splits str into vector. Works with *
#waffle_plot: created waffle plot (like pie chart - just better)
#inf.omit: omits inf værdier (til modellering), så den kan bruges på samme måde som na.omit
# vifanalysis: does VIF analysis and removes variables causing multicollinarity

#############################################################
# calculates gini and auc metrics from x,y value pairs. INclude ROC curve

vifanalysis <- function(df,vars,vif_criteria=10, showprogess=FALSE) {

  for (i in vars) {
    if (is.character(df[,i])) df[,i] <- as.factor(df[,i]) 
  }  
  
  vif_stats <- data.frame(
    var = character(0),
    vifvalue = numeric(0),
    stringsAsFactors=F
  )
  
  vif_hist <- data.frame(
    var = character(0),
    vifvalue = numeric(0),
    stringsAsFactors=F
  )
  
  max_vif <- vif_criteria+1
  exclude <- c()
  
  while (max_vif>vif_criteria) {
    vif_stats <- vif_stats[0,]
    for (l in vars[!vars %in% exclude]) {
	  if (showprogess==TRUE) print(paste("Testing: ", l))
      newvars <- vars[!vars %in% exclude]
      tekst <- paste("mulcolmod <- lm(",l,"~.,data=df[,newvars])")
      eval(parse(text=tekst))
      r_square  <- summary(mulcolmod)$r.squared
      vif <- ifelse(is.na(r_square),1000000000,ifelse(r_square==1,1000000000,1/(1-r_square)))
      if (showprogess==TRUE) print(paste(l,":",vif))
      vif_stats[nrow(vif_stats)+1,c("var","vifvalue")] <- list(l,vif)
    }
    
    vif_stats <- vif_stats[order(-vif_stats$vifvalue),]
    
    if (vif_stats[1,"vifvalue"]>vif_criteria) {
      exclude <- c(exclude,vif_stats[1,"var"])
	  if (showprogess==TRUE) print(paste("Excluding: ",vif_stats[1,"var"], " - VIF value=", vif_stats[1,"vifvalue"]))
      vif_hist[nrow(vif_hist)+1,c("var","vifvalue")] <- vif_stats[1,c("var","vifvalue")]
    }
    max_vif <- vif_stats[1,"vifvalue"]
  }
  
  restvars <- vars[!vars %in% exclude]
  
  out <- list(restvars,vif_hist)
  names(out) <- c("restvars","vif_hist")
  return(out)
}  


inf.omit <- function(df) {
  for (i in colnames(df)) {
    if (is.numeric(df[,i])) {
      df <- df[is.finite(df[,i]),]
    }
  }
  return(df)
}

giniauc <- function(df,x,y,plotroc=TRUE){
  library("ggplot2")
  library("sqldf")
  
  options(warn=-1)
  
  df <- df[,c(x,y)]
  colnames(df) <- c("x","y")
  
  if (is.factor(df$y)) df$y <- as.numeric(as.character(df$y))
  
  if (is.character(df$x)) df$x <- as.factor(df$x)
  
  if (is.numeric(df$x)) {
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
                                group by xrank, target_total 
                                order by xrank desc"))
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
    gc.gini <- gc.gini <- (gc.auc-0.5)/(0.5-mean(df$y)/2) 
    
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
    
  } else if (is.factor(df$x)) {
    gc.graphdata <- sqldf(paste("select x, count(*) as n, 1.0*count(*)/nialt as n_share,
                                avg(y) as avg_target, sum(y) as sum_target, 
                                1.0*sum(y)/target_total as captured_share,
                                avg(y)/avg_target_total as lift
                                from df
                                ,(select sum(y) as target_total, avg(y) as avg_target_total, count(*) as nialt from df) 
                                group by x, target_total, nialt 
                                order by avg(y) desc"))
    gc.graphdata <- na.omit(gc.graphdata)
    
    for (i in 1:nrow(gc.graphdata)) {
      #gc.graphdata[i,"x"] <- 1.0*i/nrow(gc.graphdata)
      if(i==1) {
        gc.graphdata[i,"captured_share_acc"] <- gc.graphdata[i,"captured_share"]
        gc.graphdata[i,"n_acc"] <- gc.graphdata[i,"n"] 
        #gc.graphdata[i,"x"] <- 1.0*gc.graphdata[i,"n_acc"]/sum(gc.graphdata[,"n"])
        gc.graphdata[i,"area"] <-gc.graphdata[i,"captured_share_acc"]*gc.graphdata[i,"n_share"]
        gc.graphdata[i,"n_share_accu"] <- gc.graphdata[i,"n_share"]
      } else {   
        gc.graphdata[i,"captured_share_acc"] <- gc.graphdata[i-1,"captured_share_acc"] + gc.graphdata[i,"captured_share"]
        gc.graphdata[i,"n_acc"] <- (gc.graphdata[i-1,"n_acc"]+gc.graphdata[i,"n"])   
        #gc.graphdata[i,"x"] <- 1.0*gc.graphdata[i,"n_acc"]/sum(gc.graphdata[,"n"])
        gc.graphdata[i,"area"] <-gc.graphdata[i,"captured_share_acc"]*gc.graphdata[i,"n_share"]
        gc.graphdata[i,"n_share_accu"] <- gc.graphdata[i,"n_share"] + gc.graphdata[i-1,"n_share_accu"]
      }
    }
    gc.graphdata[,"lift_acc"] <- gc.graphdata[,"captured_share_acc"]/gc.graphdata[,"n_share_accu"]
    
    gc.auc <- sum(gc.graphdata$area)
    gc.gini <- (gc.auc-0.5)/(0.5-mean(df$y)/2)  
    
    #allarea <- (1-mean(df$y)) + 0.5*(mean(df$y))
    #gc.gini <- (gc.auc-0.5)/(allarea-0.5)
    
    if (plotroc){
      gc.graphdata[nrow(gc.graphdata)+1,"n_share_accu"] <- 0
      gc.graphdata[nrow(gc.graphdata),"captured_share_acc"] <- 0
      
      gc.giniplot <- ggplot(data=gc.graphdata, aes(x=n_share_accu)) +
        geom_line(aes(y=captured_share_acc), color="red") +
        geom_line(aes(y=n_share_accu)) +
        expand_limits(y=0) +
        xlab("Score rank (desc)") + ylab("Captured response (accumulated)") +
        ggtitle(paste("GINI (captured response). gini=",format(gc.gini,digits=3), "; AUC=",format(gc.auc,digits=3), sep=""))
      print(gc.giniplot)
      
      gc.liftplot <- ggplot(gc.graphdata, aes(n_share_accu, y = value, color = Graphs)) + 
        geom_line(aes(y = lift, col = "Lift")) + 
        geom_line(aes(y = lift_acc, col = "AccLift"))  +
        xlab("Score rank (desc)") + ylab("Lift") +
        ggtitle(paste("Lift. gini=",format(gc.gini,digits=3), "; AUC=",format(gc.auc,digits=3), sep="")) +
        scale_colour_manual(values=c("grey", "blue"))
      print(gc.liftplot)
    }
    
  } 
  
  #  options(warn=0)
  
  outs <-list(gc.auc, gc.gini)
  names(outs) <- c("auc", "gini")
  return(outs)
 }

# sample a df. By method=c("random","random_return","first","last")
samp <- function(df,samp_n=1000, method="random"){
  if (method=="random") {
    df$rnd <- runif(nrow(df),0,1)
    df <- df[order(df$rnd),][1:samp_n,]
    df$rnd <- NULL
  }
  if (method=="random_return") {
    rnd <- round(runif(samp_n,0,nrow(df)),0)
    df <- df[rnd,]
    rnd <- NULL
  }
  if (method=="first") {
    df <- df[1:samp_n,]
  }
  if (method=="last") {
    df <- df[(nrow(df)-samp_n):nrow(df),]
  }
  return(df)
}

# calculates r-squared on x,y value pairs
# R squared
r2 <- function(true, predicted) {
  rsquare <- 1 - (sum((true-predicted )^2)/sum((true-mean(true))^2))
  if (rsquare < 0) rsquare <- 0
  return (rsquare)
}

modmetrics <- function(true, predicted) {
  require(Metrics)
  rsquare <- 1 - (sum((true-predicted )^2)/sum((true-mean(true))^2))
  if (rsquare < 0) rsquare <- 0
  
  rmse <- rmse(true,predicted)
  
  nrmse <- rmse/mean(true)
  
  bias_prct <- percent_bias(true,predicted)
  bias <- bias(true, predicted)
  
  auc <- auc(true,predicted)
  
  gini <- (auc-0.5)/((1-mean(true))/2)
  
  outs <- list(rsquare, rmse, nrmse, bias_prct, auc, gini, bias)
  names(outs) <- c("r2", "rmse", "nrmse", "bias_prct", "auc", "gini", "bias")

  return (outs)
}



# parses rpart tree to sql
rpart_to_sql <- function(model,df) {
  
  rpart.sql.value <- "case "
  rpart.sql.node <- "case "
  
  rules <- capture.output({asRules(model,compact=TRUE)})
  
  for (i in 1:length(rules)) {
    if (rules[i]!="") {
      enregel <- gsub("< ", "<", rules[i], ignore.case =FALSE, fixed=TRUE) # for some reason there is space after '<'
      
      nodename <- substr(enregel,1,regexpr('[', enregel, fixed=TRUE)-2)
      nodeprop <- substr(enregel,regexpr(',', enregel, fixed=TRUE)+1,regexpr(']', enregel, fixed=TRUE)-1)
      
      enregel <- substr(enregel,regexpr(']', enregel, fixed=TRUE)+1,100000)
      
      for (k in 1: length(names(df))) {
        name1 <- paste(" ",names(df)[k],"=",sep="")
        name2 <- paste(" ",names(df)[k],">",sep="")
        name3 <- paste(" ",names(df)[k],"<",sep="")
        enregel <- gsub(name1,paste("ÅÆ",name1,sep=""),enregel  )
        enregel <- gsub(name2,paste("ÅÆ",name2,sep=""),enregel  )
        enregel <- gsub(name3,paste("ÅÆ",name3,sep=""),enregel  )
      }
      enregel <- gsub("ÅÆÅÆ","ÅÆ",enregel)
      enregel <- gsub("'","",enregel)
      
      v <- gsub("[[:space:]]*$","",c(unlist(strsplit(enregel, split="ÅÆ"))))
      
      regel <- ""
      for (j in 2:length(v)) {
        if (j>2){regel <- paste(regel,"AND")}
        if ( regexpr("[[:alpha:]]=",v[j])>0 ){ # nye: kigger efter et "=" hvor der er tekst lige foran...
          regel <- paste(regel,paste(sub("="," in ('",gsub(",","','",v[j])),"')",sep="")) 
        } else if (regexpr("[1-9]=",v[j])>0){
          regel <- paste(regel,paste(sub("="," in ('",gsub(",","','",v[j])),"')",sep="")) 
        } else {  
          regel<-paste(regel,v[j])
        }
      }
      #print(regel)
      nodesql_value <- paste("WHEN",regel,"THEN",nodeprop)
      nodesql_node <- paste("WHEN ",regel," THEN '",nodename,"'",sep="")  
      
      rpart.sql.value <- paste(rpart.sql.value,nodesql_value)
      rpart.sql.node <- paste(rpart.sql.node,nodesql_node)
    }
  }
  
  rpart.sql.node <- paste(rpart.sql.node,"ELSE 'MISS' END ")
  
  # Finding avg target in case of MISS 
  y <- substr(capture.output(model$call),17,100)
  y <- substr(y,1,unlist(gregexpr(" ~",y))[1]-1)
  test <- sqldf(paste("select avg(y) as missgns from (select ",rpart.sql.node, " as x,",y[1]," as y from df) a where x='MISS'"))
  if (is.na(test[1,1])) {
    rpart.sql.value <- paste(rpart.sql.value," END ")
  } else {
    rpart.sql.value <- paste(rpart.sql.value,"ELSE ",test[1,1],"END ")  
  }
  
  
  outs <-list(rpart.sql.value, rpart.sql.node)
  names(outs) <- c("sql_value", "sql_node")
  return(outs)
}


# calculates gini and auc
gini_curve <- function(df,modelobj,targetdef,plotroc=FALSE){
  options(warn=-1)
  
  # DENNE STUMP SIKRER AT DATASÆTTET KUN INDEHOLDER DE KATEGORIER, SOM ER ANVENDT I MODELLEN  
  sql <- paste("select a.*,case when ",targetdef,"then 1 else 0 end as bintarget from df a where 1=1")
  for(i in names(modelobj$xlevels)) {
    sql0 <- paste("and",names(modelobj$xlevels[i]),"in (")
    b <- unlist(modelobj$xlevels[i])
    for (j in 1:length(b)) {
      if (j==length(b)) {
        sql0 <- paste(sql0,"'",b[j],"'", sep="")  
      } else {
        sql0 <- paste(sql0,"'",b[j],"',", sep="")  
      }
    }
    sql0 <- paste(sql0,")",sep="")
    sql <- paste(sql,sql0)
  }
  df <- sqldf(sql)
  
  #FJERNER EVT. MISSING VÆRDIER  
  #  df <- na.omit(df)
  gc.bintarget <- df["bintarget"]
  
  
  if (length(modelobj$method) == 0) {
    gc.prob <- predict(modelobj, newdata=df)
  } else {
    if (modelobj$method=="class") 
    {
      gc.prob<-predict(modelobj, newdata=df, type="class") #Træ
    } else if (modelobj$method=="anova") 
    {
      gc.prob<-predict(modelobj,type=c("class"), newdata=df)
    } else {
      gc.prob<-predict(modelobj,type=c("response"), newdata=df)  
    }
  }
  
  df$prob<-gc.prob
  groups <- length(unique(df$prob))
  if (groups>100) {
    groups <- 100
  }
  
  df$probrank <- ceiling(groups*rank(df$prob, ties.method= "min")/nrow(df)) 
  gc.graphdata <- sqldf(paste("select probrank, count(*) as n,
                              avg(prob) as avg_prob, avg(bintarget) as avg_target, sum(bintarget) as sum_target, 
                              1.0*sum(bintarget)/target_total as captured_share,
                              avg(bintarget)/avg_target_total as lift
                              from df
                              ,(select sum(bintarget) as target_total, avg(bintarget) as avg_target_total from df) 
                              group by probrank, target_total order by probrank desc"))
  
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
  gc.gini <- (gc.auc-0.5)/(0.5-mean(df$bintarget)/2)  
  #gc.gini <- (gc.auc-0.5)/(0.5-mean(gc.target[,1])/2)  
  #gc.gini <- (sum(na.omit(gc.graphdata$area))-0.5)/(0.5-mean(gc.target[,1])*0.5)
  
  if (plotroc){
    gc.graphdata[nrow(gc.graphdata)+1,"x"] <- 0
    gc.graphdata[nrow(gc.graphdata),"captured_share_acc"] <- 0
    
    gc.giniplot <- ggplot(data=gc.graphdata, aes(x=x)) +
      geom_line(aes(y=captured_share_acc), color="red") +
      geom_line(aes(y=x)) +
      expand_limits(y=0) +
      xlab("Score rank (desc)") + ylab("Captured response (accumulated)") +
      ggtitle(paste("GINI (captured response). (", modelobj$method,") gini=",format(gc.gini,digits=3), sep=""))
    print(gc.giniplot)
    
    gc.liftplot <- ggplot(gc.graphdata, aes(x, y = value, color = Graphs)) + 
      geom_line(aes(y = lift, col = "Lift")) + 
      geom_line(aes(y = lift_acc, col = "AccLift"))  +
      xlab("Score rank (desc)") + ylab("Lift") +
      ggtitle(paste("Lift. (", modelobj$method,") gini=",format(gc.gini,digits=3), sep="")) +
      scale_colour_manual(values=c("grey", "blue"))
    print(gc.liftplot)
  }
  
  #  options(warn=0)
  
  outs <-c(gc.auc, gc.gini)
  names(outs) <- c("auc", "gini")
  return(outs)
}

# parses glm (binary and gaussian) to sql

glm_to_sql <- function(glmmodel) {
  library("sqldf")
  vartypes <- data.frame(unlist(attr(glmmodel$terms,'dataClasses')))
  vartypes$varname <- rownames(vartypes)
  rownames(vartypes) <- NULL
  colnames(vartypes)[1] <- "vartype"
  
  xlev <- data.frame(unlist(glmmodel$xlevels))
  xlev$xlevrowname <- rownames(xlev)
  rownames(xlev) <- NULL
  colnames(xlev)[1] <- "xlevel"
  if (nrow(xlev)==0){xlev <- data.frame(xlevrowname=character(0), xlevel=character(0), stringsAsFactors=F)}
  
  modcoeffs <- data.frame(unlist(glmmodel$coefficients))
  modcoeffs$coeffname <- rownames(modcoeffs)
  rownames(modcoeffs) <- NULL
  colnames(modcoeffs)[1] <- "coeffvalue"
  modcoeffs[is.na(modcoeffs$coeffvalue),"coeffvalue"] <- 0
  
  for (i in 1:nrow(modcoeffs)) {    
    v <- unlist(strsplit(modcoeffs[i,"coeffname"], ":", fixed=FALSE))
    modcoeffs[i,"splitname1"] <- v[1]
    modcoeffs[i,"splitname2"] <- v[2]
    modcoeffs[i,"splitname3"] <- v[3]
    modcoeffs[i,"splitname4"] <- v[4]
  }
  
  varcats <- data.frame(varname=character(0), #name of variable
                        category=character(0), stringsAsFactors=F)
  if (length(glmmodel$xlevels)>0) {
    for (i in 1:length(glmmodel$xlevels)) {
      for (j in 1:nrow(data.frame(glmmodel$xlevels[i]))) {
        varcats[nrow(varcats)+1,] <- c(colnames(data.frame(glmmodel$xlevels[i])), data.frame(glmmodel$xlevels[i], stringsAsFactors=F)[j,1] )
      }  
    }
  }  
  
  modcoeffs$sql1 <- modcoeffs$splitname1
  modcoeffs$sql2 <- modcoeffs$splitname2
  modcoeffs$sql3 <- modcoeffs$splitname3
  modcoeffs$sql4 <- modcoeffs$splitname4
  modcoeffs$varname1 <- modcoeffs$splitname1
  modcoeffs$varname2 <- modcoeffs$splitname2
  modcoeffs$varname3 <- modcoeffs$splitname3
  modcoeffs$varname4 <- modcoeffs$splitname4
  
  if (length(glmmodel$xlevels)>0) {  
    for (i in 1:nrow(modcoeffs)) {    
      for (j in 1:nrow(varcats)) {    
        if (modcoeffs[i,"splitname1"] == paste(varcats[j,"varname"], varcats[j,"category"],sep="")) 
        { 
          modcoeffs[i,"sql1"] <- paste("(",varcats[j,"varname"],"='",varcats[j,"category"],"')",sep="")
          modcoeffs[i,"varname1"] <- varcats[j,"varname"]
        }
        if (is.na(modcoeffs[i,"splitname2"]) != TRUE) 
        {
          if (modcoeffs[i,"splitname2"] == paste(varcats[j,"varname"], varcats[j,"category"],sep="")) 
          { 
            modcoeffs[i,"sql2"] <- paste("(",varcats[j,"varname"],"='",varcats[j,"category"],"')",sep="")
            modcoeffs[i,"varname2"] <- varcats[j,"varname"]
          }  
        }
        if (is.na(modcoeffs[i,"splitname3"]) != TRUE) 
        {
          if (modcoeffs[i,"splitname3"] == paste(varcats[j,"varname"], varcats[j,"category"],sep="")) 
          { 
            modcoeffs[i,"sql3"] <- paste("(",varcats[j,"varname"],"='",varcats[j,"category"],"')",sep="")
            modcoeffs[i,"varname3"] <- varcats[j,"varname"]
          }  
        }
        if (is.na(modcoeffs[i,"splitname4"]) != TRUE) 
        {
          if (modcoeffs[i,"splitname4"] == paste(varcats[j,"varname"], varcats[j,"category"],sep="")) 
          { 
            modcoeffs[i,"sql4"] <- paste("(",varcats[j,"varname"],"='",varcats[j,"category"],"')",sep="")
            modcoeffs[i,"varname4"] <- varcats[j,"varname"]
          }  
        }
      }
    }
  }
  
  #coeffmatrix <- sqldf("select coeffvalue, coeffname, NULL as xlevel, '' as xlevrowname, '' as sqlstr, varname
  #                           from modcoeffs a join vartypes b on b.varname = a.coeffname where b.vartype='numeric' 
  #                     UNION ALL                    
  #                     select distinct coeffvalue, coeffname, trim(category) as xlevel, '' as xlevrowname, '' as sqlstr, varname
  #                     from modcoeffs a join varcats b on coeffname = varname || category ")
  #coeffmatrix[nrow(coeffmatrix)+1,c("coeffvalue","coeffname")] <- subset(modcoeffs[,c("coeffvalue","coeffname")], coeffname == '(Intercept)')
  #coeffmatrix[,"xlevel"] <- gsub("[\r\n]", "", coeffmatrix[,"xlevel"])
  #coeffmatrix <- sqldf("select distinct * from coeffmatrix")
  
  #j<-0
  #for (i in which(coeffmatrix$coeffvalue!=0)) {
  #  j <- j + 1
  #  if(coeffmatrix$coeffname[i] == "(Intercept)") 
  #  {
  #    coeffmatrix$sqlstr[i] <- coeffmatrix$coeffvalue[i]
  #  } else if (is.na(coeffmatrix$xlevel[i]) ) {    
  #    coeffmatrix$sqlstr[i] <- paste("(",coeffmatrix$coeffvalue[i],"*",coeffmatrix$coeffname[i],")")
  #  } else {
  #    coeffmatrix$sqlstr[i] <- paste("(case when ",coeffmatrix$varname[i],"='",coeffmatrix$xlevel[i], "' THEN ",coeffmatrix$coeffvalue[i]," ELSE 0 END)",sep="")
  #  }
  #  if (j==1){x.sql0 <- coeffmatrix$sqlstr[i]} else {x.sql0 <- paste(x.sql0,"+",coeffmatrix$sqlstr[i])}
  #}
  
  
  modcoeffs$sql <- ""
  for (i in 1:nrow(modcoeffs)) {    
    modcoeffs[i,"sql"] <- ifelse(is.na(modcoeffs[i,"sql4"])==FALSE, paste(modcoeffs[i,"coeffvalue"],"*",modcoeffs[i,"sql1"],"*",modcoeffs[i,"sql2"],"*",modcoeffs[i,"sql3"],"*",modcoeffs[i,"sql4"],sep=""),
                                 ifelse(is.na(modcoeffs[i,"sql3"])==FALSE, paste(modcoeffs[i,"coeffvalue"],"*",modcoeffs[i,"sql1"],"*",modcoeffs[i,"sql2"],"*",modcoeffs[i,"sql3"],sep=""),
                                        ifelse(is.na(modcoeffs[i,"sql2"])==FALSE, paste(modcoeffs[i,"coeffvalue"],"*",modcoeffs[i,"sql1"],"*",modcoeffs[i,"sql2"],sep=""),
                                               ifelse(is.na(modcoeffs[i,"sql1"])==FALSE, paste(modcoeffs[i,"coeffvalue"],"*",modcoeffs[i,"sql1"],sep=""),""))))
    modcoeffs[i,"sql"] <- ifelse(modcoeffs[i,"sql1"]=="(Intercept)",modcoeffs[i,"coeffvalue"],modcoeffs[i,"sql"] )
  }
  
  x.sql0 <- ""  
  for (i in 1:nrow(modcoeffs)) {    
    if (i==1){x.sql0 <- modcoeffs$sql[i]} else {x.sql0 <- paste(x.sql0,"+",modcoeffs$sql[i])}
  }
  
  if (is.null(glmmodel$family$link)){
    x.sql <- x.sql0
  } else {  
    if (glmmodel$family$link == "logit") {
      x.sql <- paste("1/(1 + exp(-(",x.sql0,")))")  
      x.sql <- paste("case when (",x.sql0,") between -40 and 40 then (", x.sql,") else 0 end ")
    } else if (glmmodel$family$link == "identity") {
      x.sql <- x.sql0
    }
  }
  
  assign("modcoeffs",modcoeffs,envir = .GlobalEnv)
  
  return(x.sql)

}

# creates nice graph on numerical variable distribution combined with avg. target values
graph_num_grouped <- function(df, varnavn,targetvar, targetvartext="TARGET"){
  library("sqldf")
  library("dplyr")
  library("ggplot2")
  
  if (varnavn!=targetvar){
    grpsize <- ceiling(nrow(df)/300)
    if(grpsize>20)
    {
      grpsize <- 20
    } 
	if (grpsize<10) grpsize <- 10
    
    d <- sqldf(paste("select ", varnavn, ",", targetvar, " as binarytarget from df order by", varnavn))
    d$RANKVAR <- ceiling(grpsize*rank(d[varnavn], ties.method= "first")/nrow(df))
    
    gns <- sqldf(paste("select RANKVAR, min(", varnavn, ") as minvar, max(", varnavn, ") as maxvar,  
                       avg(binarytarget) as target_avg, count(*) as count, stdev(binarytarget) as sd_target 
                       from d 
                       where ", varnavn, " is not null
                       group by RANKVAR 
                       order by RANKVAR"))
    
    gns[,7] <- 0
    colnames(gns)[7] <- "sd_common"
    
    gns[,8] <- 0
    colnames(gns)[8] <- "sd_sd_common"
    
    gns[,9] <- 0
    colnames(gns)[9] <- "SAKyhi"
    
    sd_target_all <- sd(unlist(df[targetvar]))
    mean_target_all <- mean(unlist(df[targetvar]))
    count_target_all <- nrow(df)
    
    cat_stats <- data.frame(varname=character(0), #name of variable
                            n_groups=numeric(0),
                            critvalue=numeric(0), #value of optimzation criteria
                            sqlstring=character(0), stringsAsFactors=F)
    
    # ny gruppering: iterativt finde dem, der matcher bedst, grupper og fortsæt indtil 2 grupper tilbage
    while (nrow(gns)>1) {
      
      #laver SQL
      gns$maxvar <- round(gns$maxvar,4)
      sql <- ""
      for (m in 1:nrow(gns)) {
        if (m==1) {
          sql <- paste(sql," when ", varnavn, " <= ", gns[m,"maxvar"]," then 'Grp",m,": ].;",gns[m,"maxvar"],"]'",sep="")
        } else {
          sql <- paste(sql," when ", varnavn, " <= ", gns[m,"maxvar"]," then 'Grp",m,": ]",gns[m-1,"maxvar"],";",gns[m,"maxvar"],"]'",sep="")
        }
      }
      sql <- paste("case ",sql," else '",varnavn,"_Grp_NA' end ",sep="")
      
      for (m in 1:nrow(gns)) {
        if(m==1)
        {
          gns[m,7] <- NA
          gns[m,8] <- NA
          gns[m,9] <- gns[m,5]*(gns[m,4]-mean_target_all)*(gns[m,4]-mean_target_all)
        } else {    
          gns[m,7] <- sqrt(((gns[m-1,5]-1)*gns[m-1,6]*gns[m-1,6] + (gns[m,5]-1)*gns[m,6]*gns[m,6])/(gns[m-1,5]+gns[m,5]-2))  
          gns[m,8] <- ((gns[m,6]-gns[m,7])^2 + (gns[m-1,6]-gns[m,7])^2)/2 # beregner "standardafvigelse mellem de to stds og den fælles stdev
          gns[m,9] <- gns[m,5]*(gns[m,4]-mean_target_all)*(gns[m,4]-mean_target_all)
        }
      }
      
      gns <- mutate(gns, diffrank = rank(sd_sd_common, ties.method = "first"))
      
      for (m in 1:nrow(gns)) {
        if(gns[m,"diffrank"]==1)
        {
          gns[m-1,"maxvar"] <- gns[m,"maxvar"]  
          gns[m-1,"target_avg"] <- (gns[m,"target_avg"]*gns[m,"count"] + gns[m-1,"target_avg"]*gns[m-1,"count"])/(gns[m,"count"]+gns[m-1,"count"]) 
          gns[m-1,"count"] <- gns[m,"count"]+gns[m-1,"count"]
          gns[m-1,"sd_target"] <- gns[m,"sd_common"]
        }
      }
      
      cat_stats[nrow(cat_stats)+1,] <- c(varnavn,nrow(gns),sum(gns[9])/(sd_target_all*sd_target_all*count_target_all), sql)
      
      gns <- sqldf("select * from gns where diffrank<>1") #fjerner diffrank=1 efter at denne er slået sammen med rækken ovenover.
      
    }  
    
    cat_stats$critvalue <- as.numeric(cat_stats$critvalue)
    cat_stats$n_groups <- as.numeric(cat_stats$n_groups)
    
    d <- sqldf("select a.varname, a.n_groups, a.critvalue, a.sqlstring from cat_stats a 
               where critvalue > (select max(critvalue)*0.98 from cat_stats where n_groups<10 ) and n_groups<10 order by a.critvalue")[1,]
    
    if (length(unique(df[,varnavn]))>10) {
      bob <- sqldf(paste("select ", d[1,"sqlstring"], " as input, avg(",targetvar,") as avg_target, count(*) as antal 
                         from df, (select count(*) as n_ialt from df) group by ", d[1,"sqlstring"], " having antal>(n_ialt*0.02)",sep=""))
    } else {
      bob <- sqldf(paste("select '_'||", varnavn, " as input, avg(",targetvar,") as avg_target, count(*) as antal 
                         from df, (select count(*) as n_ialt from df) group by '_'||", varnavn, " having antal>(n_ialt*0.02)",sep=""))
    }
    
    titel <- paste("Avg. ",targetvartext," by ",varnavn,"(grouped)",sep="")
    
    if ((nrow(bob)>8)&(nrow(bob)<25)){
      partialplot <-   ggplot(bob, aes(x=input, y=avg_target)) + geom_bar(stat='identity', fill="#FDB924") + 
        ggtitle(titel) + coord_flip() + theme(plot.title = element_text(size=22)) +
        geom_text(aes(label=paste("n=",antal))) +
        labs(x=varnavn,y=paste("Avg.",targetvartext))
      print(partialplot)
    } else if ((nrow(bob)>1)&(nrow(bob)<9)){
      partialplot <-   ggplot(bob, aes(x=input, y=avg_target)) + geom_bar(stat='identity', fill="#FDB924") + 
        ggtitle(titel) + theme(plot.title = element_text(size=22)) +
        geom_text(aes(label=paste("n=",antal))) +
        labs(x=varnavn,y=paste("Avg.",targetvartext))
      print(partialplot)
    }
    
    }
}


# creates nice graph on categorical variable distribution combined with avg. target values
graph_categorical_grouped <- function(varnavn,df,targetvar,targetvartext="TARGET"){
  
  if ((varnavn!=targetvar)&(length(unique(df[,varnavn]))<(nrow(df)/20))){
    # Laver initreftable for at gruppere kategorier med n<50 i "other" kategori
    initreftable <- sqldf(paste("select  ",varnavn, "  as var0a, case when count(*) < 50 then 'Other' else ",varnavn, " END AS var0,count(*) as count from df Group by ",varnavn)) 
    
    sql <- paste("select b.var0, avg(a.",varnavn,") as target_avg, count(*) as count, stdev(a.",varnavn,") as sd_target
                 from df a
                 join initreftable b on a.",varnavn, " = b.var0a
                 group by b.var0
                 order by target_avg", sep="")
    gns <- sqldf(sql)
    
    gns[,5] <- 0
    colnames(gns)[5] <- "sd_common"
    
    gns[,6] <- 0
    colnames(gns)[6] <- "sd_sd_common"
    
    gns[,7] <- 0
    colnames(gns)[7] <- "SAKyhi"
    
    sd_target_all <- sd(unlist(df[targetvar]))
    mean_target_all <- mean(unlist(df[targetvar]))
    count_target_all <- nrow(df)
    
    cat_stats <- data.frame(varname=character(0), #name of variable
                            n_groups=numeric(0),
                            critvalue=numeric(0), #value of optimzation criteria
                            sqlstring=character(0), stringsAsFactors=F)
    
    # ny gruppering: iterativt finde dem, der matcher bedst, grupper og fortsæt indtil 4 grupper tilbage
    sql <- ""
    while (nrow(gns)>1) {
      
      for (i in 1:nrow(gns)) {
        if(i==1)
        {
          gns[i,5] <- NA
          gns[i,6] <- NA
          gns[i,7] <- gns[i,3]*(gns[i,2]-mean_target_all)*(gns[i,2]-mean_target_all)
        } else {    
          gns[i,5] <- sqrt(((gns[i-1,3]-1)*gns[i-1,4]*gns[i-1,4] + (gns[i,3]-1)*gns[i,4]*gns[i,4])/(gns[i-1,3]+gns[i,3]-2))  
          gns[i,6] <- ((gns[i,4]-gns[i,5])^2 + (gns[i-1,4]-gns[i,5])^2)/2 # beregner "standardafvigelse mellem de to stds og den fælles stdev
          gns[i,7] <- gns[i,3]*(gns[i,2]-mean_target_all)*(gns[i,2]-mean_target_all)
        }
      }
      
      gns <- mutate(gns, diffrank = rank(sd_sd_common, ties.method = "first"))
      
      for (i in 1:nrow(gns)) {
        if(gns[i,"diffrank"]==1)
        {
          gns[i-1,"var0"] <- paste(gns[i,"var0"],"','",gns[i-1,"var0"],sep="")  
          gns[i-1,"target_avg"] <- (gns[i,"target_avg"]*gns[i,"count"] + gns[i-1,"target_avg"]*gns[i-1,"count"])/(gns[i,"count"]+gns[i-1,"count"]) 
          gns[i-1,"count"] <- gns[i,"count"]+gns[i-1,"count"]
          gns[i-1,"sd_target"] <- gns[i,"sd_common"]
        }
      }
      
      cat_stats[nrow(cat_stats)+1,] <- c(varnavn,nrow(gns),sum(gns[7])/(sd_target_all*sd_target_all*count_target_all), sql)
      
      gns <- sqldf("select * from gns where diffrank<>1") #fjerner diffrank=1 efter at denne er slået sammen med rækken ovenover.
      
      #laver SQL
      sql <- ""
      for (i in 1:nrow(gns)) {
        sql <- paste(sql," when ", varnavn, " in ('", gns[i,"var0"],"') then '",gsub("'","",gns[i,"var0"]),"' ",sep="")
      }
      sql <- paste("case ",sql," else 'NA' end",sep="")
      
    }  
    cat_stats[nrow(cat_stats)+1,] <- c(varnavn,nrow(gns),sum(gns[7])/(sd_target_all*sd_target_all*count_target_all), sql)
    
    cat_stats$critvalue <- as.numeric(cat_stats$critvalue)
    cat_stats$n_groups <- as.numeric(cat_stats$n_groups)
    
    d <- sqldf("select a.varname, a.n_groups, a.critvalue, a.sqlstring from cat_stats a 
               where critvalue > (select max(critvalue)*0.95 from cat_stats where n_groups<10 ) and a.sqlstring <> '' and a.sqlstring is not null and n_groups<10 and varname<>'binarytarget' order by a.critvalue")[1,]
    
    if (length(unique(df[,varnavn]))>10) {
      bob <- sqldf(paste("select ", d[1,"sqlstring"], " as input, avg(",targetvar,") as avg_target, count(*) as antal 
                         from df, (select count(*) as n_ialt from df) group by ", d[1,"sqlstring"]," having antal>(n_ialt*0.02)"))
    } else {
      bob <- sqldf(paste("select ", varnavn, " as input, avg(",targetvar,") as avg_target, count(*) as antal 
                         from df, (select count(*) as n_ialt from df) group by ", varnavn, "having antal>(n_ialt*0.02)"))
    }
    
    titel <- paste("Avg. ",targetvartext," by ",varnavn,"(grouped)",sep="")
    
    if ((nrow(bob)>3)&(nrow(bob)<25)){
      partialplot <-   ggplot(bob, aes(x=paste(substr(input,1,30),"..."), y=avg_target)) + geom_bar(stat='identity', fill="#FDB924") + 
        ggtitle(titel) + coord_flip() + theme(plot.title = element_text(size=22)) +
        geom_text(aes(label=paste("n=",antal))) +
        labs(x=varnavn,y=paste("Avg.",targetvartext))
      print(partialplot)
    } else if ((nrow(bob)>1)&(nrow(bob)<9)){ 
      partialplot <-   ggplot(bob, aes(x=paste(substr(input,1,30),"..."), y=avg_target)) + geom_bar(stat='identity', fill="#FDB924") + 
        ggtitle(titel) + theme(plot.title = element_text(size=22)) +
        geom_text(aes(label=paste("n=",antal))) +
        labs(x=varnavn,y=paste("Avg.",targetvartext))
      print(partialplot)
    }
    
    }
}

str_replace <- function(streng,from,to) { 
  splitpos <- c()
  splitvector <- c()
  n_element <- 0
  newstreng<-""
  streng <- as.character(streng)
  
  if (!is.na(streng)) {
    for (i in 1:nchar(streng)) {
      enstreng <- substr(streng,i,i)
      if (enstreng == from) {
        newstreng <- paste(newstreng,to,sep="")
      } else {
        newstreng <- paste(newstreng,enstreng,sep="")
      }
    }
  }
  return(newstreng)
}

# string spliiter that can handle '*'
starsplit <- function(streng,splitstr="*") { 
  splitpos <- c()
  splitvector <- c()
  n_element <- 0
  if (!is.na(streng)) {
    for (i in 1:nchar(streng)) {
      enstreng <- substr(streng,i,i)
      if (enstreng == splitstr) {
        n_element <- n_element + 1
        splitpos[n_element] <- i
      }
    }
    if (length(splitpos)>0) {
      splitvector[1] <- substr(streng,1,splitpos[1]-1)
      for (i in 1:length(splitpos)) {
        if (i<length(splitpos)) {
          splitvector[i+1] <- substr(streng,splitpos[i]+1,splitpos[i+1]-1)
        } else {
          splitvector[i+1] <- substr(streng,splitpos[i]+1,nchar(streng))
        }
      }
    }
    else {
      splitvector[1] <- streng
    }
  }
  return(splitvector)
}


bintarget_graph <- function(df, x, y, barwidth=10, pdf=FALSE, pdfname=NULL) {
  library(reshape2)
  library(ggplot2)
  library(sqldf)
  
  df <- df[, c(x, y)]
  colnames(df) <- c("x","y")
  if (is.factor(df[,c("x")])) {
    
  } else if (is.numeric(df[,c("x")])) {
    
    df[,c("x")] <- as.numeric(df[,c("x")])
    df$xmin <- ave(df$x, cut(df$x,12), FUN=min)
    df$xmax <- ave(df$x, cut(df$x,12), FUN=max)
    df$xCenter <- ave(df$x, cut(df$x,12), FUN=median)
    df2 <-   sqldf(paste("select xCenter, xmin+(xmax-xmin)/2 as xCenter2, avg(x) as avg_x, count(*) as dist, n as total, avg(y) as y_share 
                         from df, (select count(*) as n from df) a 
                         group by xCenter, xmin, xmax order by xCenter"))
    df2$share <- df2$dist/df2$total
    
    plot <- ggplot(data=df2, aes(x=xCenter2, y=share)) + #fill=y_share)) +
      geom_bar(colour="grey", fill='lightgrey', stat="identity", width=barwidth) +
      geom_smooth(se=FALSE, method='loess', linetype='dotted') +
      geom_line(aes(y=y_share), size=2, stat = "identity", position = "identity", colour="green") +
      xlab(paste(x)) + ylab("Share [0;1]") +
      ggtitle(paste("Share of '",y,"'\n - by '",x,"' \n(incl. distribution of '",x,"')",sep="")) +
      theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"),
            axis.text.x=element_text(size=15),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14)
      )
    
  } else {
    warning <- "Error: x is not numeric of factor."
  }
  print(plot)
  
}


numtarget_graph <- function(df, x, y, barwidth=10, pdf=FALSE, pdfname=NULL) {
  library(reshape2)
  library(ggplot2)
  library(sqldf)
  
  df <- df[, c(x, y)]
  colnames(df) <- c("x","y")
  if (is.factor(df[,c("x")])) {
    
  } else if (is.numeric(df[,c("x")])) {
    
    df[,c("x")] <- as.numeric(df[,c("x")])
    df$xmin <- ave(df$x, cut(df$x,12), FUN=min)
    df$xmax <- ave(df$x, cut(df$x,12), FUN=max)
    df$xCenter <- ave(df$x, cut(df$x,12), FUN=median)
    df2 <-   sqldf(paste("select xCenter, xmin+(xmax-xmin)/2 as xCenter2, avg(x) as avg_x, count(*) as dist, n as total, avg(y) as y_share 
                         from df, (select count(*) as n from df) a 
                         group by xCenter, xmin, xmax order by xCenter"))
    df2$share <- df2$dist/df2$total
    
    plot <- ggplot(data=df2, aes(x=xCenter2, y=share)) + #fill=y_share)) +
      geom_bar(colour="grey", fill='lightgrey', stat="identity", width=barwidth) +
      scale_y_continuous(
        "distribution", 
        sec.axis = sec_axis(~ . * 1.20, name = "..")
      ) +
      geom_smooth(se=FALSE, method='loess', linetype='dotted') +
      geom_line(aes(y=y_share), size=2, stat = "identity", position = "identity", colour="green") +
      xlab(paste(x)) + 
      ylab("Share [0;1]") +

      ggtitle(paste("Average of '",y,"'\n - by '",x,"' \n(incl. distribution of '",x,"')",sep="")) +
      theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"),
            axis.text.x=element_text(size=15),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14)
      )
    
  } else {
    warning <- "Error: x is not numeric of factor."
  }
  print(plot)
  
}

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
    go <- ifelse(is.numeric(df[,x]),TRUE,ifelse(length(unique(df[,x]))<40,TRUE,FALSE))
    if (go==FALSE) inputvars <- inputvars[!inputvars %in% x]
  }
  
  k<-0
  for (x in inputvars){
    #x <- "REV_TOTAL_REV_AMT"
    k<-k+1
    print(paste("working...",k,"/",length(inputvars),":",x))
    df <- na.omit(df0[, c(x, y)])
    colnames(df) <- c("x","y")
    
    if ((is.character(df[,"x"])) | (length(unique(df[,"x"]))<15)) df[,"x"] <- as.factor(df[,"x"])
    
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
      
      barwidth <- 0.7
      
      plot <- ggplot(data=df2, aes(x=xCenter2, y=share)) + #fill=y_share)) +
        geom_bar(colour="grey", fill='lightgrey', stat="identity", width=barwidth) +
        geom_line(aes(y=y_share, x=xCenter2,group=1), size=2, stat = "identity", position = "identity", colour="blue") +
        xlab(paste(x)) + ylab("Share [0;1]") +
        ggtitle(paste("Share of '",y,"' (gini=",format(ginimet, digits=3),"(in glm model))\n - by '",x,"' \n(incl. distribution of '",x,"')",sep="")) +
        theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"),
              axis.text.x=element_text(size=15),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14)
        )
      
      if (nrow(df2)>3) plot <- plot + coord_flip()
      
      print(plot)
    } else if (is.numeric(df[,c("x")])) { # numeric
      
      df[,"xcat"] <- cats(df[,"x"],n=40,method="eq_w")
      test <- data.frame(table(df[,"xcat"]))
      maks <- max(df[df$xcat==test[test$Freq>30,"Var1"],"x"])
      mini <- min(df[df$xcat==test[test$Freq>30,"Var1"],"x"])
      df[,"x"] <- ifelse( df[,"x"]>maks,maks,ifelse(df[,"x"]<mini,mini,df[,"x"]))
      
      barwidth <- (max(df[,"x"])-min(df[,c("x")]))*0.75/14

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
        geom_line(aes(y=y_share), size=2, stat = "identity", position = "identity", colour="blue") +
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

# categorizes numeric variable by either equal n or equal width


# chi2 funktion, der returnerer tabel sorteret efter standardiseret residual
chi2 <- function(df,factvars) {
  library(sqldf)
  c2 <- chisq.test(df[[factvars[1]]], df[[factvars[2]]])
  
  p <- c2$p.value
  xsquared <- c2$statistic
  degF <- c2$parameter
  
  #test$observed
  #test$expected
  tmp <- data.frame(c2$observed, std_res = data.frame(c2$stdres)[,3])
  colnames(tmp)[1:2] <- factvars
  tmp <- tmp[order(-1*tmp$std_res),]
  
  tmp <- sqldf(paste("select a.*, sum1, sum2, sumall
               from tmp a 
                join (select ", factvars[1],", sum(Freq) as sum1 from tmp group by ",factvars[1] ,") b on a.", factvars[1],"=b.", factvars[1]," 
                join (select ", factvars[2],", sum(Freq) as sum2 from tmp group by ",factvars[2] ,") c on a.", factvars[2],"=c.", factvars[2],"                      
                join (select sum(Freq) as sumall from tmp)  on 1=1"))
  tmp$expected <- tmp$sumall*(tmp$sum1/tmp$sumall)*(tmp$sum2/tmp$sumall)
  tmp$residual <- tmp$Freq - tmp$expected
  tmp$std_sq_res <- tmp$residual^2/tmp$expected
  
  out <- list(p = p,xsquared=xsquared, degF=degF, chi2data=tmp)
  return(out)  
}

cv.glmnet.wrap <- function(form,
                           data,
                           type.measure='mse',
                           nfolds=15,
                           alpha=.5, #default = elastic net
                           lambda = NULL,
                           family = "gaussian",
                           standardize = TRUE,
                           intercept=TRUE){
  
  library("glmnet")
  
  if (typeof(form)=="character") {
    warning("Formula was not parsed as formula type. Trying to correct...")
    form <- as.formula(form)
  }
  
  y <- trimws(as.character(form)[2]) 
  xes <- trimws(unlist(strsplit(as.character(form)[3], split="+", fixed=TRUE))) 
  
  if (xes[1]==".") xes <- colnames(data)[!colnames(data) %in% y]
  xform <- as.formula(paste("~",paste(xes,collapse="+")))
  
  df <- data.frame(na.omit(data[,c(y,xes)]))
  
  for (i in colnames(df)) {
    if (is.character(df[[i]])) df[[i]] <- as.factor(df[[i]])
    if (is.logical(df[[i]])) df[[i]] <- as.numeric(df[[i]])
  }

  inputmatrix <- model.matrix(xform,  df[,!colnames(df) %in% y])
  targetvector <- as.matrix(df[[y]])
  
  lambdas <- lambda
  if (is.null(lambdas)) lambdas <-  10^seq(3, -2, by = -.1) 
  
  fit = cv.glmnet(x=inputmatrix, y=targetvector, type.measure=type.measure,lambda=lambdas,
                  nfolds=nfolds,alpha=alpha, family = family,standardize = standardize, intercept=intercept) 
  
  # outputting model formula
  modform <- paste(as.character(form)[2],as.character(form)[1], as.character(form)[3],collapse=" ")
  
  
  # outputting true coefficients in df
  xes <- xes[order(nchar(xes))] # sorting to make sure correct variable is assigned to right coefficients below...
  coeffs <- data.frame(coef.name = dimnames(coef(fit))[[1]][which(coef(fit, s = "lambda.min") != 0)], 
                       coef.value = matrix(coef(fit, s = "lambda.min"))[which(coef(fit, s = "lambda.min") != 0)])
  coeffs$varname <- ""
  for (i in xes) {
    coeffs[grep(i,coeffs$coef.name),"varname"] <- i
  }
  coeffs$factor <- ""
  coeffs$sql <- ""
  for (i in 1:nrow(coeffs)) {
    coeffs[i,"factor"] <- gsub(coeffs[i,"varname"],"",coeffs[i,"coef.name"])
    if (coeffs[i,"coef.name"]=="(Intercept)") {
      coeffs[i,"sql"] <- coeffs[i,"coef.value"]
    } else if (nchar(coeffs[i,"factor"])==0) {
      coeffs[i,"sql"] <- paste(coeffs[i,"coef.value"],"*",coeffs[i,"coef.name"], sep="")
    } else {
      coeffs[i,"sql"] <- paste(coeffs[i,"coef.value"],"*(",coeffs[i,"varname"],"='", coeffs[i,"factor"],"')",sep="")
    }
  }
  
  # outputting model sql form
  sql <- paste(paste(coeffs$sql,collapse=" + "),sep="")
  
  if (family == "gaussian") {
    sql <- sql
  } else if (family=="binomial") {
    sql <- paste("1/(1 + exp(-(",sql,")))" ,sep="") 
  }
  
  fit <- list(fit,modform,coeffs,sql)
  names(fit) <- c("fit","modform","coeffsdf","sql")
  out = fit
}



waffle_plot <- function(df,x,y=NULL,maintitle=NULL, subtitle=NULL){
  require(dplyr)
  require(ggplot2)
  
  # summarized stats
  
  colnames(df)[colnames(df) %in% x] <- "group_"
  if (!is.null(y)) colnames(df)[colnames(df) %in% y] <- "numy"
  
  # sums int 2 steps - to collapse groups with lt 1%
  gb <- group_by(df,group_)
  if (is.null(y)) {
    O2 <- summarise(gb,n = n())
  } else {
    O2 <- summarise(gb,n = sum(numy))
  }
  O2 <- mutate(O2,freq=n/sum(n))
  O2 <- O2[order(-O2$freq),]
  O2$group_ <- as.character(O2$group_)
  if (nrow(O2)>11) O2[11:nrow(O2),"group_"] <- "_misc"
  O2[which(O2$freq<0.01),"group_"] <- "_misc"
  
  gb <- group_by(O2,group_)
  O3 <- summarize(gb,freq =sum(freq))
  O3$percent <- round(O3$freq,2)*100
  O3[O3$group_=="_misc","percent"] = 100 - sum(O3[!O3$group_=="_misc","percent"]) #adjusting to ensure percent sums to 100

  
  waffldata <- c(unlist(O3$percent))
  names(waffldata) <- O3$group_
  waffldata <- waffldata[order(-waffldata)]
  
  nrows <- 10
  df2 <- expand.grid(y = 1:nrows, x = 1:nrows)
  df2$category <- factor(rep(names(waffldata), waffldata))  
  
  if (is.null(maintitle)) maintitle <- paste("Distribution of",x) 
  if (is.null(subtitle)) subtitle <- paste("Class of",x) 
  
  ## Plot
  waffl <- ggplot(df2, aes(x = x, y = y, fill = category)) + 
    geom_tile(color = "white", size = 1.5) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
    scale_fill_brewer(palette = "Set3") +
    labs(title=maintitle, subtitle=subtitle,
         caption="Note: largest 11 categories are shown. Rest is collapsed to '_misc' category") + 
    theme(
      #  panel.border = element_rect(size = 2),
      plot.title = element_text(size = rel(1.2)),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.title = element_blank(),
      legend.position = "right")
  return(waffl)
}

gemtekst <- function(text,file) {
  write.table(text,file,row.names=FALSE,col.names=FALSE,quote=FALSE)   
}

checknum <- function(var) {
  any(is.na(as.numeric(as.character(var))))==FALSE
}

# categorizes numeric variable by either equal n or equal width

cats <- function(x,n,method="eq_n") { # method either "eq_n" (same n in each group) or "eq_w" (same width in each group). Default is eq_n.
  library(sqldf)  
  x <- na.omit(x)
  
  if (method=="eq_w") {
    
    width_all <- max(x)-min(x)
    width_i <- width_all/n
    tmp <- data.frame(grp = seq(n))
    tmp$start <- min(x) + (tmp$grp-1)*width_i
    tmp$slut <- min(x) + (tmp$grp)*width_i
    tmp[n,"slut"] <- tmp[n,"slut"] + 1
    tmp$category <- paste("(",format(tmp$start,digits=3),"-",format(tmp$slut,digits=3),")",sep="")
    x2 <- data.frame(x=x)
    out <- sqldf("select x, category from x2 a left join tmp b on 
                 a.x >= b.start 
                 and a.x < b.slut")
  } else {
    
    tmp <- data.frame(x,rankx = ceiling(n*rank(x, ties.method= "first")/length(x)))
    tmp2 <- aggregate(tmp, by=list(tmp$rankx), FUN=min, na.rm=TRUE)[,c("x","rankx")]
    tmp3 <- data.frame(rankx = tmp2$rankx, category=paste("(",tmp2$x,"-",aggregate(tmp, by=list(tmp$rankx), FUN=max, na.rm=TRUE)[[2]],")",sep=""))
    out <- sqldf("select category from tmp a join tmp3 b on a.rankx=b.rankx")
    
  }
  
  return(out$category)
}

rank10 <- function(x,dir) {
  rank10 <- floor(10*rank(x)/length(x))+1
  rank10 <- ifelse(rank10==11,10,rank10)
  return(rank10)
}

runscript <- function(script,con, split=";") {
  sqlsvector <- starsplit(script,split)
  errors <- c()
  for (sql in sqlsvector) {
    #ERROR HANDLING
    possibleError <- tryCatch(
      {
        print(paste("Executing:", substr(sql,1,100)))
        dbSendUpdate(con, sql)
      }
      ,
      error=function(e) {
        e
        print(paste("Oops! --> failed in ",substr(sql,1,100),"....",sep = ""))
        errors <- c(errors,sql)
      }
    )
    
    if(inherits(possibleError, "error")) next
  }
  
  if (length(errors)>0) {
    result <- "Done with errors"
  } else {
    result <- "Done with NO errors"
  }
  out <- list(result,errors)
  names(out) <- c("result","errors")
  
  return(out)
}