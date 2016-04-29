#INITIALIZING
library("RODBC")
library(dplyr)
library(sqldf)
library(stringr)
library(rpart)
library(plyr)
library(rattle)
library("ggplot2")
library("psych")
library("glmnet")


# Mangler...:
## Udvide opt.x funktion med sql db input:
#1: "SQLite"     CASE WHEN=OK; EXP="EXP";  POWER="POWER"; BETWEEN=OK; CONCATENATION="||"
#2: "Oracle"     CASE WHEN=OK; EXP="EXP";  POWER="POWER"; BETWEEN=OK; CONCATENATION="||"
#3: "SQL Server" CASE WHEN=OK; EXP="EXP";  POWER="POWER"; BETWEEN=OK; CONCATENATION="||"
#4: "IBM DB2"    CASE WHEN=OK; EXP="EXP";  POWER="POWER"; BETWEEN=OK; CONCATENATION="CONCAT(A,B)" 
#5: "MySQL"      CASE WHEN=OK; EXP="EXP";  POWER="POWER"; BETWEEN=OK; CONCATENATION="CONCAT(A,B)"
#6: "PostgreSQL" CASE WHEN; EXP; COS; POWER; BETWEEN; CONCATENATION="?"
#7: "Teradata"   CASE WHEN=OK; EXP; COS; POWER; BETWEEN; CONCATENATION="||"
#8: "Other" ?;?;?;?;?

##    - konkataneringer
##    - begrænse størrelse af sql hvis oracle db
##    - udvide funktion retur med native sql. Retur både native og sqlite version. Således at der kan laves modeldata i R OG også laves en native SQL 
## vartypen er ikke korrekt. I visse tilfælde angives CLASS hvor det rent faktisk er numerisk.

# function to calculate and return r-square of linear model
calc.r2 <- function(y='binarytarget',x,df = NULL){
  modtext <- paste("tmpmod <- glm(binarytarget ~",x,", data=df, family=gaussian)")
  eval(parse(text=modtext))
  result <- (1 - tmpmod$deviance/tmpmod$null.deviance)
  return(result)
}

imp.mean <- function(x) {
  a <- 1.0*mean(as.numeric(x), na.rm = TRUE)
  return (ifelse (is.na(x) == TRUE , a, x)) 
} 


opt.x <- function(crittypen="r2",targetdef,df,db="SQLite", includetree=TRUE, includeinteractions=TRUE) {
  
  options(warn=-1)
  
  df <- sqldf(paste("select a.*, case when ", targetdef, " then 1 else 0 end as binarytarget from df a", sep=""))
  # Please note that I use sql many places - since syntax is often simpler. Above single sql line does same as..
      # targetdef <- paste('df$',targetdef,sep="")
      # dostring <- paste("df$binarytarget <- ifelse(",targetdef, ", 1, 0)")
      # eval(parse(text=dostring))
  
  #remove vars that are completely NAs
  includevars <- c()
  for (i in 1:ncol(df)) {
    if (nrow(df)!=sum(is.na(df[,i]))) {
      includevars <- c(includevars,colnames(df)[i])
    }
  }
  df <- df[,includevars]
  
  
  targetvar <- substr(targetdef,1,regexpr('=', targetdef)-1)
  df[targetvar] <- NULL
  
  df$random_ <- NULL
  
  # VAR TYPE AND NUMBER OF DISTINCT VALUES ARE EXTRACTED FOR ALL VARS IN TABLE -> tabel2
  tabel2 <- data.frame()
  for(i in colnames(df)) {  
    tabel2[1,i] <- length(unique(df[,i]))
  }
  df <- df[,colnames(tabel2[1,tabel2[1,]>1])] #fjerner vars uden varians
  
  # table created to store variable selection criteria for all tested variables
  optx_stats <- data.frame(varname=character(0), #name of variable
                           opttype=character(0), #optimization type: charsplit, numsplit, asis, x2, x3, logx 
                           crittype=character(0), #optimzation criteria: chisq, r2, gini
                           critvalue=numeric(0), #value of optimzation criteria
                           newname=character(0), #new name
                           sqlstring=character(0), 
                           vartype=character(0), stringsAsFactors=F)
  
  # udviddet impute (lineær)
  missvars_bin <- c()
  nomissvars <- c()
  missvars_num <- c()
  missvars_mul <- c()
  for (k in colnames(tabel2[1,tabel2[1,]>1])) {
    varnamet <- k
    if (sum(is.na(df[,k]))>100) {
      if (tabel2[1,k]==2) {
        missvars_bin <- c(missvars_bin,varnamet)
      } else if (is.numeric(df[,k])){
        missvars_num <- c(missvars_num,varnamet)
      } else if (tabel2[1,k]<20) {
        missvars_mul <- c(missvars_mul,varnamet)
      }
    } else if (sum(is.na(df[,k]))<10) 
    {
      if ((tabel2[k]>15)&(is.numeric(df[,k])==FALSE)) 
      {
        # do nothing 
      } else 
      {
        nomissvars <- c(nomissvars,varnamet)
      }
    }
  }
  
  nomissvars <- nomissvars[!nomissvars %in% c('binarytarget')]
  
  for (k in missvars_num) {
    varnamet <- colnames(df[k])
    tmpmod_txt <- paste("tmp_mod <- glm(",k,"~.,data=df[,c(nomissvars,'",k,"')],family=gaussian)",sep="")
    eval(parse(text=tmpmod_txt))
    opttypen <- 'ASIS'
    sqlstringen <- paste("case when ", k ," is null then (",glm_to_sql(tmp_mod), ") else ",colnames(df[k])," end", sep="")
    tmp <- sqldf(paste("select binarytarget,",sqlstringen," as input from df"))
    tmp_mod <- glm(binarytarget ~input, data=tmp, family=binomial())
    critvaluen <- (1 - tmp_mod$deviance/tmp_mod$null.deviance)
    newnamet <- paste(k,"_IMP2",sep="")
    vartypen <- "NUM"
    optx_stats[nrow(optx_stats)+1,] <- c(varnamet,opttypen, crittypen, critvaluen, newnamet, sqlstringen, vartypen)
  }
  
  for (k in missvars_bin) {
    varnamet <- colnames(df[k])
    tmpmod_txt <- paste("tmp_mod <- glm(",k,"~.,data=df[,c(nomissvars,'",k,"')],family=binomial)",sep="")
    eval(parse(text=tmpmod_txt))
    opttypen <- 'ASIS'
    sqlstringen <- paste("case when ", k ," is null then (",glm_to_sql(tmp_mod), ") else ",colnames(df[k])," end", sep="")
    tmp <- sqldf(paste("select binarytarget,",sqlstringen," as input from df"))
    tmp_mod <- glm(binarytarget ~input, data=tmp, family=binomial())
    critvaluen <- (1 - tmp_mod$deviance/tmp_mod$null.deviance)
    newnamet <- paste(k,"_IMP2",sep="")
    vartypen <- "NUM"
    optx_stats[nrow(optx_stats)+1,] <- c(varnamet,opttypen, crittypen, critvaluen, newnamet, sqlstringen, vartypen)
  }
  
  # CHEKKING ALL VARS AND HANDLE THEM ACCORDING TO ATTRIBUTE RULES
  for(i in colnames(df)) {

    varnamet <- colnames(df[i])
    
    if (is.numeric(df[,i])) {
      
      varmean <- 1.0*mean(as.numeric(df[,i]), na.rm = TRUE)
      varsd <- 1.0*sd(as.numeric(df[,i]), na.rm = TRUE)
      
      if (max(is.na(df[,i])) == 1) {
        
        #Laver laver NY variabel, der markerer missing values i inputvar
        opttypen <- 'DUMMY'
        test <- sqldf(paste("select binarytarget, case when ",i," is null then 1 else 0 end as dummy from df"))
        model <- glm(binarytarget ~ ., data=test, family = binomial)
        critvaluen <- (1 - model$deviance/model$null.deviance)
        sqlstringen <- paste("case when ",colnames(df[i])," is null then 1 else 0 end",sep="")
        #critvaluen <- calc.r2("binarytarget",paste(colnames(df[i]),"_NA", sep=""),df)
        newnamet <- paste(colnames(df[i]),"_NA",sep="")
        vartypen <- "NUM"
        optx_stats[nrow(optx_stats)+1,] <- c(paste(colnames(df[i]), sep=""),opttypen, crittypen, critvaluen, newnamet, sqlstringen, vartypen)
        
      }

      #Variable as is... (allthough with imputed values)
      opttypen <- 'ASIS'
      sqlstringen <- paste("case when ",colnames(df[i])," is null then ",varmean, " else ",colnames(df[i])," end", sep="")
      critvaluen <- calc.r2("binarytarget",i,df)
      newnamet <- paste(colnames(df[i]),"_NEW",sep="")
      vartypen <- "NUM"
      optx_stats[nrow(optx_stats)+1,] <- c(varnamet,opttypen, crittypen, critvaluen, newnamet, sqlstringen, vartypen)
      
     

      #justerer outliers
      if (tabel2[i]>100) {
        test <- data.frame(df[,i])
        test$probrank <- ceiling(100*rank(test[,1], ties.method= "min")/nrow(df)) 
        colnames(test) <- c("var","rank")
        test <- sqldf("select min(var) as minvar, max(var) as maxvar from test where rank between 2 and 97")
        replacetxt <- paste("(case when ",colnames(df[i])," is null then ",varmean, " when ",colnames(df[i]),"<",test[1,1]," then ",test[1,1]," when ",colnames(df[i]),">",test[1,2]," then ",test[1,2], " else ",colnames(df[i])," end)", sep="")
      } else {
        replacetxt <- paste("(case when ",colnames(df[i])," is null then ",varmean, " else ",colnames(df[i])," end)", sep="")        
      } 
      
      
      #NUmeric variable squared...
      opttypen <- "X2"
      test <- sqldf(paste("select ",replacetxt,"as øx_, ",replacetxt,"*",replacetxt," as øx2_, binarytarget from df"))
      model <- glm(binarytarget ~ ., data=test, family=binomial)
      critvaluen <- (1 - model$deviance/model$null.deviance)
      modelsql <- glm_to_sql(model)
      sqlstringen <- gsub("øx2_",paste(replacetxt,"*",replacetxt,sep=""),gsub("øx_",replacetxt,modelsql))
      newnamet <- paste(colnames(df[i]),"_XX2",sep="")
      vartypen <- "NUM"
      optx_stats[nrow(optx_stats)+1,] <- c(varnamet,opttypen, crittypen, critvaluen, newnamet, sqlstringen, vartypen)
      
      #NUmeric variable power3
      opttypen <- "X3"
      test <- sqldf(paste("select ",replacetxt,"as øx_, ",replacetxt,"*",replacetxt," as øx2_, ",replacetxt,"*",replacetxt,"*",replacetxt," as øx3_, binarytarget from df"))
      model <- glm(binarytarget ~ ., data=test, family=binomial)
      critvaluen <- (1 - model$deviance/model$null.deviance)
      modelsql <- glm_to_sql(model)
      sqlstringen <- gsub("øx3_", paste(replacetxt,"*",replacetxt,"*",replacetxt,sep="")  ,gsub("øx2_",paste(replacetxt,"*",replacetxt,sep=""),gsub("øx_",replacetxt,modelsql)))
      newnamet <- paste(colnames(df[i]),"_XX3",sep="")
      vartypen <- "NUM"
      optx_stats[nrow(optx_stats)+1,] <- c(varnamet,opttypen, crittypen, critvaluen, newnamet, sqlstringen, vartypen)

      # logit model
      opttypen <- 'LOGIT'
      tmpmodtxt <- paste("tmpmod <- glm(binarytarget ~", colnames(df[i]),", data=df, family=binomial)")
      eval(parse(text=tmpmodtxt))
      tmpsql <- glm_to_sql(tmpmod)
      critvaluen <- (1 - tmpmod$deviance/tmpmod$null.deviance)
      sqlstringen <- paste("(",gsub(colnames(df[i]), replacetxt, tmpsql),")",sep="")
      newnamet <- paste(colnames(df[i]),"_LOGIT",sep="")
      vartypen <- "NUM"
      optx_stats[nrow(optx_stats)+1,] <- c(varnamet,opttypen, crittypen, critvaluen, newnamet, sqlstringen, vartypen)
      
      minvar <- min(na.exclude(df[,i]))
      maxvar <- max(na.exclude(df[,i]))
      checkzero <- max(na.exclude(df[,i]) == 0) #checks zero value in var
      
      # 1/x...
      if(checkzero==0) {
        opttypen <- "DIV1"
        test <- sqldf(paste("select ",replacetxt,"as øx_, power(",replacetxt,",-1) as øx2_, binarytarget from df"))
        model <- glm(binarytarget ~ ., data=test, family=binomial)
        critvaluen <- (1 - model$deviance/model$null.deviance)
        modelsql <- glm_to_sql(model)
        sqlstringen <- gsub("øx2_",paste("power(",replacetxt,",-1)",sep=""),gsub("øx_",replacetxt,modelsql))
        newnamet <- paste(colnames(df[i]),"_DIV1",sep="")
        vartypen <- "NUM"
        optx_stats[nrow(optx_stats)+1,] <- c(varnamet,opttypen, crittypen, critvaluen, newnamet, sqlstringen, vartypen)
      }
      
      #Numeric variable square rooted...
      if(minvar>=0) {
        opttypen <- "SQRT"
        test <- sqldf(paste("select ",replacetxt,"as øx_, power(",replacetxt,",0.5) as øx2_, binarytarget from df"))
        model <- glm(binarytarget ~ ., data=test, family=binomial)
        critvaluen <- (1 - model$deviance/model$null.deviance)
        modelsql <- glm_to_sql(model)
        sqlstringen <- gsub("øx2_",paste("power(",replacetxt,",0.5)",sep=""),gsub("øx_",replacetxt,modelsql))
        newnamet <- paste(colnames(df[i]),"_SQRT",sep="")
        vartypen <- "NUM"
        optx_stats[nrow(optx_stats)+1,] <- c(varnamet,opttypen, crittypen, critvaluen, newnamet, sqlstringen, vartypen)
      }
      
      #NUmeric EXP'ed...
      if ((maxvar<20)&(minvar>-20)) {
        opttypen <- "EXP"
        test <- sqldf(paste("select ",replacetxt," as øx_, EXP(",replacetxt,") as øx2_, binarytarget from df"))
        model <- glm(binarytarget ~ ., data=test, family=binomial)
        critvaluen <- (1 - model$deviance/model$null.deviance)
        modelsql <- glm_to_sql(model)
        sqlstringen <- gsub("øx2_",paste("EXP(",replacetxt,")",sep=""),gsub("øx_",replacetxt,modelsql))
        newnamet <- paste(colnames(df[i]),"_EXP",sep="")
        vartypen <- "NUM"
        optx_stats[nrow(optx_stats)+1,] <- c(varnamet,opttypen, crittypen, critvaluen, newnamet, sqlstringen, vartypen)
      }
      
      if(tabel2[1,i]>7) { #numeric var with many unique values
        
        varnavn <- i
        
        if (varnavn=="binarytarget") {
        } else {
          grpsize <- ceiling(nrow(df)/300)
          if(grpsize>20)
          {
            grpsize <- 20
          } 
          
          d <- sqldf(paste("select ", varnavn, ",", "binarytarget from df order by", varnavn))
          d$RANKVAR <- ceiling(grpsize*rank(d[varnavn], ties.method= "min")/nrow(df))
          
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
          
          sd_target_all <- sd(unlist(df["binarytarget"]))
          mean_target_all <- mean(unlist(df["binarytarget"]))
          count_target_all <- nrow(df)
          
          cat_stats <- data.frame(varname=character(0), #name of variable
                                  n_groups=numeric(0),
                                  critvalue=numeric(0), #value of optimzation criteria
                                  sqlstring=character(0), stringsAsFactors=F)
          
          # ny gruppering: iterativt finde dem, der matcher bedst, grupper og fortsæt indtil 2 grupper tilbage
          while (nrow(gns)>2) {
            
            #laver SQL
            sql <- ""
            for (m in 1:nrow(gns)) {
              #sql <- paste(sql," when ", varnavn, " <= ", gns[m,"maxvar"]," then '",varnavn,"_Grp",m,"_lteq",gns[m,"maxvar"],"'",sep="")
              sql <- paste(sql," when ", varnavn, " <= ", gns[m,"maxvar"]," then ",gns[m,"target_avg"],"",sep="")
            }
            sql <- paste("case ",sql," else ",mean(df[,"binarytarget"])," end ",sep="")
            
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

          d <- sqldf("select a.varname, a.n_groups, a.critvalue, a.sqlstring from cat_stats a 
                     where critvalue > (select max(critvalue)*0.9 from cat_stats) order by a.critvalue")
          newnamet <- paste(varnavn,"_GRP",sep="")
          vartypen <- "CLASS"
          optx_stats[nrow(optx_stats)+1,] <- c(varnavn,"GRP", crittypen, d$critvalue[1], newnamet, d$sqlstring[1], vartypen)
          
        }
      }  
    } else { #..else non numeric from here..
      if ((as.numeric(tabel2[1,i])<6)&(as.numeric(tabel2[1,i])>1))
      {
        opttypen <- 'ASIS'
        sqlstringen <- paste("case when", colnames(df[i]),"is null then 'EQ_NULL' ELSE",colnames(df[i]),"END")
        varnavn <- i
        critvaluen <- calc.r2("binarytarget",i,df)
        newnamet <- paste(varnavn,"_NEW",sep="")
        vartypen <- "CLASS"
        optx_stats[nrow(optx_stats)+1,] <- c(varnavn,opttypen, crittypen, critvaluen, newnamet, sqlstringen, vartypen)
        
        opttypen <- 'ASIS_VALUE'
        test <- sqldf(paste("select ",i,", avg(binarytarget) as gns_target from df group by ",i,sep=""))
        sqlstringen <- "case "
        for (j in 1:nrow(test)) 
        {
          if (is.na(test[j,1])) {
            sqlstringen <- paste(sqlstringen," when ",i," is null then ",test[j,2], sep="")
          } 
          else 
          {
            sqlstringen <- paste(sqlstringen," when ",i,"='",test[j,i],"' then ",test[j,2], sep="")
          }
        }  
        sqlstringen <- paste(sqlstringen,"end")
        test <- sqldf(paste("select ",sqlstringen," as input, binarytarget from df"))
        varnavn <- i
        critvaluen <- calc.r2("binarytarget","input",test)
        newnamet <- paste(varnavn,"_NEW_VALUE",sep="")
        vartypen <- "CLASS"
        optx_stats[nrow(optx_stats)+1,] <- c(varnavn,opttypen, crittypen, critvaluen, newnamet, sqlstringen, vartypen)
        
      } else if (as.numeric(tabel2[1,i])<100)
        { # flere end 6 kategorier

        varnavn <- colnames(df[i])
        
        # Laver initreftable for at gruppere kategorier med n<50 i "other" kategori
        initreftable <- sqldf(paste("select  ",varnavn, "  as var0a, case when count(*) < 50 then 'Other' else ",varnavn, " END AS var0,count(*) as count from df Group by ",varnavn)) 
        
        sql <- paste("select b.var0, avg(a.binarytarget) as target_avg, count(*) as count, stdev(a.binarytarget) as sd_target
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
        
        sd_target_all <- sd(unlist(df["binarytarget"]))
        mean_target_all <- mean(unlist(df["binarytarget"]))
        count_target_all <- nrow(df)
        
        cat_stats <- data.frame(varname=character(0), #name of variable
                                n_groups=numeric(0),
                                critvalue=numeric(0), #value of optimzation criteria
                                sqlstring=character(0), stringsAsFactors=F)
        
        # ny gruppering: iterativt finde dem, der matcher bedst, grupper og fortsæt indtil 4 grupper tilbage
        sql <- ""
        while (nrow(gns)>2) {
          
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
            sql <- paste(sql," when ", varnavn, " in ('", gns[i,"var0"],"') then ",gns[i,"target_avg"]," ",sep="")
          }
          sql <- paste("case ",sql," else ",mean(df[,"binarytarget"])," end",sep="")
          
        }  
        cat_stats[nrow(cat_stats)+1,] <- c(varnavn,nrow(gns),sum(gns[7])/(sd_target_all*sd_target_all*count_target_all), sql)
        
        d <- sqldf("select a.varname, a.n_groups, a.critvalue, a.sqlstring from cat_stats a 
                   where critvalue > (select max(critvalue)*0.95 from cat_stats) and varname<>'binarytarget' order by a.critvalue")
        newnamet <- paste(varnavn,"_GRP",sep="")
        vartypen <- "CLASS"
        optx_stats[nrow(optx_stats)+1,] <- c(varnavn,"GRP", crittypen, d$critvalue[1], newnamet, d$sqlstring[1], vartypen)
      }  
    }
  }
  # SLUT PÅ ENKELT EFFEKTER
  
  # Forberedelser til interaktionseffekter: laver første del af sql (til nested sql)
  
  #targetvar <- substr(targetdef,1,regexpr('=', targetdef)-1)
  
  optx_stats$critvalue <- as.numeric(optx_stats$critvalue)
  
  optx_stats <- ddply(optx_stats,.(varname),transform,Order_n = rank(critvalue,ties.method = "first"))
  
  optx_stats <- sqldf(paste("select a.* 
                            from optx_stats a 
                            where a.varname <> 'binarytarget' and a.critvalue<1
                            and sqlstring is not null and sqlstring<>'' and upper(varname) <> '",toupper(targetvar),"'
                            order by critvalue desc
                            ", sep=""))
  
  select_best_n<-nrow(optx_stats)

  #tmptable <- sqldf(sql0)  # laver tabel med fundne enkelteffekter
  #... finder de kategoriske variable der er mest signifikante 
  optx_stats_hlp <- sqldf("select * from optx_stats where critvalue < 1 and critvalue > 0 and opttype <>'IA' and vartype='CLASS' order by critvalue desc")
  
  if (nrow(optx_stats_hlp)>20){
    nvars <- 20
  } else { 
    nvars <- nrow(optx_stats_hlp)
  }
  
  if (includeinteractions==TRUE){
  #interaktionseffekter...
  
  if ((nrow(optx_stats_hlp)>1)&(nrow(df)>400)) 
  {
    for (k in 1:(nvars-1)) {
      for (l in (k+1):nvars) {
        opttypen <- 'IA'
        df2 <- sqldf(paste("select binarytarget, ", optx_stats_hlp$sqlstring[k],"||",optx_stats_hlp$sqlstring[l]," as intervar from df",sep=""))
        sqlstringen <- paste(optx_stats_hlp$sqlstring[k],"||",optx_stats_hlp$sqlstring[l])

        varnavn <- paste(optx_stats_hlp$varname[k],"*",optx_stats_hlp$varname[l])
        critvaluen <- calc.r2("binarytarget","intervar",df2)
        newnamet <- paste(optx_stats_hlp$varname[k],"_",optx_stats_hlp$varname[l],"_",opttypen,sep="")
        vartypen <- 'CLASS'
        
        # Laver initreftable for at gruppere kategorier med n<50 i "other" kategori
        initreftable <- sqldf(paste("select intervar as var0a, case when count(*) < 50 then 'Other' else intervar END AS var0,count(*) as count from df2 Group by intervar")) 
        
        sql <- paste("select b.var0, avg(a.binarytarget) as target_avg, count(*) as count, stdev(a.binarytarget) as sd_target
                     from df2 a
                     join initreftable b on a.intervar = b.var0a
                     group by b.var0
                     order by target_avg", sep="")
        gns <- sqldf(sql)
        
        gns[,5] <- 0
        colnames(gns)[5] <- "sd_common"
        
        gns[,6] <- 0
        colnames(gns)[6] <- "sd_sd_common"
        
        gns[,7] <- 0
        colnames(gns)[7] <- "SAKyhi"
        
        sd_target_all <- sd(unlist(df2["binarytarget"]))
        mean_target_all <- mean(unlist(df2["binarytarget"]))
        count_target_all <- nrow(df2)
        
        cat_stats <- data.frame(varname=character(0), #name of variable
                                n_groups=numeric(0),
                                critvalue=numeric(0), #value of optimzation criteria
                                sqlstring=character(0), stringsAsFactors=F)
        
        # ny gruppering: iterativt finde dem, der matcher bedst, grupper og fortsæt indtil 2 grupper tilbage
        sql <- ""
        while (nrow(gns)>2) {

          for (m in 1:nrow(gns)) {
            if(m==1)
            {
              gns[m,5] <- NA
              gns[m,6] <- NA
              gns[m,7] <- gns[m,3]*(gns[m,2]-mean_target_all)*(gns[m,2]-mean_target_all)
            } else {    
              gns[m,5] <- sqrt(((gns[m-1,3]-1)*gns[m-1,4]*gns[m-1,4] + (gns[m,3]-1)*gns[m,4]*gns[m,4])/(gns[m-1,3]+gns[m,3]-2))  
              gns[m,6] <- ((gns[m,4]-gns[m,5])^2 + (gns[m-1,4]-gns[m,5])^2)/2 # beregner "standardafvigelse mellem de to stds og den fælles stdev
              gns[m,7] <- gns[m,3]*(gns[m,2]-mean_target_all)*(gns[m,2]-mean_target_all)
            }
          }
          
          #print(paste(i,sum(gns[7])/(sd_target_all*sd_target_all*count_target_all)))
          
          gns <- mutate(gns, diffrank = rank(sd_sd_common, ties.method = "first"))
          
          for (m in 1:nrow(gns)) {
            if(gns[m,"diffrank"]==1)
            {
              gns[m-1,"var0"] <- paste(gns[m,"var0"],"','",gns[m-1,"var0"],sep="")  
              gns[m-1,"target_avg"] <- (gns[m,"target_avg"]*gns[m,"count"] + gns[m-1,"target_avg"]*gns[m-1,"count"])/(gns[m,"count"]+gns[m-1,"count"]) 
              gns[m-1,"count"] <- gns[m,"count"]+gns[m-1,"count"]
              gns[m-1,"sd_target"] <- gns[m,"sd_common"]
            }
          }
          
          cat_stats[nrow(cat_stats)+1,] <- c(varnavn,nrow(gns),sum(gns[7])/(sd_target_all*sd_target_all*count_target_all), sql)
          
          gns <- sqldf("select * from gns where diffrank<>1") #fjerner diffrank=1 efter at denne er slået sammen med rækken ovenover.
          
          #laver SQL
          sql <- ""
          for (m in 1:nrow(gns)) {
            #sql <- paste(sql," when (", sqlstringen, ") in ('", gns[m,"var0"],"') then 'Grp_",m,"'",sep="")
            sql <- paste(sql," when (", sqlstringen, ") in ('", gns[m,"var0"],"') then ", gns[m,"target_avg"], " ",sep="")
          }
          #sql <- paste("case ",sql," else 'GRP_NA' end",sep="")
          sql <- paste("case ",sql," else ", mean(df[,"binarytarget"]) ," end",sep="")
          
        }  
        cat_stats[nrow(cat_stats)+1,] <- c(varnavn,nrow(gns),sum(gns[7])/(sd_target_all*sd_target_all*count_target_all), sql)
        
        d <- sqldf("select a.varname, a.n_groups, a.critvalue, a.sqlstring from cat_stats a 
                   where critvalue > (select max(critvalue)*0.95 from cat_stats) and varname<>'binarytarget' order by a.critvalue")
        vartypen <- "CLASS"
        optx_stats[nrow(optx_stats)+1,] <- c(varnavn,"IA", crittypen, d$critvalue[1], newnamet, d$sqlstring[1], vartypen,0)
      }  
      
      # optx_stats[nrow(optx_stats)+1,] <- c(varnavn,opttypen, crittypen, critvaluen, newnamet, sqlstringen, vartypen)
    }
  }
  
  ## interaction SLUT ##

  ## TREE
    tryCatch(
      {
        if (includetree==TRUE){ 
          df_exclude <- c()
          for(k in colnames(df)) {
            if ((is.numeric(df[,k])==FALSE)&(as.numeric(tabel2[1,k])>100)) {
              df_exclude <- cbind(df_exclude,k)
            }
          }
          df2 <- df[,colnames(df[,!(names(df) %in% df_exclude)])] #removes category vars with more than 100 categories
          #targetvar <- substr(targetdef,1,regexpr('=', targetdef)-1)
          df2[targetvar] <- NULL
          
          optx_tree <- rpart(binarytarget ~ ., data=df2, method="class",control=rpart.control(minsplit=100, cp=0.0001, minbucket = 30)) 
          if (length(capture.output({asRules(optx_tree,compact=TRUE)}))>2) 
          {
            TREESQL <- rpart_to_sql(optx_tree,df2)$sql_value
            df2 <- sqldf(paste("select binarytarget, ",TREESQL," as TREENODE from df"))
            critvalue <- calc.r2("binarytarget","TREENODE",df2)
            optx_stats[nrow(optx_stats)+1,] <- c("MULTIPLE","TREE", crittypen, critvalue, "OPTXTREENODE", TREESQL, "CLASS",1,1)
          }
        }
      }, error=function(e){
        print("Error when building input rpart tree. SQL does not include tree as input variable")
      }
    )
  
  ## end TREE
  
  # INTERAKTIONSEFFEKTER... NUMERIC vs. CATEGORICAL. Denne holder kategoriske variable op mod de numeriske og laver en lineær regression indenfor hver kategori

  #... finder de kategoriske variable der er mest signifikante - som grundlag for 
  optx_stats$critvalue <- as.numeric(optx_stats$critvalue)
  optx_stats_hlp <- sqldf("select * from optx_stats where critvalue < 0.9999 
                          and ((vartype='NUM' and opttype='ASIS' and newname not like '%_IMP2') OR (vartype='CLASS' and opttype='GRP') OR (vartype='CLASS' and opttype='ASIS')) 
                          order by critvalue desc")

  if (nrow(optx_stats_hlp)>30){
    nvars <- 30
  } else { 
    nvars <- nrow(optx_stats_hlp)
  }
  
  if ((nrow(optx_stats_hlp)>1)&(nrow(df)>400))   
  {
    for (k in 1:(nvars-1)) {
      for (l in (k+1):nvars) {
        
        
        
        if ( (optx_stats_hlp[k,"vartype"]!=optx_stats_hlp[l,"vartype"]) ) 
        {

          if ( (optx_stats_hlp[k,"vartype"]=="NUM") ) 
          {
            df2 <- sqldf(paste("select binarytarget, ", optx_stats_hlp$sqlstring[k]," as num_x, ",optx_stats_hlp$sqlstring[l]," as cat_x from df",sep=""))
            catsql <- paste("(",optx_stats_hlp$sqlstring[l],")")
            numsql <- paste("(",optx_stats_hlp$sqlstring[k],")")
          } else {
            df2 <- sqldf(paste("select binarytarget, ", optx_stats_hlp$sqlstring[l]," as num_x, ",optx_stats_hlp$sqlstring[k]," as cat_x from df",sep=""))
            catsql <- paste("(",optx_stats_hlp$sqlstring[k],")")
            numsql <- paste("(",optx_stats_hlp$sqlstring[l],")")
          }

          fitted_models = df2 %>% group_by(cat_x) %>% do(model = glm(binarytarget ~ num_x, data = ., family=gaussian()))
          

          varsql <- "case"
          for (m in 1:length(fitted_models$cat)) {
            if (is.numeric(df2[,"cat_x"])) {
              varsql <- paste(varsql," WHEN ",catsql,"=",fitted_models$cat[[m]]," THEN ", gsub("num_2x",paste("power(",numsql,",2)"), gsub("num_x",numsql,gsub("NA *","0 ",glm_to_sql(fitted_models$model[[m]])) ) ) ,sep="" )
            } else {
              varsql <- paste(varsql," WHEN ",catsql,"='",fitted_models$cat[[m]],"' THEN ", gsub("num_2x",paste("power(",numsql,",2)"), gsub("num_x",numsql,gsub("NA *","0 ",glm_to_sql(fitted_models$model[[m]])) ) ) ,sep="" )
            }
          }
          varsql <- paste(varsql,"END")
          
          tryCatch(
            {
            test <- sqldf(paste("select binarytarget,",varsql,"as zx from df"))
            
            opttypen <- 'IARB' #IARB = InterAction Reg By - regression in by group
            sqlstringen <- varsql
            varnavn <- paste(optx_stats_hlp$varname[k],"*",optx_stats_hlp$varname[l])
            critvaluen <- calc.r2("binarytarget","zx",test)
            newnamet <- paste(optx_stats_hlp$varname[k],"_",optx_stats_hlp$varname[l],"_",opttypen,sep="")
            vartypen <- 'NUM'
            optx_stats[nrow(optx_stats)+1,] <- c(varnavn,opttypen, crittypen, critvaluen, newnamet, sqlstringen, vartypen,0)
            }, error=function(e){
              optx_stats <- sqldf(paste("select * from optx_stats where newname <>'",newnamet,"'",sep=""))
            }
            )
        }
      }  
    }
  }
  }
  ## interaction REGBY SLUT ##
  
  optx_stats$critvalue <- as.numeric(optx_stats$critvalue)
  
  optx_stats <- ddply(optx_stats,.(opttype),transform,Order_n_all = rank(critvalue*-1,ties.method = "first"))
  optx_stats$Order_n_all <- as.numeric(optx_stats$Order_n_all)
  
  optx_stats <- sqldf("select * from (select * from optx_stats where opttype='IA' and Order_n_all<3 and critvalue<0.9999
                      UNION 
                      select * from optx_stats where opttype<>'IA' and critvalue>0.01 and critvalue<0.9999
                      UNION 
                      select * from optx_stats where opttype='ASIS' and critvalue<0.9999
                      ) a 
                      order by critvalue desc")

  optx_stats <- ddply(optx_stats,.(varname),transform,Order_n = rank(critvalue,ties.method = "first"))
  optx_stats$Order_n <- as.numeric(optx_stats$Order_n)

  optx_stats <- sqldf(paste("select * from (
                            select a.* 
                            from optx_stats a 
                            join (select varname, max(Order_n) as maxorder 
                            from optx_stats 
                            group by varname) b on a.varname=b.varname 
                            where maxorder = Order_n and a.varname <> 'binarytarget' and a.critvalue<1
                            and sqlstring is not null and sqlstring<>'' 
                            UNION
                            select a.* 
                            from optx_stats a 
                            where opttype = 'ASIS' and a.critvalue<1
                            and sqlstring is not null and sqlstring<>''
                            and varname <>'random_'
                            ) x
                            order by critvalue desc
                            ", sep=""))
  
  # fjerner de variable der overskrider exp() grænser
  for (v in 1:nrow(optx_stats)) {
    tryCatch(
      {
        optx_stats[v,"sizetest"] <- sqldf(paste("select avg(score) as gns_score from (select ", optx_stats[v,"sqlstring"], " as score from df) a"))
      }, error=function(e){
        optx_stats[v,"sizetest"] <- 0
      }
    )
    
  }

  optx_stats <- optx_stats[is.na(optx_stats[,"sizetest"])==FALSE,]

  # Making optx_sql output SQL
  
  for (l in 1:nrow(optx_stats)) { # komprimerer newnames så de er maks 30 char lang
    v <- c(unlist(strsplit(optx_stats[l,"newname"], split="_")))
    if (nchar(optx_stats[l,"newname"])>29) 
      {
        optx_stats[l,"newname"] <- substr(paste(substr(v,1,4),collapse ="_"),1,30)  
      }
  }

  ## Lasso regression to select model input variables 

  for (m in 1:min(nrow(optx_stats),250)) {
    if(m==1)
    {
      optx_sql <- paste("select ", optx_stats[m,"sqlstring"],"as ",optx_stats[m,"newname"],",") 
    } else if (m==min(nrow(optx_stats),250)) {    
      optx_sql <- paste(optx_sql, optx_stats[m,"sqlstring"],"as ",optx_stats[m,"newname"],", binarytarget FROM df ") 
    } else {
      optx_sql <- paste(optx_sql, optx_stats[m,"sqlstring"],"as ",optx_stats[m,"newname"],",")
    }
  }
  
  train_mod <- sqldf(optx_sql)
  inputmatrix <- train_mod  ## <- bemærk train_mod er input df
  inputnamevector <- c()
  for(i in colnames(inputmatrix)) {
    if (is.numeric(train_mod[,i])==FALSE) {
      inputmatrix[,i] <- NULL
    } else if(i!="binarytarget") {
      inputnamevector <- cbind(inputnamevector,c(i))
    }
  }
  
  tryCatch(
    {
      inputmatrix <- na.omit(inputmatrix)
      targetvector <- as.matrix(inputmatrix$binarytarget)
      inputmatrix$binarytarget <- NULL
      inputmatrix <- as.matrix(inputmatrix[,inputnamevector])
      fit = cv.glmnet(x=inputmatrix, y=targetvector, type.measure='auc',nfolds=15,alpha=.5, family = "binomial",standardize = TRUE) # <- plastic net
      #fit = cv.glmnet(x=inputmatrix, y=targetvector, type.measure='auc',nfolds=15,alpha=1, family = "binomial",standardize = TRUE) # <- lasso
      #fit = cv.glmnet(x=inputmatrix, y=targetvector, type.measure='auc',nfolds=15,alpha=0, family = "binomial",standardize = TRUE) # <- ridge
      c <- coef(fit,s='lambda.min',exact = TRUE)
      inds<-which(c!=0)
      variables<-row.names(c)[inds]
      variables <- paste(variables,collapse="','")
      optx_stats <- sqldf(paste("select a.*, case when newname in ('",variables,"') then 1 else 0 end as lasso from optx_stats a", sep=""))
      optx_stats2 <- sqldf("select * from optx_stats where lasso=1")
    }, error=function(e){
      optx_stats2 <- optx_stats
      print("Error building Lasso/Plastic Net regression for variable selection. Most likely memory problem. Therefor all vars have been included in SQL statement.")
    }
  )

  
  for (m in 1:nrow(optx_stats2)) {
    if(m==1)
    {
      optx_sql <- paste("select ", optx_stats2[m,"sqlstring"],"as ",optx_stats2[m,"newname"],",") 
    } else if (m==nrow(optx_stats2)) {    
      optx_sql <- paste(optx_sql, optx_stats2[m,"sqlstring"],"as ",optx_stats2[m,"newname"],", case when ",targetdef, "then 1 else 0 end as binarytarget FROM ") 
    } else {
      optx_sql <- paste(optx_sql, optx_stats2[m,"sqlstring"],"as ",optx_stats2[m,"newname"],",")
    }
  }
  
  for (m in 1:nrow(optx_stats2)) {
    if(m==1)
    {
      optx_sql_score <- paste("select ", optx_stats2[m,"sqlstring"],"as ",optx_stats2[m,"newname"],",") 
    } else if (m==nrow(optx_stats2)) {    
      optx_sql_score <- paste(optx_sql_score, optx_stats2[m,"sqlstring"],"as ",optx_stats2[m,"newname"]," FROM ") 
    } else {
      optx_sql_score <- paste(optx_sql_score, optx_stats2[m,"sqlstring"],"as ",optx_stats2[m,"newname"],",")
    }
  }

  assign("optx_stats",optx_stats,envir = .GlobalEnv)
  
  print("Important: Resulting SQL may cause error if variables do not have same type in db as they do in R. This may especially be a problem if data is imported from Oracle")
  
  options(warn=0)
  
  outs <-list(optx_sql, optx_sql_score, fit)
  names(outs) <- c("optx_sql", "optx_sql_score","optx_plasticnet_model")
  return(outs)
  }

rpart_to_sql <- function(model,df) {
  library(rattle)
  
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
    
    gc.liftplot <-   ggplot(gc.graphdata, aes(x, y = value, color = Graphs)) + 
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

glm_to_sql <- function(glmmodel) {
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
  
  varcats <- data.frame(varname=character(0), #name of variable
                        category=character(0), stringsAsFactors=F)
  if (length(glmmodel$xlevels)>0) {
    for (i in 1:length(glmmodel$xlevels)) {
      for (j in 1:nrow(data.frame(glmmodel$xlevels[i]))) {
        varcats[nrow(varcats)+1,] <- c(colnames(data.frame(glmmodel$xlevels[i])), data.frame(glmmodel$xlevels[i], stringsAsFactors=F)[j,1] )
      }  
    }
  }  
  
  coeffmatrix <- sqldf("select coeffvalue, coeffname, NULL as xlevel, '' as xlevrowname, '' as sqlstr, varname
                       from modcoeffs a join vartypes b on b.varname = a.coeffname where b.vartype='numeric' 
                       UNION ALL                    
                       select distinct coeffvalue, coeffname, trim(category) as xlevel, '' as xlevrowname, '' as sqlstr, varname
                       from modcoeffs a join varcats b on coeffname = varname || category ")
  coeffmatrix[nrow(coeffmatrix)+1,c("coeffvalue","coeffname")] <- subset(modcoeffs[,c("coeffvalue","coeffname")], coeffname == '(Intercept)')
  coeffmatrix[,"xlevel"] <- gsub("[\r\n]", "", coeffmatrix[,"xlevel"])
  coeffmatrix <- sqldf("select distinct * from coeffmatrix")
  
  for (i in 1:nrow(coeffmatrix)) {
    if(coeffmatrix$coeffname[i] == "(Intercept)") 
    {
      coeffmatrix$sqlstr[i] <- coeffmatrix$coeffvalue[i]
    } else if (is.na(coeffmatrix$xlevel[i]) ) {    
      coeffmatrix$sqlstr[i] <- paste("(",coeffmatrix$coeffvalue[i],"*",coeffmatrix$coeffname[i],")")
    } else {
      coeffmatrix$sqlstr[i] <- paste("(case when ",coeffmatrix$varname[i],"='",coeffmatrix$xlevel[i], "' THEN ",coeffmatrix$coeffvalue[i]," ELSE 0 END)",sep="")
    }
    
    if (i==1){x.sql0 <- coeffmatrix$sqlstr[i]} else {x.sql0 <- paste(x.sql0,"+",coeffmatrix$sqlstr[i])}
  }
  
  if (glmmodel$family$link == "logit") {
    x.sql <- paste("1/(1 + exp(-(",x.sql0,")))")  
  } else if (glmmodel$family$link == "identity") {
    x.sql <- x.sql0
  }
  
  assign("modcoeffs",modcoeffs,envir = .GlobalEnv)
  
  return(x.sql)
}




