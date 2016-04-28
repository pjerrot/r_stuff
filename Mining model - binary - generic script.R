#### mining script - binary target

#INITIALIZING
library("RODBC")
library(dplyr)
library(sqldf)
library(stringr)
library(rpart)
library(pmml)
library(rattle)
library("ggplot2")


#### SÆTTER INPUT PARAMETRE #######
#Inputdata

ch <- odbcConnect("DWH", uid = "clem_drift_user", pwd = "Tnm4=8Ts")
sql <- "select BONUS_ID      ,SUBSCRIBER_NO,IMEI_NO,BAN,FULL_DATE,KDS_SEGMENT,PRICE_PLAN_DESC,DD,FAMILIE_ABON,ORIGINAL_INIT_DATE,COMMIT_START_DATE,
CUSTOMER_START_DATE,INITIAL_INACTIVE_DAYS,TILF,ORIGINAL_INIT_DAYS,COMMITMENT_EXPIRY_DAYS,NP_DONOR_OPERATOR,BINDING_KATEGORI,
BINDING_EXPIRATION,BINDING_EXPIRATION_FAMILY,ANCIENNITET_MD,ANCIENNITET_KAT,AGE,AGE_KAT,GENDER,NO_SUSPENSIONS,SUSP_DAYS1,
MIN_THIS_WEEK,CHRG_THIS_WEEK,CALL_STDDEV_PREV_WEEKS,CALL_AVG_PREV_WEEKS,CALL_TOTAL_PREV_WEEKS,MIN_TOTAL_PREV_WEEKS,CHRG_TOTAL_PREV_WEEKS,
CALL_TO_FIXED,NO_USED_B_NUMBER,B_NUMBER_CALL_1,B_NUMBER_CALL_2,NO_USED_CELL,NUM_CALL_1,NUM_CALL_2,CALL_THIS_WEEK_KAT,SMS_CALL_KAT,
ANTAL_CELLER_KAT,MIN_THIS_WEEK_KAT,ERROR_PCT_CELL_1,PRIMARY_CELL_ERROR_PCT_KAT,ANTAL_B_NUMRE_KAT,SMS_CALL,MINUTES_RANK10,SMS_RANK10,
DATAKALD_RANK10,COMP_CRD_LMT,PBS_FLAG,NETWORK1,ANTAL_MBB_SUBS,ANTAL_FAMILY_SUBS,ANTAL_IALT_SUBS,DLR_NAME,LEVEL5_AGENT_SUB_TYPES,
LEVEL4_AGENT_TYPE,LEVEL3_MAJOR_CATEGORIES,LEVEL2_INTERN_EXTERN,SMARTAFTALE,SMARTAFTALE_INSTALLMENTS,SMARTAFTALE_BILLEDTOTAL,
SA_RUNNING_MTHS,SA_EXPIRATION,SA_AROUNDEXPIRE,WIMP,WIMP_ACTIVATED,HANDSET_NAVN,IPHONE,HANDSET_OS,DEVICE_CATEGORY,SMARTPHONE,PHONE_ANCIEN_KAT,
PHONE_ANCIEN,PHONE_CUST_ANCIEN_KAT,PHONE_CUST_ANCIEN,SECONDHAND_PHONE,CR_CALLED,ARPU_EX_IC,IC_COST,AMT_ACCESS_FEE,TOTAL_ACCESS_FEE_DELTA_ABS,
TOTAL_ACCESS_FEE_DELTA_REL,RABAT,BADSTORE,DONOR,NP_EXITCOMMIT_TDCVIC,NP_EXITCOMMIT_TELIA,NP_EXITCOMMIT_TDC,NP_EXITCOMMIT_3,
NP_EXITCOMMIT_TELMORE,NP_EXITCOMMIT_ORANGE,NP_EXITCOMMIT_CBB,NP_EXITCOMMIT_DEB,NP_EXITCOMMIT_M1,NP_EXITCOMMIT_BIBOB,NP_EXITCOMMIT_PQZ,
NP_EXITCOMMIT_DEB2,NP_EXITCOMMIT_CALME,NP_EXITCOMMIT_ONFONE,SMS_CALLS,SSD_MB,MINUTTER,SMS_MTH_RANK10,SSDMB_MTH_RANK10,MIN_MTH_RANK10,
SMS_NULBRUGER,SSD_MB_NULBRUGER,MINUTTER_NULBRUGER,NULBRUGER,DELTA_MIN_IFTPRISPLAN,DELTA_MIN_IFTPRISPLAN_R10,DELTA_MIN_IFTPRISPLAN_PRO,
DELTA_SSDMB_IFTPRISPLAN,DELTA_SSDMB_IFTPRISPLAN_R10,DELTA_SSDMB_IFTPRISPLAN_PRO,OPT_PRICE_PLAN,AMT_OFFSET,AMT_OFFSET_RANK7,
MAP_SEGMENT_DESC,HIT_BY_DANGEROUS_NUM,DANGEROUS_ANUMBER_OWNER,COUNT_DROPPED_CALLS,SHARE_DROPPED_CALLS,D_AMT_REL,D_AMT,PRODUCT_SENIORITY,
DSL,ZERO_ACCESSFEE_INTERAC,PERMISSION,PERM_NEW,PERM_ORIGIN,PERM_NEW_DMP,RYKNING,INKASSO_SAG,BETALINGSAFTALE,RKI,N_NP,DAYS_SINCE_LAST_NP,
AVG_SENIORITY,CURR_SENI_VS_AVGSENI,NPS_UNIT,NPS_VALUE,NPS_VALUE_CHAR,NPS_COMMENT,NPS_DAYS_SINCE_CAT,NPS_ANCIENNITET_IA_CAT,CHURNET 
from temp_jwr_cbb_churnmoddata_rep where tilf<0.2 and full_date between to_date('011015','ddmmyy') and to_date('010116','ddmmyy')"

inputdata0 <- sqlQuery(ch, sql)

# parametre til "optx.mod" funktion 

df <- inputdata0 #dataset (mandatory)
dropvars <- c("HANDSET_NAVN","SUBSCRIBER_NO","IMEI_NO","BAN","FULL_DATE","ORIGINAL_INIT_DATE","CUSTOMER_START_DATE","COMMIT_START_DATE","TILF","SUSP_DAYS1","VEJNAVN","ADR_HOUSE_NO","NAME","POSTNUMMER",
                "COM_CRD_LMT","BYNAVN","FORNAVN","DLR_NAME", "TILF2","HOUSE_NR") # list of variables being excluded from model and var optimization (optional)
includevars <- c("KDS_SEGMENT","PRICE_PLAN_DESC") # list of variables being forced into model and var-optimization (optional)

targetdef <- "CHURNET=1" # defines target (mandatory)
trainshare_var <- 0.3 # sets share of dataset used for variable optimization (optional)
trainshare_mod <- 0.3 # sets share of dataset used for model (optional)
validshare <- 0.3
varoversample <- 0.20 # will attempt to create oversampled dataset for variable optimization (optional)
plotroc = TRUE # angiver om gini kurver skal vises (optional)

# .. parametre slut...

optx.binmod <- function(df,dropvars=NULL,includevars=NULL,targetdef,trainshare_var=NULL,trainshare_mod=NULL,varoversample=NULL,plotroc=FALSE){

  # lave data
    # - vælge variable - afhængig af om dropvars vektor er specificeret
  if (is.null(dropvars)){
    inputvars <- colnames(df)
  } else {
    inputvars <- colnames(df[,!(names(df) %in% dropvars)])
  }
    # - lave hhv. træning-var, træning-mod og validerings-datasæt
  inputdata <- sqldf(paste("select a.*, case when ", targetdef, " then 1 else 0 end as binarytarget from df a", sep="")) # laver binarytarget var ud fra targetdef
  inputdata$random_ <- runif(nrow(inputdata), 0.0, 1.0) # laver tilfældigt tal i hver række som grundlag for efterfølgende datasplit
  inputdata <- sqldf("select * from inputdata order by random_")
  # splitter i train/valid data

  if (is.null(trainshare_var)) { # sætter andel af data, der skal bruges til var optimering
    if (nrow(inputdata)*0.3>10000){
      trainshare_var <- 10000/nrow(inputdata)
    } else {
      trainshare_var <- 0.3
    }
  } 
  df_train_var <- subset(inputdata[,c(inputvars,"random_")], random_ < trainshare_var) # laver data til var optimering

  if (is.null(trainshare_mod)) { # sætter andel af data, der skal bruges til var modellering
    if (nrow(inputdata)*0.4>15000){
      trainshare_mod <- 15000/nrow(inputdata)
    } else {
      trainshare_mod <- 0.4
    }
  } 
  df_train_mod <- subset(inputdata[,c(inputvars,"random_")], (random_ < trainshare_mod+trainshare_var)&(random_ > trainshare_var)) # laver data til var modellering
  
  if (is.null(validshare)) { # sætter andel af data, der skal bruges til var validering
    if (nrow(inputdata)*0.3>10000){
      validshare <- 10000/nrow(inputdata)
    } else {
      validshare <- 0.3
    }
  } 
  df_valid <- subset(inputdata[,c(inputvars,"random_")], (random_ < trainshare_mod+trainshare_var+validshare)&(random_ > trainshare_mod+trainshare_var)) # laver data til var validering
  
    
  df <- df_train_var
  targetdef <- "churnet=1"
  crittypen <- "r2"
  
  dba<-"SQLite"
  includetree <-TRUE
  includeinteractions <-TRUE
  
  # kør optx var algoritme
  sql <- opt.x("r2",targetdef="churnet=1",df=df_train_var,includetree=TRUE,includeinteractions = TRUE)["optx_sql"]
  optx <- opt.x("r2",targetdef,df_train_var)
  df_train_mod <- sqldf(paste(sql,"df_train_mod"))
  # Kør model
  model <- glm(binarytarget ~ .,data=df_train_mod, family=binomial())
  model2 <- step(model)
  # beregn gini værdier og lav evt. roc grafer
  df_valid <- sqldf(paste(sql,"df_valid"))
  gini_value <- gini_curve(df_valid,model,"binarytarget=1",plotroc = TRUE)["gini"] 
  # returner resultater
  
}




if (db=="MySQL"){ # laver MySQL version af concatanering
  df2 <- sqldf(paste("select binarytarget, concat(", optx_stats_hlp$sqlstring[k],",",optx_stats_hlp$sqlstring[l],") as intervar from df",sep=""))
  sqlstringen <- paste("concat(",optx_stats_hlp$sqlstring[k],",",optx_stats_hlp$sqlstring[l],")")
}
if (db=="MSSQL"){
  optx_sql_score <- gsub("||","+",optx_sql_score)
  optx_sql <- gsub("||","+",optx_sql)
}


View(df_train_mod)

summary(model)

df <- df_train_var

df2 <- df
model <- treemod


inputvars <- colnames(inputdata0[,!(names(inputdata0) %in% dropvars)])

test <- sqldf("select KDS_SEGMENT, PRICE_PLAN_DESC, ORIGINAL_INIT_DAYS, AGE, HANDSET_OS, PHONE_ANCIEN, ARPU_EX_IC, CHURNET FROM inputdata0")

sql <- opt.x("r2",targetdef,test)["optx_sql"]
nytabel <- sqldf(paste(sql,"test"))

model <- glm(CHURNET ~ ., data=test, family=binomial())
model2 <- step(model)

gini_curve(test,model2,"CHURNET=1",plotroc=TRUE)["gini"] 

sql

#Train data share
trainshare <- 0.7
#Oversample rate
oversample <- 0.31
#Target variable
#targetvar <- "survived"
#targetcat <- "yes" # sætter den værdi som modellerne skal "skyde" efter
targetdef <- "CHURNET=1"




# Main script

inputdata0 <- sqldf(paste("select a.*, case when ", targetdef, " then 1 else 0 end as binarytarget from inputdata0 a", sep=""))
inputdata0$binarytarget <- NULL

sql <- opt.x("r2",targetdef,inputdata0)["optx_sql"]

inputdata <- inputdata0[, c(inputvars, "binarytarget")]



# datasplit - til hhv. træning og test data
inputdata$random_ <- runif(nrow(inputdata), 0.0, 1.0) # laver tilfældigt tal i hver række som grundlag for efterfølgende datasplit
inputdata <- sqldf("select * from inputdata order by random_")
# splitter i train/valid data
inputdata_train <- subset(inputdata, random_ < trainshare)
inputdata_valid <- subset(inputdata, random_ >= trainshare)

# laver input modeldatasæt, der overholder oversample rate (hvis specificerede oversample-rate > rate0)

if( mean(inputdata$binarytarget) < oversample ) 
{
  sql <- paste("select * from inputdata_train where binarytarget=1", sep="")
  inputdata_train_os <- sqldf(sql)
  
  hlpcount <- nrow(inputdata_train_os)*((1-oversample)/oversample)
  sql <- paste("select * from inputdata_train where binarytarget=0", sep="")
  hlp <- sqldf(sql)
  
  hlp <- hlp[sample(nrow(hlp), hlpcount, replace = FALSE, prob = NULL),]
  inputdata_train_os <- sqldf("Select * from inputdata_train_os UNION ALL select * from hlp")
  
} else {
  inputdata_train_os <- inputdata_train
} 
rm(hlp)


#Building models...

#Decision Tree 
  # Build the Decision Tree model.
  treemod <- rpart(binarytarget ~ .,
                   data=inputdata_train_os_reg[, c(inputvars, "binarytarget")],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0))
  
  
  
  treemod$splits
  
  #valid
  gini_train_tree <- gini_curve(inputdata_train_os,treemod,"binarytarget")["gini"] 
  gini_valid_tree <- gini_curve(inputdata_valid,treemod,"binarytarget")["gini"] 
  print(paste("Tree GINI coefficients. Train data:",gini_train_tree,"; Validation data:", gini_valid_tree, sep=""))
  
  # Export model
  saveXML(pmml(treemod),"c:/temp/treemodel.xml")


#Logistic regression model  
  
  inputdata_train_os_reg <- na.exclude(inputdata_train_os[, c(inputvars, "binarytarget")]) 
  
  logregmodel <- glm(binarytarget ~ .,data=inputdata_train_os_reg,family=binomial())

  summary(logregmodel)
  
  #exporting model to pmml
  saveXML(pmml(logregmodel),"C:/Temp/Telenor_temp/logreg_os_reg_glm.xml")

  #valid

  gini_train <- gini_curve(inputdata_train_os,logregmodel,"binarytarget")["gini"] #train data
  gini_valid <- gini_curve(inputdata_valid,logregmodel,"binarytarget")["gini"] #validerings-data
  print(paste("Logreg GINI coefficients. Train data:",gini_train,"; Validation data:", gini_valid, sep=""))
  
