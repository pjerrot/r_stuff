####################
#DATA INDLÃ†S + DIVERSE
####################

titanic <- read.table("c:/temp/R_workspace/titanic.txt", sep="\t", header=TRUE)

head(tabel) # viser f?rste r?kker i tabel
colnames(tabel) # lister kolonnenavne i tabel

colnames(DF2)=c('Died','Survived') # S?tter kolonnenavne!

####################
# Diverse funktioner
####################
unique # returnerer unikke værdier fra felt
unique(TitanicSurvival$sex)

which # sætter kriterie på tabel. Returnerer rækker, der opfylder kriterie
TitanicSurvival[which(TitanicSurvival$sex=='female'),]
# virker også uden which - således:
TitanicSurvival[TitanicSurvival$sex=='female',]

#%in%
TitanicSurvival[TitanicSurvival$sex %in% 'female',] # bemærk %in% funktionalitet

str_detect # returnerer position på fundet pattern i string
TitanicSurvival[,str_detect(colnames(TitanicSurvival),'g')] # bemærk at kun variablenavne med g i sig, bliver vist

duplicated(TitanicSurvival$age) # returnerer TRUE/FALSE på om værdi er dublet fra anden række.

which(data.frame(colnames(TitanicSurvival)) %in% 'g')

ifelse # virker på samme måde som IIF i vba.

####################
# ODBC CONNECT
####################

#NYT: Brug DBI pakken i stedet. Connecter direkte! Men vist ikke til oracle

RShowDoc("RODBC", package="RODBC")
library("RODBC")
ch <- odbcConnect("DWH2", uid = "clem_drift_user", pwd = "Tnm4=8Ts")
sqlTables(ch)
# BemÃ¦rk at det kan vÃ¦re nÃ¸dvendigt at vÃ¦lge 32/64 bit version af R i Global Options for at fÃ¥ ODBC tl at virke her

tabel1 <- sqlFetch(ch, "TEMP_JWR_I", max = 10 )

tabel1 <- sqlQuery(ch, paste("select * from TEMP_JWR_I","where rownum<100"))

eller
sql = "select produkt, handset_producent, handset_type, hsdpa_data_rates, kds_segment, anciennitet_uger, age_kat, binding_kategori, 
conzoom_group_name, pbs, nul_bruger, antal_sessions, duration, dl_data_mb, antal_celler, churn from TEMP_JWR_ULTIMATE_MBB where aar_maaned='13_01'" 
tabel1 <- sqlQuery(ch, sql)

sql = "select * from clem_drift_user.jwr_wunderland_daily_mis where rownum<200"

print(tabel1)

# ex med logreg:
titmod <- glm(CHURN ~ HANDSET_TYPE+PBS+NUL_BRUGER+ANTAL_SESSIONS+AGE_KAT, data=tabel1, family=binomial(link="probit"))
tabel1[, c("p")] <- predict(titmod, tabel1, type = "response") # p?h?fter scores i variabel p


####################
# SQL PAKKEN
####################

# Load the package
library(sqldf)

# Use the titanic data set
data(titanic3, package="PASWR")
colnames(titanic3)
head(titanic3)

sqldf('select age, count(*) from titanic3 where age is not null group by age')

library(ggplot2)

DF=sqldf('select age from titanic3 where age != "NA"')
qplot(DF$age,data=DF, geom="histogram")

DF=sqldf('select count(*) total from titanic3 where age=29 group by survived')
DF2=t(DF)

####################
# STAT ting
####################

mean(tabel1$SUSPENSION)
lm(tabel1$CALL_THIS_WEEK ~ tabel1$MIN_THIS_WEEK)

####################
# GRAF ting
####################

library("RODBC")
ch <- odbcConnect("DWH2", uid = "clem_drift_user", pwd = "Tnm4=8Ts")
sql = "select dl_data_mb_kat, antal_celler_grp, antal_sessions, mbb_churn_score from TEMP_JWR_ULTIMATE_MBB where aar_maaned='13_01'" 
tabel1 <- sqlQuery(ch, sql)
plot(tabel1$CALL_THIS_WEEK,tabel1$MIN_THIS_WEEK)

library(ggplot2)
qplot(ANTAL_SESSIONS, # x
MBB_CHURN_SCORE, # y
data=tabel1, # tabel
geom=c("point", "smooth"), # styrer regressions-linie
method="lm", # ogs? til reg-linie
formula=y~x, # ogs? til reg-linie
main="Hovedoverskrift", xlab="x label", ylab="y label",
xlim=c(0,200), ylim=c(0,1) # styrer min og max p? akserne
)

#### Eksempel p? s?jlediagram
library(sqldf)
library(ggplot2)
bob <- sqldf('select 100*round(depth/100,0) as depth, avg(mag) as avg_mag, 
+             count(*) as antal from quakes group by 100*round(depth/100,0)')

ggplot(bob, aes(x=depth, y=antal)) + geom_bar(stat='identity') + stat_smooth(method="lm") + opts(title = "Dybde vs. antal jordsk?lv") + xlab("Dybde")


 
 ####################
# EKSTRA PAKKER
####################
install.packages("")

####################
# BESLUTNINGSTR?er
####################
# 1
install.packages('party')
library(party)
titanic_ctree = ctree(SURVIVED_VALUE ~ AGE+SEX+CLASS, data=titanic)
plot(titanic_ctree, type="simple")
print(titanic_ctree, type="simple")
titanic[, c("p")] <- predict(titanic_ctree, titanic, type = "response") # p?h?fter scores


# 2
install.packages('rpart')
library("rpart")
fit <- rpart(SURVIVED_VALUE ~ CLASS+AGE+SEX, data=titanic, method="class",control=rpart.control(minsplit=30, cp=0.001)) 
plot(fit, uniform=TRUE, main="Dedde ?r en tekst")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
post(fit, file = "c:/temp/tree.ps", title = "Dedde ?r en t?sder")
titanic2 <- titanic
#predict(fit, titanic2, type = c("vector","prob", "class", "matrix"))
fit2 <- predict(fit, titanic2, type = c("prob"))
titanic[, c("P0","P1")] <- predict(fit, titanic, type = c("prob")) # p?h?fter scores

  treemod$frame
  treemod$where
  treemod$call
  treemod$terms
  treemod$method
  treemod$parms
  treemod$control
  treemod$functions
  treemod$control
  treemod$numresp
  treemod$splits
  treemod$variable.importance
  treemod$y
  treemod$ordered

x <- predict(treemod, newdata=inputdata,type = c("prob"))
x2 <- data.frame(x[,2, drop = FALSE])
inputdata$treepred <- x2$X1

####################
# LOGISTISK REGRESSION
####################

TITANIC <- read.table("C:/inetpub/wwwroot/eminer.net/raadata/titanic.txt", sep="\t", header=TRUE)
titmod <- glm(SURVIVED_VALUE ~ CLASS + SEX + AGE, data=TITANIC, family=binomial(link="logit"))
TITANIC[, c("p", "se")] <- predict(titmod, TITANIC, type = "response", se.fit = TRUE)[-3] # DENNE P?H?FTER SCORE P? OPRINDELIGT EL. NYT DATAS?T


####################
# KORRESPONDANCEANALYSE
####################
# Bem?rk at den fejler hvis antalstabellen enten indeholder celler med 0 observationer eller at der er dimensioner p? mindre end 3 kategorier
library("RODBC")
ch <- odbcConnect("DWH2", uid = "clem_drift_user", pwd = "Tnm4=8Ts")
sql = "select dl_data_mb_kat, antal_celler_grp from TEMP_JWR_ULTIMATE_MBB where aar_maaned='13_01' and antal_celler_grp is not null"
tabel1 <- sqlQuery(ch, sql)
tabel2 <- table(tabel1$DL_DATA_MB_KAT,tabel1$ANTAL_CELLER_GRP) # laver antalstabel
library("ca")
csp <- ca(tabel2)
print(csp)
plot(csp)
chisq.test(tabel2)

# EX. MED CORRESPONDENCE ANALYSE MED PLOT...


library(sqldf)
library('tcltk')
library("ca")
library("RODBC")
library(ggplot2)

ch <- odbcConnect("DWH2", uid = "clem_drift_user", pwd = "Tnm4=8Ts")
sql = "select product_name, usage_group2 as usage_group, amt_total from temp_jwr_0 "
tabel1 <- sqlQuery(ch, sql)
tabel2 <- table(tabel1$PRODUCT_NAME,tabel1$USAGE_GROUP) # laver antalstabel

csp <- ca(tabel2)

csp$sdv        # sqrt of eigenvalues
csp$rowcoord[,1:2]   # row coordinates
csp$colcoord[,1:2]   # column coordinates

grp1 <- data.frame(csp$rowcoord[,1:2])
grp1[, c("SUBJ")] <- rownames(tabel2)
grp2 <- data.frame(csp$colcoord[,1:2])
grp2[, c("SUBJ")] <- colnames(tabel2)
grp <- sqldf("select a.*, 'a' as grp from grp1 a 
             UNION 
             select b.*, 'b' as grp from grp2 b")

sql <- "select  X1, X2, a.SUBJ, a.grp, sum_amt
        from grp a
        join 
          (select product_name as obj, 10 as sum_amt from tabel1 group by product_name
                UNION
                select usage_group as obj, round(log(sum(amt_total))) as sum_amt from tabel1 group by usage_group
          ) b on a.SUBJ=obj
        "
grpt <- sqldf(sql)

labels=grpt$SUBJ
Group=grpt$grp

ggplot(grpt, aes(x=X1, y=X2, label=labels)) + 
  geom_point(stat='identity')  + 
  geom_point(aes(size = sum_amt)) +
  labs(title = "Korrespondence-analyse...") +  
  xlab("DIM 1") + ylab("DIM 2") + 
  ylim(-2, 1.5) +
  xlim(-1.8, 1.5) +
  geom_text(hjust=1, vjust=1, size=4, aes(colour=Group))  +
  theme(plot.title = element_text(size = rel(1.5)))


####################
# PMML
####################
install.packages('pmml')
library('pmml')
pmml.glm(titmod, model.name="mbb churb", app.name="Rattle/PMML") # GLM
pmml.rpart(fit, model.name="titanic_rpart_tree", app.name="Rattle/PMML")

eller blot..
saveXML(pmml(mod2),"glmPMMLexample.xml")

####################
# RATTLE
####################
install.packages("rattle")
library(rattle)
rattle()


####################
# DATA MINING
####################

# MODELLERING

library(sqldf)
library('tcltk')
library("ca")
library("RODBC")
library(ggplot2)

ch <- odbcConnect("DWH2", uid = "clem_drift_user", pwd = "Tnm4=8Ts")
sql = "select bonus_id, churn, kds_segment, binding_kategori, anciennitet_kat, frisurf_pakke, frisms, phone_cust_ancien, rabat, sms_mth_rank10, 
amt_offset_rank7, map_segment_desc from TEMP_JWR_CHURNMODEL_OVERSAMP"
tabel1 <- sqlQuery(ch, sql)
model <- glm(CHURN ~ ., tabel1, family=binomial(link="logit"))

summary(model)
save(model, file = "model_test_save.rda")

# SCORING

load("model_test_save.rda")

library(sqldf)
library("RODBC")

ch <- odbcConnect("DWH2", uid = "clem_drift_user", pwd = "Tnm4=8Ts")
sql = "select bonus_id, churn, kds_segment, binding_kategori, anciennitet_kat, frisurf_pakke, frisms, phone_cust_ancien, rabat, sms_mth_rank10, 
amt_offset_rank7, map_segment_desc from TEMP_JWR_CHURNMODEL_OVERSAMP where kds_segment<>'Cybercity'"
tabel2 <- sqlQuery(ch, sql)
SCORES <- predict(model, type="response", newdata = tabel2)
score_data0=data.frame(SCORES)

# Extract the relevant variables from the dataset.
sdata <- subset(tabel2, select=c("BONUS_ID"))
score_data <- cbind(sdata, score_data0)

# Output the combined data.

write.csv(score_data, file="churnscores.csv", row.names=FALSE)

#############
## LASSO/RIDGE/PLASTIC NET regression for variable selektion


## regsubsets version
library("ISLR")
library("leaps")

inputmatrix <- train_mod  ## <- bemærk train_mod er input df
for(i in colnames(inputmatrix)) {
  if (is.numeric(train_mod[,i])==FALSE) {
    inputmatrix[,i] <- NULL
  }
}

test <- regsubsets(binarytarget~., data=inputmatrix, nvmax=200, method="forward")
testsum <- summary(test)
variables <- names(coef(test,which.max(testsum$adjr2)))
variables <- paste(variables,collapse="','")
variables

## glmnet version
library("glmnet")

inputmatrix <- train_mod  ## <- bemærk train_mod er input df
inputnamevector <- c()
for(i in colnames(inputmatrix)) {
  if (is.numeric(train_mod[,i])==FALSE) {
    inputmatrix[,i] <- NULL
  } else if(i!="binarytarget") {
    inputnamevector <- cbind(inputnamevector,c(i))
  }
}

inputmatrix <- na.omit(inputmatrix)
targetvector <- as.matrix(inputmatrix$binarytarget)
inputmatrix$binarytarget <- NULL
inputmatrix <- as.matrix(inputmatrix[,inputnamevector])
fit = cv.glmnet(x=inputmatrix, y=targetvector, type.measure='auc',nfolds=15,alpha=.5, family = "binomial")
c <- coef(fit,s='lambda.min',exact = TRUE)
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables <- paste(variables,collapse="','")

