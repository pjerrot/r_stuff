####################################################################################################################
# Simulating to find optimal value set of variables - to beat better ranked sites on same keyword
####################################################################################################################

# FINDING OPTIMAL VALUESET
# Save as-is df
# Save as-is df for client pages: BASELINE set
# Using position 1 as GOAL set!! (earlier simulations have tried "free flowing" values but that didnt work)
# Calculate scores for BASELINE, GOAL set - and for each partial variable change (from baseline to goal value)

####################################################################################################################

# OBS: IMPORTANT: THE MODEL SHOULD ONLY USE ACTUAL GOOGLE LIGHTHOUSE MEASUREMENTS - AND NOT THE SCORES SINCE THEY ARE LINEAR TRANSFORMATIONS THEREOF

####################################################################################################################

# As-is df is 'finaldata' df !!!  ####

# Save as-is df for client pages: BASELINE set ####
# Identificerer de relevante inputvars (dem, der kan ændres for klienten)
singleinputs <- inputs[-grep("_2|_dif",inputs)]
sim_baseline_df <- unique(finaldata[finaldata$domain==clientdomain, c("keyword","domain","position","subdomain","url" ,"date","project","market","cost",
                                                                                 "difficulty","volume","ctr_valid","traffic_est","d_traffic_est_1up","d_traffic_est_top",singleinputs)]  )

finaldata_sim <- finaldata[finaldata$domain==clientdomain,]
finaldata_sim$changevar <- "BASELINE" 
tmp <- finaldata[finaldata$position==1,]
tmp$changevar <- "GOAL" 
finaldata_sim <- rbind(finaldata_sim,tmp)

# Changing one variable at a time and adding to finaldata_sim df...
for (kw in unique(finaldata_sim[finaldata_sim$changevar=="BASELINE" ,"keyword" ])){
  goalset <- finaldata_sim[finaldata_sim$changevar=="GOAL" & finaldata_sim$keyword==kw,] 
  baselineset <- finaldata_sim[finaldata_sim$changevar=="BASELINE" & finaldata_sim$keyword==kw,] 
  for (var in singleinputs){
    tmp <- baselineset
    tmp[,var] <- goalset[,var]  
    tmp$changevar <- var
    finaldata_sim <- rbind(finaldata_sim,tmp)
  }  
}  

# Competition url is only position 1 url: GOAL
finaldata2_sim <- finaldata_sim[finaldata_sim$changevar== "GOAL", ] 
colnames(finaldata2_sim) <- paste0(colnames(finaldata2_sim),"_2")

# Creating string for difvars calculation. May be removed in the future if models are created without
difvars <- c()
#vars0 <- grep("backlink_|domain_|content_|lighthouse_",colnames(finaldata),value=TRUE)
vars0 <- grep("backlink_|content_|lighthouse_",colnames(finaldata),value=TRUE)
for (var in vars0) {
  difvars <- c(difvars,paste0("a.",var," - b.",var,"_2 as ",var,"_dif"))
}
difvars <- paste(difvars,collapse=",")

# creating sim dataset with url against url2
modeldata1_sim <- sqldf(paste0("select distinct a.*, b.* ,",difvars,",
                    a.position - b.position_2 as posdif
                    from finaldata_sim a
                    join finaldata2_sim b on a.keyword=b.keyword_2"))

      variable <- "backlink_domainZoneGov"
      keyword <- "connect to remote computer"
      url <- "https://www.teamviewer.com/en-us/info/free-remote-pc/"
      View(modeldata1_sim[modeldata1_sim$keyword== "connect to remote computer" & 
                            modeldata1_sim$url== "https://www.teamviewer.com/en-us/info/free-remote-pc/" ,
                          c("keyword","url","position","traffic_est","volume","predict","posdif","changevar","backlink_domainZoneGov")] )
      
      colnames(modeldata1_sim)

for  (var in colnames(modeldata1_sim)) {
  modeldata1_sim[is.nan(modeldata1_sim[,var]),var] <- -1
}

# SCORING ####
# initialization of h2o and selecting the model
library(h2o)
#install.packages("h2o")

h2o.init(ip = 'localhost', port = 4321, nthreads= -1, max_mem_size = "6g")

usethismodel <- "XGBoost_grid__1_AutoML_20220505_141209_model_1"
saved_model <- h2o.loadModel(usethismodel)

h2o_df_sim <- as.h2o(modeldata1_sim, id="modeldata1_sim")

scores_posdif_sim <- as.data.frame(h2o.predict(object=saved_model,newdata=h2o_df_sim, type="response"))
modeldata1_sim <- cbind(modeldata1_sim,scores_posdif_sim)

# Building output dataset ####

out2 <- sqldf("select distinct a.keyword, c.domain, a.position, a.url, c.date, c.project, c.market, c.volume, c.traffic_est,
                c.d_traffic_est_1up, c.d_traffic_est_top,c.domain_name, c.keyw_cluster_label, a.changevar as variable,
                case
                  when a.changevar like '%content%' then 'Content'
                  when a.changevar like '%lighth%' then 'Lighthouse'
                  when a.changevar like '%backlink%' then 'Backlinks'
                end as variable_category,
              a.predict-b.predict as d_predict 
              from modeldata1_sim a 
              join modeldata1_sim b on a.keyword=b.keyword and 
                (a.url=b.url OR a.changevar in ('BASELINE','GOAL'))
              join finaldata c on a.keyword=c.keyword and a.url=c.url
              where b.changevar='BASELINE' and a.predict-b.predict < 0")

# Adding current (BASELINE) and goal (GOAL) values in for each changevar
for (i in 1:nrow(out2)) {
  if (!out2[i,"variable"] %in% c("BASELINE","GOAL")) { 
    out2[i,"pos_1_url"] <-  
      unique(modeldata1_sim[modeldata1_sim$keyword==out2[i,"keyword"],"url_2"])
    
    out2[i,"variable_value"] <-  
      unique(modeldata1_sim[modeldata1_sim$keyword==out2[i,"keyword"] &
                              modeldata1_sim$url==out2[i,"url"] &
                              modeldata1_sim$changevar=="BASELINE", out2[i,"variable"]])
    
    out2[i,"variable_new_value"] <-  
      unique(modeldata1_sim[modeldata1_sim$keyword==out2[i,"keyword"] &
                              modeldata1_sim$changevar=="GOAL", out2[i,"variable"]]) 
  } 
}

out2$variabel_change_dtraffic_est <- -1 * out2$d_traffic_est_1up * out2$d_predict   
out2 <- out2[order(out2$keyword,out2$url,out2$d_predict),] 

############################################################################################################
# REPEATING to calculate sum effects from categories (Backlinks, Content, Lighthouse, etc...) ####
# .. BUT only using the variables that had a positive effect from first run!
############################################################################################################
#finaldata_sim <- finaldata[finaldata$domain==clientdomain,]
#finaldata_sim$changevar <- "BASELINE" 
#tmp <- finaldata[finaldata$position==1,]
#tmp$changevar <- "GOAL" 
#finaldata_sim <- rbind(finaldata_sim,tmp)

# Changing one variable at a time... (BY CATEGORY)
for (kw in unique(finaldata_sim[finaldata_sim$changevar=="BASELINE" ,"keyword" ])){
  goalset <- finaldata_sim[finaldata_sim$changevar=="GOAL" & finaldata_sim$keyword==kw,] 
  baselineset <- finaldata_sim[finaldata_sim$changevar=="BASELINE" & finaldata_sim$keyword==kw,] 
  bestinputs <- out2[out2$keyword==kw,"variable" ] # the variables from first run that would have positive impact
  bestinputs <- bestinputs[!bestinputs %in% c("GOAL","BASELINE")] 
  
  for (catg in c("backlink","lighthouse","content")){  
    tmp <- baselineset
    for (var in grep(catg,bestinputs, value=TRUE)){
      tmp[,var] <- goalset[,var]  
    }  
    tmp$changevar <- toupper(catg)
    finaldata_sim <- rbind(finaldata_sim,tmp)
  }
}  

# Competition url is only position 1 url: GOAL
finaldata2_sim <- finaldata_sim[finaldata_sim$changevar== "GOAL", ] 
colnames(finaldata2_sim) <- paste0(colnames(finaldata2_sim),"_2")

# creating sim dataset with url against url2
modeldata2_sim <- sqldf(paste0("select distinct a.*, b.* ,",difvars,",
                    a.position - b.position_2 as posdif
                    from finaldata_sim a
                    join finaldata2_sim b on a.keyword=b.keyword_2"))

for  (var in colnames(modeldata2_sim)) {
  modeldata2_sim[is.nan(modeldata2_sim[,var]),var] <- -1
}

h2o_df2_sim <- as.h2o(modeldata2_sim, id="modeldata2_sim")

scores_posdif2_sim <- as.data.frame(h2o.predict(object=saved_model,newdata=h2o_df2_sim, type="response"))
modeldata2_sim <- cbind(modeldata2_sim,scores_posdif2_sim)

to_excelfile(modeldata2_sim,"tmp_modeldata2_sim.xlsx")



    View(modeldata2_sim[modeldata2_sim$keyword=="connect to remote computer" &  
                          modeldata2_sim$variable=="backlink_domainZoneGov" &
                          modeldata2_sim$url== "https://www.teamviewer.com/en-us/info/free-remote-pc/"
                        ,] )
    
    View(modeldata2_sim[modeldata2_sim$keyword=="remote pc" &
                          modeldata2_sim$url== "https://www.teamviewer.com/en-us/" & 
                          modeldata2_sim$changevar=="content_H1_n" 
                        ,c("keyword","url","changevar","predict","content_H1_n","content_H1_n_2")] )
    
    View(modeldata2_sim[modeldata2_sim$keyword=="remote pc" &
                          modeldata2_sim$url== "https://www.teamviewer.com/en-us/" & 
                          modeldata2_sim$changevar=="content_H1_n" 
                        ,] )
    
    
    colnames(modeldata2_sim)

# Building output dataset ####

out3 <- sqldf("select distinct a.keyword, c.domain, a.position, a.url, c.date, c.project, c.market, c.branding_keyword,
              c.volume, c.traffic_est,c.d_traffic_est_1up, c.d_traffic_est_top,c.domain_name, 
              c.keyw_cluster_label, a.changevar as variable,
                case
                  when a.changevar like '%content%' then 'Content'
                  when a.changevar like '%lighth%' then 'Lighthouse'
                  when a.changevar like '%backlink%' then 'Backlinks'
                end as variable_category,
                
              a.predict-b.predict as d_predict 
              from modeldata2_sim a 
              join modeldata2_sim b on a.keyword=b.keyword and a.url=b.url
              join finaldata c on a.keyword=c.keyword and a.url=c.url
              where b.changevar='BASELINE' and a.predict-b.predict < 0")

# Adding current and goal values in for each changevar
for (i in 1:nrow(out3)) {
  if (!toupper(out3[i,"variable"]) %in% c("BASELINE","GOAL","CONTENT","BACKLINK","LIGHTHOUSE")) { 
    out3[i,"variable_value"] <-  
      unique(modeldata1_sim[modeldata1_sim$keyword==out3[i,"keyword"] &
                              modeldata1_sim$url==out3[i,"url"] &
                              modeldata1_sim$changevar=="BASELINE", out3[i,"variable"]])
    
    out3[i,"variable_new_value"] <-  
      unique(modeldata1_sim[modeldata1_sim$keyword==out3[i,"keyword"] &
                              modeldata1_sim$changevar=="GOAL", out3[i,"variable"]]) 
  } 
}

#### test #######
tmp <- unique(modeldata1_sim[modeldata1_sim$keyword==out3[i,"keyword"], ])

unique(finaldata[finaldata$keyword==out3[i,"keyword"] &
                        finaldata$position==1, out3[i,"variable"]])

View(finaldata[finaldata$keyword==out3[i,"keyword"] &
                 finaldata$position==1, ])


tst <- withTimeout({
  keyword_crawler("best free remote desktop software	","https://www.g2.com/categories/remote-desktop/free")  
}, timeout=30, onTimeout="warning")

tst <- withTimeout({
  keyword_crawler("best free remote desktop software	","https://www.teamviewer.com")  
}, timeout=30, onTimeout="warning")
View(tst$keyword_crawl_df )
#out3$variabel_change_dtraffic_est <- -1 * out3$d_traffic_est_1up * out3$d_predict   

variable <- "backlink_domainZoneGov"
keyword <- "connect to remote computer"
url <- "https://www.teamviewer.com/en-us/info/free-remote-pc/"

#### end test #####


for (i in 1:nrow(out3)) {
  out3[i,"variabel_change_dtraffic_est"]   <- calc_traf_est(volume=out3[i,"volume"], 
                                                    pos=max(1,out3[i,"position"] - floor(-1*out3[i,"d_predict"])),
                                                    frompos= out3[i,"position"],
                                                    branded_kw = ifelse(out3[i,"branding_keyword"]==1,TRUE,FALSE))$d_traf_est 
  if (abs(out3[i,"d_predict"])<1) out3[i,"variabel_change_dtraffic_est"] <- -1 * out3[i,"d_traffic_est_1up"] * out3[i,"d_predict"]  
} 
out3 <- out3[order(out3$keyword, out3$d_predict ),] 
out3 <- out3[!out3$variable %in% c("content","lighthouse","backlink"), ]  # may be able to remove this later
# removin to NA recommendationts
out3 <- out3[is.na(out3$variable_value) == is.na(out3$variable_new_value),]  

#View(out3[out3$keyword=="remote pc", ] )
#View(out3[out3$keyword=="remote desktop app", ] )


# OUTPUT 2

output_folder <- "C:\\Users\\johnw\\OneDrive - The North Alliance\\Data Science Ressources\\senoa_files\\"
output_folder <- ""
to_excelfile(out3,paste0(output_folder,projectname,"_",market,"_",daysdate,"_recoms.xlsx"))


h2o.shutdown(prompt = TRUE)



