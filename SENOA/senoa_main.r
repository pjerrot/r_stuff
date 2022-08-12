# 

# Content/process ####
# 0: Initializing
# 1: Generate new keywords
# 2: Get basic keyword info (especially volume) (serpstat)
# 3: Remove low-volume keywords
# 4: Get top 25 urls for each keyword (serpstat)
# 5: Crawl content of all urls - identify presence of keywords etc
# 6: Cluster keywords based on their presence together
# 7: Clustering urls based on the use of keywords
# 8: Retrieve backlink info for each url (serpstat)
# 9: Retrieve google lighthouse (pagespeed) info for each url
# 10: Build model data with content, pagespeed and backlink data. With rankdif as target
# 11: Run model
# 12: Simulate input-data-variation to identify which variables that need to be changed - for each keyword
# 13:...

memory.limit(size = 15000)

# 0: Initialize ####
setwd("F:/snippets/SENOA")
setwd("~/Documents/snippets/SENOA")

rm(list = ls())

options(scipen=999)

library(openxlsx)
library(RSQLite)
library(serpstatr)
library("rjson")
library(rvest)
library(stringr)
library(xml2)
library(httr)
library(sqldf)
library(reshape2)
library(dplyr)
library(quanteda)
library(R.utils)
library(h2o)

source("/home/johnw/Documents/git/r_stuff/SENOA/senoa_functions.r")
source("https://raw.githubusercontent.com/pjerrot/r_stuff/master/pjerrots_mining_functions.R")

# Getting CTR by position estimates ####
ctrs <- read.xlsx("/home/johnw/Documents/git/r_stuff/SENOA/estimated_CTR_by_position.xlsx")
ctrs$position <- as.numeric(ctrs$position)

<<<<<<< HEAD

=======
# serpstat token
api_token <- "...." 
>>>>>>> 715368318c55304ed652640c89ea4cfd8238b3e7

# setting region / marked
regions <- c("g_us","g_uk")

clientname <- "Teamviewer"
clientdomain <- "teamviewer.com"
projectname <- "Teamviewer"
market <- paste(toupper(gsub("g_","",regions)),collapse="_")

daysdate <- gsub("-","",as.character(Sys.Date()))

# - end initialize 

# 1: Generate keywords ####

# Starting with initial set of keywords ####

# Getting initial keywords ####
kws0 <- unique(read.xlsx("TMV US Keywords.xlsx"))
colnames(kws0) <- c("keywords")
kws0 <- c("remote desktop")

keywords_all <- soa_get_keywords(domain=clientdomain, 
                             #keywords <- kws0$keywords ,
                             keywords <- kws0 ,
                             serpstat_api = api_token,
                             serpstat_regions=regions,
                             serpstat_iterations = 1,
                             use_google_suggestions = TRUE,
                             google_suggestion_iterations = 1)

View(keywords_all)


# 2: Get basic keyword info (especially volume) (serpstat) ####
if (exists("kws_info")) rm(kws_info)
kws_info <- soa_get_basic_keyword_info(keywords_all,regions, api_token)
kws_info <- kws_info[,c("keyword","cost","difficulty","region","volume")]

# 3: Remove low-volume keywords ####
kws_info <- kws_info[kws_info$volume>1,] 

# 4: Keyword top 30 domain/url positions ####
# takes quite a long time!
rm(out_positions)

out_positions <- soa_get_toprankings(kws_info ,api_token,top_n=20)


# Estimating traffic

out_positions <- sqldf("select a.*, cost, difficulty, volume, ctr_branded, ctr_nonbranded 
                       from out_positions a
                       left join kws_info b on a.keyword=b.keyword and a.region=b.region
                       left join ctrs c on a.position=c.position")

# setting CTR=0 for position +21
out_positions[is.na(out_positions$ctr_nonbranded),"ctr_nonbranded"] <- 0
out_positions[is.na(out_positions$ctr_branded),"ctr_branded"] <- 0

# Estimating keyword traffic ####
# flagging if branded keyword
# adist("teamviewer","team viwer")
for (i in 1:nrow(out_positions)) {
  out_positions[i,"branding_fuzzy_match"] <- adist(out_positions[i,"keyword"],
                                                             gsub(".com","",out_positions[i,"domain"]))
}

out_positions[,"branding_fuzzy_match_rel"] <- out_positions[,"branding_fuzzy_match"] /
  nchar(gsub(".com","",out_positions[,"domain"]))

out_positions[,"branding_keyword"]  <- ifelse(out_positions[,"branding_fuzzy_match_rel"]<0.4,1,0)
# calculating estimated kw traffic
out_positions[,"ctr_valid"] <- ifelse(out_positions[,"branding_keyword"]==1,
                                                out_positions[,"ctr_branded"],
                                      out_positions[,"ctr_nonbranded"])
out_positions$traffic_est <- as.numeric(as.character(out_positions$volume))*out_positions$ctr_valid/100


tmp <- sqldf("select distinct a.*, 
             b.traffic_est - a.traffic_est as d_traffic_est_1up, 
             c.traffic_est - a.traffic_est as d_traffic_est_top
             from out_positions a 
             left join out_positions b on a.keyword=b.keyword and b.position = a.position-1
             left join out_positions c on a.keyword=c.keyword and c.position = 1")
tmp[is.na(tmp$d_traffic_est_1up),"d_traffic_est_1up"] <- 0
out_positions <- tmp
out_positions <- unique(out_positions)

colnames(out_positions)


# OUTPUT 1 ####

output_folder <- "C:\\Users\\johnw\\OneDrive - The North Alliance\\Data Science Ressources\\senoa_files\\"
output_folder <- "/home/johnw/Documents/snippets/SENOA/output_files/"
to_excelfile(out_positions[,colnames(out_positions)[!colnames(out_positions) %in% c("types","branding_fuzzy_match","branding_fuzzy_match_rel")]],
             paste0(output_folder,projectname,"_",market,"_",daysdate,"_output_1.xlsx"))


urltraffic_sum <- out_positions %>% group_by(url) %>% summarise(sumtraf = sum(traffic_est)) 

# Reducing relevant URLs to those with minimum traffic (OBS: decided agains reducing - to have as much variance as possible in final model)
vip_urls <- urltraffic_sum[urltraffic_sum$sumtraf>200,c("url")]
vip_urls <- vip_urls$url


# 5 Crawl all content #### 

allstrings <- unique(out_positions$keyword)

# crawling the sites to locate any of the "bestwords" or search strings
# please allow for 5-7 seconds for each site!
rm(out) 

# Not crawling pdf's
urls <- out_positions[!out_positions$url %in% grep(".pdf",out_positions$url, value=TRUE) & 
                        out_positions$position<21,"url"]
urls <- unique(urls)
length(urls)

for (i in 1:length(urls)) {
  print(paste("crawling number ",i,"out of ",length(urls),": ",urls[i]))
  kws <- unique(out_positions[out_positions$url==urls[i],"keyword"  ] )
  tryCatch(
    {
      tst <- withTimeout({
        soa_keyword_crawler(kws,urls[i])  
      }, timeout=30, onTimeout="warning")
      
      if (exists("out")) {out <- rbind(out,tst$keyword_crawl_df)} else {out <- tst$keyword_crawl_df}
    }
    ,
    error=function(e) {
      print(e)
    }
  )
}
out <- out[out$keyword %in% unique(out_positions$keyword),] # should be redundant
out <- out[nchar(out$keyword)>1,]
out$in_any_logic_all <- sign(out[,"in_url_logic_all"] + out[,"in_H1_logic_all"] + out[,"in_H2_logic_all"] + 
                               out[,"in_H3_logic_all"] + out[,"in_TITLE_logic_all"] + out[,"in_DESC_logic_all"] + 
                               out[,"in_BODY_logic_all"])

#grep("logic_all",colnames(out),value=TRUE)

out$in_any_logic_any <- sign(out[,"in_meta_logic_any"] + out[,"in_links_logic_any"] + out[,"in_imgs_logic_any"] +
                               out[,"in_url_logic_any"] + out[,"in_H1_logic_any"] + out[,"in_H2_logic_any"] + 
                               out[,"in_H3_logic_any"] + out[,"in_TITLE_logic_any"] + out[,"in_DESC_logic_any"] + 
                               out[,"in_BODY_logic_any"])

content_stats_final <- out
colnames(content_stats_final) <- paste0("content_",colnames(content_stats_final))

# Imputer -1 for længde af titles - in case of missing
for (var in colnames(content_stats_final)) {
  content_stats_final[is.nan(content_stats_final[,var]),var] <- -1
  content_stats_final[is.na(content_stats_final[,var]),var] <- -1
}


rm(out)
save.image(paste0("ws1.RData"))

    # reducing to rows with keyword occuring in any of the objects
    inany <- content_stats_final[content_stats_final$in_any_logic==1,c("keyword","url","in_any_logic")]
    
    kws_count <- inany %>% group_by(keyword) %>% summarise(n_dist = n())
    
    x2 <- inany
    x2 <- data.frame(keyword = unique(out_positions[,"keyword"]))
    x2$dd <- 1
    #x2$text <- x2$keyword
    #x2$doc_id <- x2$url
    x2 <- x2[!x2$keyword %in% stopwords(),]


# 6: Clustering keywords into keyword clusters ####
n_keyw_clusters <- 7
keyw_cluster <- seo_search_clustering(searches=x2$keyword,
                                    clientname = clientname,
                                    method = c("btm"), # currently only BTNM
                                    title = paste("Keyword clusters for ",clientname),
                                    excludewords = NULL,
                                    n_clusters = n_keyw_clusters,
                                    plot_wordclouds_clusters = TRUE,
                                    plots_to_pdf = FALSE,
                                    pdf_file = NULL,
                                    output_to_excel = FALSE,
                                    output_excel_file = paste0(clientname,"_search_string_cluster.xlsx")) 

tmp <- keyw_cluster$searchstring_cluster_assignment
clusterout <- tmp[,c("cluster","sentence","text","keyword","bestp","labels")]
to_excelfile(clusterout,"keyw_cluster_BTM.xlsx", overwrite = TRUE)

# 7: Clustering urls into groups ####
n_site_clusters <- 10
#tmp <- x2[x2$keyword %in% bestwords,]
#tst <- dcast(x2[x2$keyword %in% bestwords,], url ~ keyword, fun.aggregate=length, value.var="in_any_logic")
tst <- dcast(x2, url ~ keyword, fun.aggregate=length, value.var="in_any_logic")
kmeans_urls <- kmeans(tst[,-1],n_site_clusters)
kmeans_urls$size
urls_w_cluster <- data.frame(cbind(url = tst$url, url_cluster=kmeans_urls$cluster))

View(urls_w_cluster[grep(clientdomain,urls_w_cluster$url),])



# 8: Getting backlink info from serpstat ####
# Tager ret lang tid! Ca. 4 sek i gennemsnit pr dom?ne ;) (MEGET hurtigere om aftenen!)
# 24*60*60/4 = 21600 runs pr day

rm(blout)
start_time <- Sys.time()
i <- 0
noresultdoms <- c()
for (region in unique(out_positions$region)){ 
  for (dom in unique(out_positions[out_positions$region==region,"domain"])) {
    i <- i + 1
    print(paste0("Working on ",i,": ",dom, " out of ",
                 length(unique(out_positions[out_positions$region==region,"domain"]))))
    tst <- sst_jw_domain_getBacklinkSummary(api_token=api_token,
                                            se=region,
                                            domain=dom,
                                            api_method_version = 1,
                                            searchType = "domain_with_subdomains") # or "domain"
    if (length(tst$error)==0) {
      tst2 <- data.frame(domain=dom,region=region,tst$data)
      if (exists("blout")) {blout <- rbind(blout,tst2)} else {blout <- tst2}
    } else {
      noresultdoms <- c(noresultdoms,dom)
      print(paste("No result on:",dom))
    }
  }
}

backlink_data <- blout
backlink_data$linktodom_ratio <- backlink_data$backlinks/(1+backlink_data$referring_domains )
colnames(backlink_data) <- paste0("bl_",colnames(backlink_data))
rm(blout)
end_time <- Sys.time()
bl_time <- end_time - start_time

save.image(paste0("ws2.RData"))


# 9: Google Lighthouse (pagespeed) ####

# Google lighthouse runs on each url (there is actually a lot of variance in scores for each url under a domian) ###
# Tager laaaang tid! Appr. 20 secs per run! ;)
# 24*60*60/20 = 4320 runs pr day

start_time <- Sys.time()
<<<<<<< HEAD
=======
google_api_key <- "..."
rm(lightsout)
i <- 0
>>>>>>> 715368318c55304ed652640c89ea4cfd8238b3e7


rm(lightsout)
rm(out_)
run_these_urls <- unique(out_positions[out_positions$position<21,"url"])
run_these_urls <- paste0("https://www.",unique(out_positions[out_positions$position<21,"domain"]))

lightsout <- soa_google_chrome_ux_report(urls=run_these_urls,apikey=google_api_key, friendly=TRUE)

lightsout <- soa_google_chrome_ux_report(urls=c("https://www.almbrand.dk","http://tryg.dk"),
                                         apikey=google_api_key, friendly=TRUE)

tst <- soa_google_chrome_ux_report(urls=c("https://sepatec.dk/billig-forsikring"),
                                   apikey=google_api_key, friendly=TRUE)
  
# retry on those that failed...
run_these_urls2 <- run_these_urls[!run_these_urls %in% lightsout$url]
lightsout2 <- soa_google_chrome_ux_report(urls=run_these_urls2,apikey=google_api_key, friendly=TRUE)
lightsout <- bind_rows(lightsout,lightsout2)
rm(lightsout2)
colnames(lightsout) <- paste0("lh_",colnames(lightsout))

save.image(paste0("ws2.RData"))
load("ws2.RData")

# 10: Build model data with content, pagespeed and backlink data. With rankdif as target ####

# Samler salaten til model data ####

#Content: content_stats_final
#Backlinks: backlink_data
#lighthouse: lightsout
#positioner (target): out_positions_top20_tst

out_positions_top20_tst <- out_positions[out_positions$position<21,]
for (i in 1:ncol(out_positions_top20_tst)) {
  if (is.numeric(out_positions_top20_tst[,i])) out_positions_top20_tst[,i] <- as.numeric(as.character(out_positions_top20_tst[,i]))
}

# OBS: find ud af hvorfor der er dubletter i out_position_top20...!!!
out_positions_top20_tst$types <- NULL 
out_positions_top20_tst <- unique(out_positions_top20_tst)

finaldata <- sqldf(paste0("select distinct a.*,
                  -- Lighthouse/pagespeed
                  b.*, case when b.lh_url is null then 1 else 0 end as lh_miss,
                  -- Content/crawl
                  d.*, case when d.content_url is null then 1 else 0 end as content_miss,
                  -- Backlinks
                  e.*, case when e.bl_domain is null then 1 else 0 end as bl_miss
                  from out_positions_top20_tst a
                  left join lightsout b on b.lh_url = a.url
                  left join content_stats_final d on d.content_url=a.url and a.keyword = d.content_keyword
                  left join backlink_data e on e.bl_domain = a.domain and a.region=bl_region
             "))
finaldata$date <- todaysdate
finaldata$project <- projectname

# Creating a variable to see if google favors their own sites
finaldata$is_google_domain <- as.numeric(grepl("google.com",finaldata$domain ))

# removing non-numeric vars
finaldata[,c("lh_url","lh_date","content_keyword","content_url","bl_domain","bl_region")] <- NULL

#checking number of missing values per var
for (var in colnames(finaldata)) {
  print(paste(var,":",length(which(is.na(finaldata[,var])))))
}

# OUTPUT 2 (finaldata df) ####

output_folder <- "C:\\Users\\johnw\\OneDrive - The North Alliance\\Data Science Ressources\\senoa_files\\"
output_folder <- "/home/johnw/Documents/snippets/SENOA/output_files/"
to_excelfile(finaldata,
             paste0(output_folder,projectname,"_",market,"_",daysdate,"_output_2_finaldata.xlsx"))

#finaldata <- read.xlsx("/home/johnw/Documents/snippets/SENOA/output_files/Forsikring_DK_20220805_output_2_finaldata.xlsx")

# starting modelling ####

# Creating opponents dataset
finaldata2 <- finaldata
colnames(finaldata2) <- paste0(colnames(finaldata2),"_u2")

difvars <- c()
vars0 <- grep("bl_|content_|lh_|is_google",colnames(finaldata),value=TRUE)
for (var in vars0) {
  difvars <- c(difvars,paste0("a.",var," - b.",var,"_u2 as ",var,"_dif"))
}
difvars <- paste(difvars,collapse=",")

# creating model dataset with url against url2
modeldata1 <- sqldf(paste0("select a.*, b.* ,",difvars,",
                    -- case when a.position < b.position_u2 then 1 else 0 end as target_pos1_on_top,
                    a.position - b.position_u2 as posdif
                    from finaldata a 
                    join finaldata2 b on a.keyword=b.keyword_u2 and a.region=b.region_u2
                           and a.url<>b.url_u2")) # change: include 0 posdif observations

target <- "posdif"

set.seed(2222)
samplesize <- 200000
samplesize <- -1
if (samplesize==-1){
  modeldata1_sample <- modeldata1
} else{
  modeldata1_sample <- modeldata1[order(runif(nrow(modeldata1))),][1:samplesize,]
}  


#modeldata1 <- na.omit(modeldata1) # undecided as to whether or not to do this...

# IDENTIFYING INPUT VARIABLES AS ALL VARIABLES RELATED TO BACKLINK, CONTENT AND LIGHTHOUSE
inputs <- grep("bl_|content_|lh_|is_google",colnames(modeldata1_sample),value=TRUE)
inputs <- inputs[!inputs %in% c("domain_prev_date", "domain_u2", "subdomain_u2", "domain_prev_date_u2")]

# reducing modeldataset to only modeling related variables
modeldata_lh_sample <- modeldata1_sample[,unique(c(inputs,target))]

# removing categorical variables with many distinct values (if any)
for (var in colnames(modeldata_lh_sample)) {
  if (is.character(modeldata_lh_sample[,var]) & length(unique(modeldata_lh_sample[,var]))>50) {
    inputs <- inputs[!inputs %in% var]
  }
}

# Handling missing variables 
#modeldata_lh[is.na(modeldata_lh)] <- 0
#modeldata_lh <- na.omit(modeldata_lh)
# Commented out below - will let model see missings...
#for  (var in colnames(modeldata_lh)) {
#  modeldata_lh[is.nan(modeldata_lh[,var]),var] <- -1
#}

# just checking that there are no factor vars
for (var in colnames(modeldata_lh_sample)) if (is.factor(modeldata_lh_sample[,var] )) print(var)

save.image("ws3.RData")

# Builing model####
# h2o model

#install.packages("h2o")

rm(out0,train, train.tokens,train.tokens_2g,train.tokens.df, train.tokens.dfm, train.tokens.dfm_2g)
gc()

h2o.init(ip = 'localhost', port = 4321, nthreads= -1, max_mem_size = "12g")

# Uploading df to h2o
h2o_df <- as.h2o(modeldata_lh_sample, id="modeldata1")

splitdf = h2o.splitFrame(data = h2o_df, ratios = 0.5, seed=123454)
h2o_train = splitdf[[1]]
h2o_test = splitdf[[2]]

#remove.packages("h2o")

# position_dif model 
Y <- target # position difference
X <- inputs
#X <- varimps_posdif$variable # vars id'et from first run 

# automl
TV.automl_posdif <- h2o.automl(
  x = X, 
  y = Y,
  training_frame    = h2o_train,
  leaderboard_frame = h2o_test,
  max_runtime_secs = 8 * 3600 # running number of seconds
  #max_runtime_secs = 7200 # running number of seconds
  #max_runtime_secs = 3600 # running number of seconds
  #,exclude_algos = c("DeepLearning") # allow this if running for long time
)

# leaderboard
lb <- as.data.frame(TV.automl_posdif@leaderboard)

# Picking the overall best model
automl_leader <- picked_model_dif <- TV.automl_posdif@leader
test_perf <- h2o.performance(automl_leader,h2o_test) #performance
test_perf@metrics$r2   
train_perf <- h2o.performance(automl_leader,h2o_train) #performance
train_perf@metrics$r2   
# saving model for later use
model_path <- h2o.saveModel(object=picked_model_dif, path=getwd(), force=TRUE)


# Pick non-StackedEnsemble model (for varimp)
non_stacked_ensemble_mod_id <- lb$model_id[!grepl("Stacked",lb$model_id)]  
non_stacked_ensemble_mod <- h2o.getModel(non_stacked_ensemble_mod_id[1] ) 
varimps_posdif <- as.data.frame(h2o.varimp(non_stacked_ensemble_mod))
test_perf_non_stacked <- h2o.performance(non_stacked_ensemble_mod,h2o_test) #performance
test_perf_non_stacked@metrics$r2  
# saving model for later use
model_path <- h2o.saveModel(object=non_stacked_ensemble_mod, path=getwd(), force=TRUE)

#sender varimps til excel
to_excelfile(varimps_posdif[-grep("_u2|_dif",varimps_posdif$variable),],
             paste0("variable_omportance",non_stacked_ensemble_mod_id[1],".xlsx"), overwrite = TRUE)


# load the model
# best so far (I think) !!
#saved_model <- h2o.loadModel("XGBoost_grid__1_AutoML_20220731_093639_model_1")

picked_model_dif <- h2o.getModel("XGBoost_grid__1_AutoML_20220801_132849_model_1") 

# Gemmer scores i DF for det komplette datas?t
scores_posdif <- as.data.frame(h2o.predict(object=picked_model_dif,newdata=h2o_df, type="response"))

# Hæfter scores på modeldata1
colnames(scores_posdif)[1] <- "posdif_predict"
modeldata1_sample[,colnames(scores_posdif)] <-  NULL
modeldata1_sample <- cbind(modeldata1_sample,scores_posdif[1])

modeldata1_sample <- modeldata1_sample[modeldata1_sample$position<21 & modeldata1_sample$position_u2<21,]

# Model metrics
mms <- modmetrics(modeldata1_sample[,target],modeldata1_sample$posdif_predict)
mms$r2

# Calculating final ranking ####
rm(matchscoresout0)

# sikrer numeriske scores
modeldata1_sample$posdif_predict <- as.numeric(as.character(modeldata1_sample$posdif_predict))

# Bemærk: Negativ score er bedst!

# sammentæller "scores" som sum af position-score
matchscores_dif <- rbind(
  data.frame(url=modeldata1_sample$url,keyword=modeldata1_sample$keyword,
              region=modeldata1_sample$region, position=modeldata1_sample$position, points = modeldata1_sample$posdif_predict),
  data.frame(url=modeldata1_sample$url_u2,keyword=modeldata1_sample$keyword,position=modeldata1_sample$position_u2, 
             region=modeldata1_sample$region_u2, points = -modeldata1_sample$posdif_predict)
)
matchscores2_dif <- matchscores_dif %>% 
  group_by(url,keyword,position,region) %>% 
  summarise(pointstotal=sum(points), points_median=median(points), points_mean = mean(points))
#matchscores2_dif$regscore <- predict(lm(position ~ pointstotal+points_median,data=matchscores2_dif))

rm(matchscores_dif_final) 
for (reg in unique(matchscores2_dif$region)){   
  for (kw in unique(matchscores2_dif$keyword)) {
    df0 <- matchscores2_dif[matchscores2_dif$keyword==kw & matchscores2_dif$region==reg,]
    #df0$rankscore <- rank(df0$pointstotal,ties.method = "first")
    df0$rankscore <- rank(df0$points_mean,ties.method = "first")
    #df0$rankscore <- rank(df0$points_median ,ties.method = "first")
    if  (exists("matchscores_dif_final")) {
      matchscores_dif_final <- rbind(matchscores_dif_final,df0)
    } else {
      matchscores_dif_final <- df0
    }
  }
}
matchscores_dif_final$abs_error <- abs(matchscores_dif_final$position - matchscores_dif_final$rankscore)
mean(matchscores_dif_final$abs_error)
modmetrics(matchscores_dif_final$position,matchscores_dif_final$rankscore)$r2


# Precision stats ####
# almost exact
sum(ifelse(abs(matchscores_dif_final$position-matchscores_dif_final$rankscore)<3,1,0))/nrow(matchscores_dif_final)
points=2 => 0.87
# exact
sum(ifelse(abs(matchscores_dif_final$position-matchscores_dif_final$rankscore)<2,1,0))/nrow(matchscores_dif_final)
points=2 => 0.73

sum(ifelse(abs(matchscores_dif_final$position-matchscores_dif_final$rankscore)<1,1,0))/nrow(matchscores_dif_final)


View(matchscores_dif_final)

# pivot'ing confusion matrix ####
table(matchscores_dif_final$position,matchscores_dif_final$rankscore)
library(reshape2)
confusiondf <- as.data.frame(
  reshape2::dcast(data.frame(table(matchscores_dif_final[,c("rankscore","position")])),rankscore ~ position,value.var="Freq")
)
colnames(confusiondf)[-1] <- paste0("pos_",colnames(confusiondf)[-1])

to_excelfile(confusiondf,"model_confusion.xlsx", overwrite = TRUE)
to_excelfile(matchscores_dif_final,"ranking_matchscores.xlsx",overwrite=TRUE)

# SIMULATION ###################################################################################################################
# Simulating to find optimal value set of variables - to beat better ranked sites on same keyword
##_##################################################################################################################

# FINDING OPTIMAL VALUESET
# Save as-is df
# Save as-is df for client pages: BASELINE set
# Using position 1 as GOAL set!! (earlier simulations have tried "free flowing" values but that didnt work)
# Calculate scores for BASELINE, GOAL set - and for each partial variable change (from baseline to goal value)

#_###################################################################################################################

# OBS: IMPORTANT: THE MODEL SHOULD ONLY USE ACTUAL GOOGLE LIGHTHOUSE MEASUREMENTS - AND NOT THE SCORES SINCE THEY ARE LINEAR TRANSFORMATIONS THEREOF

#_###################################################################################################################

# As-is df is 'finaldata' df !!!  ####

# Save as-is df for client pages: BASELINE set ####
# Identificerer de relevante inputvars (dem, der kan ændres for klienten)
singleinputs <- inputs[-grep("_u2|_dif",inputs)]
sim_baseline_df <- unique(finaldata[finaldata$domain==clientdomain, c("keyword","domain","position","url" ,"date","project","region","cost",
                                                                      "difficulty","volume","ctr_valid","traffic_est","d_traffic_est_1up","d_traffic_est_top",singleinputs)]  )

finaldata_sim <- finaldata[finaldata$domain==clientdomain,]
finaldata_sim$changevar <- "BASELINE" 
tmp <- finaldata[finaldata$position==1,]
tmp$changevar <- "GOAL" 
finaldata_sim <- rbind(finaldata_sim,tmp)

# Changing one variable at a time and adding to finaldata_sim df...
for (reg in unique(sim_baseline_df$region )){  
  for (kw in unique(finaldata_sim[finaldata_sim$changevar=="BASELINE" & finaldata_sim$region==reg,"keyword" ])){
    goalset <- finaldata_sim[finaldata_sim$changevar=="GOAL" & finaldata_sim$keyword==kw & finaldata_sim$region==reg,] 
    baselineset <- finaldata_sim[finaldata_sim$changevar=="BASELINE" & finaldata_sim$keyword==kw & finaldata_sim$region==reg,] 
    for (var in singleinputs){
      tmp <- baselineset
      tmp[,var] <- goalset[,var]  
      tmp$changevar <- var
      finaldata_sim <- rbind(finaldata_sim,tmp)
    }  
  }  
}


# Competition url is only position 1 url: GOAL
finaldata2_sim <- finaldata_sim[finaldata_sim$changevar== "GOAL", ] 
colnames(finaldata2_sim) <- paste0(colnames(finaldata2_sim),"_u2")

# Creating string for difvars calculation. May be removed in the future if models are created without
difvars <- c()
#vars0 <- grep("backlink_|domain_|content_|lighthouse_",colnames(finaldata),value=TRUE)
vars0 <- grep("backlink_|content_|lighthouse_",colnames(finaldata),value=TRUE)
for (var in vars0) {
  difvars <- c(difvars,paste0("a.",var," - b.",var,"_u2 as ",var,"_dif"))
}
difvars <- paste(difvars,collapse=",")

# creating sim dataset with url against url2
modeldata1_sim <- sqldf(paste0("select distinct a.*, b.* ,",difvars,",
                    a.position - b.position_u2 as posdif
                    from finaldata_sim a
                    join finaldata2_sim b on a.keyword=b.keyword_u2 and a.region=b.region_u2"))


#for  (var in colnames(modeldata1_sim)) {
#  modeldata1_sim[is.nan(modeldata1_sim[,var]),var] <- -1
#}

# SCORING ####
# initialization of h2o and selecting the model
library(h2o)
#install.packages("h2o")

h2o.init(ip = 'localhost', port = 4321, nthreads= -1, max_mem_size = "6g")

usethismodel <- "XGBoost_grid__1_AutoML_20220731_093639_model_1"
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
      unique(modeldata1_sim[modeldata1_sim$keyword==out2[i,"keyword"],"url_u2"])
    
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
colnames(finaldata2_sim) <- paste0(colnames(finaldata2_sim),"_u2")

# creating sim dataset with url against url2
modeldata2_sim <- sqldf(paste0("select distinct a.*, b.* ,",difvars,",
                    a.position - b.position_u2 as posdif
                    from finaldata_sim a
                    join finaldata2_sim b on a.keyword=b.keyword_u2"))

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
                    ,c("keyword","url","changevar","predict","content_H1_n","content_H1_n_u2")] )

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



