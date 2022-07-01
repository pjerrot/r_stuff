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

source("/home/johnw/Documents/git/r_stuff/SENOA/senoa_functions.r")
source("https://raw.githubusercontent.com/pjerrot/r_stuff/master/pjerrots_mining_functions.R")

# Getting CTR by position estimates ####
ctrs <- read.xlsx("/home/johnw/Documents/git/r_stuff/SENOA/estimated_CTR_by_position.xlsx")
ctrs$position <- as.numeric(ctrs$position)

# serpstat token
api_token <- "786fa52270333d18a211144ae3b14104" 

# setting region / marked
regions <- c("g_us","g_uk")

clientname <- "Teamviewer"
clientdomain <- "teamviewer.com"
projectname <- "Teamviewer"
#market <- toupper(gsub("g_","",se))

daysdate <- gsub("-","",as.character(Sys.Date()))

# - end initialize 

# 1: Generate keywords ####

# Starting with initial set of keywords ####

# Getting initial keywords ####
kws0 <- unique(read.xlsx("TMV US Keywords.xlsx"))
colnames(kws0) <- c("keywords")
kws0 <- c("remote desktop")

keywords_all <- soa_get_keywords(domain=clientdomain, 
                             keywords <- kws0$keywords ,
                             #keywords <- kws0 ,
                             serpstat_api = api_token,
                             serpstat_regions=regions,
                             serpstat_iterations = 1,
                             use_google_suggestions = TRUE,
                             google_suggestion_iterations = 1)

View(keywords_all)


# 2: Get basic keyword info (especially volume) (serpstat) ####

kws_info <- soa_get_basic_keyword_info(keywords_all,regions)
kws_info <- kws_info[,c("keyword","cost","difficulty","region","volume")]


# 3: Remove low-volume keywords ####
kws_info <- kws_info[kws_info$volume>1,] 


# 4: Keyword top 30 domain/url positions ####
# takes quite a long time!
rm(out_positions)

kws_info$keyword <- as.character(kws_info$keyword ) 

out_positions <- soa_get_toprankings(kws_info[101:200,] ,api_token,top_n=20)


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


tmp <- sqldf("select a.*, 
             b.traffic_est - a.traffic_est as d_traffic_est_1up, 
             c.traffic_est - a.traffic_est as d_traffic_est_top
             from out_positions a 
             left join out_positions b on a.keyword=b.keyword and b.position = a.position-1
             left join out_positions c on a.keyword=c.keyword and c.position = 1")
tmp[is.na(tmp$d_traffic_est_1up),"d_traffic_est_1up"] <- 0
out_positions <- tmp

colnames(out_positions)


# SEGMENTATION ####
# USE SENOA_Clustering.r file!

# OUTPUT 1

output_folder <- "C:\\Users\\johnw\\OneDrive - The North Alliance\\Data Science Ressources\\senoa_files\\"
to_excelfile(out_positions[,colnames(out_positions)[!colnames(out_positions) %in% c("types","branding_fuzzy_match","branding_fuzzy_match_rel")]],
             paste0(output_folder,projectname,"_",market,"_",daysdate,".xlsx"))


urltraffic_sum <- out_positions %>% group_by(url) %>% summarise(sumtraf = sum(traffic_est)) 

# Reducing relevant URLs to those with minimum traffic (OBS: decided agains reducing - to have as much variance as possible in final model)
vip_urls <- urltraffic_sum[urltraffic_sum$sumtraf>200,c("url")]
vip_urls <- vip_urls$url

# 5 Crawl all content #### 

  x2 <- data.frame(unique(out_positions$keyword))
  x2$text <- x2[,1]
  #x2$doc_id <- ifelse(!is.null(groupby), searches[,groupby],"Nogroups")
  x2$doc_id <- seq(1:nrow(x2))
  train.tokens <- tokens(x2$text, what = "word", 
                         remove_numbers = TRUE, remove_punct = TRUE,
                         remove_symbols = TRUE)
  train.tokens <- tokens_select(train.tokens, stopwords(), selection = "remove")
  train.tokens <- tokens_ngrams(train.tokens, n = 1:1)
  train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
  x3 <- convert(train.tokens.dfm,to="data.frame")
  bestwords <- colnames(x3)[-1]
  allkeywords <- x2$text
  
  allstrings <- c(bestwords,allkeywords) # both identifying single words and entire search kws

allstrings <- unique(out_positions$keyword)

# crawling the sites to locate any of the "bestwords" or search strings
# please allow for 5-7 seconds for each site!
rm(out) 
urls <- out_positions[!out_positions$url %in% grep(".pdf",out_positions$url, value=TRUE) & out_positions$position<21,"url"]
urls <- unique(urls)
length(urls)

for (i in 1:length(urls)) {
  print(paste("crawling number ",i,"out of ",length(urls),": ",urls[i]))
  tryCatch(
    {
      tst <- withTimeout({
        soa_keyword_crawler(allstrings,urls[i])  
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

grep("logic_all",colnames(out),value=TRUE)

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
for (dom in unique(out_positions$domain)) {
  i <- i + 1
  print(paste0("Working on ",i,": ",dom, " out of ",length(unique(out_positions$domain))))
  tst <- sst_jw_domain_getBacklinkSummary(api_token=api_token,
                                          se="g_us",
                                          domain=dom,
                                          searchType = "domain_with_subdomains") # or "domain"
  if (length(tst$error)==0) {
    tst2 <- data.frame(domain=dom,tst$data)
    if (exists("blout")) {blout <- rbind(blout,tst2)} else {blout <- tst2}
  } else {
    noresultdoms <- c(noresultdoms,dom)
    print(paste("No result on:",dom))
  }
}
backlink_data <- blout
colnames(backlink_data) <- paste0("bl_",colnames(backlink_data))
backlink_data$linktodom_ratio <- backlink_data$backlink_referringLinks/(1+backlink_data$backlink_referringDomains)
rm(blout)
end_time <- Sys.time()
bl_time <- end_time - start_time

save.image(paste0("ws2.RData"))


# 9: Google Lighthouse (pagespeed) ####
# Google lighthouse runs on each url (there is actually a lot of variance in scores for each url under a domian) ###
# Tager laaaang tid! Appr. 20 secs per run! ;)
# 24*60*60/20 = 4320 runs pr day

start_time <- Sys.time()
google_api_key <- "AIzaSyAEz_Z7xcFaTcXoGRpB2-8U-NE_1lU_sYo"
rm(lightsout)
i <- 0

run_these_urls <- unique(out_positions[out_positions$domain=="teamviewer.com","url"])
run_these_urls <- unique(out_positions[out_positions$position<21,"url"])
run_these_urls <- unique(out_positions[,"url"])
run_these_urls <- unique(out_positions[,"domain"])[10:15]
#run_these_urls <- vip_urls
run_these_urls <- unique(out_positions[,"domain"])
(length(run_these_urls))

dom <- run_these_urls[2]

for (dom in run_these_urls) {
  i <- i + 1
  print(paste("Working on url number ",i," - url:",dom," out of ",length(unique(run_these_urls))))
  tryCatch(
    {
      out0 <- withTimeout({
        pagespeed_mets(paste0("https://",dom),api_key=google_api_key,strategy = "desktop")  
      }, timeout=100, onTimeout="warning")
      out0_desktop <- data.frame(domain=dom,data.frame(out0$performance_mets))
      colnames(out0_desktop) <- paste0("lighthouse_desktop_",colnames(out0_desktop))
      
      out0 <- withTimeout({
        pagespeed_mets(paste0("https://",dom),api_key=google_api_key,strategy = "mobile")  
      }, timeout=100, onTimeout="warning")
      out0_mobile <- data.frame(domain=dom,data.frame(out0$performance_mets))
      colnames(out0_mobile) <- paste0("lighthouse_mobile_",colnames(out0_mobile))
      
      out0b <- cbind(out0_desktop,out0_mobile)
      
      if (exists("lightsout")) {
        lightsout <- rbind(lightsout,out0b)
      } else {
        lightsout <- out0b
      }
    }
    ,
    error=function(e) {
      print(paste("Error on ",dom,": ",e))
    }
  )
}
colnames(lightsout)[1] <- "lighthouse_domain"
lightsout$lighthouse_mobile_domain <- NULL

end_time <- Sys.time()
end_time - start_time
save.image(paste0("ws2.RData"))

# 10: Build model data with content, pagespeed and backlink data. With rankdif as target ####

# Samler salaten til model data ####

#Content: content_stats_final
#Backlinks: backlink_data
#lighthouse: lightsout
#positioner (target): out_positions_top20_tst
#domain info: domain_info
out_positions_top20_tst <- out_positions[out_positions$position<21,]
for (i in 1:ncol(out_positions_top20_tst)) {
  if (is.numeric(out_positions_top20_tst[,i])) out_positions_top20_tst[,i] <- as.numeric(as.character(out_positions_top20_tst[,i]))
}

finaldata <- sqldf(paste0("select distinct a.*,",
                          paste(colnames(lightsout)[-1],collapse=","),
                          ", case when b.lighthouse_domain is null then 1 else 0 end as lighthouse_miss,
             d.*, 
             case when d.content_url is null then 1 else 0 end as content_miss,
             e.*,
             case when e.backlink_domain is null then 1 else 0 end as backlink_miss
             from out_positions_top20_tst a
             left join lightsout b on b.lighthouse_domain = a.domain
             --left join lightsout b on b.lighthouse_domain = a.url
             left join content_stats_final d on d.content_url=a.url and a.keyword = d.content_keyword
             left join backlink_data e on e.backlink_domain = a.domain
             "))
finaldata[,c("backlink_domain","content_url","domain_domain","lighthouse_domain","content_keyword")] <- NULL

for (var in colnames(finaldata)) {
  print(paste(var,":",length(which(is.na(finaldata[,var])))))
}

######################################
summary(tst2)
View(out3)

rm(out2) 
i <- 0
for (kwo in unique(x2$keyword)) {
  i <- i+1
  print(paste0("at num ",i," out of ",lkw))
  #tmp <- x2[x2$keyword==kwo,c("url",grep("logic",colnames(x2),value=TRUE))]
  tmp <- x2[x2$keyword==kwo,c("url","in_any_logic")]
  colnames(tmp)[-1] <- paste0(kwo,"_",colnames(tmp)[-1])
  if (exists("out2")) {
    out2 <- merge(x=out2,y=tmp,by="url",all.x=TRUE)  } 
  else {
    out2 <- tmp
  }
}

View(out[out$keyword=="remote",])
View(out[out$url=="https://www.laptopmag.com/articles/how-to-video-screen-capture-windows-10",])
View(out2[out2$url=="https://www.laptopmag.com/articles/how-to-video-screen-capture-windows-10",])

#avoidwords <- c(excludewords)
#x2$text <- str_trim(gsub(paste0(avoidwords,collapse="|"),"",x2$text))

# adding 2-grams (optional but does seem to improve clusters slightly) ####
#train.tokens <- tokens(x2$text, what = "word", 
#                       remove_numbers = TRUE, remove_punct = TRUE,
#                       remove_symbols = TRUE)
#train.tokens <- tokens_select(train.tokens, stopwords(), selection = "remove")
#train.tokens <- tokens_ngrams(train.tokens, n = 1:2)
#train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
#x3 <- convert(train.tokens.dfm,to="data.frame")
#x3 <- cbind(url=x2$url,x3)
#x3$doc_id <- NULL


biterms <- sqldf("select a.keyword as term1, b.keyword as term2, count(distinct a.url) as cooc, 'x' as doc_id 
             from x2 a 
             join x2 b on a.url=b.url
             group by a.keyword, b.keyword")

traindata <- unique(data.frame(doc_id=x2$url,lemma=x2$keyword))
traindata <- unique(data.frame(doc_id="x",lemma=x2$keyword))

library(BTM)
set.seed(123456)
#traindata <- subset(anno, upos %in% c("NOUN", "ADJ", "VERB") & !lemma %in% stopwords("en") & nchar(lemma) > 2)
#traindata <- traindata[, c("doc_id", "lemma")]
model     <- BTM(traindata, biterms = biterms, k = 9, iter = 2000, background = TRUE, trace = 100)

#######
tst2 <- dcast(tst,kw1 ~ kw2,value.var="n")

?dcast
sumvars <- colnames(x3)[-1]
x4 <- x3 %>% group_by(url) %>% summarise_at(sumvars,.funs="sum")

novars <- c("doc_id","text","n")
for (j in 1:nrow(x2)) {
  x2[j,"text2"] <- paste(unique(names(x3[j,colnames(x3)[!colnames(x3) %in% novars]])[unlist(apply(x3[j,colnames(x3)[!colnames(x3) %in% novars]], 1, function(i) which(i >0 )))]),collapse=" ")
}
x2$text <- x2$text2

rm(anno)
anno    <- udpipe(x2[,c("doc_id","text")], "english", trace = 10)

biterms <- data.table(anno)
biterms <- na.omit(biterms[,c("doc_id","paragraph_id","sentence_id","sentence","start","end","term_id",
                              "token_id","token","lemma","upos" ,"xpos","head_token_id","dep_rel")])


tmp <- unique(out$keyword)
which(tmp==kwo)

rm(out2)
for (kwo in unique(out$keyword)) {
  tmp <- out[out$keyword==kwo,c("url",grep("logic",colnames(out),value=TRUE))]
  colnames(tmp)[-1] <- paste0(kwo,"_",colnames(tmp)[-1])
  if (exists("out2")) {
    out2 <- merge(x=out2,y=tmp,by="url",all.x=TRUE)  } 
  else {
    out2 <- tmp
  }
}
to_excelfile(out2,"TMV_competitor_keywordcrawl_result_by_url.xlsx")

# kmeans cluster - to find which cluster that client belong to - and then id content competitors
tst <- kmeans(out2[,-1],5)

out3 <- cbind(url = out2$url, cluster=tst$cluster)
View(out3)

