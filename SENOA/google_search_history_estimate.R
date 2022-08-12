

# Script that estimates google search volume history from serpstat volume combined with google trends

google_search_volume_history_estimate <- function(keywords, 
                                                  google_regions, 
                                                  end_date=Sys.Date(),
                                                  serpstat_api_token,
                                                  friendly=TRUE,
                                                  friendly_seconds=10) {
  
  require(gtrendsR)
  require(serpstatr)
  library(sqldf)
  require(reshape)
  require(dplyr)
  
  api_token <- serpstat_api_token
  
  allqueries <- list()
  
  keywords <- tolower(keywords)
  regions <- data.frame(name=google_regions,
                        google=google_regions,
                        serpstat=tolower(paste0("g_",google_regions)))
  
  # Google Trends ####
  
  start_date <- as.Date(end_date) - 100
  start_to_end <- paste0(start_date," ",end_date)
  start_to_end <- "today+5-y"
  source <- "web"
  
  rm(interest_over_time)
  rm(interest_by_region)
  
  combos <- data.frame(expand.grid(keywords,regions$google) )
  colnames(combos) <- c("keywords","geo")
  combos$run <- 0 
  
  run_gtrend <- function(word,geo,gprop="web",time="today+5-y") {
    print(paste("Working on: ",word, " in ",geo))
    #word <- "astrazeneca"
    #geo <- "ke"
    tst <- gtrends(keyword=word,
                   geo=toupper(geo),  
                   #time = start_to_end,
                   time = "today+5-y",
                   gprop = source)
    allqueries[[i]] <- tst
    
    rm(df_)
    df_ <- data.frame(tst$interest_over_time)  
    if (nrow(df_)>0) {
      df_$geo <- geo
      if (exists("iot")) {iot <- rbind(iot,df_)} else {iot <- df_}
    }
    rm(df_)
    df_ <- data.frame(tst$interest_by_region)  
    if (nrow(df_)>0) {
      if (exists("ior")) {ior <- rbind(ior,df_)} else {ior <- df_}
    }
    if (!exists("iot")) iot <- "Gtrends did not return interest-over-time!"
    if (!exists("ior")) ior <- "Gtrends did not return interest-over-region!"
    
    out <- list(iot,ior)
    names(out) <- c("iot","ior")
    return(out)
  } 
  
  rm("interest_over_time","interest_by_region")
  
  for (iterations in c(1,2)){ 
    for (i in 1:nrow(combos)) { 
      if (combos[i,"run"]==0 ){  
        word <- as.character(combos[i,"keywords"])
        geo <- as.character(combos[i,"geo"])
        tryCatch(
          {
            gt <- run_gtrend(word,geo)
            
            if (!gt$ior=="Gtrends did not return interest-over-region!" ){ 
              if (exists("interest_by_region")) {
                interest_by_region <- rbind(interest_by_region,gt$ior)
              } else {
                interest_by_region <- gt$ior
              }
            }
            
            if (!gt$iot=="Gtrends did not return interest-over-time!" ){ 
              if (exists("interest_over_time")) {
                interest_over_time <- rbind(interest_over_time,gt$iot)
              } else {
                interest_over_time <- gt$iot
              }
            }  
            combos[i,"run"] <- 1 
          }
          ,
          error=function(e) {
            print(e)
          }
        )
        if (friendly==TRUE) Sys.sleep(friendly_seconds)
      }  
    }
  }
  
  interest_over_time$hits <- gsub("<1","0.5",interest_over_time$hits)
  interest_over_time$hits <- as.numeric(interest_over_time$hits)
  
  interest_over_month <- interest_over_time %>% 
    group_by(keyword,geo,substr(as.character(date ),1,7)) %>%
    summarise(hits_mean = mean(hits), hits_sum=sum(hits))
  interest_over_month$date <- paste0(interest_over_month$`substr(as.character(date), 1, 7)`,"-01") 
  interest_over_month <- interest_over_month[,c("keyword","geo","hits_mean","hits_sum","date")] 
  
  # serpstat ####
  
  rm(out)
  for (se in regions$serpstat) {
    
    keyws_info <- sst_sa_keywords_info(
      api_token,
      keywords=keywords,
      se=se,
      sort = NULL,
      return_method = "df"
    )
    df0 <- keyws_info$data[,c("keyword","region_queries_count")]
    if (!is.null((df0))){  
      df0$keyword <- unlist(df0$keyword)
      df0$region_queries_count <- unlist(df0$region_queries_count)
      df0$region <- se
      if (exists("out")) {out <- rbind(out,df0)} else {out <- df0}
    }
  }
  
  # samler salaten ####
  
  for (col in colnames(interest_over_month)) interest_over_month[,col] <- unlist(interest_over_month[,col]) 
  interest_over_month <- as.data.frame(interest_over_month)
  #write.csv(interest_over_month,"tmp_.csv")
  #interest_over_month <- read.csv("tmp_.csv")
  
  hlpdf <- interest_over_month %>% group_by(geo,keyword) %>% summarise(maxdate=max(date))
  hlpdf <- data.frame(hlpdf)
  
  indxhelp <- sqldf("select distinct a.date, a.geo, a.keyword,a.hits_sum, r.name,region_queries_count, c.keyword as kw2
                              from interest_over_month a
                              join hlpdf b on a.date=b.maxdate and 
                                 a.keyword=b.keyword and
                                 a.geo=b.geo
                              left join regions r on r.google=a.geo
                              left join out c on r.serpstat=c.region and lower(c.keyword)=lower(a.keyword)")
  
  indxhelp<- na.omit(indxhelp[indxhelp$hits>0,])
  #indxhelp[indxhelp$hits==0,"hits"] <- 1
  #indxhelp[is.na(indxhelp$region_queries_count),"region_queries_count"] <- 1
  
  volume_hist_est <- sqldf("select a.*, region_queries_count,
                                 a.hits_sum*100/b.hits_sum as hits_index_100,
                                 a.hits_sum*region_queries_count/b.hits_sum as volume_estimated
                                 from interest_over_month a
                                 join indxhelp b on a.keyword=b.keyword and a.geo=b.geo")
  
  # Creating graphs ####
  
  wz.init(file = "search_volume_trends.html",color_scheme = "google", pdfcopy = FALSE)
  wz.title("Estimating search volume trends")
  wz.text("Using Google trends combined with most current Serpstat volume estimates.")
  for (geo in unique(volume_hist_est$geo )) {
    chart_title <- paste0("Estimated search volume trend for ",geo)
    wz.linechart(df=volume_hist_est[volume_hist_est$geo==geo,],x="date",num_vars = "volume_estimated", 
                 group_var = "keyword", fun="mean",
                 chart_title = chart_title, smooth=TRUE)
    #tst <- interest_over_time %>% group_by(date,keyword) %>% summarise(meanhits=mean(hits))
    #wz.linechart(tst,x="date",num_vars = "meanhits", group_var = "keyword", fun="asis", smooth = TRUE)
  }
  wz.wrapup()
  
  return(volume_hist_est)
}


## END ####
