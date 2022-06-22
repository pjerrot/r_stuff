


# Script that estimates google search volume history from serpstat volume combined with google trends

google_search_volume_history_estimate <- function(keywords, 
                                                  google_regions, 
                                                  end_date=Sys.Date(),
                                                  serpstat_api_token) {
  
  require(gtrendsR)
  require(serpstatr)
  library(sqldf)
  require(reshape)
  require(dplyr)
  source("https://raw.githubusercontent.com/pjerrot/r_stuff/master/pjerrots_mining_functions.R", encoding="utf-8")
  source("https://raw.githubusercontent.com/pjerrot/r_stuff/master/wzy.R", encoding="utf-8")
  
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
    tst <- gtrends(keyword=word,
                   geo=geo,  
                   #time = start_to_end,
                   time = "today+5-y",
                   gprop = source)
    allqueries[[i]] <- tst
    
    df <- data.frame(tst$interest_over_time)  
    if (nrow(df)>0) {
      df$geo <- geo
      if (exists("interest_over_time")) {interest_over_time <<- rbind(interest_over_time,df)} else {interest_over_time <<- df}
    }
    df <- data.frame(tst$interest_by_region)  
    if (nrow(df)>0) {
      if (exists("interest_by_region")) {interest_by_region <<- rbind(interest_by_region,df)} else {interest_by_region <<- df}
    }
  } 
  
  for (iterations in c(1,2)){ 
    for (i in 1:nrow(combos)) { 
      if (combos[i,"run"]==0 ){  
        word <- as.character(combos[i,"keywords"])
        geo <- as.character(combos[i,"geo"])
        tryCatch(
          {
            run_gtrend(word,geo)
            combos[i,"run"] <- 1 
          }
          ,
          error=function(e) {
            print(e)
          }
        )
      }  
    }
  }
  
  interest_over_month <- interest_over_time %>% 
    group_by(keyword,geo,substr(as.character(date ),1,7)) %>%
    summarise(hits = mean(hits))
  interest_over_month$date <- paste0(interest_over_month$`substr(as.character(date), 1, 7)`,"-01") 

  interest_over_month <- interest_over_month[,c("keyword","geo","hits","date")] 
  
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
  
  indxhelp <- sqldf("select distinct a.date, a.geo, a.keyword,a.hits, r.name,region_queries_count, c.keyword as kw2
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
                                 a.hits*100/b.hits as hits_index_100,
                                 a.hits*region_queries_count/b.hits as hits_estimated
                                 from interest_over_month a
                                 join indxhelp b on a.keyword=b.keyword and a.geo=b.geo")
  
  # Creating graphs ####
  
  wz.init(file = "search_volume_trends.html",color_scheme = "google", pdfcopy = FALSE)
  wz.title("Estimating search volume trends")
  wz.text("Using Google trends combined with most current Serpstat volume estimates.")
  for (geo in unique(volume_hist_est$geo )) {
    chart_title <- paste0("Estimated search volume trend for ",geo)
    wz.linechart(df=volume_hist_est[volume_hist_est$geo==geo,],x="date",num_vars = "hits_estimated", 
                 group_var = "keyword", fun="mean",
                 chart_title = chart_title, smooth=TRUE)
    #tst <- interest_over_time %>% group_by(date,keyword) %>% summarise(meanhits=mean(hits))
    #wz.linechart(tst,x="date",num_vars = "meanhits", group_var = "keyword", fun="asis", smooth = TRUE)
  }
  wz.wrapup()
  
  return(volume_hist_est)
}

## END ####

df <- interest_over_time
x <- "date"
num_vars <- "hits"
group_var <- "keyword"
fun <- "mean"
smooth <- TRUE
