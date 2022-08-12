
reddit_comments <- function(subreddit=NULL,urls=NULL,flair=NULL,search=NULL) {
  # Pretty hairy json file. Use http://jsonviewer.stack.hu/ to decompose
  
  library(rvest)
  library(dplyr)
  library(stringr)
  library(xml2)
  library(httr)
  library(R.utils)
  library(tidyverse)
  library(RJSONIO)
  library(data.table)
  source("https://raw.githubusercontent.com/pjerrot/r_stuff/master/pjerrots_mining_functions.R")
  
  html_elements <- rvest::html_nodes
  
  #url <- "https://www.reddit.com/r/Science/hot/?sort=top&t=year"
  #url <- "https://www.reddit.com/r/science/?f=flair_name%3A%22Health%22"
  #url <- "https://www.reddit.com/r/space"
  #url <- "https://www.reddit.com/r/science/search/?q=Novo%20Nordisk"
  #subreddit <- "science"
  
  # Search
  if (!is.null(search)) subreddit <- paste0(subreddit,"/search/?q=",gsub(" ","%20",search),"&restrict_sr=1&sr_nsfw=")
  # Flair
  if (!is.null(flair) & is.null(search)) subreddit <- paste0(subreddit,"/search?q=flair%3A",gsub(" ","%20",flair),"&restrict_sr=on&sort=new")
  
  
  if (!is.null(subreddit)) {
    subreddit_urls <- c(paste0("https://www.reddit.com/r/",subreddit,"/top/?t=month"),
                        paste0("https://www.reddit.com/r/",subreddit),
                        paste0("https://www.reddit.com/r/",subreddit,"/hot"),
                        paste0("https://www.reddit.com/r/",subreddit,"/new"),
                        paste0("https://www.reddit.com/r/",subreddit,"/top/?t=week"))
  } else {
    subreddit_urls <- urls
  }
  
  sublinks <- c()
  
  for (url in subreddit_urls) {
    webpage <- read_html(as.character(url))
    # reading top topics from subreddit
    permlinkposs <- gregexpr(pattern ='permalink',as.character(webpage))
    for (j in 1:length(permlinkposs[[1]])) {
      cutout1 <- substr(as.character(webpage),permlinkposs[[1]][j],permlinkposs[[1]][j]+500)
      cutout2 <- substr(cutout1,gregexpr(pattern ='http',cutout1)[[1]][1],gregexpr(pattern ='http',cutout1)[[1]][1]+500)
      cutout3 <- substr(cutout2,1,gregexpr(pattern ='"',cutout2)[[1]][1]-2)
      sublinks <- c(sublinks,cutout3)
    }
  }
  sublinks <- unique(sublinks)
  
  # Collect and store comments and replies for each sublink
  out <- NULL
  for (link in grep("https://www.reddit.com",sublinks,value=TRUE)) {
    print(link)
    tryCatch(
      {
        # retrieving the json file
        webpage2_json <- RJSONIO::fromJSON(paste0(link,"/.json"))
        #webpage2_json <- jsonlite::fromJSON(paste0(link,"/.json"))
        
        #Flattening the json file completely
        raw_json <- data.frame(enframe(unlist(webpage2_json)))
        
        if (!raw_json[grep("data.children.data.is_created_from_ads_ui",raw_json$name),"value"]=="TRUE") { # Skipping ads
          
          # Extracting the relevant values and creating a df with these comments/replies
          get_these_values <- paste0("\\.",c("id","body","text","ups","subreddit_id","author","created","depth","parent_id"))
          get_these_values_str <- paste(get_these_values,collapse="|")
          raw_json_reduced <- raw_json[grep(get_these_values_str,raw_json$name),]
          remove_these <- c("author_flair_type","author_fullname","author_patreon_flair","author_fullname","body_html",
                            "created_utc","author_premium","author_is_blocked","author_cakeday","author_flair_background_color",
                            "author_flair_css_class","author_flair_template_id","author_flair_text","author_flair_text_color","all_awardings.id")
          remove_these_str <- paste(remove_these,collapse="|")
          raw_json_reduced <- raw_json_reduced[-grep(remove_these_str,raw_json_reduced$name),]
          
          j <- 0
          for (i in 1:nrow(raw_json_reduced)) {
            if (grepl("\\.id",raw_json_reduced[i,"name"])) j <- j + 1
            #if (grepl("\\.subreddit_id",raw_json_reduced[i,"name"]) | grepl("\\.id",raw_json_reduced[i,"name"])) j <- j + 1
            raw_json_reduced[i,"seqid"] <- j
          }
          
          raw_json_reduced$varname <- substr(raw_json_reduced$name,regexpr("\\.[^\\.]*$", raw_json_reduced$name)+1,nchar(raw_json_reduced$name))
          tmp <- reshape2::dcast(raw_json_reduced, seqid ~ varname, value.var="value", fun.aggregate = min)
          
          if (all(colnames(tmp) %in% c("seqid","author","body","created","depth","id","parent_id","subreddit_id","ups"))) {
            
            # Adding info regarding original post
            tmp$post_title <- raw_json[grep("data.children.data.title",raw_json$name),"value"]
            tmp$post_ups <- as.numeric(raw_json[raw_json$name=="data.children.data.ups","value"][1])
            tmp$post_upvote_ratio <- as.numeric(raw_json[raw_json$name=="data.children.data.upvote_ratio","value"][1])
            tmp$post_upvote_author <- as.character(raw_json[raw_json$name=="data.children.data.author","value"][1])
            tmp$post_flair <- as.character(raw_json[raw_json$name=="data.children.data.link_flair_text","value"][1])
            tmp$post_subreddit <- as.character(raw_json[raw_json$name=="data.children.data.subreddit","value"][1])
            tmp$post_created_date <- as.numeric(raw_json[raw_json$name=="data.children.data.created","value"][1])
            
            # Cleaning out empty replies and other.. crap
            tmp <- tmp[which(!is.na(tmp$body)),c("post_subreddit","post_flair","post_upvote_author","post_title","post_created_date","post_ups","post_upvote_ratio",
                                                 "author","body","created","depth","id","parent_id","subreddit_id","ups" )]
            colnames(tmp) <- c("post_subreddit","post_flair","post_upvote_author","post_title","post_created_date","post_ups","post_upvote_ratio",
                               "reply_author","reply_body","reply_created","reply_depth","reply_id","reply_parent_id","reply_subreddit_id","reply_ups")
            
            #place <- grep("know what leads to an even",raw_json_reduced$value)
            #View(raw_json_reduced[(place-10):(place+20),])
            #View(tmp[grep("know what leads to an even",tmp$body),])
            
            if (exists("out")) {out <- bind_rows(out,tmp)} else {out <- tmp}
          } else {
            print(paste("Failed on",substr(link,1,30),"...: Didn't have all columns. Maybe no comments yet!"))
          }
        }
      }
      ,
      error=function(e) {
        print(e)
      }
    )
  }
  
  out$reply_datetime <- as.POSIXct(as.numeric(out$reply_created), origin="1970-01-01")
  out$post_datetime <- as.POSIXct(as.numeric(out$post_created), origin="1970-01-01")
  
  out <- out[!out$reply_author=="[deleted]",c("post_subreddit","post_datetime","post_flair","post_upvote_author","post_title","post_ups","post_upvote_ratio",
                                              "reply_author","reply_body","reply_datetime","reply_depth","reply_id","reply_parent_id","reply_subreddit_id","reply_ups")  ]
  
  return(out)
  
}
