
#### functions for SENOA

soa_serpstat_keyword_getDomainKeywords<- function (api_token, 
                                             serpstat_region, 
                                             domain,
                                             url=paste0("https://",domain)) {
  api_params <- list(se = as.character(serpstat_region), 
                     domain = domain, 
                     url=url)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatDomainProcedure.getDomainKeywords", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="list")
}


sst_return_check <- function (response_content, return_method) 
{
  if (!return_method %in% c("list", "df")) {
    stop("return_method should be one of 'list' or 'df'")
  }
  if (!is.null(response_content$error)) {
    return(list(job_id = response_content$id, error = response_content$error))
  }
  if (is.null(response_content$result$data)) {
    final_data <- response_content$result
  }
  else {
    final_data <- response_content$result$data
  }
  if (return_method != "list") {
    final_data <- sst_lists_to_df(final_data)
  }
  return(list(job_id = response_content$id, data = final_data, 
              summary_info = response_content$result$summary_info))
}

soa_keyword_suggestions <- function (api_token, 
                                     se,
                                     keyword, 
                                     page = 1, 
                                     size = 100,
                                     return_method="list") {
  api_params <- list(se = as.character(se), 
                     keyword = keyword, 
                     page = page, 
                     pageSize = size,
                     return_method=return_method)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatKeywordProcedure.getSuggestions", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method)
}


soa_serp_list_2_df <- function(alist) {
  checknum <- function(var) {any(is.na(as.numeric(as.character(var))))==FALSE}
  
  tst <- lapply(alist,unlist)
  tmpdf <- t(data.frame(tst[1]))
  names(tmpdf) <- colnames(tmpdf)
  if (length(tst)>1) {
    for (i in 2:length(tst)) {
      ny <- t(data.frame(tst[i]))
      names(ny) <- colnames(ny)
      tmpdf <- bind_rows(tmpdf,ny) 
    }
  }
  row.names(tmpdf) <- NULL
  
  for (col in colnames(tmpdf)) {
    if (checknum(tmpdf[,col])) tmpdf[,col] <- as.numeric(tmpdf[,col])   
  }
  return(tmpdf)
}

soa_getDomainUrls<- function (api_token, 
                                       serpstat_region,
                                       domain,
                                       page = 1, 
                                       size = 100) {
  api_params <- list(se = as.character(serpstat_region), 
                     domain = domain)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatDomainProcedure.getDomainUrls", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="list")
}

soa_get_keywords <- function(keywords=NULL, 
                             domain=NULL,
                             use_serpstat=TRUE,
                             serpstat_api=NULL,
                             serpstat_regions=c("g_us","g_de"),
                             serpstat_iterations=1,
                             use_google_suggestions=FALSE,
                             google_suggestion_iterations=1){
  
  # Creating dataframe for output keywords
  keywords_out <- data.frame(keyword=character(),source=character(),region=character(), iteration=numeric())
  
  # Adding originally supplied keywords to output dataframe
  if (!is.null(keywords)) {
    kw0 <- data.frame(keyword=keywords)
    kw0$source <- "Initial"
    kw0$region <- "Initial"
    kw0$iteration <- 0
    keywords_out <- rbind(keywords_out,kw0)
  } 
  
  # Warnings/stops
  if (is.null(keywords) & is.null(domain)) stop("You need to supply either keywords or a domain to start off with!")

  if (!is.null(domain) & is.null(serpstat_api)) stop("No Serpstat API key was provided!")
  if (use_serpstat==TRUE & is.null(serpstat_api)) stop("No Serpstat API key was provided!")
  
  if (serpstat_iterations>2){
    print("You can run a max of 2 iterations. Otherwise keywords may get too detached from original topic.")
    serpstat_iteratons <- 2
  }  
  
  if (google_suggestion_iterations>3){
    print("You can run a max of 3 iterations. Otherwise keywords may get too detached from original topic.")
    google_suggestion_iterations <- 3
  } 
  
  # Getting keywords from domain (via serpstat)
  if (!is.null(domain)) {
    if (exists("kws_from_doms")) rm(kws_from_doms)
    for (se in serpstat_regions){ 
      kw0 <- soa_serpstat_keyword_getDomainKeywords(api_token=serpstat_api,serpstat_region = se, domain)
      kw0 <- soa_serp_list_2_df(kw0$data )
      if (nrow(kw0)>0){ 
        kw0$region <- se 
        if (exists("kws_from_doms")) {kws_from_doms <- bind_rows(kws_from_doms, kw0)} else {kws_from_doms <- kw0} 
      }
    }
    if (!exists("kws_from_doms") & is.null(keywords)) stop("No keywords were found from domain!")
    if (exists("kws_from_doms")){
      kws_from_doms$source <- "Domain" 
      kws_from_doms2 <- kws_from_doms %>% group_by(keyword, source) %>% summarise(region = paste(region,collapse=", "))
      kws_from_doms2$iteration <- 0 
      keywords_out <- bind_rows(keywords_out,kws_from_doms2)
    }  
  } 
  
  # getting new keyword suggestions
  # from serpstat
  if (use_serpstat==TRUE) { 
    
    for (iteration in 1:serpstat_iterations){
      if (length(unique(keywords_out$keyword))<300){
        print(paste("Running Serpstat iteration: ", iteration))
        if (exists("kws2")) rm(kws2)
        i <- 0
        for (se in serpstat_regions ){ 
          print(paste(".. in region:",se))
          for (kw in unique(keywords_out$keyword)) {
            i <- i + 1
            #print(paste0("Working on keyword number ",i," out of ",length(kws0)))
            tmp <- soa_keyword_suggestions(api_token=serpstat_api,se,keyword=kw,size=200)$data
            tmp <- soa_serp_list_2_df(tmp)
          
            if (!is.null(tmp)){
              if (nrow(tmp)>0){  
                tmp <- data.frame(tmp)
                tmp <- data.frame(keyword=tmp$keyword, source="Serpstat",region=se, iteration=iteration)
                if (exists("kws2")) {
                  kws2 <- rbind(kws2,tmp) 
                } else {
                  kws2 <- tmp
                }
              }
            }
          }
        }
        keywords_out <- bind_rows(keywords_out,kws2)
        if (length(unique(keywords_out$keyword))>300) {
          print("Getting too many keywords to continue on Serpstat. Breaking...")
          break
        } 
      } else {
        print("Too many harvested keywords to continue extracting from Serpstat!")
      }  
    }
  }
  
  if (use_google_suggestions==TRUE) { 
    # Adding kws from google suggestions
    i <- 0 
    if (exists("kws2")) rm(kws2)
    
    for (iteration in 1:google_suggestion_iterations) {
      print(paste0("Running Google suggestions. Iteration:",iteration))
      kws <- unique(keywords_out$keyword)
      
      for (country in gsub("g_","",regions)) {   
        
        for (kw in kws ) {
          i <- i + 1
          tmp <- soa_google_keyword_suggest(keyword=kw, country=country)
          tmp <- data.frame(keyword=tmp$keyword_suggestions)
          
          if (!is.null(tmp)){
            if (nrow(tmp)>0){  
              tmp$source <- "Google"
              tmp$region <- country
              tmp$iteration <- iteration
              
              if (exists("kws2")) {
                kws2 <- rbind(kws2,tmp) 
              } else {
                kws2 <- tmp
              }
            }
          }
        }
      }
    }
    keywords_out <-bind_rows(keywords_out,unique(kws2))
    
  } # end use google
  
  keywords_out <- keywords_out %>% group_by(keyword) %>% summarise(source=paste0(unique(source),collapse=", "), 
                                                                    region=paste(unique(region),collapse=", "), 
                                                                    iteration=min(iteration))
  
  # making sure that regions are aligned btw. serpstat and google regions
  keywords_out$region <- gsub("g_","",keywords_out$region )
  for (region in serpstat_regions) {
    keywords_out$region <- trim(gsub(gsub("g_","",region),paste0("g_",gsub("g_","",region)),keywords_out$region ))
  }
  for (i in 1:nrow(keywords_out)) {  
    keywords_out[i,"region"] <- paste(unique(unlist(str_split(gsub(" g_","g_",keywords_out[i,"region"]),","))),collapse=",")
  }
  
  return(keywords_out)
  
}  


soa_google_keyword_suggest <- function(keyword,
                                       language = "en",
                                       country = "us",
                                       walkThrough = FALSE,
                                       questions = FALSE,
                                       prepositions = FALSE,
                                       comparisons = FALSE) {
    #Language Input check
    if (nchar(language) > 2) {
      warning("Please check your language input")
    }
    #walkThrough-Input check
    if (!is.logical(walkThrough)) {
      stop("The walkThrough should be a logical input")
    }
    keyword <- gsub(" ", "+", keyword)
    key <- xml2::read_html(
      paste0(
        "http://suggestqueries.google.com/complete/search?output=toolbar&hl=",language,
        "&q=",keyword,
        "&gl=",country
      )
    )
    sug <- xml2::xml_find_all(key, "//suggestion")
    sug <- gsub('<suggestion data="', "", sug)
    sug <- gsub('"></suggestion>', "", sug)
    sug <- as.data.frame(sug)
    colnames(sug) <- "keyword_suggestions"
    # Walk through the alphabte for the given keyword (maybe add numbers)
    if (walkThrough == TRUE) {
      alphabet <- as.data.frame(c(letters))
      for (i in 1:nrow(alphabet)) {
        #query with alphabte after keyword
        key <- xml2::read_html(
          paste0(
            "http://suggestqueries.google.com/complete/search?output=toolbar&hl=",language,
            "&q=",paste0(keyword, "+", alphabet[i, 1]),
            "&gl=",country
          )
        )
        s <- xml2::xml_find_all(key, "//suggestion")
        s <- gsub('<suggestion data="', "", s)
        s <- gsub('"></suggestion>', "", s)
        s <- as.data.frame(s)
        colnames(s) <- "keyword_suggestions"
        #query with alphabte before keyword
        key <- xml2::read_html(
          paste0(
            "http://suggestqueries.google.com/complete/search?output=toolbar&hl=",language,
            "&q=",paste0(alphabet[i, 1], "+", keyword, "+"),
            "&gl=",country
          )
        )
        ss <- xml2::xml_find_all(key, "//suggestion")
        ss <- gsub('<suggestion data="', "", ss)
        ss <- gsub('"></suggestion>', "", ss)
        ss <- as.data.frame(ss)
        colnames(ss) <- "keyword_suggestions"
        sug <- rbind(sug, s, ss)
      }
    }
    
    #Check the Keywords with question words
    
    if (isTRUE(questions)) {
      file <- paste0('~/seoR/data-raw/questions_', language, '.csv')
      questions <- readr::read_csv(file,
                                   col_names = FALSE)
      for (i in 1:nrow(questions)) {
        #query with question keyword
        key <- xml2::read_html(
          paste0(
            "http://suggestqueries.google.com/complete/search?output=toolbar&hl=",language,
            "&q=",gsub(" ", "+", paste0(questions[i, 1], "+", keyword)),
            "&gl=",country
          )
        )
        s <- xml2::xml_find_all(key, "//suggestion")
        s <- gsub('<suggestion data="', "", s)
        s <- gsub('"></suggestion>', "", s)
        s <- as.data.frame(s)
        colnames(s) <- "keyword_suggestions"
        sug <- rbind(sug, s)
      }
    }
    
    #Check the Keywords with prepositions words
    
    if (isTRUE(prepositions)) {
      file <- paste0('~/seoR/data-raw/prepositions_', language, '.csv')
      prepositions <- readr::read_csv(file)
      for (i in 1:nrow(prepositions)) {
        #query with prepositions keyword
        key <- xml2::read_html(
          paste0(
            "http://suggestqueries.google.com/complete/search?output=toolbar&hl=",language,
            "&q=",gsub(" ", "+", paste0(keyword, "+", prepositions[i, 1])),
            "&gl=",country
          )
        )
        s <- xml2::xml_find_all(key, "//suggestion")
        s <- gsub('<suggestion data="', "", s)
        s <- gsub('"></suggestion>', "", s)
        s <- as.data.frame(s)
        colnames(s) <- "keyword_suggestions"
        sug <- rbind(sug, s)
      }
    }
    
    #Check the Keywords with comparisons words
    
    if (isTRUE(comparisons)) {
      file <- paste0('~/seoR/data-raw/comparisons_', language, '.csv')
      comparisons <- readr::read_csv(file)
      for (i in 1:nrow(comparisons)) {
        #query with comparisons keyword
        key <- xml2::read_html(
          paste0(
            "http://suggestqueries.google.com/complete/search?output=toolbar&hl=",language,
            "&q=",gsub(" ", "+", paste0(keyword, "+", comparisons[i, 1])),
            "&gl=",country
          )
        )
        s <- xml2::xml_find_all(key, "//suggestion")
        s <- gsub('<suggestion data="', "", s)
        s <- gsub('"></suggestion>', "", s)
        s <- as.data.frame(s)
        colnames(s) <- "keyword_suggestions"
        sug <- rbind(sug, s)
      }
    }
    
    return(unique(sug))
  }

soa_calc_traf_est <- function(volume,pos,frompos=NULL, branded_kw=FALSE){
  ctrs <- read.xlsx("/home/johnw/Documents/git/r_stuff/SENOA/estimated_CTR_by_position.xlsx")
  ctrs$position <- as.numeric(ctrs$position)
  trafest <- volume * 0.01*ifelse(branded_kw==FALSE,ctrs[ctrs$position==pos,"ctr_nonbranded" ],ctrs[ctrs$position==pos,"ctr_branded" ] )
  if (!is.null(frompos)) {
    fromtrafest <- volume * 0.01*ifelse(branded_kw==FALSE,ctrs[ctrs$position==frompos,"ctr_nonbranded" ],ctrs[ctrs$position==frompos,"ctr_branded" ] )
    dtraf <- trafest - fromtrafest
    out <- list(trafest,dtraf)
    names(out) <- c("traf_est","d_traf_est")
  } else  {
    out <- list(trafest)
    names(out) <- c("traf_est")
  } 
  
  return(out)
}   

sst_jw_domain_getBacklinkSummary <- function (api_token, 
                                             se="g_us", 
                                             domain,
                                             api_method_version=2,
                                             searchType = "domain") { # or "domain_with_subdomains" 
  
  if (api_method_version==1){    
    api_method <- paste0("SerpstatBacklinksProcedure.getSummary")
  } else{  
    api_method <- paste0("SerpstatBacklinksProcedure.getSummaryV2")
  }
  api_params <- list(se = as.character(se), 
                     query = domain,
                     searchType=searchType)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = api_method, 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="list")
}


sst_call_api_method <- function (api_token, api_method, api_params = NULL) {
  tryCatch({
    api_response <- httr::POST(url = "http://api.serpstat.com/v4", 
                               body = list(id = as.numeric(Sys.time()) * 1000, 
                                           method = api_method, params = api_params), encode = "json", 
                               httr::add_headers(token = api_token))
    api_response <- httr::content(api_response)
    if ("list" %in% class(api_response)) {
      return(api_response)
    }
    else {
      stop(paste0("There is a problem with Serpstat API. If you get this error, ", 
                  "please contact support at https://serpstat.com/support/ ", 
                  "with the details of what you are doing."))
    }
  }, error = function(e) {
    print(e)
  })
}

sst_return_check <- function (response_content, return_method) 
{
  if (!return_method %in% c("list", "df")) {
    stop("return_method should be one of 'list' or 'df'")
  }
  if (!is.null(response_content$error)) {
    return(list(job_id = response_content$id, error = response_content$error))
  }
  if (is.null(response_content$result$data)) {
    final_data <- response_content$result
  }
  else {
    final_data <- response_content$result$data
  }
  if (return_method != "list") {
    final_data <- sst_lists_to_df(final_data)
  }
  return(list(job_id = response_content$id, data = final_data, 
              summary_info = response_content$result$summary_info))
}

sst_sa_keywords_info <- function (api_token, keywords, se, sort = NULL, return_method = "list") 
{
  api_params <- list(keywords = as.list(keywords), se = se, 
                     sort = sort)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatKeywordProcedure.getKeywordsInfo", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method)
}




soa_get_basic_keyword_info <- function(keywords_all_df,regions, api_token) { 
  
  if (exists("kws_info")) rm(kws_info)
  
  for (region in regions) { 
    kw0 <- keywords_all[grep(region,keywords_all$region), ] 
    for (i in 0:floor(nrow(kw0)/100)) { # querying 100 kws at a time
      from <- i*100 + 1
      to <- from + 99
      print(paste("Working on region: ",region," keywords ",from," to ",to))
      tmp <- sst_sa_keywords_info(
        api_token,
        keywords = na.omit(kw0$keyword[from:to]),
        se=region,
        sort = NULL,
        return_method = "list"
      )
      if (length(tmp$error)==0) { 
        if (exists("kws_info")) {
          tmp2 <- soa_serp_list_2_df(tmp$data )
          tmp2$region <- region 
          kws_info <- bind_rows(kws_info,tmp2)
        } else {
          kws_info <- soa_serp_list_2_df(tmp$data )
          kws_info$region <- region 
        }
      } else {  
        if (!length(tmp$error)==0) print(paste("ERROR was thrown at ",from,to))
      }
    }
  }
  
  kws_info$volume <- kws_info$region_queries_count 
  kws_info$region_queries_count <- NULL
  kws_info$keyword <- as.character(kws_info$keyword ) 
  kws_info$volume <-  as.numeric(as.character(kws_info$volume))
  kws_info$cost <-  as.numeric(as.character(kws_info$cost))
  kws_info$difficulty <-  as.numeric(as.character(kws_info$difficulty))
  kws_info <- kws_info[!is.na(kws_info$volume), ] 
  
  kws_info <- data.frame(kws_info)
  return(kws_info)
}

ssa_sa_keyword_top <- function (api_token, keyword, se, top_size = 100) 
{
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatKeywordProcedure.getKeywordTop", 
                                          api_params = list(keyword = keyword, se = se, filters = list(top_size = top_size)))
  sst_return_check(response_content, return_method = "list")
}


sst_lists_to_df <- function (lists, fill = NA) 
{
  column_names <- unique(unlist(lapply(lists, names)))
  if (!is.null(column_names)) {
    lists <- lapply(lists, function(x) {
      single_list <- c(x, sapply(setdiff(column_names, 
                                         names(x)), function(y) fill))
      single_list <- single_list[order(names(single_list))]
    })
  }
  data_frame <- data.frame(do.call(rbind, lists), row.names = NULL, 
                           stringsAsFactors = FALSE)
  if (is.null(column_names)) 
    names(data_frame) <- "column"
  return(data_frame)
}


soa_get_toprankings <- function(kws_info, api_token, top_n=20){  
  require(serpstatr)
  if (exists("out_")) rm(out_)
  for (region in unique(kws_info$region )) { 
    i <- 0
    tmp0 <- kws_info[kws_info$region==region, ] 
    
    for (keyword in unique(tmp0$keyword)) {
      i <- i + 1
      print(paste0("Running number ",i," out of ",length(unique(tmp0$keyword))," keywords!"))
      tmp <- sst_sa_keyword_top(api_token, keyword, se=region, top_size = top_n)
      if (!names(tmp[2])=="error") {
        out0 <- sst_lists_to_df(tmp$data$top, fill = "None") 
        out0$domain <- unlist(out0$domain ) 
        out0$position <- unlist(out0$position ) 
        out0$url <- unlist(out0$url ) 
        out0$subdomain <- NULL 
        out0$types <- NULL 
        out0 <- data.frame(keyword=keyword,out0)
        out0$region <- region 
        if (exists("out_")) {out_ <- bind_rows(out_,out0)} else {out_ <- out0}
      }
    }
    out_$domain <- as.character(out_$domain)
    out_$position <- as.numeric(out_$position)
    out_$url <- as.character(out_$url)
    # marked + dato + project_name + site cluster
    
    out_$date <- gsub("-","",as.character(Sys.Date()))
    
  }
  return(out_)
}

soa_keyword_crawler <- function(keywords,urls) {
  library(rvest)
  library(stringr)
  library(xml2)
  library(httr)
  library(R.utils)
  html_elements <- rvest::html_nodes
  
  nvl <- function(x=NULL,replace=0) ifelse(is.na(x),replace,x)
  
  # when checking links
  # html_elements(webpage,'link')
  
  # _any matches any of the words in the keyword; _all mathches the entire keyword string
  outdf <- data.frame(keyword = character(),
                      url=character(),
                      any_robots = numeric(),
                      any_sitemap = numeric(),
                      
                      meta_exists = numeric(),
                      in_meta_logic_all = numeric(),
                      in_meta_logic_any = numeric(),
                      in_meta_n_all = numeric(),
                      in_meta_n_any = numeric(),
                      
                      n_links = numeric(),
                      n_links_internal = numeric(),
                      in_links_logic_all = numeric(),
                      in_links_logic_any = numeric(),
                      in_links_n_all = numeric(),
                      in_links_n_any = numeric(),
                      
                      n_imgs = numeric(),
                      n_imgs_internal = numeric(),
                      in_imgs_logic_all = numeric(),
                      in_imgs_logic_any = numeric(),
                      in_imgs_n_all = numeric(),
                      in_imgs_n_any = numeric(),
                      
                      n_as = numeric(),
                      in_as_logic_all = numeric(),
                      in_as_logic_any = numeric(),
                      in_as_n_all = numeric(),
                      in_as_n_any = numeric(),
                      
                      n_ps = numeric(),
                      in_ps_logic_all = numeric(),
                      in_ps_logic_any = numeric(),
                      in_ps_n_all = numeric(),
                      in_ps_n_any = numeric(),
                      
                      in_url_logic_all = numeric(),
                      in_url_logic_any = numeric(),
                      
                      in_H1_logic_all = numeric(),
                      in_H1_logic_any = numeric(),
                      in_H1_pos = numeric(),
                      in_H1_n_all = numeric(),
                      in_H1_n_any = numeric(),
                      H1_length_mean = numeric(),
                      H1_length_sum = numeric(),
                      H1_n = numeric(),
                      
                      in_H1_1_logic_all = numeric(),
                      in_H1_1_logic_any = numeric(),
                      in_H1_1_pos = numeric(),
                      in_H1_1_n_all = numeric(),
                      in_H1_1_n_any = numeric(),
                      H1_1_length = numeric(),
                      
                      in_H1_2_logic_all = numeric(),
                      in_H1_2_logic_any = numeric(),
                      in_H1_2_pos = numeric(),
                      in_H1_2_n_all = numeric(),
                      in_H1_2_n_any = numeric(),
                      H1_2_length = numeric(),
                      
                      in_H2_logic_all = numeric(),
                      in_H2_logic_any = numeric(),
                      in_H2_pos = numeric(),
                      in_H2_n_all = numeric(),
                      in_H2_n_any = numeric(),
                      H2_length_mean = numeric(),
                      H2_length_sum = numeric(),
                      H2_n = numeric(),
                      
                      in_H2_1_logic_all = numeric(),
                      in_H2_1_logic_any = numeric(),
                      in_H2_1_pos = numeric(),
                      in_H2_1_n_all = numeric(),
                      in_H2_1_n_any = numeric(),
                      H2_1_length = numeric(),
                      
                      in_H2_2_logic_all = numeric(),
                      in_H2_2_logic_any = numeric(),
                      in_H2_2_pos = numeric(),
                      in_H2_2_n_all = numeric(),
                      in_H2_2_n_any = numeric(),
                      H2_2_length = numeric(),
                      
                      in_H2_3_logic_all = numeric(),
                      in_H2_3_logic_any = numeric(),
                      in_H2_3_pos = numeric(),
                      in_H2_3_n_all = numeric(),
                      in_H2_3_n_any = numeric(),
                      H2_3_length = numeric(),
                      
                      in_H3_logic_all = numeric(),
                      in_H3_logic_any = numeric(),
                      in_H3_pos = numeric(),
                      in_H3_n_all = numeric(),
                      in_H3_n_any = numeric(),
                      H3_length_mean = numeric(),
                      H3_length_sum = numeric(),
                      H3_n = numeric(),
                      
                      in_H3_1_logic_all = numeric(),
                      in_H3_1_logic_any = numeric(),
                      in_H3_1_pos = numeric(),
                      in_H3_1_n_all = numeric(),
                      in_H3_1_n_any = numeric(),
                      H3_1_length = numeric(),
                      
                      in_H3_2_logic_all = numeric(),
                      in_H3_2_logic_any = numeric(),
                      in_H3_2_pos = numeric(),
                      in_H3_2_n_all = numeric(),
                      in_H3_2_n_any = numeric(),
                      H3_2_length = numeric(),
                      
                      in_H3_3_logic_all = numeric(),
                      in_H3_3_logic_any = numeric(),
                      in_H3_3_pos = numeric(),
                      in_H3_3_n_all = numeric(),
                      in_H3_3_n_any = numeric(),
                      H3_3_length = numeric(),
                      
                      in_TITLE_logic_all = numeric(),
                      in_TITLE_logic_any = numeric(),
                      in_TITLE_pos = numeric(),
                      in_TITLE_n_all = numeric(),
                      in_TITLE_n_any = numeric(),
                      TITLE_length = numeric(),
                      
                      in_DESC_logic_all = numeric(),
                      in_DESC_logic_any = numeric(),
                      in_DESC_pos = numeric(),
                      in_DESC_n_all = numeric(),
                      in_DESC_n_any = numeric(),
                      DESC_length = numeric(),
                      
                      in_BODY_logic_all = numeric(),
                      in_BODY_logic_any = numeric(),
                      in_BODY_pos = numeric(),
                      in_BODY_n_all = numeric(),
                      in_BODY_n_any = numeric(),
                      
                      BODY_length_chr = numeric(),
                      BODY_length_words = numeric(),
                      BODY_length_chr_raw = numeric(),
                      BODY_length_words_raw = numeric(),
                      stringsAsFactors = FALSE
  )
  
  outdf_cont <- data.frame(
    url=character(),
    site_content = character(),
    stringsAsFactors = FALSE
  )
  
  u <- 0
  for (url in urls) {
    u <- u + 1 
    print(paste0("Crawling #",u," out of ",length(urls)," urls:",url))

    tryCatch(
      {
        tst <- withTimeout({

          webpage <- read_html(as.character(url))
          
          # finding domain and then robot.txt
          find2slash <- ifelse(is.na(str_locate(url,"//")[1]),1,str_locate(url,"//")[1]+2)
          tmpurl <- substr(url,find2slash,nchar(url))
          findslash <- ifelse(is.na(str_locate(tmpurl,"/")[1]),nchar(tmpurl),str_locate(tmpurl,"/")[1])
          urldomain <- gsub("www.","",gsub("/","",substr(tmpurl,1,findslash)))
          roboturl <- paste0("https://",urldomain,"/robots2.txt")
          
          rm(robot)
          tryCatch(
            {
              tst <- withTimeout({
                robot <- read_html(roboturl) 
              }, timeout=30, onTimeout="warning")
            }
            ,
            error=function(e) {
            }
          )
          if (exists("robot")) {
            robotexists <- 1 
            sitemapexists <- length(grep("sitemap",tolower(robot)))
          } else {
            robotexists <- 0
            sitemapexists <- 0
          }
          
          as <- tolower(html_elements(webpage,'a'))
          n_as <- length(as)
          
          meta <- tolower(html_elements(webpage,'meta'))
          meta_exists <- sign(length(meta))
          meta <- paste(meta,collapse = " ")
          
          links <- tolower(html_elements(webpage,'link'))
          n_links <- length(links)
          n_links_internal <- length(grep(urldomain,links))
          
          imgs <- tolower(html_elements(webpage,'img'))
          n_imgs <- length(imgs)
          n_imgs_internal <- length(grep(urldomain,imgs))
          
          h3s <- paste(tolower(html_text(html_elements(webpage,'h3'))),tolower(html_text(html_elements(webpage,'H3'))))
          h2s <- paste(tolower(html_text(html_elements(webpage,'h2'))),tolower(html_text(html_elements(webpage,'H2'))))
          h1s <- paste(tolower(html_text(html_elements(webpage,'h1'))),tolower(html_text(html_elements(webpage,'H1'))))
          titel <- paste(tolower(html_text(html_elements(webpage,'title'))),tolower(html_text(html_elements(webpage,'TITLE'))))[1]
          bdy <- paste(tolower(html_text(html_elements(webpage,'body'))),tolower(html_text(html_elements(webpage,'BODY'))))
          #xbdy <-  tolower(html_text(html_elements(webpage,'xbody')))
          desc <-  grep("description",html_elements(webpage,'meta'),value=TRUE)[1]
          desc <- substr(desc,str_locate(desc,"content"),nchar(desc))
          desc <- substr(desc,str_locate_all(desc,'"')[[1]][1]+1,str_locate_all(desc,'"')[[1]][2]-2)
          desc <- tolower(desc)
          ps <- paste(tolower(html_text(html_elements(webpage,'p'))),tolower(html_text(html_elements(webpage,'P'))))
          n_ps <- length(ps)
          
          site_content <- paste(paste(titel,collapse=";"),";",paste(desc,collapse=";"),";",paste(h1s,collapse=";"),";",
                                paste(h2s,collapse=";"),";",paste(h3s,collapse=";"),";",paste(ps,collapse=";"))
          outdf_cont <- rbind(outdf_cont,list(url=url,site_content=site_content))
          outdf_cont$url <- as.character(outdf_cont$url) 
          outdf_cont$site_content <- as.character(outdf_cont$site_content) 
          
          H1_length_mean <- mean(nchar(as.character(h1s)))
          H1_length_sum <- sum(nchar(as.character(h1s)))
          H1_1_length <- nvl(nchar(as.character(h1s[1] )),0)
          H1_2_length <- nvl(nchar(as.character(h1s[2] )),0)
          H1_n <- length(h1s)
          
          H2_length_mean <- mean(nchar(as.character(h2s)))
          H2_length_sum <- sum(nchar(as.character(h2s)))
          H2_1_length <- nvl(nchar(as.character(h2s[1])),0)
          H2_2_length <- nvl(nchar(as.character(h2s[2])),0)
          H2_3_length <- nvl(nchar(as.character(h2s[3])),0)
          H2_n <- length(h2s)
          
          H3_length_mean <- mean(nchar(as.character(h3s)))
          H3_length_sum <- sum(nchar(as.character(h3s)))
          H3_1_length <- nvl(nchar(as.character(h3s[1])),0)
          H3_2_length <- nvl(nchar(as.character(h3s[2])),0)
          H3_3_length <- nvl(nchar(as.character(h3s[3])),0)
          H3_n <- length(h3s)
          
          DESC_length <- nvl(mean(nchar(as.character(desc))),0)
          
          BODY_length_chr <- sum(nchar(as.character(ps)))
          BODY_length_words <- lengths(gregexpr("\\W+", paste(ps,collapse=". "))) + 1
          BODY_length_chr_raw <- nchar(as.character(bdy)[1])
          BODY_length_words_raw <- lengths(gregexpr("\\W+", as.character(bdy)[1])) + 1
          TITLE_length <- mean(nchar(as.character(titel)))
          
          for (kw in keywords) {
            
            in_url_logic_all <- sign(length(grep(gsub(" ","-",kw),url)))
            in_url_logic_any <- sign(length(grep(gsub(" ","|",kw),url)))
            in_url_n_any <- length(gregexpr(gsub(" ","|",kw),url)[[1]])
            
            in_meta_logic_all <- sign(length(which(!unlist(gregexpr(kw,meta))==-1)))
            in_meta_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(meta, collapse=" "))))
            in_meta_n_all <- length(which(!unlist(gregexpr(kw,meta))==-1))
            in_meta_n_any <- length(grep(gsub(" ","|",kw),unlist(str_split(paste(meta, collapse=" ")," "))))
            
            in_links_logic_all <- sign(length(which(!unlist(gregexpr(kw,links))==-1)))
            in_links_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(links, collapse=" "))))
            in_links_n_all <- str_count(paste(links, collapse=" "),kw)
            in_links_n_any <- str_count(paste(links, collapse=" "),gsub(" ","|",kw))
            
            in_imgs_logic_all <- sign(length(which(!unlist(gregexpr(kw,imgs))==-1)))
            in_imgs_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(imgs, collapse=" "))))
            in_imgs_n_all <- str_count(paste(imgs, collapse=" "),kw)
            in_imgs_n_any <- str_count(paste(imgs, collapse=" "),gsub(" ","|",kw))
            
            in_as_logic_all <- sign(length(which(!unlist(gregexpr(kw,as))==-1)))
            in_as_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(as, collapse=" "))))
            in_as_n_all <- str_count(paste(as, collapse=" "),kw)
            in_as_n_any <- str_count(paste(as, collapse=" "),gsub(" ","|",kw))
            
            in_ps_logic_all <- sign(length(which(!unlist(gregexpr(kw,ps))==-1)))
            in_ps_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(ps, collapse=" "))))
            in_ps_n_all <- str_count(paste(ps, collapse=" "),kw)
            in_ps_n_any <- str_count(paste(ps, collapse=" "),gsub(" ","|",kw))
            
            in_H1_logic_all <- sign(length(which(!unlist(gregexpr(kw,h1s))==-1)))
            in_H1_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(h1s, collapse=" "))))
            in_H1_n_all <- str_count(paste(h1s, collapse=" "),kw)
            in_H1_n_any <- str_count(paste(h1s, collapse=" "),gsub(" ","|",kw))
            in_H1_pos <- str_locate(tolower(as.character(paste(h1s,collapse=","))), kw)[1]
            
            in_H1_1_logic_all <- sign(length(which(!unlist(gregexpr(kw,h1s[1]))==-1)))
            in_H1_1_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(h1s[1], collapse=" "))))
            in_H1_1_n_all <- str_count(paste(h1s[1], collapse=" "),kw)
            in_H1_1_n_any <- str_count(paste(h1s[1], collapse=" "),gsub(" ","|",kw))
            in_H1_1_pos <- str_locate(tolower(as.character(paste(h1s[1],collapse=","))), kw)[1]
            
            in_H1_2_logic_all <- sign(length(which(!unlist(gregexpr(kw,h1s[2]))==-1)))
            in_H1_2_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(h1s[2], collapse=" "))))
            in_H1_2_n_all <- str_count(paste(h1s[2], collapse=" "),kw)
            in_H1_2_n_any <- str_count(paste(h1s[2], collapse=" "),gsub(" ","|",kw))
            in_H1_2_pos <- str_locate(tolower(as.character(paste(h1s[2],collapse=","))), kw)[1]
            
            in_H2_logic_all <- sign(length(which(!unlist(gregexpr(kw,h2s))==-1)))
            in_H2_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(h2s, collapse=" "))))
            in_H2_n_all <- str_count(paste(h2s, collapse=" "),kw)
            in_H2_n_any <- str_count(paste(h2s, collapse=" "),gsub(" ","|",kw))
            in_H2_pos <- str_locate(tolower(as.character(paste(h2s,collapse=","))), kw)[1]
            
            in_H2_1_logic_all <- sign(length(which(!unlist(gregexpr(kw,h2s[1] ))==-1)))
            in_H2_1_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(h2s[1], collapse=" "))))
            in_H2_1_n_all <- str_count(paste(h2s[1] , collapse=" "),kw)
            in_H2_1_n_any <- str_count(paste(h2s[1] , collapse=" "),gsub(" ","|",kw))
            in_H2_1_pos <- str_locate(tolower(as.character(paste(h2s[1],collapse=","))), kw)[1]
            
            in_H2_2_logic_all <- sign(length(which(!unlist(gregexpr(kw,h2s[2] ))==-1)))
            in_H2_2_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(h2s[2], collapse=" "))))
            in_H2_2_n_all <- str_count(paste(h2s[2] , collapse=" "),kw)
            in_H2_2_n_any <- str_count(paste(h2s[2] , collapse=" "),gsub(" ","|",kw))
            in_H2_2_pos <- str_locate(tolower(as.character(paste(h2s[2],collapse=","))), kw)[1]
            
            in_H2_3_logic_all <- sign(length(which(!unlist(gregexpr(kw,h2s[3] ))==-1)))
            in_H2_3_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(h2s[3], collapse=" "))))
            in_H2_3_n_all <- str_count(paste(h2s[3] , collapse=" "),kw)
            in_H2_3_n_any <- str_count(paste(h2s[3] , collapse=" "),gsub(" ","|",kw))
            in_H2_3_pos <- str_locate(tolower(as.character(paste(h2s[3],collapse=","))), kw)[1]
            
            in_H3_logic_all <- sign(length(which(!unlist(gregexpr(kw,h3s))==-1)))
            in_H3_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(h3s, collapse=" "))))
            in_H3_n_all <- str_count(paste(h3s, collapse=" "),kw)
            in_H3_n_any <- str_count(paste(h3s, collapse=" "),gsub(" ","|",kw))
            in_H3_pos <- str_locate(tolower(as.character(paste(h3s,collapse=","))), kw)[1]
            
            in_H3_1_logic_all <- sign(length(which(!unlist(gregexpr(kw,h3s[1]))==-1)))
            in_H3_1_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(h3s[1], collapse=" "))))
            in_H3_1_n_all <- str_count(paste(h3s[1], collapse=" "),kw)
            in_H3_1_n_any <- str_count(paste(h3s[1], collapse=" "),gsub(" ","|",kw))
            in_H3_1_pos <- str_locate(tolower(as.character(paste(h3s[1],collapse=","))), kw)[1]
            
            in_H3_2_logic_all <- sign(length(which(!unlist(gregexpr(kw,h3s[2]))==-1)))
            in_H3_2_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(h3s[2], collapse=" "))))
            in_H3_2_n_all <- str_count(paste(h3s[2], collapse=" "),kw)
            in_H3_2_n_any <- str_count(paste(h3s[2], collapse=" "),gsub(" ","|",kw))
            in_H3_2_pos <- str_locate(tolower(as.character(paste(h3s[2],collapse=","))), kw)[1]
            
            in_H3_3_logic_all <- sign(length(which(!unlist(gregexpr(kw,h3s[3]))==-1)))
            in_H3_3_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(h3s[3], collapse=" "))))
            in_H3_3_n_all <- str_count(paste(h3s[3], collapse=" "),kw)
            in_H3_3_n_any <- str_count(paste(h3s[3], collapse=" "),gsub(" ","|",kw))
            in_H3_3_pos <- str_locate(tolower(as.character(paste(h3s[3],collapse=","))), kw)[1]
            
            in_DESC_logic_all <- sign(length(which(!unlist(gregexpr(kw,desc))==-1)))
            in_DESC_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(desc, collapse=" "))))
            in_DESC_n_all <- str_count(paste(desc, collapse=" "),kw)
            in_DESC_n_any <- str_count(paste(desc, collapse=" "),gsub(" ","|",kw))
            in_DESC_pos <- str_locate(tolower(as.character(paste(desc,collapse=","))), kw)[1]
            
            in_BODY_logic_all <- sign(length(which(!unlist(gregexpr(kw,bdy))==-1)))
            in_BODY_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(bdy, collapse=" "))))
            in_BODY_n_all <- str_count(paste(bdy, collapse=" "),kw)
            in_BODY_n_any <- str_count(paste(bdy, collapse=" "),gsub(" ","|",kw))
            in_BODY_pos <- str_locate(tolower(as.character(paste(bdy,collapse=","))), kw)[1]
            
            in_TITLE_logic_all <- sign(length(which(!unlist(gregexpr(kw,titel))==-1)))
            in_TITLE_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(titel, collapse=" "))))
            in_TITLE_n_all <- str_count(paste(titel, collapse=" "),kw)
            in_TITLE_n_any <- str_count(paste(titel, collapse=" "),gsub(" ","|",kw))
            in_TITLE_pos <- str_locate(tolower(as.character(paste(titel,collapse=","))), kw)[1]
            
            out0df <- list(keyword=kw,
                           url=url,
                           any_robots = robotexists,
                           any_sitemap = sitemapexists,
                           
                           meta_exists = meta_exists,
                           in_meta_logic_all = in_meta_logic_all,
                           in_meta_logic_any = in_meta_logic_any,
                           in_meta_n_all = in_meta_n_all,
                           in_meta_n_any = in_meta_n_any,
                           
                           n_links=n_links,
                           n_links_internal = n_links_internal,
                           in_links_logic_all = in_links_logic_all,
                           in_links_logic_any =in_links_logic_any,
                           in_links_n_all = in_links_n_all,
                           in_links_n_any = in_links_n_any,
                           
                           n_imgs=n_imgs,
                           n_imgs_internal = n_imgs_internal,
                           in_imgs_logic_all = in_imgs_logic_all,
                           in_imgs_logic_any =in_imgs_logic_any,
                           in_imgs_n_all = in_imgs_n_all,
                           in_imgs_n_any = in_imgs_n_any,
                           
                           n_as=n_as,
                           in_as_logic_all = in_as_logic_all,
                           in_as_logic_any =in_as_logic_any,
                           in_as_n_all = in_as_n_all,
                           in_as_n_any = in_as_n_any,
                           
                           n_ps=n_ps,
                           in_ps_logic_all = in_ps_logic_all,
                           in_ps_logic_any =in_ps_logic_any,
                           in_ps_n_all = in_ps_n_all,
                           in_ps_n_any = in_ps_n_any,
                           
                           in_url_logic_all=in_url_logic_all,
                           in_url_logic_any=in_url_logic_any,
                           
                           in_H1_logic_all = in_H1_logic_all,
                           in_H1_logic_any = in_H1_logic_any,
                           in_H1_pos = in_H1_pos,
                           in_H1_n_all = in_H1_n_all,
                           in_H1_n_any = in_H1_n_any,
                           H1_length_mean = H1_length_mean,
                           H1_length_sum = H1_length_sum,
                           H1_n = H1_n,
                           
                           in_H1_1_logic_all = in_H1_1_logic_all,
                           in_H1_1_logic_any = in_H1_1_logic_any,
                           in_H1_1_pos = in_H1_1_pos,
                           in_H1_1_n_all = in_H1_1_n_all,
                           in_H1_1_n_any = in_H1_1_n_any,
                           H1_1_length = H1_1_length,
                           
                           in_H1_2_logic_all = in_H1_2_logic_all,
                           in_H1_2_logic_any = in_H1_2_logic_any,
                           in_H1_2_pos = in_H1_2_pos,
                           in_H1_2_n_all = in_H1_2_n_all,
                           in_H1_2_n_any = in_H1_2_n_any,
                           H1_2_length = H1_2_length,
                           
                           in_H2_logic_all = in_H2_logic_all,
                           in_H2_logic_any = in_H2_logic_any,
                           in_H2_pos = in_H2_pos,
                           in_H2_n_all = in_H2_n_all,
                           in_H2_n_any = in_H2_n_any,
                           H2_length_mean = H2_length_mean,
                           H2_length_sum = H2_length_sum,
                           H2_n = H2_n,
                           
                           in_H2_1_logic_all = in_H2_1_logic_all,
                           in_H2_1_logic_any = in_H2_1_logic_any,
                           in_H2_1_pos = in_H2_1_pos,
                           in_H2_1_n_all = in_H2_1_n_all,
                           in_H2_1_n_any = in_H2_1_n_any,
                           H2_1_length = H2_1_length,
                           
                           in_H2_2_logic_all = in_H2_2_logic_all,
                           in_H2_2_logic_any = in_H2_2_logic_any,
                           in_H2_2_pos = in_H2_2_pos,
                           in_H2_2_n_all = in_H2_2_n_all,
                           in_H2_2_n_any = in_H2_2_n_any,
                           H2_2_length = H2_2_length,
                           
                           in_H2_3_logic_all = in_H2_3_logic_all,
                           in_H2_3_logic_any = in_H2_3_logic_any,
                           in_H2_3_pos = in_H2_3_pos,
                           in_H2_3_n_all = in_H2_3_n_all,
                           in_H2_3_n_any = in_H2_3_n_any,
                           H2_3_length = H2_3_length,
                           
                           in_H3_logic_all = in_H3_logic_all,
                           in_H3_logic_any = in_H3_logic_any,
                           in_H3_pos = in_H3_pos,
                           in_H3_n_all = in_H3_n_all,
                           in_H3_n_any = in_H3_n_any,
                           H3_length_mean = H3_length_mean,
                           H3_length_sum = H3_length_sum,
                           H3_n = H3_n,
                           
                           in_H3_1_logic_all = in_H3_1_logic_all,
                           in_H3_1_logic_any = in_H3_1_logic_any,
                           in_H3_1_pos = in_H3_1_pos,
                           in_H3_1_n_all = in_H3_1_n_all,
                           in_H3_1_n_any = in_H3_1_n_any,
                           H3_1_length = H3_1_length,
                           
                           in_H3_2_logic_all = in_H3_2_logic_all,
                           in_H3_2_logic_any = in_H3_2_logic_any,
                           in_H3_2_pos = in_H3_2_pos,
                           in_H3_2_n_all = in_H3_2_n_all,
                           in_H3_2_n_any = in_H3_2_n_any,
                           H3_2_length = H3_2_length,
                           
                           in_H3_3_logic_all = in_H3_3_logic_all,
                           in_H3_3_logic_any = in_H3_3_logic_any,
                           in_H3_3_pos = in_H3_3_pos,
                           in_H3_3_n_all = in_H3_3_n_all,
                           in_H3_3_n_any = in_H3_3_n_any,
                           H3_3_length = H3_3_length,
                           
                           in_TITLE_logic_all = in_TITLE_logic_all,
                           in_TITLE_logic_any = in_TITLE_logic_any,
                           in_TITLE_pos = in_TITLE_pos,
                           in_TITLE_n_all = in_TITLE_n_all,
                           in_TITLE_n_any = in_TITLE_n_any,
                           TITLE_length = TITLE_length,
                           
                           in_DESC_logic_all = in_DESC_logic_all,
                           in_DESC_logic_any = in_DESC_logic_any,
                           in_DESC_pos = in_DESC_pos,
                           in_DESC_n_all = in_DESC_n_all,
                           in_DESC_n_any = in_DESC_n_any,
                           DESC_length = DESC_length,
                           
                           in_BODY_logic_all = in_BODY_logic_all,
                           in_BODY_logic_any = in_BODY_logic_any,
                           in_BODY_pos = in_BODY_pos,
                           in_BODY_n_all = in_BODY_n_all,
                           in_BODY_n_any = in_BODY_n_any,
                           
                           BODY_length_chr = BODY_length_chr,
                           BODY_length_words = BODY_length_words,
                           BODY_length_chr_raw = BODY_length_chr_raw,
                           BODY_length_words_raw = BODY_length_words_raw,
                           stringsAsFactors = FALSE
            )
            
            outdf <- rbind(outdf,out0df)
            outdf$keyword <- as.character(outdf$keyword)
            outdf$url <- as.character(outdf$url ) 
          }
        }, timeout=30, onTimeout="warning")
      }
      ,
      error=function(e) {
        print(e)
      }
    )
  }
  out <- list(outdf,outdf_cont)
  names(out) <- c("keyword_crawl_df","page_content_df")
  return(out)
}

soa_google_chrome_ux_report <- function(urls,apikey,friendly=TRUE) {
  require(rvest)
  require(stringr)
  require(xml2)
  require(httr)
  require(dplyr)
  if (exists("out_")) rm(out_)
  
  todaysdate <- Sys.Date()
  for (url in urls) {
    if (grepl("http",url)) {
      
      print(paste0("Trying url:",url,""))
      api_response <- httr::POST(url = paste0("https://chromeuxreport.googleapis.com/v1/records:queryRecord?key=",apikey), 
                                 query = list(origin = url), 
                                 add_headers('Content-Type' = "application/json"))
      
      if (friendly==TRUE) Sys.sleep(1) # delay to escape rate limit cap
      
      if (api_response$status_code==200) {
        tmp <- content(api_response)
        tmp <- data.frame(tmp$record$metrics)
        colnames(tmp) <- gsub(".","_",colnames(tmp),fixed=TRUE)
        tmp$url <- url
        tmp$date <- todaysdate
        if (exists("out_")) out_ <- bind_rows(out_,tmp) else out_ <- tmp
      } else {
        print(paste0("FAILURE on: ",url,": ",api_response$status_code))
      }
      
    } else {
      print("ERROR: Url(s) need to be specified with 'https' and 'www' protocol!")
    }
  }
  return(out_) 
}

# Uploading a dataframe (df) to BigQuery ###
senoa_bq_insert <- function(df, # dataframe
                            BQ_table, # table on BQ
                            dataset="seo_output",
                            your_project_id_with_the_billing_source = "seo-rank-prediction-model",
                            project_id_containing_the_data = "seo-rank-prediction-model") {
  
  require(bigrquery)  
  
  for (var in colnames(df)){
    if (is.numeric(df[,var] )) df[,var] <- as.numeric(df[,var] ) 
  } 
  
  bq_auth(email = "access.dk@noaconnect.com")
  name_of_bq_table <- BQ_table
  dataset <- "seo_output" # database (or dataset in BQ)
  bq_table = bq_table(project = your_project_id_with_the_billing_source, 
                      dataset = dataset, 
                      table = name_of_bq_table)
  bq_table_upload(x=bq_table, 
                  values= df, 
                  create_disposition='CREATE_IF_NEEDED', 
                  write_disposition='WRITE_APPEND')
  
} 


# Function to apply BQ table format to dataframe
senoa_change_format_for_bq <- function(df,bqtable){
  tb <- bq_project_query(
    "seo-rank-prediction-model",
    paste0("SELECT * FROM seo-rank-prediction-model.seo_output.",bqtable," limit 100")
  )
  tmp <- bq_table_download(tb)
  
  common <- colnames(df)
  
  missvars <- colnames(tmp)[!colnames(tmp) %in% colnames(df)] 
  if (length(missvars)>0) print(paste0("These variables are missing from finaldata: ",
                                      paste(missvars,collapse=",")))
  
  toomuch <- colnames(df)[!colnames(df) %in% colnames(tmp)] 
  if (length(toomuch)>0) print(paste0("These variables are in finaldata but not in '",bqtable,"': ",
                                       paste(toomuch,collapse=",")))
  
  if (length(toomuch)==0 & length(missvars)==0) {  
    df[common] <- lapply(common, function(x) {
      match.fun(paste0("as.", class(tmp[[x]])))(df[[x]])
    })
    return(df)
  }
}  

# Retrieving Lighthouse data in parallel.

pagespeed_mets_parallel <- function(siteurls, api_key=api_key, 
                                    strategy=c("both","mobile","desktop"), 
                                    n_cores=16) { 
  require(parallel)
  strategy <- as.character(strategy[1])
  start_time <- Sys.time()
  api_key <<- api_key
  
  tryCatch(
    {
      # test each number in sample_numbers for primality
      # create cluster object
      cl <- makeCluster(n_cores)
      if (exists("metout")) rm(metout) 
      if (exists("results_mobile")) rm(results_mobile)
      if (exists("results_desktop")) rm(results_desktop)
      
      if (strategy %in% c("both","mobile")) {
        print(paste0("Running mobile - strategy=",strategy))
        results_mobile <- parSapply(cl, siteurls, pagespeed_mets_mobile )
        for (i in 1:length(results_mobile[3,])) {
          if (!is.null(results_mobile[3,i][[1]]$TBT_value)){  
            url <- results_mobile[1,i] 
            strategy <- results_mobile[2,i]
            mets <- results_mobile[3,][[i]] 
            mets[sapply(mets, is.null)] <- NULL
            tmp <- data.frame(url,strategy,t(unlist(mets)))
            colnames(tmp)[1:2] <- c("url","strategy") 
            if (exists("metout")) {metout <- bind_rows(metout,tmp)} else {metout <- tmp}  
          }
        }
      }
      
      if (strategy %in% c("both","desktop")) { 
        print(paste0("Running desktop - strategy=",strategy))
        results_desktop <- parSapply(cl, siteurls, pagespeed_mets_desktop )
        for (i in 1:length(results_desktop[3,])) {
          if (!is.null(results_desktop[3,i][[1]]$TBT_value)){  
            url <- results_desktop[1,i] 
            strategy <- results_desktop[2,i]
            mets <- results_desktop[3,][[i]] 
            mets[sapply(mets, is.null)] <- NULL
            tmp <- data.frame(url,strategy,t(unlist(mets)))
            colnames(tmp)[1:2] <- c("url","strategy") 
            if (exists("metout")) {metout <- bind_rows(metout,tmp)} else {metout <- tmp}  
          }
        }
      }
      
      # close
      stopCluster(cl)
      
      end_time <- Sys.time()
      bl_time <- end_time - start_time
      print(paste0("Getting the lighthouse data took:", bl_time))

    }
    ,
    error=function(e) {
    }
  )
  
  if (!exists("metout")){
    metout <- rbind(data.frame(url=siteurls,strategy="desktop"),
                 data.frame(url=siteurls,strategy="mobile"))
    metout[,c("TBT_value","TBT_score","CLS_value","CLS_score",       
           "FCP_value","FCP_score","SI_value","SI_score","LCP_value","LCP_score",       
           "TTI_value","TTI_score","Lighthouse_score")] <- NA 
    
  }  
  
  return(metout)
  
}


pagespeed_mets_desktop <- function(siteurl) {
  library(rvest)
  library(stringr)
  library(xml2)
  library(httr)
  #library("rjson")
  
  api_key <- as.character(read.csv("/home/johnw/Documents/snippets/ks.csv")[1,"value"])
  strategy <- "desktop"
  print(paste0("Running: ",siteurl))
  
  rawinfo <- NULL
  rawinfo <- GET(
    url = "https://www.googleapis.com",
    path = "pagespeedonline/v5/runPagespeed",
    query = list(
      url = siteurl, 
      strategy = strategy,
      key = api_key))
  
  cont <- httr::content(rawinfo)
  
  # Total blocking time
  TBT_value <- cont$lighthouseResult$audits$`total-blocking-time`$numericValue
  TBT_score <- cont$lighthouseResult$audits$`total-blocking-time`$score
  
  #Cumulative Layout Shift
  CLS_value <- cont$lighthouseResult$audits$`cumulative-layout-shift`$numericValue
  CLS_score <- cont$lighthouseResult$audits$`cumulative-layout-shift`$score
  
  #First Contentful Paint
  FCP_value <- cont$lighthouseResult$audits$`first-contentful-paint`$numericValue
  FCP_score <- cont$lighthouseResult$audits$`first-contentful-paint`$score
  
  #Speed index
  SI_value <- cont$lighthouseResult$audits$`speed-index`$numericValue
  SI_score <- cont$lighthouseResult$audits$`speed-index`$score
  
  # LCP (Largest Contentful Paint)
  LCP_value <- cont$lighthouseResult$audits$`largest-contentful-paint`$numericValue
  LCP_score <- cont$lighthouseResult$audits$`largest-contentful-paint`$score
  
  # TTI (Time to interactive)
  TTI_value <- cont$lighthouseResult$audits$interactive$numericValue
  TTI_score <- cont$lighthouseResult$audits$interactive$score
  
  Lighthouse_score <- 0.1*FCP_score + 0.1*SI_score + 0.25*LCP_score + 0.1*TTI_score + 0.3*TBT_score + 0.15*CLS_score
  
  performance_mets <- list(TBT_value,TBT_score,CLS_value,CLS_score, FCP_value, FCP_score, SI_value, SI_score, LCP_value, LCP_score, TTI_value, TTI_score, Lighthouse_score)
  names(performance_mets) <- c("TBT_value","TBT_score","CLS_value","CLS_score", "FCP_value", "FCP_score", "SI_value", "SI_score", "LCP_value", "LCP_score", "TTI_value", 
                               "TTI_score", "Lighthouse_score")
  
  out <- list(siteurl, strategy, performance_mets)
  names(out) <- c("siteurl","strategy", "performance_mets")
  return(out)
}


pagespeed_mets_mobile <- function(siteurl) {
  library(rvest)
  library(stringr)
  library(xml2)
  library(httr)
  #library("rjson")
  
  print(paste0("Running: ",siteurl))
  
  api_key <- as.character(read.csv("/home/johnw/Documents/snippets/ks.csv")[1,"value"])
  strategy <- "mobile"
  
  rawinfo <- NULL
  rawinfo <- GET(
    url = "https://www.googleapis.com",
    path = "pagespeedonline/v5/runPagespeed",
    query = list(
      url = siteurl, 
      strategy = strategy,
      key = api_key))
  
  cont <- httr::content(rawinfo)
  
  # Total blocking time
  TBT_value <- cont$lighthouseResult$audits$`total-blocking-time`$numericValue
  TBT_score <- cont$lighthouseResult$audits$`total-blocking-time`$score
  
  #Cumulative Layout Shift
  CLS_value <- cont$lighthouseResult$audits$`cumulative-layout-shift`$numericValue
  CLS_score <- cont$lighthouseResult$audits$`cumulative-layout-shift`$score
  
  #First Contentful Paint
  FCP_value <- cont$lighthouseResult$audits$`first-contentful-paint`$numericValue
  FCP_score <- cont$lighthouseResult$audits$`first-contentful-paint`$score
  
  #Speed index
  SI_value <- cont$lighthouseResult$audits$`speed-index`$numericValue
  SI_score <- cont$lighthouseResult$audits$`speed-index`$score
  
  # LCP (Largest Contentful Paint)
  LCP_value <- cont$lighthouseResult$audits$`largest-contentful-paint`$numericValue
  LCP_score <- cont$lighthouseResult$audits$`largest-contentful-paint`$score
  
  # TTI (Time to interactive)
  TTI_value <- cont$lighthouseResult$audits$interactive$numericValue
  TTI_score <- cont$lighthouseResult$audits$interactive$score
  
  Lighthouse_score <- 0.1*FCP_score + 0.1*SI_score + 0.25*LCP_score + 0.1*TTI_score + 0.3*TBT_score + 0.15*CLS_score
  
  performance_mets <- list(TBT_value,TBT_score,CLS_value,CLS_score, FCP_value, FCP_score, SI_value, SI_score, LCP_value, LCP_score, TTI_value, TTI_score, Lighthouse_score)
  names(performance_mets) <- c("TBT_value","TBT_score","CLS_value","CLS_score", "FCP_value", "FCP_score", "SI_value", "SI_score", "LCP_value", "LCP_score", "TTI_value", 
                               "TTI_score", "Lighthouse_score")
  
  out <- list(siteurl, strategy, performance_mets)
  names(out) <- c("siteurl","strategy", "performance_mets")
  return(out)
}


pagespeed_mets <- function(siteurl, api_key, strategy="desktop") {
  library(rvest)
  library(stringr)
  library(xml2)
  library(httr)
  #library("rjson")
  
  rawinfo <- GET(
    url = "https://www.googleapis.com",
    path = "pagespeedonline/v5/runPagespeed",
    query = list(
      url = siteurl, 
      strategy = strategy,
      key = api_key))
  
  cont <- httr::content(rawinfo)
  
  # Total blocking time
  TBT_value <- cont$lighthouseResult$audits$`total-blocking-time`$numericValue
  TBT_score <- cont$lighthouseResult$audits$`total-blocking-time`$score
  
  #Cumulative Layout Shift
  CLS_value <- cont$lighthouseResult$audits$`cumulative-layout-shift`$numericValue
  CLS_score <- cont$lighthouseResult$audits$`cumulative-layout-shift`$score
  
  #First Contentful Paint
  FCP_value <- cont$lighthouseResult$audits$`first-contentful-paint`$numericValue
  FCP_score <- cont$lighthouseResult$audits$`first-contentful-paint`$score
  
  #Speed index
  SI_value <- cont$lighthouseResult$audits$`speed-index`$numericValue
  SI_score <- cont$lighthouseResult$audits$`speed-index`$score
  
  # LCP (Largest Contentful Paint)
  LCP_value <- cont$lighthouseResult$audits$`largest-contentful-paint`$numericValue
  LCP_score <- cont$lighthouseResult$audits$`largest-contentful-paint`$score
  
  # TTI (Time to interactive)
  TTI_value <- cont$lighthouseResult$audits$interactive$numericValue
  TTI_score <- cont$lighthouseResult$audits$interactive$score
  
  Lighthouse_score <- 0.1*FCP_score + 0.1*SI_score + 0.25*LCP_score + 0.1*TTI_score + 0.3*TBT_score + 0.15*CLS_score
  
  performance_mets <- list(TBT_value,TBT_score,CLS_value,CLS_score, FCP_value, FCP_score, SI_value, SI_score, LCP_value, LCP_score, TTI_value, TTI_score, Lighthouse_score)
  names(performance_mets) <- c("TBT_value","TBT_score","CLS_value","CLS_score", "FCP_value", "FCP_score", "SI_value", "SI_score", "LCP_value", "LCP_score", "TTI_value", 
                               "TTI_score", "Lighthouse_score")
  loadexperience_mets <- data.frame(cont$loadingExperience$metrics)
  
  google_lighthouse_url <- paste0("https://googlechrome.github.io/lighthouse/scorecalc/#FCP=",FCP_value,"&SI=",SI_value,"&FMP=4000&TTI=",TTI_value,"&FCI=6500&LCP=",LCP_value,
                                  "&TBT=",TBT_value,"&CLS=",CLS_value,"&device=",strategy,"&version=8")
  
  orig_loadexperience_mets <- data.frame(cont$originLoadingExperience$metrics)
  
  out <- list(siteurl, strategy, rawinfo,cont,loadexperience_mets,orig_loadexperience_mets, performance_mets, google_lighthouse_url)
  names(out) <- c("siteurl","strategy","rawinfo","cont","loadexperience_mets","orig_loadexperience_mets", "performance_mets", "google_lighthouse_url")
  return(out)
}

senoa_keyw_clustering <- function(searches, 
                                  groupby = NULL,
                                  method=c("btm","taxonomy"), 
                                  title = "search_string_cluster",
                                  excludewords = NULL,
                                  n_clusters=12, n_super_clusters=5, 
                                  taxonomy_file = NULL,
                                  plot_wordclouds_clusters=TRUE, plot_wordclouds_superclusters=TRUE,
                                  plots_to_pdf=FALSE, pdf_file= NULL, 
                                  output_to_excel = TRUE,
                                  output_excel_file="search_string_cluster.xlsx"){
  
  library(quanteda)
  library(NLP)
  library(tm)
  library(RColorBrewer)
  library(wordcloud)
  #library(topicmodels)
  library(SnowballC)
  library(udpipe)
  library(data.table)
  library(stopwords)
  library(BTM)
  library(sqldf)
  library(dplyr)
  library(textplot)
  library(ggraph)
  library(concaveman)
  library(psych)
  library(reshape2)
  library(ca)
  library(stringr)
  library(openxlsx)
  
  keyword <- title # just for naming of files etc.
  method <- method[1]
  
  if (method=="btm") {
    
    #########
    
    # Unsupervised clustering of search strings ####
    x2 <- data.frame(searches)
    x2$text <- searches
    #x2$doc_id <- ifelse(!is.null(groupby), searches[,groupby],"Nogroups")
    x2$doc_id <- seq(1:nrow(x2))
    avoidwords <- c(excludewords)
    x2$text <- str_trim(gsub(paste0(avoidwords,collapse="|"),"",x2$text))
    
    # adding 2-grams (optional but does seem to improve clusters slightly) ####
    train.tokens <- tokens(x2$text, what = "word", 
                           remove_numbers = TRUE, remove_punct = TRUE,
                           remove_symbols = TRUE)
    train.tokens <- tokens_select(train.tokens, stopwords(), selection = "remove")
    train.tokens <- tokens_ngrams(train.tokens, n = 1:2)
    train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
    x3 <- convert(train.tokens.dfm,to="data.frame")
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
    
    # please note that below biterms has failed if 2grams are included
    biterms <- biterms[, cooccurrence(x = lemma,
                                      relevant = upos %in% c("NOUN", "ADJ", "VERB") & nchar(lemma) > 2 & !lemma %in% c(stopwords("en")),
                                      skipgram = 3),
                       by = list(doc_id)]
    
    set.seed(123456)
    traindata <- subset(anno, upos %in% c("NOUN", "ADJ", "VERB") & !lemma %in% stopwords("en") & nchar(lemma) > 2)
    traindata <- na.omit(traindata[, c("doc_id", "lemma")])
    model     <- BTM(traindata, biterms = biterms, k = n_clusters, iter = 2000, background = TRUE, trace = 100)
    
    # Identifying most important terms in each cluster ####
    ord <- terms(model, top_n=30)
    orddf <- ord[[1]]
    orddf$cluster <- 1
    for (i in 2:length(ord)) {
      orddf0 <- ord[[i]]
      orddf0$cluster <- i
      orddf <- rbind(orddf,orddf0)
    }
    orddf <- orddf[order(orddf$cluster,-orddf$probability),]
    
    # Using the 2 most important words in cluster as label for cluster ####
    lalbes <- terms(model,top_n=2)
    labels <- c()
    for (i in 1:length(lalbes)) {labels <- c(labels,paste(lalbes[[i]]$token,collapse="/"))}
    labels <- data.frame(labels)
    labels$cluster <- seq(nrow(labels))
    
    # Plotting identified clusters with labels ####
    btmplot <- plot(model, top_n = n_clusters,
                    title = keyword, 
                    subtitle = paste("Clustering keywords:",keyword),
                    labels = labels
    )
    plot(btmplot)
    
    # Predicting cluster for each search term. NOTE: REPLACES OTHER METHOD !!!!!!!!!
    anno$id <- paste0(anno$doc_id,"_",anno$paragraph_id)
    
    forpred <- unique(data.frame(anno[,c("id","token")]))
    
    pred <- predict(object=model,newdata=forpred,type="sum_b")
    pred <- data.frame(pred)
    pred$cluster <- unlist(apply( pred, 1, which.max))
    pred$id <- row.names(pred)
    pred <- sqldf("select distinct a.*, b.sentence, c.searches 
                from pred a 
                left join anno b on a.id=b.id
                left join x2 c on c.doc_id=b.doc_id")
    pred[, "bestp"] <- apply(pred[, grep("X",colnames(pred))], 1, max)
    
    # Assigning cluster to all search strings ####
    tmp <- sqldf("select distinct a.*, b.* from x2 a 
                                join orddf b on a.text like '%' || token || '%'")
    
    # type 1: combined probability
    tst <- tmp %>% group_by(text,cluster) %>% summarise(bestp = 1- prod((1-probability)))  
    tst <- data.frame(tst)
    
    searchstring_cluster_assignment_unsup <- sqldf("select distinct a.*, labels 
                                            from tst a 
                                            join (select text, max(bestp) as maxbestp
                                                from tst group by text) b on bestp=maxbestp and a.text=b.text
                                            join labels c on c.cluster = a.cluster")
    
    searchstring_cluster_assignment_unsup <- sqldf("select distinct a.*, labels 
                                            from pred a 
                                            join labels c on c.cluster = a.cluster")
    
    searchstring_cluster_assignment_unsup <- searchstring_cluster_assignment_unsup[order(searchstring_cluster_assignment_unsup$cluster,
                                                                                         -searchstring_cluster_assignment_unsup$bestp),]
    #write.csv(searchstring_cluster_assignment_unsup, paste0("searchstring_cluster_assignment_unsup_",keyword,".csv"), row.names=FALSE)
    
    searchstring_cluster_assignment_unsup %>% group_by(cluster, labels) %>% summarise(n=n())
    
    #View(searchstring_cluster_assignment_unsup)
    clusterstats <- searchstring_cluster_assignment_unsup %>% 
      group_by(cluster,labels) %>% 
      summarise(n = n())
    clusterstats$freq <- clusterstats$n/sum(clusterstats$n)   
    
    # super clusters - through PCA ####  
    df2 <- pred
    df2$cluster <- paste0("c_",as.character(df2$cluster))
    df3 <- reshape2::dcast(df2,searches ~ cluster,sum,value.var="bestp")
    for (v in colnames(df3)) df3[is.na(df3[,v]),v] <- 0
    fit2 <- principal(df3[,grep("c_",colnames(df3),value=TRUE)], 
                      nfactors = n_super_clusters, residuals = FALSE,rotate="varimax")
    fit2loads <- data.frame(fit2$loadings[,1:n_super_clusters])
    fit2loads$cluster <- row.names(fit2loads)
    fit2loads$hvilken <- apply( abs(fit2loads[,grep("RC",colnames(fit2loads),value=TRUE)]), 1, which.max)
    fit2loads$supercluster <- colnames(fit2loads[,fit2loads$hvilken ])
    for (i in 1:nrow(fit2loads)) {fit2loads[i,"supercluster"] <- str_split(fit2loads[i,"supercluster"],"\\.") }
    fit2loads[,grep("hvilken",colnames(fit2loads),value=TRUE)] <- NULL
    
    searchstring_cluster_assignment_unsup$cluster <- paste0("c_",searchstring_cluster_assignment_unsup$cluster)
    searchstring_cluster_assignment_unsup <- sqldf("select a.*, supercluster 
                                                  from searchstring_cluster_assignment_unsup a
                                                  join fit2loads b on a.cluster = b.cluster")
    
    grplabels <- function(labels) {paste(unique(unlist(str_split(labels,"/"))),collapse="/")}
    superclusterlabels <- searchstring_cluster_assignment_unsup %>% 
      group_by(supercluster) %>%
      summarise(superclusterlabel = grplabels(labels))
    fit2loads <- sqldf("select a.*, labels, b.superclusterlabel 
                     from fit2loads a 
                     join superclusterlabels b on a.supercluster=b.supercluster 
                     left join labels c on 'c_' || c.cluster = a.cluster")
    
    searchstring_cluster_assignment_unsup <- sqldf("select a.*, superclusterlabel 
                                                  from searchstring_cluster_assignment_unsup a
                                                  left join superclusterlabels b on a.supercluster = b.supercluster")
    
    superclusterstats <- searchstring_cluster_assignment_unsup %>% 
      group_by(supercluster,superclusterlabel) %>%
      summarise(n_search_strings = n(),
                n_clusters = length(unique(cluster)))
    
    # correspondance map clusters vs super clusters ####
    df2 <- cbind(label = paste0(fit2loads$cluster,"_",fit2loads$labels),abs(fit2loads[,grep("RC",colnames(fit2loads),value=TRUE)]))
    for (v in colnames(df2)) df2[is.na(df2[,v]),v] <- 1
    for (v in 2:ncol(df2)) df2[,v] <- as.numeric(df2[,v])
    row.names(df2) <- df2[,1]
    df2[,1] <- NULL
    ca1 = ca(df2)
    plot(ca1, main=paste("Correspondence plot of clusters and superclusters"))
    #plot(ca1, xlim=c(-1,1.5),ylim=c(-1.5,1.5), main=paste("Correspondence plot of",keyword,"clusters"))
    #plot(ca1, xlim=c(-3,3),ylim=c(-3,3), main=paste("Correspondence plot of",keyword,"clusters"))
    
    if (plots_to_pdf==TRUE) {
      dev.off()
      pdf(file = pdf_file,   # The directory you want to save the file in
          width = 9, # The width of the plot in inches
          height = 6) # The height of the plot in inches
      
      # plotting model again but this time inside pdf
      plot(model, top_n = n_clusters,
           title = keyword, 
           subtitle = paste("Clustering keywords:",keyword),
           labels = labels
      )
    }
    
    if (plot_wordclouds_clusters==TRUE) {
      for (grp in unique(searchstring_cluster_assignment_unsup$labels)) {
        #for (grp in unique(searchstring_cluster_assignment_unsup$labels)) {
        # Convert to tm corpus and use its API for some additional fun
        corpus <- Corpus(VectorSource(searchstring_cluster_assignment_unsup[which(searchstring_cluster_assignment_unsup$labels==grp),"searches"]))  # Create corpus object
        
        # Remove English stop words. This could be greatly expanded!
        # Don't forget the mc.cores thing
        corpus <- tm_map(corpus, removeWords, stopwords("en"))  
        
        # Remove numbers. This could have been done earlier, of course.
        corpus <- tm_map(corpus, removeNumbers, mc.cores=1)
        
        # Stem the words. Google if you dont understand
        corpus <- tm_map(corpus, stemDocument)
        
        # Remove the stems associated with our search terms
        corpus <- tm_map(corpus, removeWords, avoidwords)
        
        pal <- brewer.pal(8, "Dark2")
        layout(matrix(c(1, 2), nrow=2), heights=c(1, 8))
        par(mar=rep(0, 4))
        plot.new()
        text(x=0.5, y=0.5, paste0("Cluster: ",grp))
        wordcloud(corpus, min.freq=2, max.words = 200, random.order = FALSE, col = pal, main="title")
      }
    }
    
    if (plots_to_pdf==TRUE) plot(ca1, main=paste("Correspondence plot of clusters and superclusters"))
    
    if (plot_wordclouds_superclusters==TRUE) {
      for (grp in unique(searchstring_cluster_assignment_unsup$superclusterlabel)) {
        #for (grp in unique(searchstring_cluster_assignment_unsup$labels)) {
        # Convert to tm corpus and use its API for some additional fun
        corpus <- Corpus(VectorSource(searchstring_cluster_assignment_unsup[which(searchstring_cluster_assignment_unsup$superclusterlabel==grp),"searches"]))  # Create corpus object
        
        # Remove English stop words. This could be greatly expanded!
        # Don't forget the mc.cores thing
        corpus <- tm_map(corpus, removeWords, stopwords("en"))  
        
        # Remove numbers. This could have been done earlier, of course.
        corpus <- tm_map(corpus, removeNumbers, mc.cores=1)
        
        # Stem the words. Google if you don't understand
        corpus <- tm_map(corpus, stemDocument)
        
        # Remove the stems associated with our search terms!
        corpus <- tm_map(corpus, removeWords, avoidwords)
        
        pal <- brewer.pal(8, "Dark2")
        layout(matrix(c(1, 2), nrow=2), heights=c(1, 8))
        par(mar=rep(0, 4))
        plot.new()
        text(x=0.5, y=0.5, paste0("Super cluster: ",grp))
        wordcloud(corpus, min.freq=2, max.words = 200, random.order = FALSE, col = pal, main="title")
      }
    }
    if (plots_to_pdf) {dev.off()}
    
    if (output_to_excel==TRUE) {
      
      newwb <- createWorkbook(
        creator = "John Westberg",
        title = paste0(keyword," search study"),
        subject = NULL,
        category = NULL
      )
      
      addWorksheet(wb=newwb,sheetName="search_strings", zoom=120)
      writeDataTable(
        wb=newwb,
        sheet="search_strings",
        x=searchstring_cluster_assignment_unsup,
        startCol = 1,
        startRow = 1,
        sep = ", ",
        stack = FALSE
      )
      
      addWorksheet(wb=newwb,sheetName="words", zoom=150)
      writeDataTable(
        wb=newwb,
        sheet="words",
        x=orddf,
        startCol = 1,
        startRow = 1,
        sep = ", ",
        stack = FALSE
      )
      
      addWorksheet(wb=newwb,sheetName="clusterstats", zoom=150)
      writeDataTable(
        wb=newwb,
        sheet="clusterstats",
        x=clusterstats,
        startCol = 1,
        startRow = 1,
        sep = ", ",
        stack = FALSE
      )
      
      addWorksheet(wb=newwb,sheetName="superclusterstats", zoom=150)
      writeDataTable(
        wb=newwb,
        sheet="superclusterstats",
        x=superclusterstats,
        startCol = 1,
        startRow = 1,
        sep = ", ",
        stack = FALSE
      )
      
      saveWorkbook(newwb, paste0(keyword,"_clustering.xlsx"), overwrite = TRUE, returnValue = FALSE)
    } # end if write to excel
    
    out <- list(searchstring_cluster_assignment_unsup,
                orddf,
                clusterstats,
                btmplot,
                superclusterstats,
                ca1)
    names(out) <- c("searchstring_cluster_assignment","words","clusterstats","btmplot","superclusterstats","Cluster_vs_supercluster_correspondance_analysis")
    
  } else {
    print("here") # if taxonomy
  }
  return(out)
}
