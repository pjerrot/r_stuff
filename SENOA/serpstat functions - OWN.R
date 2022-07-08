

# get keywords! Dont exaclty know how that differs from suggestions ####

sst_jw_keyword_getkeywords <- function (api_token, se, 
                                     keyword, page = 1, 
                                     size = 100,
                                     return_method="df") {
  api_params <- list(se = as.character(se), 
                     keyword = keyword, 
                     page = page, 
                     pageSize = size)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatKeywordProcedure.getKeywords", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method)
}

#api_token <- "786fa52270333d18a211144ae3b14104" 
#tst <- sst_jw_keyword_getkeywords(api_token="786fa52270333d18a211144ae3b14104",
#                                  se="g_dk",keyword="Kardiovaskul?r sygdom",page=1,size=500)
#View(tst$data)


# extracting keyword suggestions! ####
sst_jw_keyword_suggestions <- function (api_token, se, 
                                      keyword, page = 1, 
                                      size = 100,
                                      return_method="df") {
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


#tst2 <- sst_jw_keyword_suggestions(api_token,se,keyword="screen share",page=1,size=100)
#View(tst2$data)


# competitors: domains that rank for the given keyword in Google top-20 results. ####
sst_jw_keyword_competitors <- function (api_token, 
                                     se, 
                                     keyword, 
                                     size = 100) {
  api_params <- list(se = as.character(se), 
                     keyword = keyword, 
                     size = size)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatKeywordProcedure.getCompetitors", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="list")
}

#tst <- sst_jw_keyword_competitors(api_token=api_token,se=se,keyword="remote access",size=100)
#tst2 <- sst_lists_to_df(tst$data)
#View(tst2)

# competitors: list of competitors in PPC in Google top-20 results. ####
sst_jw_keyword_ads_competitors <- function (api_token, 
                                        se, 
                                        keyword, 
                                        size = 100,
                                        page=1) {
  api_params <- list(se = as.character(se), 
                     query = keyword, 
                     size = size,
                     page=page)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatKeywordProcedure.getAdsCompetitors", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="list")
}

#tst <- sst_jw_keyword_Ads_competitors(api_token=api_token,se=se,
#                                      keyword="remote access",size=100)
#tst2 <- sst_lists_to_df(tst$data)
#View(tst2)

# competitors: list of competitors from URL ####
sst_jw_url_competitors <- function (api_token, 
                                            se, 
                                            url, 
                                            page=1) {
  api_params <- list(se = as.character(se), 
                     url = url, 
                     page=page)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatUrlProcedure.getUrlCompetitors", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="df")
}



#
#tst <- sst_jw_url_competitors(api_token=api_token,se=se, url="https://www.teamviewer.com")
#tst2 <- sst_lists_to_df(tst$data)
#View(tst2)

# domain info ####
sst_jw_domain_getDomainInfo<- function (api_token, 
                                     se, 
                                     domains) {
  api_params <- list(se = as.character(se), 
                     domains = as.list(domains))
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatDomainProcedure.getDomainsInfo", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="list")
}

#se <- "g_us"
#tst <- sst_jw_domain_getDomainInfo(api_token=api_token,se=se,domains=c("teamviewer.com","nike.com"))
#tst <- sst_jw_domain_getDomainInfo(api_token=api_token,se=se,domains=c("teamviewer.com"))
#tst <- sst_jw_domain_getDomainInfo(api_token=api_token,se="g_dk",domains=c("autouncle.dk"))
#tst2 <- sst_lists_to_df(tst$data)
#View(tst2)

# backlink summary (note diff between searchtypes!) ####
sst_jw_domain_getBacklinkSummary<- function (api_token, 
                                        se="g_us", 
                                        domain,
                                        searchType = "domain") { # or "domain_with_subdomains" 
  api_params <- list(se = as.character(se), 
                     query = domain,
                     searchType=searchType)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatBacklinksProcedure.getSummaryV2", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="list")
}

#tst <- sst_jw_domain_getBacklinkSummary(api_token=api_token,se="g_dk",domain=c("autouncle.dk","teamviewer.com"))
#tst_b <- sst_jw_domain_getBacklinkSummary(api_token=api_token,se="g_dk",domain="autouncle.dk", 
#                                        searchType = "domain_with_subdomains")
#tst2 <- data.frame(tst$data)
#tst2_b <- as.data.frame(tst_b$data)

# organic domain keywords: Which keywords are associated with domain?! ####
sst_jw_keyword_getDomainKeywords<- function (api_token, 
                                      se, 
                                      domain,
                                      url=paste0("https://",domain)) {
  api_params <- list(se = as.character(se), 
                     domain = domain, url=url)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatDomainProcedure.getDomainKeywords", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="list")
}

#tst <- sst_jw_keyword_getDomainKeywords(api_token=api_token,se=se,
#                                        domain="teamviewer.com", url="https://teamviewer.com")
#tst <- sst_jw_keyword_getDomainKeywords(api_token=api_token,se="g_uk",domain="autouncle.co.uk")
#tst <- sst_jw_keyword_getDomainKeywords(api_token=api_token,se="g_dk",domain="adidas.dk", url="https://www.adidas.dk/maend")
#tst2 <- sst_lists_to_df(tst$data)
#View(tst2)
#tst$error

# organic url keywords: Which keywords are associated with url?! ####
sst_jw_keyword_getUrlKeywords<- function(api_token, 
                     se, 
                     url) {
                       api_params <- list(se = as.character(se), 
                                          url=url)
                       response_content <- sst_call_api_method(api_token = api_token, 
                                                               api_method = "SerpstatUrlProcedure.getUrlKeywords", 
                                                               api_params = api_params)
                       sst_return_check(response_content, return_method="df")
                     }

# backlink retrieval! ####
sst_jw_getnewbacklinks <- function (api_token, # see https://serpstat.com/api/318-active-backlinks-getnewbacklinks/#Example
                                    domain,
                                    searchType = "domain", # c(domain, domain_with_subdomains, URL, part_url
                                    page = 1,
                                    size = 100) {
  api_params <- list(query = domain,
                     searchType=searchType,
                     page=page,
                     size=size)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatBacklinksProcedure.getNewBacklinks", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="list")
}

#tst <- sst_jw_getnewbacklinks(api_token=api_token,domain="teamviewer.com", size=50)
#tstdf <- sst_lists_to_df(tst$data)


# referring domains function! ####
sst_jw_getRefDomains <- function (api_token, # see https://serpstat.com/api/318-active-backlinks-getnewbacklinks/#Example
                                  domain,
                                  searchType = "domain", # c(domain, domain_with_subdomains, URL, part_url
                                  page = 1,
                                  size = 100) {
  api_params <- list(query = domain,
                     searchType=searchType,
                     page=page,
                     size=size)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatBacklinksProcedure.getRefDomains", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="list")
}

#tst <- sst_jw_getRefDomains(api_token=api_token,domain="teamviewer.com", size=500)
#tstdf <- sst_lists_to_df(tst$data)


# extracting all urls under a domain! ####
sst_jw_getDomaineUrls <- function (api_token, 
                                  domain,
                                  se="g_us",
                                  page = 1,
                                  size = 100) {
  print("NOTE: Use 'sst_jw_getDomaineTopUrls' function (with same options) instead. It gives same results but also provides estimated traffic!")
  api_params <- list(domain = domain,
                     page=page,
                     se=se,
                     size=size)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatDomainProcedure.getDomainUrls", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="list")
}

#tst <- sst_jw_getDomaineUrls(api_token=api_token,domain="kyowakirinhub.com", size=1000, page=1, se="g_uk")
#tstdf2b <- sst_lists_to_df(tst$data)

# extracting top urls under a domain! ####
sst_jw_getDomaineTopUrls <- function (api_token, 
                                   domain,
                                   se="g_uk",
                                   page = 1,
                                   size = 100) {
  api_params <- list(domain = domain,
                     page=page,
                     se=se,
                     size=size)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatDomainProcedure.getTopUrls", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="list")
}

#tst <- sst_jw_getDomaineTopUrls(api_token=api_token,domain="kyowakirinhub.com", size=1000, page=1)
#tstdf2 <- sst_lists_to_df(tst$data)


# extracting competitors under a domain! ####
sst_jw_getDomaineCompetitors <- function (api_token, 
                                      domain,
                                      se="g_uk",
                                      page = 1,
                                      size = 100) {
  api_params <- list(domain = domain,
                     page=page,
                     se=se,
                     size=size)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatDomainProcedure.getCompetitors", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="list")
}

#tst <- sst_jw_getDomaineCompetitors(api_token=api_token,domain="kyowakirinhub.com", size=1000, page=1)
#tstdf2 <- sst_lists_to_df(tst$data)

#tstdf2 <- sst_jw_return_true_df(tst$data)
#sst_df <- tst$data
#sst_df <- ranking_kws_alt
#sst_df$geo_names <- NULL
#rm(out_)
#tst <- sst_jw_return_true_df(sst_df)

sst_jw_return_true_df <- function(sst_df) {
  #sst_df <- sst_lists_to_df(sst_df)
  #sst_df[is.null(sst_df)] <- NA
  #sst_df[is.na(sst_df)] <- NULL
  for (navn in colnames(sst_df)) {
    tmpval <- c()
    if (is.list(sst_df[,navn])) {
      for (i in 1:length(sst_df[,navn])) {
        tmpval <- c(tmpval,paste(unlist(sst_df[i,navn]),collapse=", "))
      }
    } else {
      tmpval <- sst_df[,navn]
    }
#    tmp <- sst_df[,navn]
#    tmp[sapply(tmp, is.null)] <- NA
#    tmp2 <- unlist(tmp,use.names = FALSE)
#    if (exists("out_")) {out_ <- cbind(out_,tmp2)} else {out_ <- data.frame(tmp2)}
    if (exists("out_")) {out_ <- cbind(out_,tmpval)} else {out_ <- data.frame(tmpval)}
  }
  colnames(out_) <- colnames(sst_df)
  return(out_)
}



# extracting all keywords that a domian is ranking on (google top 100)! ####
sst_jw_getDomaineKWs <- function (api_token, 
                                   domain,
                                   se="g_us",
                                   page = 1,
                                   size = 100) 
{
  api_params <- list(domain = domain,
                     page=page,
                     se=se,
                     size=size)
  response_content <- sst_call_api_method(api_token = api_token, 
                                          api_method = "SerpstatDomainProcedure.getDomainKeywords", 
                                          api_params = api_params)
  sst_return_check(response_content, return_method="df")
}

#tst <- sst_jw_getDomaineKWs(api_token=api_token,domain="autouncle.dk", se="g_dk",size=1000, page=1)
#tstdf2 <- sst_lists_to_df(tst$data)

##### required functions for own wrapper functions ####

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



sst_jw_list_2_df <- function(alist) {
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