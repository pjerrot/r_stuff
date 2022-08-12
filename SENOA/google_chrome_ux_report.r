google_chrome_ux_report <- function(urls,apikey) {
  require(rvest)
  require(stringr)
  require(xml2)
  require(httr)
  
  todaysdate <- Sys.Date()
  for (url in urls) {
    if (grepl("www",url) & grepl("http",url)) {
    
      print(paste0("Trying url:",url,""))
      api_response <- httr::POST(url = paste0("https://chromeuxreport.googleapis.com/v1/records:queryRecord?key=",apikey), 
                                 query = list(origin = url), 
                                 add_headers('Content-Type' = "application/json"))
      if (api_response$status_code==200) {
        tmp <- content(api_response)
        tmp <- data.frame(tmp$record$metrics)
        colnames(tmp) <- gsub(".","_",colnames(tmp),fixed=TRUE)
        tmp$url <- url
        tmp$date <- todaysdate
        if (exists("out_")) out_ <- rbind(out_,tmp) else out_ <- tmp
      } else {
        print("FAILURE on: ",url)
      }
      
    } else {
      print("ERROR: Url(s) need to be specified with 'https' and 'www' protocol!")
    }
  }
  return(out_) 
}

tst <- google_chrome_ux_report(urls = c("https://www.teamviewer.com","https://www.splashtop.com","https://www.anydesk.com"),
                               apikey = google_api_key)

url <- "https://www.teamviewer.com"
