#tmp <- out_positions[10:30,c("keyword","url")] 

#keywords <- unique(out_positions[,"keyword"])[1:50]  
#urls <- tmp[,"url"] 
url <- urls[19] 
#tmp <- keyword_crawler(keywords,urls)
#View(tmp$keyword_crawl_df  )
#ncol(tmp$keyword_crawl_df)
#View(tmp$page_content_df )

keyword_crawler <- function(keywords,urls) {
  library(rvest)
  library(stringr)
  library(xml2)
  library(httr)
  library(R.utils)
  html_elements <- rvest::html_nodes
  
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
                      H1_length = numeric(),
                      H1_n = numeric(),
                      
                      in_H2_logic_all = numeric(),
                      in_H2_logic_any = numeric(),
                      in_H2_pos = numeric(),
                      in_H2_n_all = numeric(),
                      in_H2_n_any = numeric(),
                      H2_length = numeric(),
                      H2_n = numeric(),
                      
                      in_H3_logic_all = numeric(),
                      in_H3_logic_any = numeric(),
                      in_H3_pos = numeric(),
                      in_H3_n_all = numeric(),
                      in_H3_n_any = numeric(),
                      H3_length = numeric(),
                      H3_n = numeric(),
                      
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
  
  for (url in urls) {
    
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
    
    H1_length <- mean(nchar(as.character(h1s)))
    H1_n <- length(h1s)
    H2_length <- mean(nchar(as.character(h2s)))
    H2_n <- length(h2s)
    H3_length <- mean(nchar(as.character(h3s)))
    H3_n <- length(h3s)
    DESC_length <- mean(nchar(as.character(desc)))
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
      in_links_n_all <- length(which(!unlist(gregexpr(kw,links))==-1))
      in_links_n_any <- length(grep(gsub(" ","|",kw),unlist(str_split(paste(links, collapse=" ")," "))))
      
      in_imgs_logic_all <- sign(length(which(!unlist(gregexpr(kw,imgs))==-1)))
      in_imgs_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(imgs, collapse=" "))))
      in_imgs_n_all <- length(which(!unlist(gregexpr(kw,imgs))==-1))
      in_imgs_n_any <- length(grep(gsub(" ","|",kw),unlist(str_split(paste(imgs, collapse=" ")," "))))
      
      in_as_logic_all <- sign(length(which(!unlist(gregexpr(kw,as))==-1)))
      in_as_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(as, collapse=" "))))
      in_as_n_all <- length(which(!unlist(gregexpr(kw,as))==-1))
      in_as_n_any <- length(grep(gsub(" ","|",kw),unlist(str_split(paste(as, collapse=" ")," "))))
      
      in_ps_logic_all <- sign(length(which(!unlist(gregexpr(kw,ps))==-1)))
      in_ps_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(ps, collapse=" "))))
      in_ps_n_all <- length(which(!unlist(gregexpr(kw,ps))==-1))
      in_ps_n_any <- length(grep(gsub(" ","|",kw),unlist(str_split(paste(ps, collapse=" ")," "))))
      
      in_H1_logic_all <- sign(length(which(!unlist(gregexpr(kw,h1s))==-1)))
      in_H1_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(h1s, collapse=" "))))
      in_H1_n_all <- length(which(!unlist(gregexpr(kw,h1s))==-1))
      in_H1_n_any <- length(grep(gsub(" ","|",kw),paste(h1s, collapse=" ")))
      in_H1_pos <- str_locate(tolower(as.character(paste(h1s,collapse=","))), kw)[1]
      
      in_H2_logic_all <- sign(length(which(!unlist(gregexpr(kw,h2s))==-1)))
      in_H2_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(h2s, collapse=" "))))
      in_H2_n_all <- length(which(!unlist(gregexpr(kw,h2s))==-1))
      in_H2_n_any <- length(grep(gsub(" ","|",kw),paste(h2s, collapse=" ")))
      in_H2_pos <- str_locate(tolower(as.character(paste(h2s,collapse=","))), kw)[1]
      
      in_H3_logic_all <- sign(length(which(!unlist(gregexpr(kw,h3s))==-1)))
      in_H3_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(h3s, collapse=" "))))
      in_H3_n_all <- length(which(!unlist(gregexpr(kw,h3s))==-1))
      in_H3_n_any <- length(grep(gsub(" ","|",kw),paste(h3s, collapse=" ")))
      in_H3_pos <- str_locate(tolower(as.character(paste(h3s,collapse=","))), kw)[1]
      
      in_DESC_logic_all <- sign(length(which(!unlist(gregexpr(kw,desc))==-1)))
      in_DESC_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(desc, collapse=" "))))
      in_DESC_n_all <- length(which(!unlist(gregexpr(kw,desc))==-1))
      in_DESC_n_any <- length(grep(gsub(" ","|",kw),paste(desc, collapse=" ")))
      in_DESC_pos <- str_locate(tolower(as.character(paste(desc,collapse=","))), kw)[1]
      
      in_BODY_logic_all <- sign(length(which(!unlist(gregexpr(kw,ps))==-1)))
      in_BODY_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(ps, collapse=" "))))
      in_BODY_n_all <- length(which(!unlist(gregexpr(kw,ps))==-1))
      in_BODY_n_any <- length(grep(gsub(" ","|",kw),paste(ps, collapse=" ")))
      in_BODY_pos <- str_locate(tolower(as.character(paste(ps,collapse=","))), kw)[1]
      
      in_TITLE_logic_all <- sign(length(which(!unlist(gregexpr(kw,titel))==-1)))
      in_TITLE_logic_any <- sign(length(grep(gsub(" ","|",kw),paste(titel, collapse=" "))))
      in_TITLE_n_all <- length(which(!unlist(gregexpr(kw,titel))==-1))
      in_TITLE_n_any <- length(grep(gsub(" ","|",kw),paste(titel, collapse=" ")))
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
                     H1_length = H1_length,
                     H1_n = H1_n,
                     
                     in_H2_logic_all = in_H2_logic_all,
                     in_H2_logic_any = in_H2_logic_any,
                     in_H2_pos = in_H2_pos,
                     in_H2_n_all = in_H2_n_all,
                     in_H2_n_any = in_H2_n_any,
                     H2_length = H2_length,
                     H2_n = H2_n,
                     
                     in_H3_logic_all = in_H3_logic_all,
                     in_H3_logic_any = in_H3_logic_any,
                     in_H3_pos = in_H3_pos,
                     in_H3_n_all = in_H3_n_all,
                     in_H3_n_any = in_H3_n_any,
                     H3_length = H3_length,
                     H3_n = H3_n,
                     
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
  }
  out <- list(outdf,outdf_cont)
  names(out) <- c("keyword_crawl_df","page_content_df")
  return(out)
}

