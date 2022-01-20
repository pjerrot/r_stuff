# init ####
wzy.init <- function(title="output", file="output.html", author="", 
                     leftmargin = 70,
                     cellpadding = 7,
                     width = 900,
                     color_scheme = NULL,
                     color_scheme_manual_colors = NULL,
                     sidepanelscolor="grey",
                     includecontentblock=TRUE, 
                     pdfcopy=FALSE) {
  
  require(pagedown)
  require(httpuv)
  require(RCurl)
  
  if (is.null(color_scheme)) {
    color_scheme <- "default"
  } else {
    if (color_scheme=="manual" & is.null(color_scheme_manual_colors)) {
      stop("You need to specify vector of manual colors to use - in 'color_scheme_manual_colors' parameter.")
    }
  }
  # Available color schemes: c("default","manual","palette1","palette2","palette3","palette4","palette5",
    # "rustic","sky","flower","sand","beach","google","twitter","facebook","microsoft")
  
  .colorstr <- ifelse(color_scheme=="palette1","'#a2b9bc','#b2ad7f','#878f99','#6b5b95','#6b5b95','#feb236','#d64161','#ff7b25'",
                      ifelse(color_scheme=="palette2","'#d6cbd3','#eca1a6','#bdcebe','#ada397','#d5e1df','#e3eaa7','#b5e7a0','#86af49'",
                             ifelse(color_scheme=="palette3","'#b9936c','#dac292','#e6e2d3','#c4b7a6','#3e4444','#82b74b','#405d27','#c1946a'",
                                    ifelse(color_scheme=="palette4","'#92a8d1','#034f84','#f7cac9','#f7786b','#deeaee','#b1cbbb','#eea29a','#c94c4c'",
                                           ifelse(color_scheme=="palette5","'#f0f0f0','#c5d5c5','#9fa9a3','#e3e0cc','#eaece5','#b2c2bf','#c0ded9','#3b3a30'",
                                                  ifelse(color_scheme=="rustic","'#c8c3cc','#563f46','#8ca3a3','#484f4f','#e0e2e4','#c6bcb6','#96897f','#625750'",
                                                         ifelse(color_scheme=="sky","'#bccad6','#8d9db6','#667292','#f1e3dd','#cfe0e8','#b7d7e8','#87bdd8','#daebe8'",
                                                                ifelse(color_scheme=="sand","'#fbefcc','#f9ccac','#f4a688','#e0876a','#fff2df','#d9ad7c','#a2836e','#674d3c'",
                                                                       ifelse(color_scheme=="flower","'#f9d5e5','#eeac99','#e06377','#c83349','#5b9aa0','#d6d4e0','#b8a9c9','#622569'",
                                                                              ifelse(color_scheme=="beach", "'#96ceb4','#ffeead','#ffcc5c','#ff6f69','#588c7e','#f2e394','#f2ae72','#d96459'",
                                                                                     ifelse(color_scheme=="google","'#4285F4','#FBBC05','#34A853','#EA4335'",
                                                                                            ifelse(color_scheme=="twitter","'#55ACEE','#292F33','#66757F','#CCD6DD','#E1E8ED','#FFFFFF'",
                                                                                                   ifelse(color_scheme=="noac","'#28A050','#AFF0D2','#46595A','#A0D583','#142828','#808080','#BCB4A4'",
                                                                                                     ifelse(color_scheme=="facebook","'#3B5998','#8B9DC3','#DFE3EE','#F7F7F7','#FFFFFF'",
                                                                                                            ifelse(color_scheme=="microsoft","'#F65314','#7CBB00','#00A1F1','#FFBB00'",
                                                                                                                   ifelse(color_scheme=="manual",paste0("'",paste(color_scheme_manual_colors, collapse="','"),"'"),""))))))))))))))))
  
  
  
  defcol <- "'#3366cc','#dc3912','#ff9900','#109618','#990099','#0099c6','#dd4477','#66aa00','#b82e2e','#316395','#994499','#22aa99','#aaaa11','#6633cc','#e67300','#8b0707',
'#651067','#329262','#5574a6','#3b3eac','#b77322','#16d620','#b91383','#f4359e','#9c5935','#a9c413','#2a778d','#668d1c','#bea413','#0c5922','#743411'"
  
  .colorstr <<- ifelse(color_scheme=="default",defcol,paste0(.colorstr,",",defcol))   
  
  .wjavafuns <<- c()
  .wcontent <<- c()
  .wcontentblock <<- c()
  .wtitel <<- title
  .wfile <<- file
  .author <<- author
  .leftmargin <<- leftmargin
  .cellpadding <<- cellpadding
  .includecontentblock <<- includecontentblock
  .pdfcopy <<- pdfcopy
  .width <<- width
  .sidepanelscolor <<- sidepanelscolor
}

.replace_ae_oe_aa <- function(str) {
  str <- gsub("æ","&aelig;",str)
  str <- gsub("ø","&oslash;",str)
  str <- gsub("å","&aring;",str)
  str <- gsub("Æ","&AElig;",str)
  str <- gsub("Ø","&Oslash;",str)
  str <- gsub("Å","&Aring;",str)
  return(str)
}

# insert.TITLE ####
wzy.TITLE.insert <- function(str, size=c("h3","h1","h2","h4","h5","h6"), 
                             small=FALSE, 
                             link=NULL, 
                             align="left", 
                             color="black",
                             hr_line=TRUE,
                             Hr_line_color="#E9E8E8") {
  size <- size[1]
  a_name <- paste0(size,"_idx_",floor(runif(1)*10000))
  contentblock_str <- paste0("<a href='#",a_name,"'>",str,"</a>")
  .wcontentblock <<- c(.wcontentblock,contentblock_str)
  
  tmp <- paste0("<table width='100%' cellpadding=",.cellpadding,"><tr><td width=",.leftmargin,"></td><td align=",align,">")
  if (is.null(link)) {
    .wcontent <<- c(.wcontent,paste0(tmp,"<",size,">",ifelse(small==TRUE,"<small>",""),"<a name='",a_name,"' style='text-decoration:none;'><font color='",color,"'><br>",str,"</font></a>",ifelse(small==TRUE,"</small>",""),"</",size,">",ifelse(hr_line==TRUE,paste0("<hr width='100%' color='",Hr_line_color,"'>",""),""),"</td></tr></table>\n"))
  } else {
    .wcontent <<- c(.wcontent,paste0(tmp,"<",size,"><a name='",a_name,"' href='",link,"' style='text-decoration:none;'>",ifelse(small==TRUE,"<small>",""),"<font color='",color,"'><br>",
                                     str,"</font>",ifelse(small==TRUE,"</small>",""),"</a></",size,">",ifelse(hr_line==TRUE,paste0("<hr width='100%' color='",Hr_line_color,"'>"),""),"</td></tr></table>\n"))
  }
}

# insert.HORISONTALLINE ####
wzy.HORISONTALLINE.insert <- function(color="black",thickness=1, width="80%") {
  .wcontent <<- c(.wcontent,paste0("<table width='100%'><tr><td><hr align='center' style='height:",thickness,"px;width:",width,";background-color:",color,"'/></td></tr></table>\n"))
}

# insert.LINEBREAK ####
wzy.LINEBREAK.insert <- function(count=1) {
  for (i in 1:count) {.wcontent <<- c(.wcontent,paste0("<br>"))}
}

# insert.HTML ####
wzy.HTML.insert <- function(str) {.wcontent <<- c(.wcontent,str)}

# insert.TEXT ####
wzy.TEXT.insert <- function(str, fontsize=16, fontface="Arial", 
                            align="left", 
                            boxwidth=850,
                            boxheight=120,
                            scrollbar=FALSE,
                            leftmargin = .leftmargin,
                            cellpadding=10) {
  htmp <- paste0("<table width=",boxwidth," cellpadding=",.cellpadding,"><tr><td width=",.leftmargin,"></td><td align='",align,"' style='font-family:",fontface,"; font-size:",fontsize,"'>",str,"</td></tr></table>")
  if (scrollbar==TRUE) {
    htmp <- paste0("<table align='",align,"' width=",boxwidth," cellpadding=",.cellpadding,"><tr><td width=",
                   .leftmargin,"></td><td align='",align,"' style='font-family:",fontface,
                   "; font-size:",fontsize,"'><div style='height:",boxheight,"px; border:1px solid #ccc;overflow:auto;'>",str,"</div></td></tr></table>")
    if (.pdfcopy==TRUE) warning("Scrollbars does not show correctly in pdf file!")
  }
  
  .wcontent <<- c(.wcontent,htmp)
}

# insert.DATAFRAME ####
wzy.DATAFRAME.insert <- function(df, align="center", maxrows=50, pagesize=10, 
                                 showRowNumber="FALSE", width="100%", height="60%",
                                 title = NULL, titlefontsize=18) {
  tilfstr <- as.character(floor(runif(1)*1000))
  
  htmp <- paste0("google.charts.load('current', {'packages':['table']}); \n")
  htmp <- paste0(htmp,"google.charts.setOnLoadCallback(drawTable",tilfstr,"); \n")
  
  
  htmp <- paste0(htmp, "function drawTable",tilfstr,"() {\n")
  htmp <- paste0(htmp,"  var data = new google.visualization.DataTable(); \n")
  for (var in colnames(df)) {
    if (is.factor(df[,var])) {
      htmp <- paste0(htmp, "data.addColumn('string', '",var,"'); \n")
    } else if (is.numeric(df[,var])) {
      htmp <- paste0(htmp, "data.addColumn('number', '",var,"'); \n")
    } else {
      htmp <- paste0(htmp, "data.addColumn('string', '",var,"'); \n")
    }
  }
  htmp <- paste0(htmp,"data.addRows([\n")
  maxrows <- min(maxrows,nrow(df))
  lastvar <- colnames(df)[ncol(df)]
  
  for (i in 1:maxrows) {
    htmp <- paste0(htmp,"[")
    for (var in colnames(df)) {
      if (is.factor(df[,var])) {
        htmp <- paste0(htmp, "'",df[i,var],"'",ifelse(!var==lastvar,",",""))
      } else if (is.numeric(df[,var])) {
        htmp <- paste0(htmp,df[i,var],ifelse(!var==lastvar,",",""))
      } else {
        htmp <- paste0(htmp, "'",df[i,var],"'",ifelse(!var==lastvar,",",""))
      }
    }
    htmp <- paste0(htmp,"]",ifelse(!i==maxrows,",\n","\n"))
  }
  htmp <- paste0(htmp,"]);\n")
  
  htmp <- paste0(htmp, "var table = new google.visualization.Table(document.getElementById('table_div",tilfstr,"'));\n")
  htmp <- paste0(htmp,"table.draw(data, {page: 'enable', pageSize: ",pagesize,", showRowNumber: ",tolower(showRowNumber),", width: '",width,"', height: '",height,"'});\n")
  htmp <- paste0(htmp, "}\n\n")
  
  .wjavafuns <<- c(.wjavafuns,htmp)
  .wcontent <<- c(.wcontent,paste0("<table width='",width," 'align='",align,"' cellpadding=",.cellpadding,"><tr><td>",title,"</td></tr><tr><td><div id='table_div",tilfstr,"'></div></td></tr></table><br>\n"))
}

#wrapup ####
wzy.wrapup <- function() {
  
  html <- paste("<html>\n")
  html <- paste(html,"<head>\n")
  html <- paste(html,"<link rel='stylesheet' href='https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css' integrity='sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh' crossorigin='anonymous'>\n")
  #html <- paste(html,"<link rel='stylesheet' href='https://maxcdn.bootstrapcdn.com/bootstrap/3.4.0/css/bootstrap.min.css'>\n")
  #html <- paste(html,"<style rel='stylesheet' type='text/css'> body {font-family: Arial, Helvetica, sans-serif; font-size: 13px;} </style>")
  
  html <- paste(html,"<script type='text/javascript' src='https://www.gstatic.com/charts/loader.js'></script>\n")
  html <- paste(html,"<script type='text/javascript'>\n")
  seednum <- 1234
  
  html0 <- ""
  if (length(.wjavafuns)>0) {
    for (i in 1:length(.wjavafuns)) {
      seednum <- seednum + 1000
      set.seed(seednum)
      html0 <- paste(html0,.wjavafuns[i],"\n")
    }
  }
  html <- paste(html,html0)
  
  html <- paste(html,"</script>\n")
  
  html <- paste(html,"</head>\n")
  
  # Creating content block
  #  contblock <- "<h1>Report content</h1><ul>"
  #  for (i in 1:length(.wcontentblock)) {
  #    contblock <- paste0(contblock,"<li>")
  #    contstr <- .wcontentblock[i]
  
  #  }
  
  html <- paste(html,"<body style='background-color:",.sidepanelscolor,";'>\n")
  html <- paste(html,"<table ><tr><td width='17%' bgcolor=",.sidepanelscolor,"></td><td width=",.width," bgcolor='white'>\n")
  
  html0 <- ""
  if (length(.wcontent)>0) {
    for (i in 1:length(.wcontent)) {
      seednum <- seednum + 1000
      set.seed(seednum)
      if(grepl("HTMLTABLE_",.wcontent[i])) {
        tablehtml0 <- .wcontent[i]
        table_id <- substr(.wcontent[i],as.numeric(gregexpr(pattern ='id=',.wcontent[i])[[1]])+3,
                           as.numeric(gregexpr(pattern ='align',.wcontent[i])[[1]])-2)
        for (j in 1:length(.wcontent)) {
          if (grepl("INSERT2HTMLTABLE",.wcontent[j]) & 
              substr(.wcontent[j],as.numeric(gregexpr(pattern ='id=',.wcontent[j])[[1]])+3,
                     as.numeric(gregexpr(pattern ='cell_id',.wcontent[j])[[1]])-2) == table_id) {
            cell_id <- substr(.wcontent[j],as.numeric(gregexpr(pattern ='cell_id=',.wcontent[j])[[1]])+8,
                              as.numeric(gregexpr(pattern =')',.wcontent[j])[[1]])-1)
            content <- .wcontent[j+1]
            .wcontent[j+1] <- ""
            tablehtml0 <- paste(gsub(cell_id,content,tablehtml0, fixed=TRUE),"\n<br>")
          }
        }
        for (k in 1:20) {
          for (l in 1:20) {
            tablehtml0 <- gsub(paste0("[",k,",",l,"]"),"",tablehtml0, fixed=TRUE)
          }
        }
        
        html0 <- paste(html0,as.character(tablehtml0))
      } else if (!grepl("INSERT2HTMLTABLE",.wcontent[i])) {
        html0 <- paste(html0,as.character(.wcontent[i]))
      }
    }
  }
  
  html <- paste(html,html0)
  
  html <- paste(html,"\n</td><td bgcolor='",.sidepanelscolor,"'></td></tr>\n</table>\n</body>\n")
  html <- paste(html,"</html>")
  
  html <- .replace_ae_oe_aa(html)
  
  write.table(html,.wfile,row.names=FALSE,col.names=FALSE,quote=FALSE)   
  print(paste0("Your HTML file has been saved as: ",.wfile))
  
  if (.pdfcopy==TRUE) {
    print("PDF copy is still beta.")
    pdffile <- gsub("html","pdf",.wfile)
    chrome_print(.wfile,pdffile,format="pdf",timeout=50,wait=3)
  }
  
  #return(html)
}

# insert.BARCHART ####
wzy.BARCHART.insert <- function(df, group_var, num_vars, fun=c("asis","sum","mean","median","sd"), 
                                annotation_var = NULL,
                                chart_title=NULL,
                                titlefontsize=18,
                                #subtitle = NULL,
                                legendposition = c("right"),
                                align="center", width="600", height="600",
                                chart_options = NULL) {
  library(dplyr)
  
  tilfstr <- as.character(floor(runif(1)*1000))
  fun <- fun[1]
  
  # prepping data
  if (fun=="asis") {
    df2 <- df
  } else {
    df2 <- df %>% group_by(.data[[group_var]]) %>% summarise_at(.vars=num_vars,.funs=fun)
  }
  
  htmp <- paste0("\ngoogle.charts.load('current', {'packages':['corechart','bar']}); \n")
  htmp <- paste0(htmp,"google.charts.setOnLoadCallback(drawChart",tilfstr,"); \n")
  
  htmp <- paste0(htmp, "function drawChart",tilfstr,"() {\n")
  htmp <- paste0(htmp,"var data = google.visualization.arrayToDataTable([ \n")
  
  # column names 
  htmp <- paste0(htmp, paste0("['",group_var,"','",paste(num_vars,collapse="','"),"'",ifelse(!is.null(annotation_var),", { role: 'annotation' }",""),"],\n"))
  
  # data values
  df2 <- data.frame(df2)
  for (i in 1:nrow(df2)) {
    htmp <- paste0(htmp, paste0("['",as.character(df2[i,group_var]),"',",as.character(paste(df2[i,num_vars],collapse=",")),
                                ifelse(!is.null(annotation_var),paste0(",'",as.character(df2[i,annotation_var]),"'"),""),"]",ifelse(i==nrow(df2),"",","),"\n"))
  }
  
  htmp <- paste0(htmp,"]);\n\n") 
  
  htmp <- paste0(htmp,"var options = {\n")
  htmp <- paste0(htmp,paste0("title: '",ifelse(!is.null(chart_title),chart_title,paste0("Bar chart of ",paste(num_vars, collapse=" and ")," - by ",group_var)),"',\n"))
  htmp <- paste0(htmp,"width: ",width,",\n")
  htmp <- paste0(htmp,"height: ",height,",\n")
  htmp <- paste0(htmp,"colors: [",.colorstr,"],\n")
  htmp <- paste0(htmp,"titleTextStyle: {fontSize: ",titlefontsize,"},\n")
  if (length(num_vars)==1) {htmp <- paste0(htmp,"hAxis: {title: '",gsub("asis ","",paste(fun,num_vars[1])),"'},\n")}
  htmp <- paste0(htmp,"vAxis: {title: '",group_var,"'},\n")
  htmp <- paste0(htmp,"bar: {groupWidth: '95%'},\n")
  htmp <- paste0(htmp,"legend: { position: '",legendposition,"' },\n")

  if (length(chart_options)>0) {
    for (l in 1:length(chart_options)) {
      htmp <- paste0(htmp,chart_options[l],",\n")
    }
  }
  
  htmp <- paste0(htmp,"chartArea: {backgroundColor: {stroke: '#4322c0',strokeWidth: 1}},\n")
  htmp <- paste0(htmp,"};\n\n")
  
  htmp <- paste0(htmp, "var chart = new google.visualization.BarChart(document.getElementById('barchart_values",tilfstr,"'));\n")
  htmp <- paste0(htmp,"chart.draw(data, options);\n")
  htmp <- paste0(htmp, "}\n\n") # closing function...
  
  .wjavafuns <<- c(.wjavafuns,htmp)
  .wcontent <<- c(.wcontent,paste0("<table align='",align,"' cellpadding=",.cellpadding,",><tr><td><div id='barchart_values",tilfstr,"'></div></td></tr></table>\n"))
}

# insert.GGPLOT ####
wzy.GGPLOT.insert <- function(plot, chart_title=NULL, titlefontsize=18,align="center") {
  library(ggplot2)
  library(RCurl)
  library(htmltools)
  
  encodeGraphic <- function(g) {
    png(tf1 <- tempfile(fileext = ".png"))  # Get an unused filename in the session's temporary directory, and open that file for .png structured output.
    print(g)  # Output a graphic to the file
    dev.off()  # Close the file.
    txt <- RCurl::base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")  # Convert the graphic image to a base 64 encoded string.
    myImage <- htmltools::HTML(sprintf('<img src="data:image/png;base64,%s">', txt))  # Save the image as a markdown-friendly html object.
    return(myImage)
  }
  
  hg <- encodeGraphic(plot)  # run the function that base64 encodes the graph
  htmp <- paste0("<table align='",align,"' border=0 width=800 cellpadding=",.cellpadding,"><tr><td align='center'><p style='font-family:arial; font-size:",titlefontsize,"px'><b>&nbsp;&nbsp;&nbsp;",chart_title,"</b></p></td></tr><tr><td align='center'>",hg,"</td></tr></table><br>\n")
  .wcontent <<- c(.wcontent,htmp)
}

# insert.COLUMNCHART ####
wzy.COLUMNCHART.insert <- function(df, group_var, num_vars, 
                                   fun=c("asis","sum","mean","median","sd"), 
                                   stacked = FALSE,
                                   fullstacked = FALSE,
                                   annotation_var = NULL,
                                   chart_title=NULL,
                                   titlefontsize=18,
                                   #subtitle = NULL,
                                   legendposition = c("right"),
                                   align="left", width="700", height="600",
                                   chart_options = NULL) {
  library(dplyr)
  
  tilfstr <- as.character(floor(runif(1)*1000))
  fun <- fun[1]
  
  # prepping data
  if (fun=="asis") {
    df2 <- df
  } else {
    df2 <- df %>% group_by(.data[[group_var]]) %>% summarise_at(.vars=num_vars,.funs=fun)
  }
  
  htmp <- paste0("\ngoogle.charts.load('current', {'packages':['corechart','bar']}); \n")
  htmp <- paste0(htmp,"google.charts.setOnLoadCallback(drawChart",tilfstr,"); \n")
  
  htmp <- paste0(htmp, "function drawChart",tilfstr,"() {\n")
  htmp <- paste0(htmp,"var data = google.visualization.arrayToDataTable([ \n")
  
  # column names 
  htmp <- paste0(htmp, paste0("['",group_var,"','",paste(num_vars,collapse="','"),"'",ifelse(!is.null(annotation_var),", { role: 'annotation' }",""),"],\n"))
  
  # data values
  df2 <- data.frame(df2)
  for (i in 1:nrow(df2)) {
    htmp <- paste0(htmp, paste0("['",as.character(df2[i,group_var]),"',",as.character(paste(df2[i,num_vars],collapse=",")),
                                ifelse(!is.null(annotation_var),paste0(",'",as.character(df2[i,annotation_var]),"'"),""),"]",ifelse(i==nrow(df2),"",","),"\n"))
  }
  
  htmp <- paste0(htmp,"]);\n\n") 
  
  htmp <- paste0(htmp,"var options = {\n")
  htmp <- paste0(htmp,paste0("title: '",ifelse(!is.null(chart_title),chart_title,paste0("Column chart of ",paste(num_vars, collapse=" and ")," - by ",group_var)),"',\n"))
  #if (!is.null(subtitle)) {htmp <- paste0(htmp,"subtitle: '",subtitle,"',\n")}
  htmp <- paste0(htmp,"width: ",width,",\n")
  htmp <- paste0(htmp,"height: ",height,",\n")
  htmp <- paste0(htmp,"chartArea: {backgroundColor: {stroke: '#4322c0',strokeWidth: 1}},\n")
  if (fullstacked==TRUE) {
    htmp <- paste0(htmp,"isStacked: 'percent',\n")
  } else if (stacked==TRUE) {
    htmp <- paste0(htmp,"isStacked: true,\n")
  } else {
    htmp <- htmp
  }
  
  htmp <- paste0(htmp,"colors: [",.colorstr,"],\n")
  htmp <- paste0(htmp,"titleTextStyle: {fontSize: ",titlefontsize,"},\n")
  if (length(num_vars)==1) {htmp <- paste0(htmp,"vAxis: {title: '",gsub("asis ","",paste(fun,num_vars[1])),"'},\n")}
  htmp <- paste0(htmp,"hAxis: {title: '",group_var,"'},\n")
  htmp <- paste0(htmp,"bar: {groupWidth: '95%'},\n")
  htmp <- paste0(htmp,"legend: { position: '",legendposition,"' },\n")
  
  if (length(chart_options)>0) {
    for (l in 1:length(chart_options)) {
      htmp <- paste0(htmp,chart_options[l],",\n")
    }
  }
  
  htmp <- paste0(htmp,"};\n\n")
  
  htmp <- paste0(htmp, "var chart = new google.visualization.ColumnChart(document.getElementById('barchart_values",tilfstr,"'));\n")
  htmp <- paste0(htmp,"chart.draw(data, options);\n")
  htmp <- paste0(htmp, "}\n\n") # closing function...
  
  .wjavafuns <<- c(.wjavafuns,htmp)
  .wcontent <<- c(.wcontent,paste0("<table align='",align,"' cellpadding=",.cellpadding,"><tr><td><div id='barchart_values",tilfstr,"'></div></td></tr></table>\n"))
}

# insert.SCATTERPLOT ####
wzy.SCATTERPLOT.insert <- function(df, x, y,
                                   samplesize = 100,
                                   seed = NULL,
                                   trendline = FALSE,
                                   chart_title=NULL,
                                   titlefontsize=18,
                                   #subtitle = NULL,
                                   legendposition = c("right"),
                                   align="left", width="700", height="600",
                                   chart_options = NULL) {
  library(dplyr)
  
  tilfstr <- as.character(floor(runif(1)*1000))
  
  htmp <- paste0("\ngoogle.charts.load('current', {'packages':['corechart','bar']}); \n")
  htmp <- paste0(htmp,"google.charts.setOnLoadCallback(drawChart",tilfstr,"); \n")
  
  htmp <- paste0(htmp, "function drawChart",tilfstr,"() {\n")
  htmp <- paste0(htmp,"var data = google.visualization.arrayToDataTable([ \n")
  
  # column names 
  htmp <- paste0(htmp, paste0("['",x,"','",y,"'],\n"))
  
  set.seed(ifelse(is.null(seed),1234,seed))
  df <- df[order(runif(nrow(df))),][1:min(samplesize,nrow(df)),]
  
  # data values
  df <- data.frame(df)
  for (i in 1:nrow(df)) {
    htmp <- paste0(htmp, paste0("[",df[i,x],",",df[i,y],collapse=","),"]",ifelse(!i==nrow(df),",",""),"\n")
  }
  
  htmp <- paste0(htmp,"]);\n\n") 
  
  htmp <- paste0(htmp,"var options = {\n")
  htmp <- paste0(htmp,paste0("title: '",ifelse(!is.null(chart_title), chart_title, paste0("Scatter chart of ",x, " vs. ",y)),"',\n"))
  htmp <- paste0(htmp,"width: ",width,",\n")
  htmp <- paste0(htmp,"height: ",height,",\n")
  htmp <- paste0(htmp,"colors: [",.colorstr,"],\n")
  htmp <- paste0(htmp,"chartArea: {backgroundColor: {stroke: '#4322c0',strokeWidth: 1}},\n")
  htmp <- paste0(htmp,"titleTextStyle: {fontSize: ",titlefontsize,"},\n")
  htmp <- paste0(htmp,"vAxis: {title: '",y,"', minValue: ",min(df[,y]) - (max(df[,y]) - min(df[,y]))*0.1,", maxValue: ",max(df[,y]) + (max(df[,y]) - min(df[,y]))*0.1,"}, \n")
  htmp <- paste0(htmp,"hAxis: {title: '",x,"', minValue: ",min(df[,x]) - (max(df[,x]) - min(df[,x]))*0.1,", maxValue: ",max(df[,x]) + (max(df[,x]) - min(df[,x]))*0.1,"}, \n")
  if (!is.null(legendposition)) {htmp <- paste0(htmp,"legend: { position: '",legendposition,"' },\n")}
  if (trendline==TRUE) {htmp <- paste0(htmp,"trendlines: { 0: {} },\n")}
  
  if (length(chart_options)>0) {
    for (l in 1:length(chart_options)) {
      htmp <- paste0(htmp,chart_options[l],",\n")
    }
  }
  
  htmp <- paste0(htmp,"};\n\n")
  
  htmp <- paste0(htmp, "var chart = new google.visualization.ScatterChart(document.getElementById('scatterplot_values",tilfstr,"'));\n")
  htmp <- paste0(htmp,"chart.draw(data, options);\n")
  htmp <- paste0(htmp, "}\n\n") # closing function...
  
  .wjavafuns <<- c(.wjavafuns,htmp)
  .wcontent <<- c(.wcontent,paste0("<table align='",align,"' cellpadding=",.cellpadding,"><tr><td><div id='scatterplot_values",tilfstr,"'></div></td></tr></table><br>\n"))
}

# insert.BUBBLECHART ####
wzy.BUBBLECHART.insert <- function(df, 
                                   x, y,
                                   idvar = NULL,
                                   groupvar = NULL,
                                   sizevar = NULL,
                                   bubblesize_min_max = c(1,30),
                                   samplesize = -1,
                                   seed=NULL,
                                   chart_title=NULL,
                                   #subtitle=NULL,
                                   titlefontsize=18,
                                   legendposition = c("right"),
                                   align="left", width="700", height="600",
                                   chart_options = NULL) {
  library(dplyr)
  
  tilfstr <- as.character(floor(runif(1)*1000))
  
  htmp <- paste0("\ngoogle.charts.load('current', {'packages':['corechart','bar']}); \n")
  htmp <- paste0(htmp,"google.charts.setOnLoadCallback(drawChart",tilfstr,"); \n")
  
  htmp <- paste0(htmp, "function drawChart",tilfstr,"() {\n")
  htmp <- paste0(htmp,"var data = google.visualization.arrayToDataTable([ \n")
  
  if (is.null(idvar)) {
    df[,"id_"] <- seq(1:nrow(df))
    idvar <- "id_"
  }
  if (is.null(groupvar)) {
    groupvar <- "grp_"
    df[,groupvar] <- "all"
  }
  if (is.null(sizevar)) {
    sizevar <- "grpsize_"
    df[,sizevar] <- 1
  }
  df <- na.omit(df)
  
  # column names 
  htmp <- paste0(htmp, paste0("['",idvar,"','",x,"','",y,"','",groupvar,"','",sizevar,"'],\n"))
  
  set.seed(ifelse(is.null(seed),1234,seed))
  if (samplesize>-1) {
    df <- df[order(runif(nrow(df))),][1:min(samplesize,nrow(df)),]
  }
  
  # data values
  #df <- data.frame(df)
  for (i in 1:nrow(df)) {
    htmp <- paste0(htmp, paste0("['",df[i,idvar],"', ", df[i,x],",",df[i,y],", '",df[i,groupvar],"',", df[i,sizevar]," ]",ifelse(!i==nrow(df),",",""),"\n"))
  }
  htmp <- paste0(htmp,"]);\n\n") 
  
  htmp <- paste0(htmp,"var options = {\n")
  htmp <- paste0(htmp,paste0("title: '",ifelse(!is.null(chart_title),chart_title,paste0("Bubble chart of ",x, " vs. ",y)),"',\n"))
  #if (!is.null(subtitle)) {htmp <- paste0(htmp,"subtitle: '",subtitle,"',\n")}
  htmp <- paste0(htmp,"width: ",width,",\n")
  htmp <- paste0(htmp,"height: ",height,",\n")
  htmp <- paste0(htmp,"colors: [",.colorstr,"],\n")
  #htmp <- paste0(htmp,"chartArea: {backgroundColor: {stroke: '#4322c0',strokeWidth: 1}},\n")
  htmp <- paste0(htmp,"titleTextStyle: {fontSize: ",titlefontsize,"},\n")
  htmp <- paste0(htmp,"vAxis: {title: '",y,"', minValue: ",min(df[,y]) - (max(df[,y]) - min(df[,y]))*0.1,", maxValue: ",max(df[,y]) + (max(df[,y]) - min(df[,y]))*0.1,"}, \n")
  htmp <- paste0(htmp,"hAxis: {title: '",x,"', minValue: ",min(df[,x]) - (max(df[,x]) - min(df[,x]))*0.1,", maxValue: ",max(df[,x]) + (max(df[,x]) - min(df[,x]))*0.1,"}, \n")
  
  if (sizevar=="grpsize_") htmp <- paste0(htmp,"bubble:  {textStyle: {fontSize: 8}, sizeAxis: {maxValue: 3}},\n")
  htmp <- paste0(htmp,"sizeAxis: {minSize: ",bubblesize_min_max[1],", maxSize: ",bubblesize_min_max[2],"},\n")
  
  if (length(chart_options)>0) {
    for (l in 1:length(chart_options)) {
      htmp <- paste0(htmp,chart_options[l],",\n")
    }
  }
  
  #  if (sizevar=="grpsize_") htmp <- paste0(htmp," {pointSize: 2}, \n")
  #htmp <- paste0(htmp,"},\n")
  
  if (!is.null(legendposition)) {htmp <- paste0(htmp,"legend: { position: '",legendposition,"' },\n")}
  htmp <- paste0(htmp,"};\n\n")
  
  htmp <- paste0(htmp, "var chart = new google.visualization.BubbleChart(document.getElementById('scatterplot_values",tilfstr,"'));\n")
  htmp <- paste0(htmp,"chart.draw(data, options);\n")
  htmp <- paste0(htmp, "}\n\n") # closing function...
  
  .wjavafuns <<- c(.wjavafuns,htmp)
  .wcontent <<- c(.wcontent,paste0("<table align='",align,"' cellpadding=",.cellpadding,"><tr><td><div id='scatterplot_values",tilfstr,"'></div></td></tr></table><br>\n"))
}

# insert.PIECHART ####
wzy.PIECHART.insert <- function(df, group_var, num_var=NULL, fun=c("asis","n","sum","mean","median","sd"), 
                                is3D = FALSE,
                                annotation_var = NULL,
                                chart_title=NULL,
                                titlefontsize=18,
                                #subtitle = NULL,
                                legendposition = c("right"),
                                sliceVisibilityThreshold = 0.05,
                                align="center", width=550, height=500,
                                chart_options = NULL) {
  library(dplyr)
  
  tilfstr <- as.character(floor(runif(1)*1000))
  fun <- fun[1]
  
  # prepping data
  if (!is.null(num_var)) {
    if (fun=="asis") {
      df2 <- df
    } else {
      df2 <- df %>% group_by(.data[[group_var]]) %>% summarise_at(.vars=num_var,.funs=fun)
    }
  } else {
    num_var <- "n"
    df2 <- df %>% group_by(.data[[group_var]]) %>% summarise(n=n())
  }
  
  htmp <- paste0("\ngoogle.charts.load('current', {'packages':['corechart','bar']}); \n")
  htmp <- paste0(htmp,"google.charts.setOnLoadCallback(drawChart",tilfstr,"); \n")
  
  htmp <- paste0(htmp, "function drawChart",tilfstr,"() {\n")
  htmp <- paste0(htmp,"var data = google.visualization.arrayToDataTable([ \n")
  
  # column names 
  htmp <- paste0(htmp, paste0("['",group_var,"','",paste(num_var,collapse="','"),"'",ifelse(!is.null(annotation_var),", { role: 'annotation' }",""),"],\n"))
  
  # data values
  df2 <- data.frame(df2)
  for (i in 1:nrow(df2)) {
    htmp <- paste0(htmp, paste0("['",as.character(df2[i,group_var]),"',",as.character(paste(df2[i,num_var],collapse=",")),
                                ifelse(!is.null(annotation_var),paste0(",'",as.character(df2[i,annotation_var]),"'"),""),"]",ifelse(i==nrow(df2),"",","),"\n"))
  }
  
  htmp <- paste0(htmp,"]);\n\n") 
  
  htmp <- paste0(htmp,"var options = {\n")
  htmp <- paste0(htmp,paste0("title: '",ifelse(!is.null(chart_title),chart_title,paste0("Pie chart: ",ifelse(!fun=="asis",paste0("(",fun," of) "), ""), num_var," - by ",group_var)),"',\n"))
  htmp <- paste0(htmp,"is3D: ",tolower(is3D),",\n")
  htmp <- paste0(htmp,"chartArea: {backgroundColor: {stroke: '#4322c0',strokeWidth: 1}},\n")
  #  htmp <- paste0(htmp,"height: ",height,",\n")
  htmp <- paste0(htmp,"colors: [",.colorstr,"],\n")
  htmp <- paste0(htmp,"titleTextStyle: {fontSize: ",titlefontsize,"},\n")
  htmp <- paste0(htmp,"sliceVisibilityThreshold: ",sliceVisibilityThreshold,",\n")
  htmp <- paste0(htmp,"chartArea:{left:0,top:70,width:'100%',height:'100%'},\n")
  
  if (length(chart_options)>0) {
    for (l in 1:length(chart_options)) {
      htmp <- paste0(htmp,chart_options[l],",\n")
    }
  }
  
  #  htmp <- paste0(htmp,"legend: { position: '",legendposition,"' },\n")
  htmp <- paste0(htmp,"};\n\n")
  
  htmp <- paste0(htmp, "var chart = new google.visualization.PieChart(document.getElementById('piechart_values",tilfstr,"'));\n")
  htmp <- paste0(htmp,"chart.draw(data, options);\n")
  htmp <- paste0(htmp, "}\n\n") # closing function...
  
  .wjavafuns <<- c(.wjavafuns,htmp)
  .wcontent <<- c(.wcontent,paste0("<table border = 0 align='",align,"' cellpadding=",.cellpadding,"><tr><td><div id='piechart_values",tilfstr,"' style='width: ",width,"px; height: ",height,"px;'></div></td></tr></table>\n"))
}

# insert.LINECHART ####
wzy.LINECHART.insert <- function(df, x=NULL, 
                                 num_vars=NULL, 
                                 group_var=NULL, 
                                 samplesize=100,
                                 fun=c("asis","n","acc_n","sum","mean","median","sd"),
                                 smooth=FALSE,
                                 trendline = FALSE,
                                 trendlinefunction = c("linear","exponential"),
                                 annotation_var = NULL,
                                 chart_title=NULL,
                                 titlefontsize=18,
                                 #subtitle = NULL,
                                 legendposition = c("right"),
                                 align="left", width="700", height="600",
                                 chart_options = NULL) {
  library(dplyr)
  library(reshape2)
  
  tilfstr <- as.character(floor(runif(1)*1000))
  fun <- fun[1]
  
  if (length(num_vars)>1 & !is.null(annotation_var)) {
    annotation_var <- NULL
    print("Unfortunately annotation only works with one numerical value.")
  }
  
  if (trendline==TRUE & is.character(df[,x])) {
    trendline <- FALSE
    print("Unfortunately trendlines only works with one numerical x variable.")
  }
  
  if (is.null(x)) {
    x <- "row"
    df[,x] <- seq(1:nrow(df))
    fun <- "asis"
  }
  
  # prepping data
  if (is.null(group_var)) {
    if (!is.null(num_vars)) {
      if (fun=="asis") {
        df2 <- df
      } else {
        if (fun=="n") {
          num_vars <- "n"
          df2 <- df %>% group_by(.data[[x]]) %>% summarise(n=n())
          df2 <- data.frame(df2)
        } else if (fun=="acc_n") {
          1=1
        } else {
          df2 <- df %>% group_by(.data[[x]]) %>% summarise_at(.vars=num_vars,.funs=fun)
          df2 <- data.frame(df2)
        }
        df2 <- df2[order(df2[,x]),]
      }
    } else {
      if (fun=="sum") {
        #num_vars <- "n"
        df2 <- df %>% group_by(.data[[x]]) %>% summarise_at(.vars=num_vars,.funs="sum")
        df2 <- data.frame(df2)
        df2 <- df2[order(df2[,x]),]
      } else if (fun=="acc_n") {
        num_vars <- "acc_n"
        df2 <- df %>% group_by(.data[[x]]) %>% summarise_at(.vars=num_vars,.funs="cumsum")
        df2 <- data.frame(df2)
        df2$acc_n <- cumsum(df2$n)
        df2 <- df2[order(df2[,x]),]
      }
    }
  } else {
    if (!is.null(num_vars)) {
      if (fun=="asis") {
        df2 <- dcast(data=df,fun.aggregate=NULL, paste(x," ~ ",group_var),value.var=num_vars)
        df2 <- data.frame(df2)
        num_vars <- colnames(df2)[-1]
        df2 <- df2[order(df2[,x]),]
      } else {
        fun <- "n"
        num_vars <- "n"
        df2 <- df %>% group_by(.data[[x]],.data[[group_var]]) %>% summarise(n=n())
        df2 <- as.data.frame(df2)
        df2 <- dcast(df2,as.formula(paste(x," ~ ",group_var)),value.var=num_vars)
        colnames(df2)[-1] <- paste0("n_",colnames(df2)[-1])
        df2[is.na(df2)] <- 0
        num_vars <- colnames(df2)[-1]
        df2 <- df2[order(df2[,x]),]
      }
    } else {
      if (fun=="acc_n") {
        num_vars <- "acc_n"
        df2 <- df %>% group_by(.data[[x]],.data[[group_var]]) %>% summarise(n=n())
        df2 <- df2 %>% group_by(.data[[group_var]]) %>% summarise(x=.data[[x]],n=n,acc_n=cumsum(n))
        df2 <- data.frame(df2)
        colnames(df2)[which(colnames(df2)=="x")] <- x
        df2 <- dcast(df2,as.formula(paste(x," ~ ",group_var)),value.var="acc_n")
        colnames(df2)[-1] <- paste0("acc_n_",colnames(df2)[-1])
        for (col in 2:ncol(df2)) {
          for (i in 1:nrow(df2)) {
            if (i==1 & is.na(df2[i,col])) {
              df2[i,col] <- 0
            } else if (i>1 & is.na(df2[i,col])) {
              df2[i,col] <- df2[i-1,col]
            }
          }
        }
        num_vars <- colnames(df2)[-1]
        #df2 <- df2[order(df2[,x]),]
      } else {
        fun <- "n"
        num_vars <- "n"
        df2 <- df %>% group_by(.data[[x]],.data[[group_var]]) %>% summarise(n=n())
        df2 <- as.data.frame(df2)
        df2 <- dcast(df2,as.formula(paste(x," ~ ",group_var)),value.var=num_vars)
        colnames(df2)[-1] <- paste0("n_",colnames(df2)[-1])
        colnames(df2) <- gsub("/","_",colnames(df2),fixed=TRUE)
        df2[is.na(df2)] <- 0
        num_vars <- colnames(df2)[-1]
        df2 <- df2[order(df2[,x]),]
      }
    }
  }
  
  # does random sampling if too many rows
  tilf <- runif(nrow(df2))
  if (nrow(df2)>samplesize) {
    df2 <- df2[order(tilf),][1:samplesize,]
    df2 <- df2[order(df2[,x]),]
  }
  
  num_vars <- num_vars[num_vars %in% colnames(df2)]
  num_vars <- num_vars[!num_vars %in% c("NA")]
  
  
  htmp <- paste0("\ngoogle.charts.load('current', {'packages':['corechart','line']}); \n")
  htmp <- paste0(htmp,"google.charts.setOnLoadCallback(drawChart",tilfstr,"); \n")
  
  htmp <- paste0(htmp, "function drawChart",tilfstr,"() {\n")
  htmp <- paste0(htmp,"var data = google.visualization.arrayToDataTable([ \n")
  
  # column names 
  htmp <- paste0(htmp, paste0("['",x,"','",paste(num_vars,collapse="','"),"'",ifelse(!is.null(annotation_var),", { role: 'annotation' }",""),"],\n"))
  
  # data values
  df2 <- data.frame(df2)
  for (i in 1:nrow(df2)) {
    insx <- ifelse(is.factor(df2[,x]), paste0("'",as.character(df2[i,x]),"'"),
                   ifelse(is.numeric(df2[,x]), paste0(as.character(df2[i,x])), paste0("'",as.character(df2[i,x]),"'")))
    
    htmp <- paste0(htmp, paste0("[",insx,",",as.character(paste(df2[i,num_vars],collapse=",")),
                                ifelse(!is.null(annotation_var),paste0(",'",as.character(df2[i,annotation_var]),"'"),""),"]",ifelse(i==nrow(df2),"",","),"\n"))
  }
  
  htmp <- paste0(htmp,"]);\n\n") 
  
  htmp <- paste0(htmp,"var options = {\n")
  htmp <- paste0(htmp,paste0("title: '",ifelse(!is.null(chart_title),chart_title,paste0("Line chart of ",paste(num_vars, collapse=" and ")," - by ",x)),"',\n"))
  htmp <- paste0(htmp,"width: ",width,",\n")
  htmp <- paste0(htmp,"height: ",height,",\n")
  htmp <- paste0(htmp,"colors: [",.colorstr,"],\n")
  if (smooth==TRUE) {htmp <- paste0(htmp,"curveType: 'function',\n")}
  htmp <- paste0(htmp,"titleTextStyle: {fontSize: ",titlefontsize,"},\n")
  if (length(num_vars)==1) {htmp <- paste0(htmp,"vAxis: {title: '",gsub("asis ","",paste(fun,num_vars[1])),"'},\n")}
  htmp <- paste0(htmp,"hAxis: {title: '",x,"'},\n")
  htmp <- paste0(htmp,"chartArea: {backgroundColor: {stroke: '#4322c0',strokeWidth: 1}},\n")

  if (length(chart_options)>0) {
    for (l in 1:length(chart_options)) {
      htmp <- paste0(htmp,chart_options[l],",\n")
    }
  }
  
  if (trendline==TRUE) {
    htmp <- paste0(htmp,"trendlines: {\n")
    htmp <- paste0(htmp,"  0: {type: '",trendlinefunction[1],"', color: '#333', opacity: 1},\n")
    #htmp <- paste0(htmp,"  1: {type: 'linear', color: '#111', opacity: .3} \n")
    htmp <- paste0(htmp,"} ,\n")
  }
  #htmp <- paste0(htmp,"bar: {groupWidth: '95%'},\n")
  htmp <- paste0(htmp,"legend: { position: '",legendposition,"' },\n")
  htmp <- paste0(htmp,"};\n\n")
  htmp <- paste0(htmp, "var chart = new google.visualization.LineChart(document.getElementById('linechart_values",tilfstr,"'));\n")
  htmp <- paste0(htmp,"chart.draw(data, options);\n")
  htmp <- paste0(htmp, "}\n\n") # closing function...
  
  .wjavafuns <<- c(.wjavafuns,htmp)
  .wcontent <<- c(.wcontent,paste0("<table align='",align,"' cellpadding=",.cellpadding,"><tr><td><div id='linechart_values",tilfstr,"'></div></td></tr></table>\n"))
}

# insert.AREACHART ####
wzy.AREACHART.insert <- function(df, x=NULL, num_vars, fun=c("asis","n","acc_n","sum","mean","median","sd"),
                                 stacked = FALSE,
                                 fullstacked = FALSE,
                                 smooth=FALSE,
                                 annotation_var = NULL,
                                 chart_title=NULL,
                                 titlefontsize=18,
                                 #subtitle = NULL,
                                 legendposition = c("right"),
                                 align="left", width="700", height="600",
                                 chart_options = NULL) {
  library(dplyr)
  
  tilfstr <- as.character(floor(runif(1)*1000))
  fun <- fun[1]
  
  if (length(num_vars)>1 & !is.null(annotation_var)) {
    annotation_var <- NULL
    print("Unfortunately annotation only works with one numerical value.")
  }
  
  if (is.null(x)) {
    x <- "row"
    df[,x] <- seq(1:nrow(df))
    fun <- "asis"
  }
  
  # prepping data
  if (!is.null(num_vars)) {
    if (fun=="asis") {
      df2 <- df
    } else {
      df2 <- df %>% group_by(.data[[x]]) %>% summarise_at(.vars=num_vars,.funs=fun)
      df2 <- df2[order(df2[,x]),]
    }
  } else {
    if (fun=="n") {
      num_vars <- "n"
      df2 <- df %>% group_by(.data[[x]]) %>% summarise(n=n())
      df2 <- df2[order(df2[,x]),]
    } else if (fun=="acc_n") {
      num_vars <- "acc_n"
      df2 <- df %>% group_by(.data[[x]]) %>% summarise(n=n())
      df2$acc_n <- cumsum(df2$n)
      df2 <- df2[order(df2[,x]),]
    }
  }
  
  htmp <- paste0("\ngoogle.charts.load('current', {'packages':['corechart','line']}); \n")
  htmp <- paste0(htmp,"google.charts.setOnLoadCallback(drawChart",tilfstr,"); \n")
  
  htmp <- paste0(htmp, "function drawChart",tilfstr,"() {\n")
  htmp <- paste0(htmp,"var data = google.visualization.arrayToDataTable([ \n")
  
  # column names 
  htmp <- paste0(htmp, paste0("['",x,"','",paste(num_vars,collapse="','"),"'",ifelse(!is.null(annotation_var),", { role: 'annotation' }",""),"],\n"))
  
  # data values
  df2 <- data.frame(df2)
  for (i in 1:nrow(df2)) {
    insx <- ifelse(is.factor(df2[,x]), paste0("'",as.character(df2[i,x]),"'"),
                   ifelse(is.numeric(df2[,x]), paste0(as.character(df2[i,x])), paste0("'",as.character(df2[i,x]),"'")))
    
    htmp <- paste0(htmp, paste0("[",insx,",",as.character(paste(df2[i,num_vars],collapse=",")),
                                ifelse(!is.null(annotation_var),paste0(",'",as.character(df2[i,annotation_var]),"'"),""),"]",ifelse(i==nrow(df2),"",","),"\n"))
  }
  
  htmp <- paste0(htmp,"]);\n\n") 
  
  htmp <- paste0(htmp,"var options = {\n")
  htmp <- paste0(htmp,paste0("title: '",ifelse(!is.null(chart_title),chart_title,paste0("Line chart of ",paste(num_vars, collapse=" and ")," - by ",x)),"',\n"))
  htmp <- paste0(htmp,"width: ",width,",\n")
  htmp <- paste0(htmp,"height: ",height,",\n")
  htmp <- paste0(htmp,"chartArea: {backgroundColor: {stroke: '#4322c0',strokeWidth: 1}},\n")
  
  if (length(chart_options)>0) {
    for (l in 1:length(chart_options)) {
      htmp <- paste0(htmp,chart_options[l],",\n")
    }
  }
  
  if (fullstacked==TRUE) {
    htmp <- paste0(htmp,"isStacked: 'percent',\n")
  } else if (stacked==TRUE) {
    htmp <- paste0(htmp,"isStacked: true,\n")
  } else {
    htmp <- htmp
  }
  
  htmp <- paste0(htmp,"colors: [",.colorstr,"],\n")
  if (smooth==TRUE) {htmp <- paste0(htmp,"curveType: 'function',\n")}
  htmp <- paste0(htmp,"titleTextStyle: {fontSize: ",titlefontsize,"},\n")
  htmp <- paste0(htmp,"vAxis: {title: '",gsub("asis ","",paste(fun,num_vars[1])),ifelse(stacked==TRUE || fullstacked==TRUE," (stacked)",""),"'},\n")
  htmp <- paste0(htmp,"hAxis: {title: '",x,"'},\n")
  
  #htmp <- paste0(htmp,"bar: {groupWidth: '95%'},\n")
  htmp <- paste0(htmp,"legend: { position: '",legendposition,"' },\n")
  htmp <- paste0(htmp,"};\n\n")
  htmp <- paste0(htmp, "var chart = new google.visualization.AreaChart(document.getElementById('linechart_values",tilfstr,"'));\n")
  htmp <- paste0(htmp,"chart.draw(data, options);\n")
  htmp <- paste0(htmp, "}\n\n") # closing function...
  
  .wjavafuns <<- c(.wjavafuns,htmp)
  .wcontent <<- c(.wcontent,paste0("<table align='",align,"' cellpadding=",.cellpadding,"><tr><td><div id='linechart_values",tilfstr,"'></div></td></tr></table>\n"))
}

# HTMLTABLE.init ####
wzy.HTMLTABLE.init <- function(table_id=1,n_rows=2,n_cols=2,width='80%', border=0,align="center") {
  htmp <- paste0("<table name='HTMLTABLE_",table_id,"' cellpadding='1' id=",table_id," align='",align,"' width='",width,"' border=", border,">\n")
  for (i in 1:n_rows) {
    htmp <- paste0(htmp,"<tr>")
    for (j in 1:n_cols) {
      htmp <- paste0(htmp, "<td>[",i,",",j,"]</td>")
    }
    htmp <- paste0(htmp,"</tr>\n")
  }
  htmp <- paste0(htmp,"</table>\n<br>")
  .wcontent <<- c(.wcontent,htmp)
}

# 2HTMLTABLE.insert ####
wzy.2HTMLTABLE.insert <- function(table_id=1, cell_id="[1,1]", content) {
  .wcontent <<- c(.wcontent,paste0("INSERT2HTMLTABLE(id=",table_id,",cell_id=",cell_id,")"))
  content
}

# insert.HISTOGRAM ####
wzy.HISTOGRAM.insert <- function(df, num_var, breaks=10, 
                                 min_value=NULL,
                                 max_value=NULL,
                                 log=FALSE,
                                 chart_title=NULL,
                                 annotation=TRUE,
                                 titlefontsize=18,
                                 legendposition = c("right"),
                                 align="left", width="700", height="600",
                                 chart_options = NULL) {
  
  if (is.null(chart_title)) {chart_title <- paste0("Histogram of ",num_var,ifelse(log==TRUE," (log)",""))}
  
  if (!is.null(min_value)) df <- df[df[,num_var]>=min_value,]
  if (!is.null(max_value)) df <- df[df[,num_var]<=max_value,]
  
  #df$cat <- ifelse(log==TRUE,cut(log(df[,num_var]),breaks),cut(df[,num_var],breaks))
  
  if (log==TRUE) {
    if (any(na.omit(df[,num_var])<=0)==TRUE) {warning(paste0("Rows with ",num_var,"<=0 have been removed!"))}
    df <- df[df[,num_var]>0,]
    df$cat <- cut(log(df[,num_var]),breaks)
  } else {
    df$cat <- cut(df[,num_var],breaks)
  }
  
  xname <- paste0(num_var,"_interval")
  tmp <- df %>% group_by(cat) %>% summarise(n=n(),min_value=min(get(num_var)),max_value=max(get(num_var)))
  tmp[,xname] <- paste0("(",tmp$min_value,",",tmp$max_value,"]")
  
  annotation_var <- NULL
  if (annotation==TRUE) {annotation_var <- "n" }

  wzy.COLUMNCHART.insert(tmp, group_var = xname, num_vars = c("n"), chart_title=chart_title,
                         legendposition=legendposition, annotation_var = annotation_var, align=align,
                         width=width,height=height,titlefontsize=titlefontsize,chart_options=chart_options)
}
