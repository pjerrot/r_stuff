# init ####
wzy.init <- function(title="output", file="output.html", author="", 
                     leftmargin = 70,
                     cellpadding = 7,
                     width = 900,
                     sidepanelscolor="grey",
                     includecontentblock=TRUE, pdfcopy=FALSE) {
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
wzy.TITLE.insert <- function(str, size=c("h3","h1","h2","h4","h5","h6"), small=FALSE, link=NULL, align="left", color="black") {
  size <- size[1]
  a_name <- paste0(size,"_idx_",floor(runif(1)*10000))
  contentblock_str <- paste0("<a href='#",a_name,"'>",str,"</a>")
  .wcontentblock <<- c(.wcontentblock,contentblock_str)
  
  tmp <- paste0("<table width='100%' cellpadding=",.cellpadding," align=",align,"><tr><td width=",.leftmargin,"></td><td>")
  if (is.null(link)) {
    .wcontent <<- c(.wcontent,paste0(tmp,"<",size,">",ifelse(small==TRUE,"<small>",""),"<a name='",a_name,"'><font color='",color,"'>",str,"</font></a>",ifelse(small==TRUE,"</small>",""),"</",size,"></td></tr></table>\n"))
  } else {
    .wcontent <<- c(.wcontent,paste0(tmp,"<",size,"><a name='",a_name,"' href='",link,"'>",ifelse(small==TRUE,"<small>",""),"<font color='",color,"'>",
                                     str,"</font>",ifelse(small==TRUE,"</small>",""),"</a></",size,"></td></tr></table>\n"))
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
wzy.TEXT.insert <- function(str, fontsize=14, fontface="Arial", 
                            align="left", 
                            boxwidth=850,
                            leftmargin = .leftmargin,
                            cellpadding=10) {
  htmp <- paste0("<table align='",align,"' width=",boxwidth," cellpadding=",.cellpadding,"><tr><td width=",.leftmargin,"></td><td style='font-family:",fontface,"; font-size:",fontsize,"'>",str,"</td></tr></table>")
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
  
  return(html)
}

# insert.BARCHART ####
wzy.BARCHART.insert <- function(df, group_var, num_vars, fun=c("asis","sum","mean","median","sd"), 
                                  annotation_var = NULL,
                                  chart_title=NULL,
                                  titlefontsize=18,
                                  #subtitle = NULL,
                                  legendposition = c("right"),
                                  align="center", width="700", height="600") {
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
  htmp <- paste0(htmp,"colors: ['#b0120a', '#ffab91'],\n")
  htmp <- paste0(htmp,"titleTextStyle: {fontSize: ",titlefontsize,"},\n")
  if (length(num_vars)==1) {htmp <- paste0(htmp,"hAxis: {title: '",gsub("asis ","",paste(fun,num_vars[1])),"'},\n")}
  htmp <- paste0(htmp,"vAxis: {title: '",group_var,"'},\n")
  htmp <- paste0(htmp,"bar: {groupWidth: '95%'},\n")
  htmp <- paste0(htmp,"legend: { position: '",legendposition,"' },\n")
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
wzy.COLUMNCHART.insert <- function(df, group_var, num_vars, fun=c("asis","sum","mean","median","sd"), 
                                     stacked = FALSE,
                                     fullstacked = FALSE,
                                     annotation_var = NULL,
                                     chart_title=NULL,
                                     titlefontsize=18,
                                     #subtitle = NULL,
                                     legendposition = c("right"),
                                     align="left", width="700", height="600") {
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
  htmp <- paste0(htmp,"width: ",width,",\n")
  htmp <- paste0(htmp,"height: ",height,",\n")
  
  if (fullstacked==TRUE) {
    htmp <- paste0(htmp,"isStacked: 'percent',\n")
  } else if (stacked==TRUE) {
    htmp <- paste0(htmp,"isStacked: true,\n")
  } else {
    htmp <- htmp
  }
  
#  if (fullstacked==TRUE) htmp <- paste0(htmp,"options_fullStacked = {isStacked: 'percent'},\n")
  htmp <- paste0(htmp,"colors: ['#b0120a', '#ffab91'],\n")
  htmp <- paste0(htmp,"titleTextStyle: {fontSize: ",titlefontsize,"},\n")
  if (length(num_vars)==1) {htmp <- paste0(htmp,"vAxis: {title: '",gsub("asis ","",paste(fun,num_vars[1])),"'},\n")}
  htmp <- paste0(htmp,"hAxis: {title: '",group_var,"'},\n")
  htmp <- paste0(htmp,"bar: {groupWidth: '95%'},\n")
  htmp <- paste0(htmp,"legend: { position: '",legendposition,"' },\n")
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
                                     trendline = FALSE,
                                     chart_title=NULL,
                                     titlefontsize=18,
                                     #subtitle = NULL,
                                     legendposition = c("right"),
                                     align="left", width="700", height="600") {
  library(dplyr)
  
  tilfstr <- as.character(floor(runif(1)*1000))
  
  htmp <- paste0("\ngoogle.charts.load('current', {'packages':['corechart','bar']}); \n")
  htmp <- paste0(htmp,"google.charts.setOnLoadCallback(drawChart",tilfstr,"); \n")
  
  htmp <- paste0(htmp, "function drawChart",tilfstr,"() {\n")
  htmp <- paste0(htmp,"var data = google.visualization.arrayToDataTable([ \n")
  
  # column names 
  htmp <- paste0(htmp, paste0("['",x,"','",y,"'],\n"))
  
  set.seed(1234)
  df <- df[order(runif(nrow(df))),][1:min(samplesize,nrow(df)),]
  
  # data values
  df <- data.frame(df)
  for (i in 1:nrow(df)) {
    htmp <- paste0(htmp, paste0("[",df[i,x],",",df[i,y],collapse=","),"]",ifelse(!i==nrow(df),",",""),"\n")
  }
  
  htmp <- paste0(htmp,"]);\n\n") 
  
  htmp <- paste0(htmp,"var options = {\n")
  htmp <- paste0(htmp,paste0("title: '",ifelse(!is.null(chart_title),chart_title,paste0("Scatter chart of ",x, " vs. ",y,"',\n"))))
  htmp <- paste0(htmp,"width: ",width,",\n")
  htmp <- paste0(htmp,"height: ",height,",\n")
  htmp <- paste0(htmp,"colors: ['#b0120a', '#ffab91'],\n")
  htmp <- paste0(htmp,"titleTextStyle: {fontSize: ",titlefontsize,"},\n")
  htmp <- paste0(htmp,"vAxis: {title: '",y,"', minValue: ",min(df[,y]) - (max(df[,y]) - min(df[,y]))*0.1,", maxValue: ",max(df[,y]) + (max(df[,y]) - min(df[,y]))*0.1,"}, \n")
  htmp <- paste0(htmp,"hAxis: {title: '",x,"', minValue: ",min(df[,x]) - (max(df[,x]) - min(df[,x]))*0.1,", maxValue: ",max(df[,x]) + (max(df[,x]) - min(df[,x]))*0.1,"}, \n")
  if (!is.null(legendposition)) {htmp <- paste0(htmp,"legend: { position: '",legendposition,"' },\n")}
  if (trendline==TRUE) {htmp <- paste0(htmp,"trendlines: { 0: {} },\n")}
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
                                   groupsizevar = NULL,
                                   samplesize = -1,
                                   chart_title=NULL,
                                   titlefontsize=18,
                                   legendposition = c("right"),
                                   align="left", width="700", height="600") {
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
    groupsizevar = "grpsize_"
    df[,groupsizevar] <- 1
  }
  if (is.null(groupsizevar)) {
    groupsizevar <- "grpsize_"
    df[,groupsizevar] <- 1
  }
  df <- na.omit(df)
  
  # column names 
  htmp <- paste0(htmp, paste0("['",idvar,"','",x,"','",y,"','",groupvar,"','",groupsizevar,"'],\n"))
  
  set.seed(1234)
  if (samplesize>-1) {
    df <- df[order(runif(nrow(df))),][1:min(samplesize,nrow(df)),]
  }
  
  # data values
  #df <- data.frame(df)
  for (i in 1:nrow(df)) {
    htmp <- paste0(htmp, paste0("['",df[i,idvar],"', ", df[i,x],",",df[i,y],", '",df[i,groupvar],"',", df[i,groupsizevar]," ]",ifelse(!i==nrow(df),",",""),"\n"))
  }
  
  htmp <- paste0(htmp,"]);\n\n") 
  
  htmp <- paste0(htmp,"var options = {\n")
  htmp <- paste0(htmp,paste0("title: '",ifelse(!is.null(chart_title),chart_title,paste0("Bubble chart of ",x, " vs. ",y,"',\n"))))
  htmp <- paste0(htmp,"width: ",width,",\n")
  htmp <- paste0(htmp,"height: ",height,",\n")
  htmp <- paste0(htmp,"colors: ['#b0120a', '#ffab91'],\n")
  htmp <- paste0(htmp,"titleTextStyle: {fontSize: ",titlefontsize,"},\n")
  htmp <- paste0(htmp,"vAxis: {title: '",y,"', minValue: ",min(df[,y]) - (max(df[,y]) - min(df[,y]))*0.1,", maxValue: ",max(df[,y]) + (max(df[,y]) - min(df[,y]))*0.1,"}, \n")
  htmp <- paste0(htmp,"hAxis: {title: '",x,"', minValue: ",min(df[,x]) - (max(df[,x]) - min(df[,x]))*0.1,", maxValue: ",max(df[,x]) + (max(df[,x]) - min(df[,x]))*0.1,"}, \n")
  
  if (groupsizevar=="grpsize_") htmp <- paste0(htmp,"bubble:  {textStyle: {fontSize: 8}, sizeAxis: {maxValue: 3}},\n")
  htmp <- paste0(htmp,"sizeAxis: {minSize: 8, maxSize: 9},\n")
  
  #
  #  if (groupsizevar=="grpsize_") htmp <- paste0(htmp," {pointSize: 2}, \n")
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
wzy.PIECHART.insert <- function(df, group_var, num_var, fun=c("asis","sum","mean","median","sd"), 
                                  is3D = TRUE,
                                  annotation_var = NULL,
                                  chart_title=NULL,
                                  titlefontsize=18,
                                  #subtitle = NULL,
                                  legendposition = c("right"),
                                  align="left", width="700", height="600") {
  library(dplyr)
  
  tilfstr <- as.character(floor(runif(1)*1000))
  fun <- fun[1]
  
  # prepping data
  if (fun=="asis") {
    df2 <- df
  } else {
    df2 <- df %>% group_by(.data[[group_var]]) %>% summarise_at(.vars=num_var,.funs=fun)
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
  htmp <- paste0(htmp,"width: ",width,",\n")
  htmp <- paste0(htmp,"height: ",height,",\n")
  htmp <- paste0(htmp,"colors: ['#b0120a', '#ffab91'],\n")
  htmp <- paste0(htmp,"titleTextStyle: {fontSize: ",titlefontsize,"},\n")
  #  htmp <- paste0(htmp,"legend: { position: '",legendposition,"' },\n")
  htmp <- paste0(htmp,"};\n\n")
  
  htmp <- paste0(htmp, "var chart = new google.visualization.PieChart(document.getElementById('piechart_values",tilfstr,"'));\n")
  htmp <- paste0(htmp,"chart.draw(data, options);\n")
  htmp <- paste0(htmp, "}\n\n") # closing function...
  
  .wjavafuns <<- c(.wjavafuns,htmp)
  .wcontent <<- c(.wcontent,paste0("<table align='",align,"' cellpadding=",.cellpadding,"><tr><td><div id='piechart_values",tilfstr,"'></div></td></tr></table>\n"))
}

# insert.LINECHART ####
wzy.LINECHART.insert <- function(df, x=NULL, num_vars, fun=c("asis","sum","mean","median","sd"),
                                   smooth=FALSE,
                                   trendline = FALSE,
                                   trendlinefunction = c("linear","exponential"),
                                   annotation_var = NULL,
                                   chart_title=NULL,
                                   titlefontsize=18,
                                   #subtitle = NULL,
                                   legendposition = c("right"),
                                   align="left", width="700", height="600") {
  library(dplyr)
  
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
  if (fun=="asis") {
    df2 <- df
  } else {
    df2 <- df %>% group_by(.data[[x]]) %>% summarise_at(.vars=num_vars,.funs=fun)
    df2 <- df2[order(df2[,x]),]
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
    insx <- ifelse(is.factor(df2[,x]), paste("'",as.character(df2[i,x]),"'"),
                   ifelse(is.numeric(df2[,x]), paste(as.character(df2[i,x])), paste("'",as.character(df2[i,x]),"'")))
    
    htmp <- paste0(htmp, paste0("[",insx,",",as.character(paste(df2[i,num_vars],collapse=",")),
                                ifelse(!is.null(annotation_var),paste0(",'",as.character(df2[i,annotation_var]),"'"),""),"]",ifelse(i==nrow(df2),"",","),"\n"))
  }
  
  htmp <- paste0(htmp,"]);\n\n") 
  
  htmp <- paste0(htmp,"var options = {\n")
  htmp <- paste0(htmp,paste0("title: '",ifelse(!is.null(chart_title),chart_title,paste0("Line chart of ",paste(num_vars, collapse=" and ")," - by ",x)),"',\n"))
  htmp <- paste0(htmp,"width: ",width,",\n")
  htmp <- paste0(htmp,"height: ",height,",\n")
  htmp <- paste0(htmp,"colors: ['#b0120a', '#ffab91'],\n")
  if (smooth==TRUE) {htmp <- paste0(htmp,"curveType: 'function',\n")}
  htmp <- paste0(htmp,"titleTextStyle: {fontSize: ",titlefontsize,"},\n")
  if (length(num_vars)==1) {htmp <- paste0(htmp,"vAxis: {title: '",gsub("asis ","",paste(fun,num_vars[1])),"'},\n")}
  htmp <- paste0(htmp,"hAxis: {title: '",x,"'},\n")
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
wzy.AREACHART.insert <- function(df, x=NULL, num_vars, fun=c("asis","sum","mean","median","sd"),
                                 stacked = FALSE,
                                 fullstacked = FALSE,
                                 smooth=FALSE,
                                 annotation_var = NULL,
                                 chart_title=NULL,
                                 titlefontsize=18,
                                 #subtitle = NULL,
                                 legendposition = c("right"),
                                 align="left", width="700", height="600") {
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
  if (fun=="asis") {
    df2 <- df
  } else {
    df2 <- df %>% group_by(.data[[x]]) %>% summarise_at(.vars=num_vars,.funs=fun)
    df2 <- df2[order(df2[,x]),]
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
    insx <- ifelse(is.factor(df2[,x]), paste("'",as.character(df2[i,x]),"'"),
                   ifelse(is.numeric(df2[,x]), paste(as.character(df2[i,x])), paste("'",as.character(df2[i,x]),"'")))
    
    htmp <- paste0(htmp, paste0("[",insx,",",as.character(paste(df2[i,num_vars],collapse=",")),
                                ifelse(!is.null(annotation_var),paste0(",'",as.character(df2[i,annotation_var]),"'"),""),"]",ifelse(i==nrow(df2),"",","),"\n"))
  }
  
  htmp <- paste0(htmp,"]);\n\n") 
  
  htmp <- paste0(htmp,"var options = {\n")
  htmp <- paste0(htmp,paste0("title: '",ifelse(!is.null(chart_title),chart_title,paste0("Line chart of ",paste(num_vars, collapse=" and ")," - by ",x)),"',\n"))
  htmp <- paste0(htmp,"width: ",width,",\n")
  htmp <- paste0(htmp,"height: ",height,",\n")
  
  if (fullstacked==TRUE) {
    htmp <- paste0(htmp,"isStacked: 'percent',\n")
  } else if (stacked==TRUE) {
    htmp <- paste0(htmp,"isStacked: true,\n")
  } else {
    htmp <- htmp
  }
  
  htmp <- paste0(htmp,"colors: ['#b0120a', '#ffab91'],\n")
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