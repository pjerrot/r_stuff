glm_to_sql <- function(glmmodel) {
  vartypes <- data.frame(unlist(attr(glmmodel$terms,'dataClasses')))
  vartypes$varname <- rownames(vartypes)
  rownames(vartypes) <- NULL
  colnames(vartypes)[1] <- "vartype"
  
  xlev <- data.frame(unlist(glmmodel$xlevels))
  xlev$xlevrowname <- rownames(xlev)
  rownames(xlev) <- NULL
  colnames(xlev)[1] <- "xlevel"
  if (nrow(xlev)==0){xlev <- data.frame(xlevrowname=character(0), xlevel=character(0), stringsAsFactors=F)}
  
  modcoeffs <- data.frame(unlist(glmmodel$coefficients))
  modcoeffs$coeffname <- rownames(modcoeffs)
  rownames(modcoeffs) <- NULL
  colnames(modcoeffs)[1] <- "coeffvalue"
  modcoeffs[is.na(modcoeffs$coeffvalue),"coeffvalue"] <- 0
  
  for (i in 1:nrow(modcoeffs)) {    
    v <- unlist(strsplit(modcoeffs[i,"coeffname"], ":", fixed=FALSE))
    modcoeffs[i,"splitname1"] <- v[1]
    modcoeffs[i,"splitname2"] <- v[2]
    modcoeffs[i,"splitname3"] <- v[3]
    modcoeffs[i,"splitname4"] <- v[4]
  }
  
  varcats <- data.frame(varname=character(0), #name of variable
                        category=character(0), stringsAsFactors=F)
  if (length(glmmodel$xlevels)>0) {
    for (i in 1:length(glmmodel$xlevels)) {
      for (j in 1:nrow(data.frame(glmmodel$xlevels[i]))) {
        varcats[nrow(varcats)+1,] <- c(colnames(data.frame(glmmodel$xlevels[i])), data.frame(glmmodel$xlevels[i], stringsAsFactors=F)[j,1] )
      }  
    }
  }  
  modcoeffs$sql1 <- modcoeffs$splitname1
  modcoeffs$sql2 <- modcoeffs$splitname2
  modcoeffs$sql3 <- modcoeffs$splitname3
  modcoeffs$sql4 <- modcoeffs$splitname4
  modcoeffs$varname1 <- modcoeffs$splitname1
  modcoeffs$varname2 <- modcoeffs$splitname2
  modcoeffs$varname3 <- modcoeffs$splitname3
  modcoeffs$varname4 <- modcoeffs$splitname4
  
  for (i in 1:nrow(modcoeffs)) {    
    for (j in 1:nrow(varcats)) {    
      if (modcoeffs[i,"splitname1"] == paste(varcats[j,"varname"], varcats[j,"category"],sep="")) 
      { 
        modcoeffs[i,"sql1"] <- paste("(",varcats[j,"varname"],"='",varcats[j,"category"],"')",sep="")
        modcoeffs[i,"varname1"] <- varcats[j,"varname"]
      }
      if (is.na(modcoeffs[i,"splitname2"]) != TRUE) 
      {
        if (modcoeffs[i,"splitname2"] == paste(varcats[j,"varname"], varcats[j,"category"],sep="")) 
        { 
          modcoeffs[i,"sql2"] <- paste("(",varcats[j,"varname"],"='",varcats[j,"category"],"')",sep="")
          modcoeffs[i,"varname2"] <- varcats[j,"varname"]
        }  
      }
      if (is.na(modcoeffs[i,"splitname3"]) != TRUE) 
      {
        if (modcoeffs[i,"splitname3"] == paste(varcats[j,"varname"], varcats[j,"category"],sep="")) 
        { 
          modcoeffs[i,"sql3"] <- paste("(",varcats[j,"varname"],"='",varcats[j,"category"],"')",sep="")
          modcoeffs[i,"varname3"] <- varcats[j,"varname"]
        }  
      }
      if (is.na(modcoeffs[i,"splitname4"]) != TRUE) 
      {
        if (modcoeffs[i,"splitname4"] == paste(varcats[j,"varname"], varcats[j,"category"],sep="")) 
        { 
          modcoeffs[i,"sql4"] <- paste("(",varcats[j,"varname"],"='",varcats[j,"category"],"')",sep="")
          modcoeffs[i,"varname4"] <- varcats[j,"varname"]
        }  
      }
    }
    
  }
  
  modcoeffs$sql <- ""
  for (i in 1:nrow(modcoeffs)) {    
    modcoeffs[i,"sql"] <- ifelse(is.na(modcoeffs[i,"sql4"])==FALSE, paste(modcoeffs[i,"coeffvalue"],"*",modcoeffs[i,"sql1"],"*",modcoeffs[i,"sql2"],"*",modcoeffs[i,"sql3"],"*",modcoeffs[i,"sql4"],sep=""),
                                 ifelse(is.na(modcoeffs[i,"sql3"])==FALSE, paste(modcoeffs[i,"coeffvalue"],"*",modcoeffs[i,"sql1"],"*",modcoeffs[i,"sql2"],"*",modcoeffs[i,"sql3"],sep=""),
                                        ifelse(is.na(modcoeffs[i,"sql2"])==FALSE, paste(modcoeffs[i,"coeffvalue"],"*",modcoeffs[i,"sql1"],"*",modcoeffs[i,"sql2"],sep=""),
                                               ifelse(is.na(modcoeffs[i,"sql1"])==FALSE, paste(modcoeffs[i,"coeffvalue"],"*",modcoeffs[i,"sql1"],sep=""),""))))
    modcoeffs[i,"sql"] <- ifelse(modcoeffs[i,"sql1"]=="(Intercept)",modcoeffs[i,"coeffvalue"],modcoeffs[i,"sql"] )
  }
  
  x.sql0 <- ""  
  for (i in 1:nrow(modcoeffs)) {    
    if (i==1){x.sql0 <- modcoeffs$sql[i]} else {x.sql0 <- paste(x.sql0,"+",modcoeffs$sql[i])}
  }
  
  if (is.null(glmmodel$family$link)){
    x.sql <- x.sql0
  } else {  
    if (glmmodel$family$link == "logit") {
      x.sql <- paste("1/(1 + exp(-(",x.sql0,")))")  
    } else if (glmmodel$family$link == "identity") {
      x.sql <- x.sql0
    }
  }
  assign("modcoeffs",modcoeffs,envir = .GlobalEnv)
  
  return(x.sql)
}
