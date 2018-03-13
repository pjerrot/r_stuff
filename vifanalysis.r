
df <- coax
vars <- inputs

vifanalysis <- function(df,vars,vif_criteria=10) {
  
  vif_stats <- data.frame(
    var = character(0),
    vifvalue = numeric(0),
    stringsAsFactors=F
  )
  
  vif_hist <- data.frame(
    var = character(0),
    vifvalue = numeric(0),
    stringsAsFactors=F
  )
  
  max_vif <- vif_criteria+1
  exclude <- c()
  
  while (max_vif>vif_criteria) {
    for (l in vars) {
      newvars <- vars[!vars %in% exclude]
      tekst <- paste("mulcolmod <- lm(",l,"~.,data=df[,newvars])")
      eval(parse(text=tekst))
      r_square  <- summary(mulcolmod)$r.squared
      vif <- ifelse(is.na(r_square),1000000000,ifelse(r_square==1,1000000000,1/(1-r_square)))
      #print(paste(l,":",vif))
      vif_stats[nrow(vif_stats)+1,c("var","vifvalue")] <- list(l,vif)
    }
    
    vif_stats <- vif_stats[order(-vif_stats$vifvalue),]
    if (vif_stats[1,"vifvalue"]>vif_criteria) {
      exclude <- c(exclude,vif_stats[i,"var"])
      vif_stats[nrow(vif_stats)+1,c("var","vifvalue")] <- list(l,vif_stats[1,"vifvalue"])
    }
    max_vif <- vif_stats[1,"vifvalue"]
  }
}  
  
  #  all(is.element(x,y))==TRUE
  
  # topvif_var og .. value er for hele tiden at kunne se max vif, ogs??? selvom vi har var obligatorisk
  topvif_var <- sqldf("select 
                      case
                      when varname3 is null and varname2 is null then varname1
                      when varname3 is null then varname1 || '*' || varname2
                      else varname1 || '*' || varname2 || '*' || varname3
                      end as vifvar,vif
                      from modcoeffs
                      where coeffvalue<>0
                      order by vif desc
                      ")[1,"vifvar"]
  
  topvif_value <- sqldf("select 
                        case
                        when varname3 is null and varname2 is null then varname1
                        when varname3 is null then varname1 || '*' || varname2
                        else varname1 || '*' || varname2 || '*' || varname3
                        end as vifvar,vif
                        from modcoeffs
                        where coeffvalue<>0
                        order by vif desc
                        ")[1,"vif"]
  
  
  vifvar_sorteda <- sqldf(paste("select varname1, varname2, varname3,
                                var_desc as vifvar,vif
                                from modcoeffs
                                where coeffvalue<>0
                                and var_desc not in ",paste("('",paste(mandatoryvars0,collapse="','"),"')",sep=""),"
                                order by
                                case
                                when varname3 is not null then 3
                                when varname2 is not null then 2
                                else 1
                                end desc,
                                vif desc
                                ",sep=""))
  
  
  vifvar_sorted <- sqldf("select * from vifvar_sorteda where vifvar not in ('km*year*fuel','km*fuel','year*fuel')")
  
  maxvif <- vifvar_sorted[1,"vif"]
  vifvar <- vifvar_sorted[1,"vifvar"]

}

