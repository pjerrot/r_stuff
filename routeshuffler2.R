
#########

library(googleway)
library(geosphere)
library(dplyr)
library(htmlwidgets)
library(TSP)

api_key <- "AIzaSyBkYZIiBgvnYH5UrQudZRNF65etDWcWolA"

# Finding Points of interest!! ####
poi <- c("kirke","kyst","skov","udsigtspunkt","sø","shelter","slot","museum",
         "restaurant","cafe","campground","dagligvare","park","sti","seværdighed")
requested_pois <- c("udsigtspunkt","monument","park","shopping","Empire State","seværdighed")
requested_pois <- c("scenic view","monument","park","seværdighed")
requested_pois <- c("bar")

startposition <- c(56.17609130816868, 10.203958794446983) # home
startposition <- c(56.15694504346022, 10.210435066971705) # domkirken
startposition <- c(56.19776514660628, 10.676984075257698) # ebeltoft
startposition <- c(40.76701351999271, -73.97699338872003) # central park
startposition <- c(41.37861361890402, 2.1754184456750445) # Rambla, Barcelona
startposition <- c(41.90535963278869, 12.505269313039008) # Rom
startposition <- c(55.86435870815893, 10.548313918246768) # Samsø

startposition <- c(56.20181341954494, 10.709597283178919)
endposition <- c(56.16997536280391, 10.645086010617646)

endposition <- c(56.24439437148399, 10.337024308047274)

endposition <- startposition

# geodist afstand enten som længde af gåtur eller fra start til slutposition
geodist_start_end <- distm(x = rev(startposition), y = rev(endposition),fun = distGeo)[,1]
geodist_start_end <- 5000 
geodist_start_end <- 6500 
geodist_start_end <- 10000 
geodist_start_end <- 12000 
geodist_start_end <- 15000 
geodist_start_end <- 20000 
geodist_start_end <- 40000 
geodist_start_end <- 50000 

# Finder requestede pois ####

rm(outpois)
for (rpoi in requested_pois) {
  tmp <- google_places(search_string=rpoi,
                       location=startposition,
                       radius=geodist_start_end/2,
                       key=api_key)
  if (tmp$status=="OK") {
    tmp <- tmp$results
    row.names(tmp) <- NULL
    tmp[,"lat"] <- tmp$geometry$location$lat
    tmp[,"lng"] <- tmp$geometry$location$lng
    tmp$poi_type <- rpoi
    for (i in 1:nrow(tmp)) {
      tmp[i,"geoDist2start"] <- distm(x = rev(startposition), y = rev(c(tmp[i,"lat"],tmp[i,"lng"])),fun = distGeo)[,1]
      tmp[i,"geoDist2end"] <- distm(x = rev(c(tmp[i,"lat"],tmp[i,"lng"])), y = rev(endposition),fun = distGeo)[,1]
    }
    if (exists("outpois")) {outpois <- bind_rows(outpois,tmp)} else {outpois <- tmp}
  }
}

vars <- colnames(outpois)[!colnames(outpois) %in% c("poi_type","photos")]
outpois <- outpois %>% group_by_at(vars) %>%
  summarise(poi_type = paste0(poi_type,collapse=","),
            photos = photos[1])

outpois$geoDistTotal <- outpois$geoDist2start+outpois$geoDist2end

outpois <- outpois[outpois$geoDistTotal<(geodist_start_end*1.5),]
if (equals(startposition,endposition)) {
  outpois <- outpois[outpois$geoDistTotal < (geodist_start_end*0.7),]
}

outpois$user_ratings_sum <- outpois$rating*outpois$user_ratings_total

# basic criteria (ratings, distance)
outpois <- outpois[outpois$geoDist2start<geodist_start_end,]
outpois <- outpois[outpois$geoDist2end<geodist_start_end,]
#outpois <- outpois[outpois$user_ratings_total>10,]
outpois <- outpois[outpois$rating>3.5,]

# Sorterer så der - såvidt muligt - kommer en af hver pois type
outpois <- outpois %>% 
  arrange(poi_type,geoDistTotal) %>%
  group_by(poi_type) %>% 
  mutate(ranks = rank(geoDistTotal)) %>%
  ungroup() %>%
  mutate(ranks_rating = rank(-rating*user_ratings_total/geoDistTotal))
  
outpois$rankssum <- outpois$ranks + outpois$ranks_rating

# Meget vigtigt at waypoints er sorteret efter attraktivitet 
outpois <- outpois[order(outpois$rankssum,outpois$geoDistTotal),]
outpois <- data.frame(outpois)
outpois$farve <- "lavender"
outpois <- outpois[!is.na(outpois$geoDistTotal),]

# Geodistances ordered according to travelling salesman algo ####
# TRAVELLING SALESMAN
# Purpose is to estimate the right number of waypoints - to reduce api runs

if (equals(startposition,endposition)) { # kun hvis rundtur
  totalgeodist <- 0
  n <- 0
  while (totalgeodist < geodist_start_end) {
    n <- n + 1
    xy  <- rbind(startposition,outpois[1:n,c("lat","lng")],endposition)
    xytsp <- ETSP(data.frame(xy))
    colnames(xytsp) <- c("x", "y")
    xytour <- solve_TSP(xytsp)
    plot(xytsp, xytour, pch=20, tour_col="red", tour_lty="solid")
    re_ordered_xy <- xy[as.numeric(xytour), ]
    
    for (i in 1:nrow(re_ordered_xy)) if (re_ordered_xy[i,c("lat","lng")]==startposition) pos <- i
    re_ordered_xy2 <- rbind(re_ordered_xy[pos:nrow(re_ordered_xy),],re_ordered_xy[1:(pos-1),])
    for (i in 2:nrow(re_ordered_xy2)) {
      re_ordered_xy2[i,"distance_to_last_point"] <- distm(x = rev(re_ordered_xy2[i-1,c("lat","lng")]), y = rev(re_ordered_xy2[i,c("lat","lng")]),fun = distGeo)[,1]
      re_ordered_xy2[i,"distance_sum"] <- sum(na.omit(re_ordered_xy2[2:i,"distance_to_last_point"]))
      re_ordered_xy2[i,"distance_roundtrip"] <- re_ordered_xy2[i,"distance_sum"] + distm(x = rev(re_ordered_xy2[i,c("lat","lng")]), y = rev(endposition),fun = distGeo)[,1]
    }
    totalgeodist <- max(na.omit(re_ordered_xy2[,"distance_roundtrip"]))
  }
  n <- n - 1
} else { # fra A til B
  n <- nrow(outpois)
}

# Generating route(s)  ####

makemap <- function(n_waypoints,
                    startposition,
                    mode = "walking",
                    endposition, 
                    outpois, 
                    api_key) {
  
  # Generating n waypoints from outpois df
  waypoints <- list()
  for (i in 1:n_waypoints) { # bruger så mange waypoints som fundet ovenfor
    waypoints[i] <- list(stop=c(outpois[i,"lat"],outpois[i,"lng"]))
  }
  
  # Generate route 
  res <- google_directions(
    origin = startposition,
    destination = endposition,
    mode = mode,
    waypoints = waypoints,
    optimise_waypoints = TRUE,
    alternatives = FALSE,
    avoid = "highways",
    units = c("metric"),
    language = NULL,
    key = api_key,
    simplify = TRUE,
    curl_proxy = NULL
  )
  
  route <- res$routes$overview_polyline$points
  legs <- as.data.frame(res$routes$legs)
  totaldistance <- sum(legs$distance$value)
  
  ## convert the results to a data.frame
  df_directions <- data.frame(origin=startposition, 
                              destination=endposition, 
                              route=route)
  
  #reorder waypoints bc direction url cannot optimize
  # Generating n waypoints from outpois df
  waypoints2 <- list()
  for (i in 2:(nrow(legs))) { # bruger så mange waypoints som fundet ovenfor
    waypoints2[i-1] <- list(stop=c(legs$start_location[i,"lat"],legs$start_location[i,"lng"]))
  }

  url <- google_maps_direction_url(
    origin = startposition,
    destination = endposition,
    travel_mode = "walking",
    waypoints = waypoints2
  )
  
  ## plot the map
  mapplot <- google_map(key = api_key ) %>%
    add_polylines(data = df_directions, polyline = "route") %>%
    add_markers(data=outpois[1:n,],lat="lat",lon="lng",
                label="poi_type", colour="farve",
                mouse_over = "name")
  
  out <- list(res,route,legs,totaldistance,mapplot, url)
  names(out) <- c("route","routehash","legs","totaldistance","mapplot","mapurl")
  return(out)
  
} # end makemap


distance_factor_criteria <- ifelse(equals(startposition,endposition),1.1,1.5)
repeat { # Repeat'er indtil ruten er mindre end spec+10%
  outmap <- makemap(n_waypoints = n,
                    startposition = startposition,
                    endposition = endposition,
                    outpois = outpois,
                    api_key = api_key
                    )
  if (outmap$totaldistance<(geodist_start_end*distance_factor_criteria)) {
    break
  } else {
    n <- n - 1
  }
}

outmap$totaldistance
View(outmap$legs)
saveWidget(outmap$mapplot, file="aarhus_6km_sights.html")
saveWidget(outmap$mapplot, file="newyork_12km_sights.html")

outmap$mapurl
outmap$route
outmap$mapplot
outmap$totaldistance
View(outmap$legs)

#################
#################
google_map_directions(
  origin = startposition,
  destination = endposition,
  travel_mode = "walking",
  waypoints = waypoints
)

?google_map_directions

url <- google_maps_direction_url(
  origin = startposition,
  destination = endposition,
  travel_mode = "walking",
  waypoints = waypoints
)
url

# Photos
https://maps.googleapis.com/maps/api/place/photo
?maxwidth=400
&photo_reference=Aap_uEA7vb0DDYVJWEaX3O-AtYp77AaswQKSGtDaimt3gt7QCNpdjp1BkdM6acJ96xTec3tsV_ZJNL_JP-lqsVxydG3nh739RE_hepOOL05tfJh2_ranjMadb3VoBYFvF0ma6S24qZ6QJUuV6sSRrhCskSBP5C1myCzsebztMfGvm7ij3gZT
&key=AIzaSyBkYZIiBgvnYH5UrQudZRNF65etDWcWolA