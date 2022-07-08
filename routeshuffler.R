
# Algorithm to find walking route based on points-of-interest and walking distance

5 km : 2-3 points of interest
10 km: 4-5 poi

Proces:
  * Find og saml et antal pois - udfra afstand samt kvalitet(ratings)

#########

library(googleway)
library(geosphere)
library(dplyr)
library(htmlwidgets)

api_key <- "AIzaSyBkYZIiBgvnYH5UrQudZRNF65etDWcWolA"

# Finding Points of interest!! ####
poi <- c("kirke","kyst","skov","udsigtspunkt","sø","shelter","slot","museum",
         "restaurant","cafe","campground","dagligvare","park","sti")
requested_pois <- c("udsigtspunkt","monument","park","rådhus")

startposition <- c(56.17609130816868, 10.203958794446983) # home
startposition <- c(56.15694504346022, 10.210435066971705) # domkirken
startposition <- c(56.19776514660628, 10.676984075257698) # ebeltoft
endposition <- c(56.24439437148399, 10.337024308047274)
endposition <- startposition

# geodist afstand enten som længde af gåtur eller fra start til slutposition
geodist_start_end <- distm(x = rev(startposition), y = rev(endposition),fun = distGeo)[,1]
geodist_start_end <- 6500 
geodist_start_end <- 10000 

# Finder requestede pois

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
outpois$geoDistTotal <- outpois$geoDist2start+outpois$geoDist2end

outpois <- outpois[outpois$geoDistTotal<(geodist_start_end*sqrt(2)),]
if (startposition==endposition) outpois <- outpois[outpois$geoDistTotal < geodist_start_end,]
# basic criteria (ratings, distance)
outpois <- outpois[outpois$geoDist2start<geodist_start_end,]
outpois <- outpois[outpois$geoDist2end<geodist_start_end,]
outpois <- outpois[outpois$user_ratings_total>10,]
outpois <- outpois[outpois$rating>3.5,]
# Sorterer så der - såvidt muligt - kommer en af hver pois type
outpois <- outpois %>% 
  arrange(poi_type,geoDistTotal) %>%
  group_by(poi_type) %>% 
  mutate(ranks = rank(geoDistTotal)) 
# Meget vigtigt at waypoints er sorteret efter attraktivitet 
outpois <- outpois[order(outpois$ranks,outpois$geoDistTotal),]
outpois <- data.frame(outpois)
outpois$farve <- "lavender"

# Geodistances ordered according to travelling salesman algo ####
library(TSP)

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
    re_ordered_xy2[i,"distonce_to_last_point"] <- distm(x = rev(re_ordered_xy2[i-1,c("lat","lng")]), y = rev(re_ordered_xy2[i,c("lat","lng")]),fun = distGeo)[,1]
    re_ordered_xy2[i,"distonce_sum"] <- sum(na.omit(re_ordered_xy2[2:i,"distonce_to_last_point"]))
    re_ordered_xy2[i,"distonce_roundtrip"] <- re_ordered_xy2[i,"distonce_sum"] + distm(x = rev(re_ordered_xy2[i,c("lat","lng")]), y = rev(endposition),fun = distGeo)[,1]
  }
  totalgeodist <- max(na.omit(re_ordered_xy2[,"distonce_roundtrip"]))
}
n <- n - 1

# Generating route(s)  ####

# Generating n waypoints from outpois df
waypoints <- list()
for (i in 1:n) { # bruger så mange waypoints som fundet ovenfor
  waypoints[i] <- list(stop=c(outpois[i,"lat"],outpois[i,"lng"]))
}

# Generate route 

res <- google_directions(
  origin = startposition,
  destination = endposition,
  mode = "walking",
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

tmp <- as.data.frame(res$routes$legs)
sum(tmp$distance$value)
tmp$steps

## set up a data.frame of locations
## can also use 'lat/lon' coordinates as the origin/destination
df_locations <- data.frame(
  origin = startposition,
  destination = endposition,
  stringsAsFactors = F
)

## loop over each pair of locations, and extract the polyline from the result
lst_directions <- apply(df_locations, 1, function(x){
  res <- google_directions(
                            origin = startposition,
                            destination = endposition,
                            mode = "walking",
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
  
  df_result <- data.frame(
    origin = x[['origin']],
    destination = x[['destination']],
    route = res$routes$overview_polyline$points
  )
  return(df_result)
})

## convert the results to a data.frame
df_directions <- do.call(rbind, lst_directions)

## plot the map
etkort <- google_map(key = api_key ) %>%
  add_polylines(data = df_directions, polyline = "route") %>%
  add_markers(data=outpois[1:n,],lat="lat",lon="lng",label="poi_type", colour="farve")

saveWidget(etkort, file="testmap.html")


# Photos
https://maps.googleapis.com/maps/api/place/photo
?maxwidth=400
&photo_reference=Aap_uEA7vb0DDYVJWEaX3O-AtYp77AaswQKSGtDaimt3gt7QCNpdjp1BkdM6acJ96xTec3tsV_ZJNL_JP-lqsVxydG3nh739RE_hepOOL05tfJh2_ranjMadb3VoBYFvF0ma6S24qZ6QJUuV6sSRrhCskSBP5C1myCzsebztMfGvm7ij3gZT
&key=AIzaSyBkYZIiBgvnYH5UrQudZRNF65etDWcWolA