
# Geodistances ordered according to travelling salesman algo ####
# TRAVELLING SALESMAN
# Purpose is to estimate the right number of waypoints - to reduce api runs

tscalc <- function(xy,outpois, startposition, endposition) {
  xy0 <- xy
  for (j in 2:(nrow(xy0)-1)) {
    xy <- xy0[-j,]
    nu_kor <- xy0[j,c("lat","lng")]
    xytsp <- ETSP(data.frame(xy[,1:2]))
    colnames(xytsp)[1:2] <- c("x", "y")
    xytour <- solve_TSP(xytsp)
    plot(xytsp, xytour, pch=20, tour_col="red", tour_lty="solid")
    re_ordered_xy <- xy[as.numeric(xytour), ]
    
    # Making sure that route starts with start and ends with end..
    for (i in 1:nrow(re_ordered_xy)) if (equals(re_ordered_xy[i,c("lat","lng")],startposition)) pos <- i
    re_ordered_xy2 <- rbind(re_ordered_xy[pos:nrow(re_ordered_xy),],re_ordered_xy[1:(pos-1),])
    
    for (i in 2:nrow(re_ordered_xy2)) {
      re_ordered_xy2[i,"distance_to_last_point"] <- distm(x = rev(re_ordered_xy2[i-1,c("lat","lng")]), y = rev(re_ordered_xy2[i,c("lat","lng")]),fun = distGeo)[,1]
      re_ordered_xy2[i,"distance_sum"] <- sum(na.omit(re_ordered_xy2[2:i,"distance_to_last_point"]))
      re_ordered_xy2[i,"distance_roundtrip"] <- re_ordered_xy2[i,"distance_sum"] + distm(x = rev(re_ordered_xy2[i,c("lat","lng")]), y = rev(endposition),fun = distGeo)[,1]
    }
    sumdist <- max(na.omit(re_ordered_xy2[,"distance_roundtrip"]))
    xy0[xy0$lat==nu_kor$lat & xy0$lng==nu_kor$lng,"sumdist"] <- sumdist
  }
  
  return(xy_df,)
}

n <- nrow(outpois) # if A to B. Changed below if not..

if (equals(startposition,endposition)) { # kun hvis rundtur
  sumdist <- geodist_start_end + 1
  n <- nrow(outpois)
  xy  <- rbind(startposition,outpois[1:n,c("lat","lng")],endposition)
  while (sumdist > geodist_start_end) {
    
    xytsp <- ETSP(data.frame(xy))
    colnames(xytsp) <- c("x", "y")
    xytour <- solve_TSP(xytsp)
    plot(xytsp, xytour, pch=20, tour_col="red", tour_lty="solid")
    re_ordered_xy <- xy[as.numeric(xytour), ]
    
    # Making sure that route starts with start and ends with end..
    for (i in 1:nrow(re_ordered_xy)) if (equals(re_ordered_xy[i,c("lat","lng")],startposition)) pos <- i
    re_ordered_xy2 <- rbind(re_ordered_xy[pos:nrow(re_ordered_xy),],re_ordered_xy[1:(pos-1),])
    
    for (i in 2:nrow(re_ordered_xy2)) {
      re_ordered_xy2[i,"distance_to_last_point"] <- distm(x = rev(re_ordered_xy2[i-1,c("lat","lng")]), y = rev(re_ordered_xy2[i,c("lat","lng")]),fun = distGeo)[,1]
      re_ordered_xy2[i,"distance_sum"] <- sum(na.omit(re_ordered_xy2[2:i,"distance_to_last_point"]))
      re_ordered_xy2[i,"distance_roundtrip"] <- re_ordered_xy2[i,"distance_sum"] + distm(x = rev(re_ordered_xy2[i,c("lat","lng")]), y = rev(endposition),fun = distGeo)[,1]
    }
    sumdist <- max(na.omit(re_ordered_xy2[,"distance_roundtrip"]))
    
    re_ordered_xy3 <- left_join(re_ordered_xy2, outpois, by = c("lat" = "lat", "lng" = "lng")) %>%              # Automatically delete ID
      select(lat, lng, distance_to_last_point, distance_sum, distance_roundtrip,user_ratings_sum)
    
  }
  n <- n - 1
} 
