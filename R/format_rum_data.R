
#' Function to format RUM data

#' Function calls mlogit

#' @param data_in Data going in to the function; default is filt_clusts
##' @param trip_dists Distances covered by each trip
#' @param the_port Port of focus; Default is Astoria
#' @param min_year Minimum year used to filter the data
#' @param max_year Maximum year used to filter the data, also RUM data is filtered to be from the max_year
#' @param risk_coefficient Coefficient to adjust the quota prices up or down, feeding into net revenue calculations
#' @param tow_num_range Filter the tows to be the first tow or subsequent tows
#' @param ndays Number of previous days data to use in revenue expectations

#' @export

format_rum_data <- function(data_in = filt_clusts, trip_dists1 = trip_dists, the_port = "ASTORIA / WARRENTON",
  min_year = 2011, max_year = 2012, risk_coefficient = 1, tow_num_range = c(1, 1),
  ndays = 60){

  ##Filter the data
  dat <- data_in %>% filter(dport_desc == the_port, set_year >= min_year,
    set_year <= max_year, haul_num >= tow_num_range[1], haul_num <= tow_num_range[2])
  if(nrow(dat) == 0) stop("no values")
  #---------------------------------------------------------------
  #Calculate haul revenues
  #Turn NA prices into zeroes
  dat[which(is.na(dat$avg_quota_price)), 'avg_quota_price'] <- 0
  
  #Adjust the net prices based on risk coefficient
  dat$rc <- risk_coefficient
  dat$net_price <- (dat$exval_pound - dat$rc * dat$avg_quota_price)
  dat$net_revenue <- dat$net_price * dat$hpounds

  #Sum the haul revenues
  dat <- dat %>% group_by(haul_id) %>% 
    mutate(haul_net_revenue = sum(net_revenue, na.rm = T))
  
  #Create data set, for each tow
  dist_hauls <- dat %>% distinct(haul_id, .keep_all = T) %>% select(haul_id, unq_clust, set_month, 
    drvid, trip_id, set_day, set_year, haul_net_revenue, avg_long_clust, avg_lat_clust) %>% as.data.frame

  #---------------------------------------------------------------
  #Filter the data by times
  #Use expectations from N days prior; maybe use lubridate
  dist_hauls$set_date <- paste(dist_hauls$set_year, dist_hauls$set_month, dist_hauls$set_day, 
    sep = "-")
  dist_hauls$set_date <- ymd(dist_hauls$set_date)
  dist_hauls$prev_date <- dist_hauls$set_date - days(ndays)

  dates <- data.frame(set_date = unique(dist_hauls$set_date), 
    date_id = 1:length(unique(dist_hauls$set_date)))

  dist_hauls <- dist_hauls %>% left_join(dates, by = "set_date")

  #Calculate average revenues and distances in each cluster
  rum_vals <- lapply(unique(dist_hauls$date_id), FUN = function(xx){
    the_period <- dist_hauls %>% filter(date_id == xx)
    the_early_date <- unique(the_period$prev_date)
    the_set_date <- unique(the_period$set_date)
    
    temp <- dist_hauls %>% filter(set_date <= the_set_date, 
      set_date >= the_early_date) %>% group_by(unq_clust) %>% 
      summarize(avg_revenue = mean(haul_net_revenue, na.rm = T))
    temp$date_id <- xx
    return(temp)
  })
  
  #Add in the periods
  rum_vals <- ldply(rum_vals)

  #---------------------------------------------------------------------------------
  #Distances for cluster combinations
  
  #Calculate distances between each cluster; I know the distances between each port
  #All combinations of clusters
  if(1 %in% tow_num_range){
    port_combs <- dat %>% ungroup %>% select(dport_desc, unq_clust,
      d_port_long, d_port_lat, avg_long_clust, avg_lat_clust) %>%
      distinct(unq_clust, .keep_all = T)
  
    port_combs$d_port_long <- deg2rad(port_combs$d_port_long)
    port_combs$d_port_lat <- deg2rad(port_combs$d_port_lat)
    port_combs$avg_long_clust <- deg2rad(port_combs$avg_long_clust)
    port_combs$avg_lat_clust <- deg2rad(port_combs$avg_lat_clust)

    port_combs$btw_clust_dist <- gcd_slc(port_combs$d_port_long, 
      port_combs$d_port_lat, port_combs$avg_long_clust, 
      port_combs$avg_lat_clust)

    port_combs$btw_clust_dist <- round(port_combs$btw_clust_dist, digits = 6)
    port_combs <- port_combs %>% as.data.frame
    names(port_combs)[2] <- 'alt_clust'
    # clust_combs <- port_combs
    # names(clust_combs)[1:2] <- c('dport_desc', 'alt_clust')
      
  #Different clusters if it's the first tow
    clust_combs <- expand.grid(unique(dat$unq_clust), unique(dat$unq_clust))
    names(clust_combs) <- c('tow_clust', 'alt_clust')
    clust_combs <- clust_combs %>% left_join(port_combs %>% select(alt_clust, btw_clust_dist),
      by = 'alt_clust')
    clust_combs$btw_clust_dist <- round(clust_combs$btw_clust_dist, digits = 6)
  } 
  
  #If not looking at the first tows, calculate the distance between different clusters at sea
  if(1 %in% tow_num_range == FALSE){
    clust_combs <- expand.grid(unique(dat$unq_clust), unique(dat$unq_clust))
    names(clust_combs) <- c('clust1', 'clust2')
  
    clust_coords <- dist_hauls %>% distinct(unq_clust, .keep_all = T) %>% 
      select(unq_clust, avg_long_clust, avg_lat_clust) 
    
    #Add clust1 Values
    clust_combs <- clust_combs %>% left_join(clust_coords, by = c('clust1' = 'unq_clust'))
    names(clust_combs)[3:4] <- c('long_clust1', 'lat_clust1')
    clust_combs <- clust_combs %>% left_join(clust_coords, by = c('clust2' = 'unq_clust'))
    names(clust_combs)[5:6] <- c('long_clust2', 'lat_clust2')
    clust_combs$long_clust1 <- deg2rad(clust_combs$long_clust1)
    clust_combs$long_clust2 <- deg2rad(clust_combs$long_clust2)
    clust_combs$lat_clust1 <- deg2rad(clust_combs$lat_clust1)
    clust_combs$lat_clust2 <- deg2rad(clust_combs$lat_clust2)
    
    clust_combs$btw_clust_dist <- gcd_slc(clust_combs$long_clust1, 
      clust_combs$lat_clust1, clust_combs$long_clust2, clust_combs$lat_clust2)
    
    # clust_combs <- clust_combs %>% filter(clust1 != clust2)
    clust_combs$btw_clust_dist <- round(clust_combs$btw_clust_dist, digits = 6)
    names(clust_combs)[1:2] <- c('tow_clust', 'alt_clust')
  }

  #---------------------------------------------------------------------------------
  #Add the rum_vals into dist_hauls
  dist_hauls1 <- dist_hauls %>% full_join(rum_vals, by = 'date_id') %>%
    plyr::rename(c('unq_clust.x' = "tow_clust",
    "unq_clust.y" = "alt_clust"))
  
  #Expand so that each haul has all alternative clusters
  dist_hauls1 <- dist_hauls1 %>% complete(haul_id, alt_clust) 
  dist_hauls1 <- clust_combs %>% select(tow_clust, alt_clust, btw_clust_dist) %>% 
    right_join(dist_hauls1, by = c('tow_clust', 'alt_clust'))

  #Takes a while
  dist_hauls1 <- dist_hauls1 %>% group_by(haul_id) %>% fill(tow_clust)

  #Re add the btw_clust_distances
  dist_hauls1$btw_clust_dist <- NULL
  dist_hauls1 <- clust_combs %>% select(tow_clust, alt_clust, btw_clust_dist) %>% 
    right_join(dist_hauls1, by = c('tow_clust', 'alt_clust'))

  #Set distances in the same cluster to 0
  dist_hauls1[which(dist_hauls1$tow_clust == dist_hauls1$alt_clust), 
    "btw_clust_dist"] <- 0
 
  #Add column indicating if the cluster was fished or not 
  dist_hauls1$fished <- FALSE
  dist_hauls1[which(dist_hauls1$tow_clust == dist_hauls1$alt_clust), 
    "fished"] <- TRUE
  
  #Add in integer value for haul_id
  hh <- data_frame(haul_id = unique(dist_hauls1$haul_id), 
    haul_id_id = 1:length(unique(dist_hauls1$haul_id)))
  dist_hauls1 <- dist_hauls1 %>% left_join(hh, by = 'haul_id')
  dist_hauls1 <- dist_hauls1 %>% as.data.frame

  row.names(dist_hauls1) <- paste(dist_hauls1$haul_id_id, 
    dist_hauls1$alt_clust, sep = '.')

  #Filter to only look at one year
  hauls_max_year <- dist_hauls1 %>% filter(set_year == max_year) %>% distinct(haul_id)
  hauls_max_year <- dist_hauls1 %>% filter(haul_id %in% hauls_max_year$haul_id)

  #Cast the data into the right format
  #Distances, keep only values that are have a tow_clust value
  dists <- hauls_max_year %>% dcast(haul_id_id + tow_clust ~ alt_clust, 
    value.var = "btw_clust_dist") 

  #Fill in the dists columns if looking at first tows
  if(1 %in% tow_num_range){
    temp_dists <- dists[, 3:ncol(dists)]
    temp_dists[is.na(temp_dists)] <- 0
    temp_dists1 <- lapply(temp_dists, FUN = function(xx){
      out <- rep(max(xx), length(xx))
      return(out)
    })
  
    for(zz in 1:length(temp_dists1)){
      temp_dists[, zz] <- temp_dists1[[zz]]
    }
    
    dists[, 3:ncol(dists)] <- temp_dists
  }

  dists_keeps <- c(1, 2, which(names(dists) %in% as.character(unique(dists$tow_clust))))
  dists <- dists[, dists_keeps]
  names(dists)[3:ncol(dists)] <- paste("dist", names(dists)[3:ncol(dists)], sep = ".")
  dists <- dists %>% filter(is.na(tow_clust) == FALSE)
  
  #Revenues, keep only values that are have a tow_clust value
  revs <- hauls_max_year %>% dcast(haul_id_id + tow_clust ~ alt_clust, 
    value.var = "avg_revenue") 
  revs_keeps <- c(1, 2, which(names(revs) %in% as.character(unique(revs$tow_clust))))
  revs <- revs[, revs_keeps]
  names(revs)[3:ncol(revs)] <- paste("revs", names(revs)[3:ncol(revs)], sep = ".")
  revs <- revs %>% filter(is.na(tow_clust) == FALSE)
  #---------------------------------------------------------------------------------
  #Join the two data sets
  rum_dat <- dists %>% left_join(revs, by = c('haul_id_id', 'tow_clust'))
  rum_dat <- rum_dat %>% filter(is.na(tow_clust) == FALSE)

  #Fill NA values with zeroes in rum_2012
  rum_dat[is.na(rum_dat)] <- 0

  #Return the data
  rum_dat_out <- mlogit.data(rum_dat, shape = 'wide', choice = "tow_clust",
    varying = 3:ncol(rum_dat), id = "haul_id_id")

  return(rum_dat_out)
}







