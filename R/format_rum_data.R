
#---------------------------------------------------------------------------------
#Format RUM data for two scenarios, very risk averse and normal

#-------Arguments if I ever wanted to make this into a function
#For now do this just for ASTORIA
# the_port <- "ASTORIA / WARRENTON"

# #Make sure that years are limited
# min_year <- 2011
# max_year <- 2013

# #Risk coefficient adjustment for quota prices
# risk_coefficient <- 1
# # min_tow_num <- 2
# tow_num_range <- c(1, 1)
# ndays <- 60
#-------
# format_rum_data(tow_num_range = c(2, 20))

#' Function to format RUM data

#' Function calls mlogit

#' @param data_in Data going in to the function; default is filt_clusts
#' @param trip_dists Distances covered by each trip
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

  #Add in the trip distances
  # dist_hauls <- dist_hauls %>% left_join(trip_dists1[, c('trip_id', "dist")], 
  #   by = "trip_id") %>% plyr::rename(c("dist" = "trip_dist"))

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
  #
  #Calculate distances between each cluster; I know the distances between each port
  #All combinations of clusters
  if(1 %in% tow_num_range){
    # browser()

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

    # clust_coords <- dist_hauls %>% distinct(unq_clust, .keep_all = T) %>% 
    #   select(unq_clust, avg_long_clust, avg_lat_clust) 
    
    #Add clust1 Values
    # clust_combs <- clust_combs %>% left_join(clust_coords, by = c('clust1' = 'unq_clust'))
    # names(clust_combs)[3:4] <- c('long_clust1', 'lat_clust1')
    # clust_combs <- clust_combs %>% left_join(clust_coords, by = c('clust2' = 'unq_clust'))
    # names(clust_combs)[5:6] <- c('long_clust2', 'lat_clust2')
    # clust_combs$long_clust1 <- deg2rad(clust_combs$long_clust1)
    # clust_combs$long_clust2 <- deg2rad(clust_combs$long_clust2)
    # clust_combs$lat_clust1 <- deg2rad(clust_combs$lat_clust1)
    # clust_combs$lat_clust2 <- deg2rad(clust_combs$lat_clust2)
    
    # clust_combs$btw_clust_dist <- gcd_slc(clust_combs$long_clust1, 
    #   clust_combs$lat_clust1, clust_combs$long_clust2, clust_combs$lat_clust2)
    
    # clust_combs <- clust_combs %>% filter(clust1 != clust2)
    clust_combs$btw_clust_dist <- round(clust_combs$btw_clust_dist, digits = 6)
    # names(clust_combs)[1:2] <- c('tow_clust', 'alt_clust')
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










#Simulate the data for 2013, using 2011-2012 data depending

#How to combine rum_vals and periods

#How to merge the data frames?
# firsts <- dist_hauls %>% filter(date_id %in% c(1, 2))
# firsts_rums <- rum_vals %>% filter(date_id %in% c(1, 2))

#Check this for now

# dist_hauls1 <- dh2

#Now fill set_month, drvid, trip_id, set_day, set_year, set_date, prev_date, date_id
# dist_hauls1 <- dist_hauls1 %>% group_by(haul_id) %>% 
#   fill(date_id, )

# dist_hauls1 %>% filter(haul_id == "100000") %>% as.data.frame
#   fill()
# dist_hauls1 %>% filter(haul_id == "100000") %>% select(set_date) %>% 
#   distinct()
# dist_hauls1 %>% group_by(haul_id) %>% 
#   summarize(nalts = length(unique(alt_clust))) %>% ungroup %>% distinct(nalts)

#Make sure each hauls has 130 clusters or so

# left_join for each haul
# temp <- subset(dist_hauls1, haul_id == '100000')
# clust_combs %>% filter(tow_clust == unique(temp$tow_clust)) %>% 
#   left_join(temp, by = c('tow_clust', "alt_clust")) %>% head
# one_haul <- dist_hauls1 %>% filter(haul_id == "100000")

#Remove hauls with distances less than the trip distance
# dist_hauls1 <- dist_hauls1 %>% filter(trip_dist >= btw_clust_dist)


# dist_hauls1 %>% filter(haul_id == '100008')
# dist_hauls1 %>% group_by(haul_id) %>% summarize(nalts = length(unique(alt_clust))) %>%
#   ungroup %>% distinct(nalts)

# dist_hauls1 %>% group_by(tow_clust) %>% 
#   summarize(nalts = length(unique(alt_clust))) %>% ungroup %>% distinct(nalts)


# row.names(dist_hauls1_inner) <- paste(dist_hauls1_inner$haul_id_id, 
#   dist_hauls1_inner$alt_clust, sep = '.')

# dist_hauls1 <- dist_hauls1 %>% filter(haul_id_id != 5476)


# dist_hauls1 %>% group_by(haul_id_id) %>% 
#   summarize(nclusts = length(unique(alt_clust)))

#Try running this
# which(duplicated(rownames(dist_hauls1))) 

#Need to get it working with mlogit.data
# dist_hauls1 %>% filter(haul_id_id == 1)




# #Try the rum_2012 with mlogit.data
# library(mlogit)

# save(rum_2012_dat, file = 'output/rum_2012_dat.Rdata')

# start_time <- Sys.time()
# res1 <- mlogit(tow_clust ~ dist + revs, rum_2012_dat)
# run_time <- Sys.time() - start_time

# save(res1, file = 'output/rum_2012_res.Rdata')

# start_time <- Sys.time()
# res1 <- mlogit(fished ~ btw_clust_dist + avg_revenue, dist_hauls1)
# run_time <- Sys.time() - start_time


# #Make sure that each tow_clust has info for all the alternative clusters
# clust_combs %>% group_by(tow_clust) %>% summarize(nclusts = length(unique(alt_clust)))



# dist_hauls1 %>% filter(haul_id_id == 5) %>% group_by(haul_id_id) %>%
#   summarize(nclusts = length(unique(alt_clust)))
# length(unique(dist_hauls1$alt_clust))


# dh1 <- dist_hauls1 %>% select(haul_id_id, tow_clust, alt_clust)
# dh1$rep <- 1
# dh1 <- dh1 %>% expand(nesting(tow_clust, alt_clust), rep)
# dh1 %>% group_by(tow_clust) %>% 
#   summarize(nclusts = length(unique(alt_clust))) %>% ungroup %>% distinct(nclusts)

#  expand(nesting(haul_id_id, tow_clust), date_id)



# expand(df, nesting(school_id, student_id), date)

# dist_hauls1 %>% group_by(haul_id_id) %>% 
#   summarize(nclusts = length(unique(alt_clust))) %>% distinct(nclusts)


# dist_hauls1$idcase <- tow_

# firsts_merge %>% filter(tow_clust == alt_clust)

# #Format this for mlogit
# # Have to have distances and revenues with each cluster in columns
# # dist_hauls1 %>% filter(tow_clust == 19, date_id == 1) %>% 
# #   dcast(tow_clust + haul_id ~ alt_clust, value.var = c('avg_revenue',
# #     "btw_clust_dist"))

# # the_dists <- dist_hauls1  %>% 
# #   dcast(tow_clust + haul_id + date_id ~ alt_clust, value.var = "btw_clust_dist")
# # names(the_dists)[-(1:3)] <- paste("distance", names(the_dists)[-(1:3)], sep = ".")

# # the_revs <- dist_hauls1  %>% 
# #   dcast(tow_clust + haul_id + date_id ~ alt_clust, value.var = "avg_revenue")
# # names(the_revs)[-(1:3)] <- paste("revenue", names(the_revs)[-(1:3)], sep = ".")

# # wide_rum <- the_dists %>% left_join(the_revs, by = c('tow_clust', 'haul_id',
# #   'date_id'))


# #Format the data for RUM in mlogit
# library(mlogit)
# # mlogit_data <- mlogit.data(wide_rum, shape = 'wide', choice = "tow_clust",
# #   varying = c(4, ncol(wide_rum)))




# paste("tow_clust", names(the_dists)[-(1:4)], sep = ".")
# dcast(tow_clust ~ alt_clust, 
#   value.var = "avg_revenue")



# ##Distances
# #---------------------------------------------------------------------------------
# #Calculate distances between each cluster; I know the distances between each port
# #All combinations of clusters
# clust_combs <- expand.grid(unique(dat$unq_clust), unique(dat$unq_clust))
# names(clust_combs) <- c('clust1', 'clust2')

# clust_coords <- dist_hauls %>% distinct(unq_clust, .keep_all = T) %>% 
#   select(unq_clust, avg_long_clust, avg_lat_clust) 

# #Add clust1 Values
# clust_combs <- clust_combs %>% left_join(clust_coords, by = c('clust1' = 'unq_clust'))
# names(clust_combs)[3:4] <- c('long_clust1', 'lat_clust1')
# clust_combs <- clust_combs %>% left_join(clust_coords, by = c('clust2' = 'unq_clust'))
# names(clust_combs)[5:6] <- c('long_clust2', 'lat_clust2')
# clust_combs$long_clust1 <- deg2rad(clust_combs$long_clust1)
# clust_combs$long_clust2 <- deg2rad(clust_combs$long_clust2)
# clust_combs$lat_clust1 <- deg2rad(clust_combs$lat_clust1)
# clust_combs$lat_clust2 <- deg2rad(clust_combs$lat_clust2)

# clust_combs$btw_clust_dist <- gcd_slc(clust_combs$long_clust1, 
#   clust_combs$lat_clust1, clust_combs$long_clust2, clust_combs$lat_clust2)

# # clust_combs <- clust_combs %>% filter(clust1 != clust2)
# clust_combs$btw_clust_dist <- round(clust_combs$btw_clust_dist, digits = 6)
# names(clust_combs)[1:2] <- c('tow_clust', 'alt_clust')


# #Filter out duplicated clust_distances; suggesting they identical
# # clust_combs[which(duplicated(clust_combs$btw_clust_dist)), ] %>% head

# #Check that there two values for each coordinate
# # clust_combs %>% group_by(btw_clust_dist) %>% summarize(nvals = length(btw_clust_dist), 
# #   nunq_vals = length(unique(btw_clust_dist))) %>% filter(nvals != 2)

# #remove duplicated values
# # clust_combs <- clust_combs[which(duplicated(clust_combs$btw_clust_dist) == FALSE), ]




# # ##Try this on a subset of the data to see how it goes first
# # only_some <- dist_hauls %>% filter(unq_clust %in% c(19, 92, 6, 34, 13))
# # hauls <- only_some %>% select(haul_id, set_date)

# # haul1 <- only_some$haul_id[1]
# # hauls %>% 



# # do({
# #   start_date <- set_date[1]


# # })


# # dist_hauls %>% group_by(haul_id) %>% do({
# #   dist_hauls %>% filter(set_date <= (date1 + days(ndays)),
# #     set_date >= date1) 
# # })




# # date1 <- dist_hauls$set_date[1]
# # (date1 + days(1))

# # ymd(date1 + days(1))



# #   between(set_date, (date1 + days(1)))) %>% dim

# #   set_date <= (date1 + days(1))) %>% dim

# # dist_hauls %>% filter(set_date < (date1 + days(30))) %>% dim

# # ymd(dist_hauls$set_year, dist_hauls$set_month, dist_hauls$set_day)




# # # clust_combs %>% filter(btw_clust_dist == 153.81047)


# # #---------------------------------------------------------------------------------




# # filt_clusts$d_port_clust_dist <- gcd_slc(deg2rad(filt_clusts$avg_long_clust), 
#   deg2rad(filt_clusts$avg_lat_clust), deg2rad(filt_clusts$d_port_long), 
#   deg2rad(filt_clusts$d_port_lat))

# #Check the net prices
# dat %>% filter(is.na(net_price) == T, type %in% c('targets', "weaks")) %>% distinct(species)


# is.na$net_price

# #Calculate revenues associate with each haul

# #Filter the data to only




# sum(dist_hauls$haul_id %in% hh$haul_id)
# sum(hh$haul_id %in% dist_hauls$haul_id)

# dist_hauls1 %>% group_by(haul_id) %>% 
#   summarize(nclusts = length(unique(alt_clust))) %>% ungroup %>% 
#   distinct(nclusts) %>% as.data.frame

# dist_hauls1_inner <- dist_hauls1 %>% inner_join(hh, by = 'haul_id')
# dist_hauls1_inner %>% filter(haul_id_id == 1) %>% 


# browser()
#Seems like many tow values were dropped somewhere
# apply(rum_dat, MAR = 2, FUN = function(x) unique(x))

#Find unique values
# unqs <- lapply(rum_dat, FUN = function(xx) unique(xx))
# fished_clusts <- unique(rum_dat$tow_clust)
# fished_clusts[order(fished_clusts)]

# #Make sure that revs and dists have the same dimensions
# rev_names <- names(rum_dat)[grep("revs", names(rum_dat))]
# rev_names <- gsub('revs.', "", rev_names)

# rev_keeps <- rev_names[rev_names %in% as.character(fished_clusts)]
# rev_keeps <- paste0(rev_keeps, collapse = "|")
# length(grep(rev_keeps, names(rum_dat)))

# rum_dat <- rum_dat[, c(1, 2, grep(rev_keeps, names(rum_dat) ))]



# as.character(fished_clusts) %in% rev_names
# dist_names <- names(rum_dat)[grep("dist", names(rum_dat))]
# dist_names <- gsub('dist.', "", dist_names)
# the_names <- unique(c(rev_names, dist_names))

# the_names[which(the_names %in% as.character(fished_clusts) == FALSE)]

# the_names[which(the_names %in% as.character(fished_clusts) == FALSE)]

# rm_cols <- the_names[which(the_names %in% as.character(fished_clusts) == FALSE)]
# rm_cols <- paste(rm_cols, collapse = "|")

#Keep columns that are not in rm_cols
# keepers <- names(rum_dat)[which(grepl(rm_cols, names(rum_dat)) == FALSE)]
#keep haul_id_id and tow_clust in there

# rum_dat <- rum_dat[, names(rum_dat)[which(grepl(rm_cols, names(rum_dat)) == FALSE)]]
 
#Check to make sure that all the tow_clusts have values
# names(rum_dat)[-c(1, 2)]
# length(unique(rum_dat$tow_clust))

# rum_dat$

 # names(rum_dat)[which(grepl(rm_cols, names(rum_dat)) == FALSE)]
# as.character(fished_clusts) %in% the_names
  # fished_clusts <- paste0(as.character(unique(rum_dat$tow_clust)), collapse = "|")
  # rm_cols <- which(grepl(fished_clusts, names(rum_dat)) == FALSE)
  # rum_dat <- rum_dat[, -rm_cols[which(rm_cols > 3)]]
  #Remove columns that have are never fished in
  # tow_clust_columns <- names(rum_dat)[grep("revs.", names(rum_dat))]
  # col_values <- as.numeric(gsub("revs.", "", tow_clust_columns))
  
  # rm_cols <- as.character(col_values[which(col_values %in% unique(rum_dat$tow_clust) == FALSE)])
  # rm_cols <- paste0(rm_cols, collapse = "|")
  # rum_dat <- rum_dat[, -grep(rm_cols, names(rum_dat))]

#factor levels are different somewhere
# grepl(paste0(as.character(unique(rum_dat$tow_clust)) , collapse = "|"), names(rum_dat) )
# data_frame(dist = names(rum_dat)[grep("dist", names(rum_dat))], 
#   revs = names(rum_dat)[grep("revs", names(rum_dat))]) %>% as.data.frame
# find_zeros <- lapply(rum_dat, FUN = function(xx) unique(xx))