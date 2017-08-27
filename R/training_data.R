#' Sample dataset for generating predictions

#' Function calls mlogit

#' @param data_in Data going in to the function; default is filt_clusts
##' @param trip_dists Distances covered by each trip
#' @param the_port Port of focus; Default is Astoria
#' @param min_year Minimum year used to filter the data
#' @param max_year Maximum year used to filter the data, also RUM data is filtered to be from the max_year
#' @param risk_coefficient Coefficient to adjust the quota prices up or down, feeding into net revenue calculations
#' @param ndays Number of previous days data to use in revenue expectations
#' @param focus_year Year to focus on for the models
#' @param nhauls_sampled Number of hauls to sample from the full data set
#' @param seed Seed for sampling tows

#' @export

#Add argument for focus_year
# focus_year <- 2012
# nhauls_sampled <- 50


training_data <- function(data_in = filt_clusts, the_port = "ASTORIA / WARRENTON",
  min_year = 2011, max_year = 2012, risk_coefficient = 1,
  ndays = 60, focus_year = 2012, nhauls_sampled = 50, seed = 301){

#Start by sampling 50 tows within the same fleet  
#Figure out how close the different clusters are
browser()
  
  ##Filter the data
  dat <- data_in %>% filter(dport_desc == the_port, set_year >= min_year,
    set_year <= max_year)
  
  #---------------------------------------------------------------
  #Calculate haul revenues
  #Turn NA prices into zeroes
  dat[which(is.na(dat$avg_quota_price)), 'avg_quota_price'] <- 0
  
  #Adjust the net prices based on risk coefficient
  dat$rc <- risk_coefficient

  # print("only weak stock species adjusted for risk")  

  dat$net_price <- dat$exval_pound
  weak_inds <- which(dat$type == 'weaks')
  dat[weak_inds, 'net_price'] <- dat$exval_pound[weak_inds] - dat$rc[weak_inds] * 
    dat$avg_quota_price[weak_inds]

  # dat$net_price <- (dat$exval_pound - dat$rc * dat$avg_quota_price)
  dat$net_revenue <- dat$net_price * dat$hpounds

  #Sum the haul revenues
  dat <- dat %>% group_by(haul_id) %>% 
    mutate(haul_net_revenue = sum(net_revenue, na.rm = T))
  
  #Create data set, for each tow
  dist_hauls <- dat %>% distinct(haul_id, .keep_all = T) %>% select(haul_id, unq_clust, set_month, 
    drvid, trip_id, set_day, set_year, haul_net_revenue, avg_long_clust, avg_lat_clust,
    haul_num, avg_long, avg_lat) %>% as.data.frame

  #Look at net revenues by cluster to see if they differ much
  # dist_hauls %>% filter(set_year == 2011) %>% ggplot() + geom_histogram(aes(x = haul_net_revenue)) + 
  #   facet_wrap(~ unq_clust,) + geom_vline(xintercept = 0)
  
  set.seed(seed)
  
  #For each tow in the focus year, sample other tows
  #Hauls in focus year
  hauls <- dist_hauls %>% filter(set_year == focus_year) %>% arrange(trip_id, haul_num)
  hauls$prev_haul_num <- hauls$haul_num - 1
  
  #Data frame of previous haul locations
  prev_hauls <- hauls %>% select(trip_id, haul_num, avg_long, avg_lat)

  #add in zero haul_num values
  zero_hauls <- prev_hauls %>% distinct(trip_id)  
  zero_hauls$haul_num <- 0
  zero_hauls$avg_long <- unique(dat$d_port_long)
  zero_hauls$avg_lat <- unique(dat$d_port_lat)
  
  #Add into previous hauls data frame
  prev_hauls <- rbind(prev_hauls, zero_hauls) %>% arrange(trip_id, haul_num)
  names(prev_hauls)[2:4] <- c('prev_haul_num', "prev_avg_long", 'prev_avg_lat')
  
  #Add this into the hauls data frame
  hauls <- hauls %>% left_join(prev_hauls, by = c('trip_id', 'prev_haul_num'))
  
  #-----------------------------------------------------------------------------
  #Sample hauls and calculate distances
  #For each haul in the focus year, sample nhauls_sampled tows
  
  sampled_hauls <- lapply(1:nrow(hauls), FUN = function(xx){
    the_samples <- dist_hauls %>% filter(haul_id != hauls[xx, 'haul_id']) %>% 
      sample_n(size = nhauls_sampled, replace = F)
  
    #Now calculate the distances between the points and the actual points
    actual_haul <- hauls[xx, ]
  
    #calculate distances in km
    prev_point <- actual_haul %>% select(prev_avg_long, prev_avg_lat)
    the_samples$prev_avg_long <- prev_point$prev_avg_long
    the_samples$prev_avg_lat <- prev_point$prev_avg_lat
    the_samples[, c('avg_long', 'avg_lat', 'prev_avg_lat', 'prev_avg_long')] <- deg2rad(the_samples[, 
      c('avg_long', 'avg_lat', 'prev_avg_lat', 'prev_avg_long')])
  
    the_samples$distance <- gcd_slc(the_samples$prev_avg_long, 
        the_samples$prev_avg_lat, the_samples$avg_long, the_samples$avg_lat)
  
    #Calculate distance of empirical haul
    actual_haul[, c('avg_long', 'avg_lat', 'prev_avg_lat', 'prev_avg_long')] <- deg2rad(actual_haul[, 
        c('avg_long', 'avg_lat', 'prev_avg_lat', 'prev_avg_long')])  
    actual_haul$distance <- gcd_slc(actual_haul$prev_avg_long, 
        actual_haul$prev_avg_lat, actual_haul$avg_long, actual_haul$avg_lat)
  
    #Combine the sampled values and the empirical haul
    actual_haul$prev_haul_num <- NULL
    actual_haul$fished <- TRUE
    actual_haul$fished_haul <- actual_haul$haul_id
    the_samples$fished <- FALSE
    the_samples$fished_haul <- actual_haul$haul_id
  
    the_samples <- rbind(actual_haul, the_samples)
  
    #Define the set_date
    the_samples$set_date <- ymd(paste(actual_haul$set_year, actual_haul$set_month, actual_haul$set_day, sep = "-"))
    
    return(the_samples)
  })
  
  sampled_hauls <- ldply(sampled_hauls)

#Find tows that have NA values for the distances

sampled_hauls[which(is.na(sampled_hauls$distance)), ] %>% head
sum(is.na(sampled_hauls$distance) )
  
  #-----------------------------------------------------------------------------
  #Calculate revenues from each period
  sampled_hauls$prev_days_date <- sampled_hauls$set_date - days(ndays)
  sampled_hauls$prev_year_set_date <- sampled_hauls$set_date - days(365)
  sampled_hauls$prev_year_days_date <- sampled_hauls$prev_days_date - days(365)
  
  #What were the average revenues in each location
  tow_dates <- sampled_hauls %>% 
    select(unq_clust, set_date, prev_days_date, prev_year_set_date, prev_year_days_date)
  
  #Look at the unique dates and clusters only
  # td1 <- tow_dates %>% distinct(unq_clust, set_date)
  tow_dates$days_inter <- interval(tow_dates$prev_days_date, tow_dates$set_date)
  tow_dates$prev_year_days_inter <- interval(tow_dates$prev_year_days_date, tow_dates$prev_year_set_date)
  
  td1 <- tow_dates %>% distinct(unq_clust, set_date, .keep_all = T)
  
  dummys <- lapply(1:nrow(td1), FUN = function(xx){
    temp_dat <- td1[xx, ]  
    
    clust_dat <- dat %>% filter(unq_clust == temp_dat$unq_clust)
  
    #If towed in the previous ndays 
    towed_prev_days <- sum(clust_dat$set_date %within% temp_dat$days_inter)
    towed_prev_days_rev <- 0
    if(towed_prev_days != 0){
      hauls_in_period <- clust_dat %>% filter(set_date %within% temp_dat$days_inter) %>% 
        distinct(haul_id, .keep_all = T) 
      towed_prev_days_rev <- mean(hauls_in_period$haul_net_revenue )
    }
  
    #If towed in the previous year's ndays 
    towed_prev_year_days <- sum(clust_dat$set_date %within% temp_dat$prev_year_days_inter)
    towed_prev_year_days_rev <- 0
    if(towed_prev_year_days != 0){
      hauls_in_period <- clust_dat %>% filter(set_date %within% temp_dat$prev_year_days_inter) %>% 
        distinct(haul_id, .keep_all = T) 
      towed_prev_year_days_rev <- mean(hauls_in_period$haul_net_revenue )
    }
  
    outs <- data_frame(dummy_prev_days = towed_prev_days, prev_days_rev = towed_prev_days_rev,
      dummy_prev_year_days = towed_prev_year_days, prev_year_days_rev = towed_prev_year_days_rev)
  
    return(outs)
  })
  
  dummys1 <- ldply(dummys)
  
  #Change values to be 0 and 1 for dummy variables
  # tow_dates <- cbind(tow_dates, dummys1)
  td1 <- cbind(td1, dummys1)
  
  #converte set_date to character string to merge with the sampled_hauls
  tow_dates$set_date_chr <- as.character(tow_dates$set_date)
  td1$set_date_chr <- as.character(td1$set_date)
  
  #Add in charcater set_date for sampled_hauls
  sampled_hauls$set_date_chr <- as.character(sampled_hauls$set_date)
  td1 <- td1 %>% select(unq_clust, set_date_chr, dummy_prev_days, prev_days_rev, dummy_prev_year_days, 
    prev_year_days_rev) 
  
  sampled_hauls <- sampled_hauls %>% left_join(td1, by = c("unq_clust", "set_date_chr"))
  
  #Make sure that missing values have dummy variable value of 1 for prev_days
  sampled_hauls[which(sampled_hauls$dummy_prev_days != 0), 'dummy_prev_days'] <- 0
  sampled_hauls[which(sampled_hauls$prev_days_rev == 0), "dummy_prev_days"] <- 1
  sampled_hauls[which(sampled_hauls$dummy_prev_year_days != 0), 'dummy_prev_year_days'] <- 1
  
  #-----------------------------------------------------------------------------
  #Format as mlogit.data
  rdo <- sampled_hauls %>% select(haul_id, unq_clust, haul_num, distance, fished, fished_haul, 
    dummy_prev_days, prev_days_rev, dummy_prev_year_days, prev_year_days_rev)
  
  rdo <- rdo %>% group_by(fished_haul) %>% mutate(alt_tow = 1:length(haul_id)) %>% as.data.frame 
  
  #Filter out tows with missing values for distance
  rdo <- rdo %>% filter(is.na(distance) == FALSE)

  #-----------------------------------------------------------------------------
  #Fit mlogit models returning the coefficients, the models, and the data going into the 
  # models
  
  #Split first tows and later tows
  tows1 <- rdo %>% filter(fished == TRUE, haul_num == 1) %>% select(fished_haul)
  first_tows <- rdo %>% filter(fished_haul %in% tows1$fished_haul)
  
  first_tows <- mlogit.data(first_tows, shape = 'long', choice = 'fished', alt.var = 'alt_tow',
    chid.var = 'fished_haul')
  
  # res1 <- mlogit(fished ~ distance + prev_days_rev + dummy_prev_days - 1, first_tows)
  # res1 <- mlogit(fished ~ distance + prev_days_rev + dummy_prev_days + dummy_prev_year_days +
  #  prev_year_days_rev - 1, first_tows)
  
  #Second tows
  second_tows <- rdo[which(rdo$fished_haul %in% tows1$fished_haul == FALSE), ]
  second_tows <- mlogit.data(second_tows, shape = 'long', choice = 'fished', alt.var = 'alt_tow',
    chid.var = 'fished_haul')
  
  # res2 <- mlogit(fished ~ distance + prev_days_rev + dummy_prev_days - 1, second_tows)
  # res2 <- mlogit(fished ~ distance + prev_days_rev + dummy_prev_days + dummy_prev_year_days +
  #   prev_year_days_rev - 1, second_tows)
  
  outs <- list(first_tows = first_tows, second_tows = second_tows)
  
  return(outs)

}




