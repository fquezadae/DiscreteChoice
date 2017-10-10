#' Process dummy variables
#' Function to calculate revenues and distances

#' @param xx Index value
#' @param td2 The tow dates
#' @export

process_dummys2 <- function(xx, td2 = td1, dat1 = dat){
  temp_dat <- td2[xx, ]
# browser()
habit_distance <- 8
rev_distance <- 5  
  #-----------------------------------------------------------------------------------------------
  #Did vessel fish in past 30 days?
  dum30 <- dat1 %>% filter(haul_id != temp_dat$haul_id, set_date %within% temp_dat$days_inter,
    depth_bin == temp_dat$depth_bin, drvid == temp_dat$fished_drvid, 
    fleet_name == temp_dat$fleet_name)
  dum30 <- dum30 %>% distinct(haul_id, .keep_all = T)
  
  #calculate distances
  dum30$dist <- gcd_slc(long1 = temp_dat$set_long, lat1 = temp_dat$set_lat,
    long2 = deg2rad(dum30$set_long), deg2rad(dum30$set_lat))
  dum30 <- dum30 %>% filter(dist <= habit_distance)
  
  #Add dummy coefficient
  dum30_val <- nrow(dum30)
  # dum30_val <- 0
  # if(nrow(dum30) > 0) dum30_val <- 1

  #-----------------------------------------------------------------------------------------------
  # Vessel fish in the past 30 days of last year?
  dum30y <- dat1 %>% filter(haul_id != temp_dat$haul_id, set_date %within% temp_dat$prev_year_days_inter,
    depth_bin == temp_dat$depth_bin, drvid == temp_dat$fished_drvid, 
    fleet_name == temp_dat$fleet_name)
  dum30y <- dum30y %>% distinct(haul_id, .keep_all = T)
  
  #calculate distances
  dum30y$dist <- gcd_slc(long1 = temp_dat$set_long, lat1 = temp_dat$set_lat,
    long2 = deg2rad(dum30y$set_long), deg2rad(dum30y$set_lat))
  dum30y <- dum30y %>% filter(dist <= habit_distance)
  
  #Add dummy coefficient
  dum30y_val <- nrow(dum30y)
  # if(nrow(dum30y) > 0) dum30y_val <- 1
  
  #-----------------------------------------------------------------------------------------------
  #Calculate the revenues within a finer radius and from the whole fleet, rather than individual vessel
  dum_rev <- dat1 %>% filter(haul_id != temp_dat$haul_id, set_date %within% temp_dat$days_inter,
    depth_bin == temp_dat$depth_bin, fleet_name == temp_dat$fleet_name)
  dum_rev <- dum_rev %>% distinct(haul_id, .keep_all = T)

  #Calculate distance
  dum_rev$dist <- gcd_slc(long1 = temp_dat$set_long, lat1 = temp_dat$set_lat,
    long2 = deg2rad(dum_rev$set_long), lat2 = deg2rad(dum_rev$set_lat))
  dum_rev <- dum_rev %>% filter(dist <= rev_distance)

# browser()

  #Calculate revenue in a faster way with fewer if statements
  dum_rev_val <- nrow(dum_rev)

#####Here control whether you use value of all species or just target and groundfish species
  #Right now including all species "tgow_rev"
  # dum_rev[is.na(dum_rev$weak_quota_value), 'weak_quota_value'] <- 0
  mean_rev <- mean(dum_rev$tgow_rev)
  mean_rev <- replace(mean_rev, is.na(mean_rev), 0)
  mean_weak <- mean(dum_rev$weak_quota_value, na.rm = T)
  mean_weak <- replace(mean_weak, is.na(mean_weak), 0)
  
  dum_rev_dollars <- mean_rev - mean_weak
  
  # if(nrow(dum_rev) > 0){
  #   dum_rev_val <- 1
  #   #Split weaks and non weaks
  #   the_revs <- dat1 %>% filter(haul_id %in% dum_rev$haul_id)
    
  #   managed <- the_revs %>% filter(type != 'other') %>% group_by(haul_id) %>% 
  #     summarize(gross_rev = sum(gross_rev, na.rm = T)) %>% mutate(avg_rev = mean(gross_rev))
  #   managed_val <- unique(managed$avg_rev)
    
  #   weaks <- the_revs %>% filter(type == 'weaks') %>% group_by(haul_id) %>% 
  #     summarize(quota_val = sum(quota_val, na.rm = T)) %>% mutate(avg_quota_val = mean(quota_val))
    
  #   if(nrow(weaks) == 0) weak_val <- 0
  #   if(nrow(weaks) > 0) weak_val <- unique(weaks$avg_quota_val)
    
  #   #Calculate net revenue value
  #   dum_rev_dollars <- managed_val - weak_val
  # }

  temp_dat$dummy_prev_days <- dum30_val
  temp_dat$dummy_prev_year_days <- dum30y_val
  temp_dat$dummy_miss <- dum_rev_val
  temp_dat$miss_rev <- dum_rev_dollars
  return(temp_dat)
  # outs <- data_frame(dummy_prev_days = dum30_val, dummy_prev_year_days = dum30y_val, 
  #   dummy_miss = dum_rev_val, miss_rev = dum_rev_dollars)

  #-----------------------------------------------------------------------------------------------
  # return(outs)

}
  


  # }, mc.cores = ncores)
