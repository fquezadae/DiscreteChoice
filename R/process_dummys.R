#' Process dummy variables
#' Function to calculate revenues and distances

#' @param xx Index value
#' @export

process_dummys <- function(xx){
  temp_dat <- td1[xx, ]

  #Filter based on unq_bin rather than cluster, I may be missing data by using clusters    
  
  clust_dat <- dat %>% filter(unq_clust >= temp_dat$unq_clust - 5, 
                              unq_clust <= temp_dat$unq_clust + 5) %>% 
        distinct(haul_id, .keep_all = T) %>%
        filter(set_date <= temp_dat$set_date)
  
  #Convert degrees to radians
  clust_dat$avg_long <- deg2rad(clust_dat$avg_long)
  clust_dat$avg_lat <- deg2rad(clust_dat$avg_lat)

  #Calculate distances
  clust_dat$dist_from_samp_tow <- gcd_slc(temp_dat$avg_long, temp_dat$avg_lat,
    clust_dat$avg_long, clust_dat$avg_lat)

  #Remove points that are greater than 5 km away
  clust_dat <- clust_dat %>% filter(dist_from_samp_tow <= 5)
    
  #Filter based on the depths also, hard coded to be within 50fm range
  clust_dat <- clust_dat %>% filter(avg_depth >= temp_dat$avg_depth - 25,
    avg_depth <= temp_dat$avg_depth + 25)

  #If towed in the previous ndays 
  towed_prev_days <- sum(clust_dat$set_date %within% temp_dat$days_inter)
  towed_prev_days_rev <- 0
  if(towed_prev_days != 0){
    hauls_in_period <- clust_dat %>% filter(set_date %within% temp_dat$days_inter) %>% 
      distinct(haul_id, .keep_all = T) 
    towed_prev_days_rev <- mean(hauls_in_period$haul_net_revenue, na.rm = TRUE)
  }

  #If towed in the previous year's ndays 
  towed_prev_year_days <- sum(clust_dat$set_date %within% temp_dat$prev_year_days_inter)
  towed_prev_year_days_rev <- 0
  if(towed_prev_year_days != 0){
    hauls_in_period <- clust_dat %>% filter(set_date %within% temp_dat$prev_year_days_inter) %>% 
      distinct(haul_id, .keep_all = T) 
    towed_prev_year_days_rev <- mean(hauls_in_period$haul_net_revenue, na.rm = TRUE)
  }
  
  outs <- data_frame(dummy_prev_days = towed_prev_days, prev_days_rev = towed_prev_days_rev,
    dummy_prev_year_days = towed_prev_year_days, prev_year_days_rev = towed_prev_year_days_rev)

  return(outs)

}
  


  # }, mc.cores = ncores)
