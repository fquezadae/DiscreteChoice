#' Process dummy variables
#' Function to calculate revenues and distances

#' @param xx Index value
#' @param td2 The tow dates
#' @export

process_dummys <- function(xx, td2 = td1, dat1 = dat){
  temp_dat <- td2[xx, ]
  # browser()
  #Filter based on unq_bin rather than cluster, I may be missing data by using clusters    
  clust_dat <- dat1 %>% filter(unq_clust >= temp_dat$unq_clust - 5, 
                              unq_clust <= temp_dat$unq_clust + 5) %>% 
        distinct(haul_id, .keep_all = T) %>%
        filter(set_date <= temp_dat$set_date)
  
  #Convert degrees to radians
  # clust_dat$avg_long <- deg2rad(clust_dat$avg_long)
  # clust_dat$avg_lat <- deg2rad(clust_dat$avg_lat)

  #Use set points instead, to see if they set in that area recently
  clust_dat$set_long <- deg2rad(clust_dat$set_long)
  clust_dat$set_lat <- deg2rad(clust_dat$set_lat)

#Change this to set points and end points...?
  #Calculate distances
  clust_dat$dist_from_samp_tow <- gcd_slc(temp_dat$set_long, temp_dat$set_lat,
    clust_dat$set_long, clust_dat$set_lat)

  #------------------------------------------------------------
  ######Add Dummys for points within 5 miles (8.05 km)
  #These are the dum30 and dum30y coefficients
  clust_dat <- clust_dat %>% filter(dist_from_samp_tow <= 8.05, 
    depth_bin == temp_dat$depth_bin)

  #Did this vessel fish here within the past 30 days
  towed_prev_days <- sum(clust_dat$set_date %within% temp_dat$days_inter & 
    clust_dat$drvid ==  temp_dat$drvid)

  #Did this vessel fish here in the previous 30 days of last year?
  towed_prev_year_days <- sum(clust_dat$set_date %within% temp_dat$prev_year_days_inter &
    clust_dat$drvid == temp_dat$drvid)

  #------------------------------------------------------------
  #Now filter the data to calculate revenues and dumMissing

##Add depth bin here too
##Make sure this can be from the entire fleet
  #Remove points that are greater than 5 km away
  clust_dat <- clust_dat %>% filter(dist_from_samp_tow <= 5)
    
  #Filter based on the depths also, hard coded to be within 50fm range
  # clust_dat <- clust_dat %>% filter(avg_depth >= temp_dat$avg_depth - 25,
  #   avg_depth <= temp_dat$avg_depth + 25)

  #If towed in the previous ndays 
  towed_miss <- sum(clust_dat$set_date %within% temp_dat$days_inter)
  towed_miss_rev <- 0
  if(towed_miss != 0){
    hauls_in_period <- clust_dat %>% filter(set_date %within% temp_dat$days_inter) %>% 
      distinct(haul_id, .keep_all = T) 
    towed_miss_rev <- mean(hauls_in_period$haul_net_revenue, na.rm = TRUE)
  }

  #If towed in the previous year's ndays 
  # towed_prev_year_days <- sum(clust_dat$set_date %within% temp_dat$prev_year_days_inter)
  # towed_prev_year_days_rev <- 0
  # if(towed_prev_year_days != 0){
  #   hauls_in_period <- clust_dat %>% filter(set_date %within% temp_dat$prev_year_days_inter) %>% 
  #     distinct(haul_id, .keep_all = T) 
  #   towed_prev_year_days_rev <- mean(hauls_in_period$haul_net_revenue, na.rm = TRUE)
  # }
  
  outs <- data_frame(dummy_prev_days = towed_prev_days, dummy_prev_year_days = towed_prev_year_days,
    dummy_miss = towed_miss, miss_rev = towed_miss_rev)

  #  prev_days_rev = towed_prev_days_rev,
  # , prev_year_days_rev = towed_prev_year_days_rev)

  return(outs)

}
  


  # }, mc.cores = ncores)
