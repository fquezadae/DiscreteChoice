#' Function to calculate catch-quota balancing and revenues

#' Filters the data so that the catches remain below the tac. Catches are allowed to go 
#' over the tac for one species. 

#cut data to be under quota and calculate revenues
#' @param quotas Data frame of the quota amounts for each species
#' @param catches Resampled catches from RUM

#' @export


calc_ctac_rev <- function(quotas, catches){
  # browser()
  temp_catches <- catches %>% arrange(rep, drvid_id, trip_id, tow_index) %>% 
    group_by(rep, species) %>% mutate(cum_catch = cumsum(hpounds)) %>% as.data.frame

  #Compare catches to quotas
  temp_catches <- temp_catches %>% right_join(quotas, by = c('species', 'type'))  

  #Figure out points in each replicate  at which cum_catch goes over tac
  temp_catches$catch_tac <- temp_catches$cum_catch / temp_catches$tac

  #Number tows within each trip
  tows_over <- temp_catches %>% filter(catch_tac >= 1) %>% group_by(rep, drvid_id) %>% 
    summarize(first_trip_tow_over = min(trip_tow_id))
  temp_catches <- temp_catches %>% left_join(tows_over, by = c('rep', 'drvid_id'))

  unders <- temp_catches %>% filter(trip_tow_id <= first_trip_tow_over)

  #Calculate the final catch tac
  final_catch_tac <- unders %>% group_by(rep, drvid, species, type) %>% 
    summarize(catch = sum(hpounds), tac = unique(tac), catch_tac = catch / tac) %>%
    as.data.frame

  #Revenues
  haul_revs <- unders %>% group_by(rep, drvid_id, trip_id, haul_id) %>% 
    summarize(haul_value = unique(haul_value))
  
  #Trip revenues
  trip_revs <- haul_revs %>% group_by(rep, drvid_id, trip_id) %>% 
    summarize(trip_value = sum(haul_value))

  #Annual revenues
  annual_revs <- trip_revs %>% group_by(rep, drvid_id) %>% 
    summarize(annual_value = sum(trip_value))
  
  #And trip revenues, and vessel revenues,
  outs <- list(final_catch_tac = final_catch_tac, haul_revs = haul_revs, 
    trip_revs = trip_revs, annual_revs = annual_revs)
  return(outs)
}
