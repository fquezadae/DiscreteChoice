#'Summarize catch

#' Function to take haul_id, and count the things that came out
#' @param input Data as input; defaults to filt_clusts
#' @param haul_id Haul_id value

#' @export

summarize_catch <- function(clust = filt_clusts, haul_id1){

  #Pull out the specific tow
  tow <- clust %>% filter(haul_id == haul_id1)
  
  catch <- tow %>% group_by(species, type) %>% summarize(hpounds = sum(hpounds, na.rm = T),
    haul_value = unique(haul_value), haul_profit = unique(haul_profit), 
    profit_fuel_only = unique(profit_fuel_only), unq_clust_bin = unique(unq_clust_bin), 
    xbin = unique(xbin), ybin = unique(ybin), unq = unique(unq),
    haul_id = unique(haul_id), unq_clust = unique(unq_clust),
    avg_long = unique(avg_long), avg_lat = unique(avg_lat)) %>% as.data.frame

  return(catch)

}

