#'Summarize catch

#' Function to take haul_id, and count the things that came out
#' @param input Data as input; defaults to filt_clusts
#' @param haul_id_vec Vector of haul_id values

#' @export

summarize_catch <- function(clust = filt_clusts, sampled_hauls1, rc = 1){  
  

  #Adjust haul_values based on the risk coefficient, which adjusts net prices
  clust$rc <- rc

  # print("only weak stock species adjusted for risk")  

  clust$net_price <- clust$exval_pound
  weak_inds <- which(clust$type == 'weaks')
  clust[weak_inds, 'net_price'] <- clust$exval_pound[weak_inds] - clust$rc[weak_inds] * 
    clust$avg_quota_price[weak_inds]

  # dat$net_price <- (dat$exval_pound - dat$rc * dat$avg_quota_price)
  clust$net_revenue <- clust$net_price * clust$hpounds

  #Sum the haul revenues
  clust <- clust %>% group_by(haul_id) %>% 
    mutate(haul_net_revenue = sum(net_revenue, na.rm = T))
  
  #Filter and process the catfch data
  catch <- clust %>% filter(haul_id %in% sampled_hauls1$haul_id) %>% group_by(haul_id, species, type) %>% 
    summarize(hpounds = sum(hpounds, na.rm = T), haul_net_revenue = unique(haul_net_revenue),
      unq_clust_bin = unique(unq_clust_bin),
      xbin = unique(xbin), ybin = unique(ybin), unq = unique(unq), 
      unq_clust = unique(unq_clust), avg_long = unique(avg_long), 
      avg_lat = unique(avg_lat)) %>% as.data.frame

  catch <- catch %>% left_join(sampled_hauls1, by = c("unq_clust", "haul_id"))
  return(catch)

   # group_by(haul_id, species, type) %>% 
   #  summarize(hpounds = sum(hpounds, na.rm = T),
   #  haul_net_revenue = unique(haul_value), , unq_clust_bin = unique(unq_clust_bin),
   #  xbin = unique(xbin), ybin = unique(ybin), unq = unique(unq), 
   #  unq_clust = unique(unq_clust), avg_long = unique(avg_long), 
   #  avg_lat = unique(avg_lat)) %>% as.data.frame

  # haul_ids <- data.frame(haul_id = haul_id1, tow_num = 1:length(haul_id1))
  # haul_ids$haul_id <- as.character(haul_ids$haul_id)

  # catch <- catch %>% left_join(haul_ids, by = 'haul_id')
  # catch <- catch %>% arrange(tow_num)

  # catch <- tow %>% group_by(species, type) %>% summarize(hpounds = sum(hpounds, na.rm = T),
  #   haul_value = unique(haul_value), haul_profit = unique(haul_profit), 
  #   profit_fuel_only = unique(profit_fuel_only), unq_clust_bin = unique(unq_clust_bin), 
  #   xbin = unique(xbin), ybin = unique(ybin), unq = unique(unq),
  #   haul_id = unique(haul_id), unq_clust = unique(unq_clust),
  #   avg_long = unique(avg_long), avg_lat = unique(avg_lat)) %>% as.data.frame
}

