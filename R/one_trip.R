#' Function to conduct one trip
#' One Trip Function

#' @param p1s Probabilities of fishing in clusters for first tow. From RUM
#' @param p2s Probabilities of fishing in clusters not-first tows. From RUM
#' @param data_of_interest Data used to sample tows from. Defaults to filt_clusts but could 
#' be data specific to a vessel
#' @param nhauls Number of hauls on specific trip. Can be specified in vess_vals$hauls_per_trip[1]

#' @export

one_trip <- function(p1s, p2s, data_of_interest = filt_clusts, nhauls){
  #Sample clusters for the tows
  first_tow <- sample_n(p1s, size = 1, weight = p1s$probs, replace = T)

  #May need to scope by distance
  #Could scope this by distance somehow I think
  other_tows <- sample_n(p2s, size = vess_vals$hauls_per_trip[1] - 1, 
    weight = second_probs$probs, replace = T)

  trip_clusts <- c(first_tow$unq_clust, other_tows$unq_clust)

  ntows_in_each_clust <- table(trip_clusts)

  the_tows <- lapply(1:length(ntows_in_each_clust), FUN = function(xx){
    temp_hauls <- data_of_interest %>% filter(unq_clust == as.numeric(names(ntows_in_each_clust)[xx])) %>%
      distinct(haul_id) %>% sample_n(size = ntows_in_each_clust[xx], replace = T)
    return(data.frame(unq_clust = as.numeric(names(ntows_in_each_clust)[xx]), 
      haul_id = temp_hauls, index = 1:ntows_in_each_clust[xx]))
  })

  the_tows <- ldply(the_tows)
  the_tows <- plyr::rename(the_tows, c("index" = 'inclust_index'))

  #Add in the hauls
  trip_clusts <- data_frame(unq_clust = trip_clusts, tow_index = 1:length(trip_clusts))
  trip_clusts <- trip_clusts %>% group_by(unq_clust) %>% mutate(inclust_index = 1:n())
  trip_clusts <- trip_clusts %>% left_join(the_tows, by = c('unq_clust', "inclust_index"))

  catches <- data_of_interest %>% filter(haul_id %in% trip_clusts$haul_id)

  #What are the columns to save
  catch <- catches %>% group_by(haul_id, species, type) %>% summarize(hpounds = sum(hpounds, na.rm = T),
      haul_value = unique(haul_value),  haul_profit = unique(haul_profit),
      profit_fuel_only = unique(profit_fuel_only), unq_clust_bin = unique(unq_clust_bin),
      xbin = unique(xbin), ybin = unique(ybin), unq = unique(unq), 
      unq_clust = unique(unq_clust), avg_long = unique(avg_long), 
      avg_lat = unique(avg_lat)) %>% as.data.frame
  trip_clusts <- ungroup(trip_clusts)
  catch <- catch %>% left_join(trip_clusts %>% select(haul_id, tow_index), by = 'haul_id') 
  catch <- catch %>% arrange(tow_index)

  return(catch)
}
