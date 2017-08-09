#' Sample hauls for one trip

#' Function to sample hauls for one trip

#' @param filt_data Filtered data input into function; This data should not be vessel-specific
#' @param cluster_value Cluster specified to fish in
#' @param num_hauls Number of hauls to sample in each cluster (or rather each clust_bin)
#' @param unq_bin_scope Scope of bins to sample
#' @param quotas Quotas to  track

#' @export

#Input cluster value; number of hauls, scope of bins -- default to 0, quotas
one_trip <- function(filt_data, cluster_value, num_hauls, unq_bin_scope = 0, quotas){
  
  #Allow boat to pick tows from all the clusters in the 10x10km grid
  unq_bin <- filt_data %>% filter(unq_clust == cluster_value) %>% ungroup %>% 
    distinct(unq_clust_bin) %>% as.data.frame 
  hauls <- filt_data %>% filter(unq_bin == unq_bin) %>% distinct(haul_id, .keep_all = T) %>%
    ungroup %>% sample_n(size = num_hauls) %>% distinct(haul_id)

  the_trip <- filt_data %>% filter(haul_id %in% hauls$haul_id) 

  #Summarize catch
  trip_catch <- summarize_catch(clust = the_trip, haul_id1 = hauls$haul_id)
  
  #Compare to quotas
  quotas <- quotas %>% left_join(trip_catch %>% group_by(species) %>% 
    summarize(hpounds = sum(hpounds, na.rm = T)), by = 'species')
  quotas[is.na(quotas$hpounds), 'hpounds'] <- 0
  quotas$catch <- quotas$catch + quotas$hpounds
  quotas$hpounds <- NULL
  
  return(list(trip_catch = trip_catch, quotas = quotas))

}