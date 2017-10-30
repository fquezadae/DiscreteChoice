#' Function to calculate values for delta plots
#' Skew and proportion of zeroes specifically

#' @param data_type Type of data; either tows_clust, or logbook
#' @param when Time period to focus on
#' @param skew Skew of non-zero, logged pound values

#' @export

when_spp_delta <- function(data_type = tows_clust, period, spp){
# browser()  
  tt <- data_type %>% ungroup %>% filter(when == period) 
  all_tows <- tt %>% distinct(haul_id)
  
  spp_catch <- tt %>% filter(species == spp) %>% select(haul_id, apounds)
  the_tows <- all_tows %>% left_join(spp_catch, by = 'haul_id')
  the_tows[which(is.na(the_tows$apounds)), 'apounds'] <- 0
  
  zeroes <- the_tows %>% filter(apounds == 0) %>% nrow 
  prop_zero <- zeroes / nrow(the_tows)
  
  skew <- calc_skew(log(subset(the_tows, apounds != 0)$apounds))
# browser()  
  #Make sure to include species type in the output
  the_type <- unique(subset(tt, species == spp)$type)
  # spp
# browser()
  return(data.frame(species = spp, when = period, prop_zero = prop_zero, skew = skew, 
    type = the_type))
}
