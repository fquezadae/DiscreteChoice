#' Function to calculate values for delta plots
#' Skew and proportion of zeroes specifically

#' @param year Year to focus on
#' @param skew Skew of non-zero, logged pound values

#' @export


year_spp_delta <- function(year, spp){
  tt <- tows_clust %>% filter(set_year == year) 
  all_tows <- tt %>% distinct(haul_id)
  
  spp_catch <- tt %>% filter(species == spp) %>% select(haul_id, apounds)
  the_tows <- all_tows %>% left_join(spp_catch, by = 'haul_id')
  the_tows[which(is.na(the_tows$apounds)), 'apounds'] <- 0
  
  zeroes <- the_tows %>% filter(apounds == 0) %>% nrow 
  prop_zero <- zeroes / nrow(the_tows)
  
  skew <- calc_skew(log(subset(the_tows, apounds != 0)$apounds))
  
  return(prop_zero, skew)
}
