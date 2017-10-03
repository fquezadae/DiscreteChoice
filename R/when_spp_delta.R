#' Function to calculate values for delta plots
#' Skew and proportion of zeroes specifically

#' @param when Time period to focus on
#' @param skew Skew of non-zero, logged pound values

#' @export

when_spp_delta <- function(when, spp){
# browser()  
  tt <- tows_clust %>% filter(when == when) 
  all_tows <- tt %>% distinct(haul_id)
  
  spp_catch <- tt %>% filter(species == spp) %>% select(haul_id, apounds)
  the_tows <- all_tows %>% left_join(spp_catch, by = 'haul_id')
  the_tows[which(is.na(the_tows$apounds)), 'apounds'] <- 0
  
  zeroes <- the_tows %>% filter(apounds == 0) %>% nrow 
  prop_zero <- zeroes / nrow(the_tows)
  
  skew <- calc_skew(log(subset(the_tows, apounds != 0)$apounds))
  
  #Make sure to include species type in the output
  the_type <- unique(subset(tt, species == spp)$type)
  when
# browser()
  return(data.frame(species = spp, when = when, prop_zero = prop_zero, skew = skew, 
    type = the_type))
}
