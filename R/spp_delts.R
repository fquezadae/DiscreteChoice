#--------------------------------------------------------------------
#'Function to calculate deltas for species
#' spp_delts
#' @param sppz Species
#' @param managed2 Managed data frame
#' @export

spp_delts <- function(sppz, managed2){
  #Take unique hauls in each period
  bef_hauls <- managed2 %>% filter(when == 'before') %>% select(haul_id)
  aft_hauls <- managed2 %>% filter(when == 'after') %>% select(haul_id)
  spp_hauls <- managed2 %>% filter(species == sppz) %>% select(species, haul_id, apounds, type)
  
  bef_hauls <- bef_hauls %>% left_join(spp_hauls, fill = 0, by = 'haul_id')
  aft_hauls <- aft_hauls %>% left_join(spp_hauls, fill = 0, by = 'haul_id')
  
  bef_propzero <- sum(is.na(bef_hauls$apounds)) / nrow(bef_hauls)
  aft_propzero <- sum(is.na(aft_hauls$apounds)) / nrow(aft_hauls)
  
  bef_hauls <- bef_hauls %>% filter(is.numeric(apounds), apounds != 0) %>% mutate(log_apounds = log(apounds))
  bef_skew <- calc_skew(bef_hauls$log_apounds)
  
  aft_hauls <- aft_hauls %>% filter(is.numeric(apounds), apounds != 0) %>% mutate(log_apounds = log(apounds))
  aft_skew <- calc_skew(aft_hauls$log_apounds)
  
  spp_delts <- data.frame(prop_zero = c(bef_propzero, aft_propzero), skew = c(bef_skew, aft_skew), 
    when = c('before', 'after'), species = sppz, type = unique(spp_hauls$type))
  return(spp_delts)
}
