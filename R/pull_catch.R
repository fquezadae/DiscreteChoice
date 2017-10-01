#' Find catch from unique tows

#' Function to count number of tows in bins. Uses ggplot stat_bin2d functions.

#' @param data Input data, defaults to wc_data
#' @param spp Species of interest
#' @param focus Column of interest, Options are hpounds, apounds

#' @export
#' @examples
#' pull_catch(data = wc_data, spp = 'Dover Sole', focus = 'apound')

#' compare_two_spp(data = wc_data, species1 = "Dover Sole", species2 = "Sablefish", focus = 'hperc')

pull_catch <- function(data, spp, focus = 'apound'){
  data$species <- tolower(data$species) #just make sure that everything is lowercase

  unq_hauls <- data %>% select(haul_id) %>% distinct
 
  to_add <- data %>% filter(species == spp) %>% select(haul_id, drvid,
    set_lat, set_long, up_lat, up_long, avg_depth, hpounds, apounds, set_day,
    set_month, set_year)
  focus_column <- grep(focus, names(to_add))
  for_merge <- to_add[, c(1, focus_column)]

  unq_hauls <- left_join(unq_hauls, for_merge, by = 'haul_id')
  # unq_hauls[, 2] <- na_to_zero(unq_hauls[, 2])
  unq_hauls[is.na(unq_hauls[, 2]), 2] <- 0

  return(unq_hauls)
}
