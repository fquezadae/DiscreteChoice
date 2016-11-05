#'Calculate Bycatch

#'Function to calculate catch correlations between two species

#'@param tows Data frame of tows with species and catches
#'@param output Output from calc_bycatch function
#'@param two_species Two species to calculate correlations

#'@export
#'@examples
#' Put example in


########################################################################################
#Function to plot interaction bycatch
#Pass the output of calc_bycatch, and pass the tows dataframe

#Calculate species correlations

spp_corrs <- function(tows = tows, output, two_species = c('dover sole', 'sablefish')){
  
  #Pull out the sampled tows from each 
  the_tows <- lapply(out$hauls, FUN = function(x) tows %>% filter(haul_id %in% x))
 
  filtered_tows <- lapply(the_tows, function(x) x %>% filter(species %in% two_species) %>% 
                     select(haul_id, apounds, species))

  #cast the tows to plot them
  casted <- lapply(filtered_tows, FUN = function(x) dcast(x, haul_id ~ species, value.var = 'apounds'))
  names(casted) <- 1:length(casted)

  #Replace the NAs with 0
  casted <- ldply(casted)
  names(casted)[1] <- 'replicate'
  casted[is.na(casted[, 3]), 3] <- 0
  casted[is.na(casted[, 4]), 4] <- 0
  
  #Calculate correlations between pairs
  names(casted)[3] <- 'spp1'
  names(casted)[4] <- 'spp2'

  casted %>% group_by(replicate) %>% do({
    mod <- lm(spp2 ~ spp1, data = .)
    r2 <- summary(mod)$r.squared
    data.frame(., r2)
  }) %>% as.data.frame -> corrs

  unq_r2 <- corrs %>% distinct(replicate, r2)

  return(list("pairs" = casted, 'corrs' = unq_r2))
}
