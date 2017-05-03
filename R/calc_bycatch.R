#'Calculate Bycatch

#'Function to calculate bycatch from sampled tows

#'@param nreps Number of replicates
#'@param nsamps Number of samples in each replicate
#'@param tac Total allowable catch value
#'@param seed Seed value
#'@param of_interest Data frame of interest, filtered by target species or location possibly

#'@export
#'@examples
#' Put example in

########################################################################################
#Function to Calculate Bycatch
#of_interest is the data frame that is filtered to be only one species, maybe in a specific
#region.

calc_bycatch <- function(nreps = 5000, nsamps = 50, tac = 50000, seed = 300,
  of_interest){
# browser() 
  # of_interest <- tows %>% filter(species == target_species) %>% arrange(desc(apounds))

  samps <- sample_tows(nreps = nreps, nsamps = nsamps, tac = tac, seed = seed)

  #Pull out sampled rows and look at bycatch
  ind_rows <- lapply(samps[[3]], FUN = function(x){
    hauls <- of_interest[x, 'haul_id']
  })

  ########---------------------------------------########
  #Do two things with the sampled rows, right now only focusing on apounds
  #1. Save the bycatch associated with each sample
    #Only save the target and constraining species
  #2. Save the coordinates of each tow
  ########---------------------------------------########
  
  #1 Aggregated Bycatch
  agg_byc <- lapply(ind_rows, FUN = function(x){
               byc <- tows[tows$haul_id %in% x,]
               agg_byc_t <- byc %>% filter(category == 'targets' | category == 'constraining') %>% 
                 group_by(species) %>% summarize(tot_apounds = sum(apounds, na.rm = TRUE)) %>% 
                 arrange(desc(tot_apounds)) %>% as.data.frame
               return(agg_byc_t)
               }
             )

  #2 Haul Locations From West Coast data
  locs <- lapply(ind_rows, FUN = function(x){
    ll <- wc_data[wc_data$haul_id %in% x, c('lat', 'long')]
  })

  return(list('agg_byc' = agg_byc , 'locs' = locs, 'hauls' = ind_rows))
}