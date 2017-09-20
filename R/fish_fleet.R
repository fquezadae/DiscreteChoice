#' Function to resample averages for each vessel in a fleet

#' @param fleet_chars  Vessel charcteristics, can filter here if fewere samplees for each
#' vessel desired vess_vals
#' @param rum_res RUM results, formatted as list

#' @export


fish_fleet <- function(fleet_chars = vess_vals, rum_res = the_probs, seed, the_dat){  

  set.seed(seed)
  drvid_trips <- vector('list', length = nrow(fleet_chars))

  #Sample the averages for each vessel
  for(ii in 1:nrow(fleet_chars)){
    #specify number of trips and number of hauls
    all_trips <- lapply(1:fleet_chars$avg_ntrips[ii], FUN = function(xx){
      temp <- one_trip(the_models = rum_res, data_of_interest = the_dat, 
        nhauls = fleet_chars$avg_ntrips[ii])
      # temp <- one_trip(p1s = rum_res[[1]], p2s = rum_res[[2]], data_of_interest = the_dat, 
      #   nhauls = fleet_chars$avg_ntrips[ii], mod2 = rum_res[[4]])
      return(temp)
    })
    all_trips <- list_to_df(all_trips, ind_name = '', col_ind_name = 'trip_id')
    
    drvid_trips[[ii]] <- all_trips
  }

  drvid_trips <- list_to_df(drvid_trips, ind_name = "", col_ind_name = "drvid_id")
  
  #Combine the actual drvid values in rather than indices
  #Data frame of drvid values
  drvdrv <- data_frame(drvid = fleet_chars$drvid, drvid_id = 1:length(fleet_chars$drvid))
  drvdrv$drvid_id <- as.character(drvdrv$drvid_id)

  drvid_trips <- drvid_trips %>% left_join(drvdrv, by = 'drvid_id')

  return(drvid_trips)
}


