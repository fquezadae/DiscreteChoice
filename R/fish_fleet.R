#' Function to resample averages for each vessel in a fleet

#' @param fleet_chars  Vessel charcteristics, can filter here if fewere samplees for each
#' vessel desired vess_vals
#' @param rum_res RUM results, formatted as list

#' @export


fish_fleet <- function(fleet_chars = vess_vals, rum_res = the_probs){  
  drvid_trips <- vector('list', length = nrow(vess_vals))

  #Sample the averages for each vessel
  for(ii in 1:nrow(vess_vals)){
  
    #specify number of trips and number of hauls
    all_trips <- lapply(1:vess_vals$avg_ntrips[ii], FUN = function(xx){
      temp <- one_trip(p1s = first_probs, p2s = second_probs, data_of_interest = filt_clusts, 
        nhauls = vess_vals$avg_ntrips[ii])
      return(temp)
    })
    all_trips <- list_to_df(all_trips, ind_name = '', col_ind_name = 'trip_id')
    
    drvid_trips[[ii]] <- all_trips
    print(ii)
  }

  drvid_trips <- list_to_df(drvid_trips, ind_name = "", col_ind_name = "drvid_id")
  
  #Combine the actual drvid values in rather than indices

  #Data frame of drvid values
  drvdrv <- data_frame(drvid = vess_vals$drvid, drvid_id = 1:length(vess_vals$drvid))
  drvdrv$drvid_id <- as.character(drvdrv$drvid_id)

  drvid_trips <- drvid_trips %>% left_join(drvdrv, by = 'drvid_id')

  return(drvid_trips)
}


