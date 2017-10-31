#' Function to calculate predictions and AIC values
#' input a port value
#' 
#' @param the_port Port name
#' @param the_seed Seed value for rmultinom and predictions
#' @export



get_aic <- function(the_port, the_seed = 300){
# browser() 
  #Find the files
  the_string <- paste0(the_port, "_runs1_rev100_minyr2007_focyr2012_seed10")
  file_vector <- the_files[grep(the_string, the_files)]
  
  set.seed(the_seed)
  
  #------------------------------------------
  #Model fits
  start_time <- Sys.time()
  fits <- mclapply(file_vector, FUN = function(xx){
    load(paste0("model_runs/", xx))
    probs <- fitted(mod, outcome = F)
  
    #Go through each row and sample one value
    preds <- apply(probs, MARGIN = 1, FUN = function(yy){
  	  rmultinom(n = 1, size = 1, prob = yy)
    })
    
    zone_dist <- (strsplit(xx, "hdist"))[[1]][2]
    zone_dist <- strsplit(zone_dist, '.Rdata')[[1]]
    zone_dist <- as.numeric(zone_dist)
    if(is.na(zone_dist)) zone_dist <- 5
  
    #Is the choice in the same zone as the fished location?

    model_rums <- mod$model
    model_rums$ids <- row.names(model_rums)
    haul_ids <- model_rums %>% filter(fished == TRUE)
    haul_ids$ids <- substr(haul_ids$ids, 1, nchar(haul_ids$ids) - 2)
    
    #Loop through each empirical tow
    ncorrect <- rep(0, nrow(haul_ids))
    
    for(ii in 1:nrow(haul_ids)){
      pred_tow <- which(preds[, ii] == 1)

      # Is the distance close?
      indexes <- grep(haul_ids$ids[ii], model_rums$ids)
      check_distances <- model_rums[indexes , ]
      the_distance <- abs(diff(check_distances[c(1, pred_tow), 'distance']))

      #Is the predicted tow in the same depth bin?
      ndepths <- length(unique(choices[indexes[c(1, pred_tow)], 'depth_bin' ]))

      if(ndepths == 1 & the_distance <= zone_dist | 
        length(check_distances[c(1, pred_tow), 'distance']) == 1) ncorrect[ii] <- 1            
    }

#------------------------------------------
    #Calculate the percentage of correct values
    perc_right <- sum(ncorrect) / length(ncorrect)
  
    #Then see how often they pick tow 1, the actual tow
    outs <- data_frame(aic_val = AIC(mod), LL = logLik(mod), perc_right = perc_right)
    return(outs)
  }, mc.cores = min(length(file_vector), 6))
  run_time <- Sys.time() - start_time; run_time
  names(fits) <- file_vector

  fits <- ldply(fits)
  names(fits)[1] <- 'filename'
  
  #Add in number of days
  fits$ndays <- 14
  fits[grep('14', fits$filename, invert = T), 'ndays'] <- 30
  
  #add in distance radius
  fits$radius <- 999
  fits[grep('hdist8', fits$filename), 'radius'] <- 8
  fits[grep('hdist16', fits$filename), 'radius'] <- 16
  fits[which(fits$radius == 999), 'radius'] <- 5
  fits$fleet <- substr(fits$filename, 1, 3)
  fits <- fits %>% arrange(aic_val)
  fits$delta_aic <- fits$aic_val - min(fits$aic_val)

  return(fits)
}
