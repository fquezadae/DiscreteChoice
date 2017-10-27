#' Function to calculate predictions and AIC values
#' input a port value
#' 
#' @param the_port Port name
#' @param the_seed Seed value for rmultinom and predictions
#' @export

get_aic <- function(the_port, the_seed = 300){
 
  #Find the files
  the_string <- paste0(the_port, "_runs1_rev100_minyr2007_focyr2012_seed10")
  file_vector <- the_files[grep(the_string, the_files)]
  
  set.seed(the_seed)
  
  #Model fits
  fits <- mclapply(file_vector, FUN = function(xx){
    load(paste0("model_runs/", xx))
    probs <- fitted(mod, outcome = F)
  
    #Go through each row and sample one value
    preds <- apply(probs, MARGIN = 1, FUN = function(yy){
  	  rmultinom(n = 1, size = 1, prob = yy)
    })
    
    #Calculate the percentage of correct values
    perc_right <- rowSums(preds)[1] / sum(rowSums(preds))
  
    #Then see how often they pick tow 1, the actual tow
    outs <- data_frame(aic_val = AIC(mod), LL = logLik(mod), perc_right = perc_right)
    return(outs)
  }, mc.cores = min(length(to_load), 6))

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
