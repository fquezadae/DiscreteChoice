#' Function to process coefficients
#' 
#' Function to process coefficients and add in relevant arguments
#' 
#' @param filename Name of file
#' @param dir Path to directory
#' @export

process_coefficients <- function(filename, dir = "/Volumes/udrive/"){
  #load coefficients
  load(paste0(dir, filename))

  #Add coefficient variable column
  coefs <- lapply(coefs, FUN = function(xx) {
    xx$coef <- row.names(xx)
    return(xx)
  })
# browser()

  #melt the lists
  coefs1 <- melt(coefs, id.vars = c('coefs', 'p_values', 'significance', 'coef'))
  coefs1 <- plyr::rename(coefs1, c("L1" = "port"))
  coefs1 <- plyr::rename(coefs1, c('coefs' = 'value'))  

  #Parse filename and add details in to coefs1
  filename_args <- unlist(strsplit(filename, split = "_"))
  coefs1$price_multiplier <- gsub("coefs", "", filename_args[1])
  coefs1$year <- gsub("focyr", "", filename_args[4])
  coefs1$hdist <- gsub("hdist", "", filename_args[8])
  coefs1$spp <- gsub("qspecies", "", filename_args[10])
  coefs1$spp <- gsub('.Rdata', "", coefs1$spp)	
  return(coefs1)
}


#Test function
# process_coefficients(filename = udrive_files[grep('coefs5_', udrive_files)][1])