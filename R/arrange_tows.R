#' Arrange the tows so they all are in the same direction

#' Function to Orient all tows in the same direction

#' @param dat Input data 
#' @param direction Direction to orient all tows in. Default is "NW" and everything is hard coded which I could change at some point

#' @export

# ' @examples
# ' arrange_tows(dat = part_wc_data, direction = 'NW')

#arrange tow directions so they are all oriented in the same direction
#Dat is input data frame
# direction is direction of all tows

arrange_tows <- function(dat, direction = "NW"){
  #Set dummy value for all direction column
  dat$direction <- '999'

  #Assign direction to tows
  dat[which(dat$up_lat >= dat$set_lat & dat$up_long >= dat$set_long), 'direction'] <- 'NW'
  dat[which(dat$up_lat >= dat$set_lat & dat$up_long <= dat$set_long), 'direction'] <- 'NE'
  dat[which(dat$up_lat <= dat$set_lat & dat$up_long <= dat$set_long), 'direction'] <- 'SE'
  dat[which(dat$up_lat <= dat$set_lat & dat$up_long >= dat$set_long), 'direction'] <- 'SW'

  nws <- subset(dat, direction == 'NW')
  
  #Orient all tows based on the specified direction
  #northeast
  nes <- subset(dat, direction == 'NE') #switch longitudes
  nes$temp <- nes$up_long
  nes$up_long <- nes$set_long
  nes$set_long <- nes$temp
  nes <- nes %>% select(-temp)

  #southeast
  ses <- subset(dat, direction == 'SE') #Switch Longitudes and latitudes
  ses$temp_long <- ses$up_long
  ses$up_long <- ses$set_long
  ses$set_long <- ses$temp_long
  
  ses$temp_lat <- ses$up_lat
  ses$up_lat <- ses$set_lat
  ses$set_lat <- ses$temp_lat
  ses <- ses %>% select(-c(temp_lat, temp_long))

  #Southwest
  sws <- subset(dat, direction == 'SW')
  sws$temp_lat <- sws$up_lat
  sws$up_lat <- sws$set_lat
  sws$set_lat <- sws$temp_lat
  sws <- sws %>% select(-temp_lat)

  out <- rbind(nws, nes, ses, sws)
  out$direction <- 'NW'

  #Update the longitude transformations also; important for clustering steps
  out$trans_set_long <- out$set_long * cos((2 * pi * out$set_lat) / 360)
  out$trans_up_long <- out$up_long * cos((2 * pi * out$up_lat) / 360)

  return(out)
  
  #To check orientation of everything
  # temp <- sws
  # ggplot(temp) + geom_segment(aes(x = -set_long, xend = -up_long, y = set_lat,
  #   yend = up_lat), arrow = arrow(length = unit(0.1,"cm")))

}
