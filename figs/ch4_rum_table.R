#-----------------------------------------------------------------
#Make table of RUM coefficients
library(xtable)

#-----------------------------------------------------------------
#Load data 
load("/Volumes/udrive/coefs1.Rdata")
load("/Volumes/udrive/coefs50.Rdata")

#Format Data
port_names <- c('Moss Landing / San Francisco', "Fort Bragg", 'Eureka',
  "Crescent City / Brookings", "Charleston", 'Newport', 'Astoria', 'Ilwaco / Newport')

names(coefs1) <- port_names
names(coefs50) <- port_names

#-----------------------------------------------------------------
#Paste significance values to 

coefs1 <- lapply(coefs1, FUN = function(xx){
  xx$value = paste(formatC(xx$coefs, digits = 5, format = 'f'), xx$significance)
  return(xx)
} )

#Calculate difference between tables
lapply(1:8, FUN = function(xx){
  temp <- coefs50[[xx]]$coefs - coefs1[[xx]]$coefs
  coefs1[[xx]]$change <- temp
  return(coefs1[[xx]])
} )

#Try foramtting this shit in xtable
coefs_table <- 

xtable()


coefs1[[1]]$coefs - coefs50[[1]]$coefs
coefs50[[1]]

obs_data <- obs_data %>% arrange(trip_id, haul_num)




