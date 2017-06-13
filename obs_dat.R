#load observer data
setwd('/Users/peterkuriyama/School/Research/ch4')
load_all()

#Load Packages
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(devtools)
library(maps)
library(doParallel)

#Load Data
# load("C:\\Users\\Lewis\\Documents\\Data\\OBDATA_Barnett_OBProcessed_Catch_Data_2002_2014_2015-10-21.Rda")
# obs_data <- OB.ad2
# rm(OB.ad2)

# load("/Users/peterkuriyama/Desktop/AST.Rdata")
# obs_data <- ast

#States Map
# states_map <- map_data("state")

#Install delta plot functions
devtools::install_github("peterkuriyama/ch2vms/ch2vms")
library(ch2vms)

#get column names in the right format
# names(obs_data) <- tolower(names(obs_data))
# 
# #Remove mt and scientific name columns
# obs_data[, c(80, 117)] <- NULL
# 
# #Convert latitudes to longitudes
# obs_data$trans_set_long <- obs_data$set_long * cos((2 * pi * obs_data$set_lat) / 360)
# obs_data$trans_up_long <- obs_data$up_long * cos((2 * pi * obs_data$up_lat) / 360)

#Names to change
obs_data <- plyr::rename(obs_data, c("d_port" = 'dport_desc'))
load_all("GitHub/ch4")

#---------------------------------------------------------------------------------
#Calculate distances between tows
#Function to convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)

#Calculate distance with haversine distance
gcd_slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}


#Add in the distances
obs_data$dist_slc_km <- gcd_slc(deg2rad(obs_data$set_long), deg2rad(obs_data$set_lat), 
  deg2rad(obs_data$up_long), deg2rad(obs_data$up_lat))

#Filter data by speed
obs_data$km_duration <- obs_data$dist_slc_km / obs_data$haul_duration

#Maybe filter to be 10km 
# obs_data %>% filter(km_duration < 10) %>% dim
# obs_data %>% distinct(km_duration) %>% arrange(desc(km_duration)) %>% head(n = 100)

obs_data$hpounds <- obs_data$lb
#---------------------------------------------------------------------------------
#Remove seattle values from obs_data
obs_data <- obs_data %>% filter(dport_desc != "SEATTLE")

obs_data1 <- ch4_format_data(obs_data, top100 = FALSE) 
#Save this once and never run again hopefully
save(obs_data1, file = "Data/obs_data1.Rdata")

rm(obs_data)

#Compare the two data types, also filtered to be 2007-2014 only*****
obs_data %>% group_by(dport_desc) %>% summarize(nports = n()) %>% arrange(desc(nports))
obs_data1 %>% group_by(dport_desc) %>% summarize(nports = n()) %>% arrange(desc(nports))
head(obs)

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#Load processed data
load(file = "C:\\Users\\Lewis\\Documents\\Data\\obs_data1.Rdata")

obs_data <- obs_data1
rm(obs_data1)

#---------------------------------------------------------------------------------
obs_data %>% group_by(dport_desc) %>% summarize
obs_data %>% select(ntows, nvess, unq_clust) %>% head(n = 30)

#---------------------------------------------------------------------------------
#Aggregate measures
#See ch4_centroid

obs_data %>% group_by(dyear) %>% summarize(ntows = length(unique(haul_id)),
  avg_dist = mean(dist_slc_km, na.rm = T))


#Rename columns
obs_data <- plyr::rename(obs_data, c('lb' = 'hpounds', 
  'dmonth' = 'tow_month', 'dyear' = 'tow_year',
  'dday' = 'tow_day', 'set_depth' = 'depth1',
  'haul_duration' = 'duration'))

#---------------------------------------------------------------------------------
#Function to calculate values for delta plots
obs_deltas <- delta_plot(data = obs_data)

ggplot(obs_deltas, aes(x = prop_zero, y = skew)) + geom_point() + facet_wrap(~ year) + 
  geom_text(aes(label = short))
#Should look like this, attached this file in the email

load('output/survey_deltas.Rdata')




