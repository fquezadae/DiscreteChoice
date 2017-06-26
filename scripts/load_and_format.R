#---------------------------------------------------------------------------------
#Load, format, and combine logbook and observer data

load("C://Users//Lewis//Documents//Data//OBDATA_Barnett_OBProcessed_Catch_Data_2002_2014_2015-10-21.Rda")
obs_data <- OB.ad2
rm(OB.ad2)


load("C://Users//Lewis//Documents//Data//LBKDATA_Barnett_Logbook_Data_2002_2015_2016-06-07.Rda")
lbk <- LBK
rm(LBK)

#Configure the two datat types so they 
names(obs_data) <- tolower(names(obs_data))
names(lbk) <- tolower(names(lbk))

#---------------------------------------------------------------------------------
#Get all the lbk set times to have four characters
lbk$set_time <- as.character(lbk$set_time)
#Ones with only 1 digit
lbk[which(nchar(lbk$set_time) == 1), 'set_time'] <- paste0("000", lbk[which(nchar(lbk$set_time) == 1), 'set_time'])
lbk[which(nchar(lbk$set_time) == 2), 'set_time'] <- paste0("00", lbk[which(nchar(lbk$set_time) == 2), 'set_time'])
lbk[which(nchar(lbk$set_time) == 3), 'set_time'] <- paste0("0", lbk[which(nchar(lbk$set_time) == 3), 'set_time'])

lbk[which(nchar(lbk$set_time) == 2), 'set_time']

unique(nchar(lbk$set_time))

#Convert set_time in observer data to hours and minutes
hours <- floor(obs_data$set_time)
minutes <- round((obs_data$set_time - hours) * 60, digits = 0)

minutes <- as.character(minutes)
minutes[which(nchar(minutes) == 1)] <- paste0("0", minutes[which(nchar(minutes) == 1)])

hours[which(nchar(hours) == 1)] <- paste0(0, hours[which(nchar(hours) == 1)]) 
obs_data$set_time <- paste0(hours, minutes)

# obs_data$set_time <- as.integer(obs_data$set_time)

#Parse out set_mont
lbk$towdate <- as.character(lbk$towdate)
vs <- lbk$towdate
vs <- ldply(strsplit(vs, "-"))
names(vs) <- c('set_day', "set_month", "set_year")

mnths <- data.frame(set_month = c('JAN', 'FEB', "MAR", "APR", 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'),
                    month_num = 1:12)
mnths$set_month <- as.character(mnths$set_month)
vs <- vs %>% left_join(mnths, by = 'set_month') 
names(vs) <- c('set_day', 'mm', 'set_year', 'set_month')
lbk <- cbind(lbk, vs[, c('set_day', 'set_year', 'set_month')])

#---------------------------------------------------------------------------------
#Try combining by fish tickets
lbk$ftid <- as.character(lbk$ftid)
lbk$ftid1 <- as.character(lbk$ftid1)
lbk$ftid2 <- as.character(lbk$ftid2)
lbk$ftid3 <- as.character(lbk$ftid3)
lbk$ftid4 <- as.character(lbk$ftid4)
lbk$ftid5 <- as.character(lbk$ftid5)
lbk$ftid6 <- as.character(lbk$ftid6)

# #Combine these into one ftid
# lbk$fish_tickets <- paste(lbk$ftid, lbk$ftid1, lbk$ftid2, lbk$ftid3, lbk$ftid4,
#                            lbk$ftid5, lbk$ftid6, sep = ";")

lbk$fish_tickets <- lbk$ftid

lbk_before <- subset(lbk, set_year < 2011)

#Logbook fish tickets before
unq_lbkft <- unique(lbk_before$fish_tickets)

longs <- which(nchar(unq_lbkft) > 8)
longs <- unq_lbkft[longs]
longs <- unlist(strsplit(longs, split = ":"))
longs <- unique(longs)

shorts <- which(nchar(unq_lbkft) <= 8)
shorts <- unq_lbkft[shorts]

unq_lbkft <- c(longs, shorts)

# unq_lbkft <- unique(c(lbk$ftid, lbk$ftid1, lbk$ftid2, lbk$ftid3, lbk$ftid4,
#                       lbk$ftid5, lbk$ftid6))

obs_data_before <- subset(obs_data, set_year < 2011 )
unq_obsft <- unique(obs_data_before$fish_tickets)

longs <- which(nchar(unq_obsft) > 8)
longs <- unq_obsft[longs]

longs <- unlist(strsplit(longs, split = ";"))

shorts <- which(nchar(unq_obsft) <= 8)
shorts <- unq_obsft[shorts]

unq_obsft <- c(longs, shorts)
unq_obsft <- unq_obsft[-which(nchar(unq_obsft) == 0)]

#All the fish tickets from the observer data should be in the logbook data
#About 1000  fish tickets from the observer data are not in the logbook data
#Which ones are not there?
to_change <- unq_obsft[(unq_obsft %in% unq_lbkft)]
miss_ft <- unq_obsft[(unq_obsft %in% unq_lbkft == FALSE)]

#How much am I losing?
missing <- paste0(miss_ft, collapse = "|" )
missing_obs <- grep(missing, obs_data$fish_tickets)
mm <- obs_data[missing_obs, ]
mm %>% group_by(ryear) %>% summarize(ntows = length(unique(haul_id)))



#---------------------------------------------------------------------------------
#Obs_data depth is in fathom
obs_data1 <- obs_data[, c("drvid", "dmonth", "dday", "dyear", "d_port", "d_port_group", 
                          "d_state", "r_port", "rmonth", "rday", "ryear", "r_port_group", 
                          "pcid", "r_state", "trip_duration", "haul_id", "haul_num", 'target',
                          "set_lat", "set_long", "set_depth", "up_time", "type",
                          "up_lat", "up_long", "up_depth", "haul_duration", "avg_depth", 
                          "trip_id", "lb", "dis", "species", 'set_month', 'set_day', 'set_year', 'set_time',
                          'fish_tickets', 'avg_long', 'avg_lat')]


lbk1 <- lbk[, c("trip_id", "agid", "dday", 
                "dmonth", "dyear", "dport", "rday", 
                "rmonth", "ryear", "rport", "drvid", "townum", 
                "set_lat", "set_long", "up_time", "duration", 
                "up_lat", "up_long", "depth1", "target", "hpounds", "apounds", 
                "haul_id", "spc.name", "type", "d_portgrp", "r_portgrp", 'set_month', 'set_day',
                'set_year', 'set_time', 'fish_tickets', 'avglong', 'avglat')]

lbk1 <- plyr::rename(lbk1, c('spc.name' = 'species', "agid" = 'd_state', "dport" = "d_port",
                             "rport" = "r_port", "townum" = "haul_num", "duration" = "haul_duration",
                             "depth1" = "avg_depth", "d_portgrp" = "d_port_group", 
                             "r_portgrp" = "r_port_group", 'avglong' = 'avg_long', 'avglat' = 'avg_lat'))


#Change lbk1 colums from factor to character class
facts <-  as.vector(which(sapply(lbk1[1, ], FUN = class) == "factor"))

for(ii in 1:length(facts)){
  lbk1[, facts[ii]] <- as.character(lbk1[, facts[ii]]) 
}

obs_data1$type <- as.character(obs_data1$type)

#Add hpounds and apounds into obs_data1
obs_data1$apounds <- obs_data1$lb
obs_data1$hpounds <- obs_data1$lb
obs_data1$lb <- NULL
obs_data1$dis <- NULL
obs_data1$pcid <- NULL 

lbk1$r_state <- lbk1$d_state
lbk1$trip_duration <- 999
lbk1$set_depth <- lbk1$avg_depth
lbk1$up_depth <- lbk1$avg_depth

#Double check that numbers of columns is the same
sum(names(obs_data1) %in% names(lbk1)) == 38
sum(names(lbk1) %in% names(obs_data1)) == 38

#Replace tows in logbook data with tows from the observer data
#Subset logbook data to be before 2011
# to_change contains the logbook tows that can be replaced
obs_before <- obs_data1 %>% filter(set_year < 2011)
obs_after <- obs_data1 %>% filter(set_year >= 2011)

lbk_before <- lbk1 %>% filter(set_year < 2011)
lbk_after <- lbk1 %>% filter(set_year >= 2011)

#Now pull out the fish tickets in common and substitute them in to the logbook data
to_change1 <- paste0(to_change[1:1000], collapse = "|") 
to_change2 <- paste0(to_change[1001:2000], collapse = "|")
to_change3 <- paste0(to_change[2001:3000], collapse = "|")
to_change4 <- paste0(to_change[3001:length(to_change)], collapse = "|")

#Pull the observer data
from_obs_ind1 <- grep(to_change1, obs_before$fish_tickets)
from_obs_ind2 <- grep(to_change2, obs_before$fish_tickets)
from_obs_ind3 <- grep(to_change3, obs_before$fish_tickets)
from_obs_ind4 <- grep(to_change4, obs_before$fish_tickets)

to_pull <- c(from_obs_ind1, from_obs_ind2, from_obs_ind3, from_obs_ind4)

#Remove the tows from the logbook data
remove_lbk_ind1 <- grep(to_change1, lbk_before$fish_tickets)
remove_lbk_ind2 <- grep(to_change2, lbk_before$fish_tickets)
remove_lbk_ind3 <- grep(to_change3, lbk_before$fish_tickets)
remove_lbk_ind4 <- grep(to_change4, lbk_before$fish_tickets)

to_remove <- c(remove_lbk_ind1, remove_lbk_ind2, remove_lbk_ind3, remove_lbk_ind4)

#####
#Now move the data around
lavar <- obs_before[to_pull, ]

dim(lbk_before)
# 1008684      36

keeps <- lbk_before[-to_remove, ]
removes <- lbk_before[to_remove, ]

comb_data_before <- rbind(lavar, keeps)

#Now check all this
head(to_change)

grep(to_change[2], keeps$fish_tickets)
grep(to_change[2], removes$fish_tickets)
grep(to_change[2], lavar$fish_tickets)

# comb_data <- rbind(comb_data_before, obs_after)

#Are there any tows that had different dyears and ryears?
# which(lbk1$dyear != lbk1$ryear)
# max(lbk1[which(lbk1$dyear != lbk1$ryear), 'ryear'])

#---------------------------------------------------------------------------------
#Combine the two data sets
# obs_data2 <- obs_data1 %>% filter(ryear >= 2011)
# lbk2 <- subset(lbk1, ryear < 2011)
lbk2 <- comb_data_before

#adjust longitudes
lbk2$up_long <- -lbk2$up_long
lbk2$set_long <- -lbk2$set_long

#Of states
lbk2[which(lbk2$d_state == "O"), "d_state"] <- "OR"
lbk2[which(lbk2$d_state == "W"), "d_state"] <- "WA"
lbk2[which(lbk2$d_state == "C"), "d_state"] <- "CA"

lbk2[which(lbk2$r_state == "O"), "r_state"] <- "OR"
lbk2[which(lbk2$r_state == "W"), "r_state"] <- "WA"
lbk2[which(lbk2$r_state == "C"), "r_state"] <- "CA"

obs_data2 <- obs_after

#Look at return ports, probably less variable because have to offload the fish
missing_ports <- unique(lbk2$r_port_group)[which(unique(lbk2$r_port_group) %in% unique(obs_data2$r_port_group) == FALSE)]

lbk2 %>% filter(d_port_group %in% missing_ports) %>% group_by(d_port_group) %>% 
  summarize(nhauls = length(unique(trip_id)))

#---------------------------------------------------------------------------------
#Read in port codes
port_codes <- read.csv("C://Users//Lewis//Documents//Data//port_codes.csv",  stringsAsFactors = F)
names(port_codes) <- c("pcid", "agid", "port", "port_desc")
port_codes$port <- as.character(port_codes$port)

#Need to update d_port values
obs_data2 %>% distinct(r_port, r_port_group) %>% arrange(r_port_group)
lbk2 %>% distinct(d_port, d_port_group) %>% arrange(d_port_group)
lbk2 %>% distinct(r_port, r_port_group) %>% arrange(r_port_group)

#Manually Change ports that are not in port_codes
lbk2[lbk2$d_port == "02", 'd_port'] <- "ASTORIA / WARRENTON"
lbk2[lbk2$r_port == "02", 'r_port'] <- "ASTORIA / WARRENTON"
lbk2[lbk2$d_port == "ILW", 'd_port'] <- "ILWACO"
lbk2[lbk2$r_port == "ILW", 'r_port'] <- "ILWACO"
lbk2[lbk2$d_port == "BEL", 'd_port'] <- "BELLINGHAM BAY"
lbk2[lbk2$r_port == "BEL", 'r_port'] <- "BELLINGHAM BAY"
lbk2[lbk2$d_port == "WES", 'd_port'] <- "WESTPORT"
lbk2[lbk2$r_port == "WES", 'r_port'] <- "WESTPORT"
lbk2[lbk2$d_port == "BLA", 'd_port'] <- "BLAINE"
lbk2[lbk2$r_port == "BLA", 'r_port'] <- "BLAINE"
lbk2[lbk2$d_port == "N.B", 'd_port'] <- "NEAH BAY"
lbk2[lbk2$r_port == "N.B", 'r_port'] <- "NEAH BAY"
lbk2[lbk2$d_port == "NB", 'd_port'] <- "NEAH BAY"
lbk2[lbk2$r_port == "N.B", 'r_port'] <- "NEAH BAY"

#Add in ports that are in port codes
lbk_ports <- unique(c(unique(lbk2$r_port), unique(lbk2$d_port)))

#Separate the ports out
remove <- lbk_ports[which(lbk_ports %in% port_codes$port)]
the_same <-  lbk2 %>% filter(d_port %in% remove == FALSE, r_port %in% remove == FALSE)

to_change <- lbk2 %>% filter(d_port %in% remove)

to_change1 <- to_change %>% left_join(port_codes[, c('agid', 'port', 'port_desc')], 
                                      by = c('d_port' = 'port', 'd_state' = 'agid')) 
to_change1$d_port <- to_change1$port_desc
to_change1$port_desc <- NULL

to_change1 <- to_change1 %>% left_join(port_codes[, c('agid', 'port', 'port_desc')],
                                       by = c('r_port' = 'port', 'r_state' = 'agid'))

to_change1[which(is.na(to_change1$port_desc)), 'port_desc'] <- "ASTORIA / WARRENTON"
to_change1$r_port <- to_change1$port_desc
to_change1$port_desc <- NULL

lbk2 <- rbind(the_same, to_change1)

#Lbk2 formatted with port descriptions

#---------------------------------------------------------------------------------
comb_data <- rbind(obs_data2, lbk2)
dim(obs_data2)
dim(obs_after)


obs_data <- comb_data

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

#Make sure all the longitudes are negative
obs_data[which(obs_data$set_long >= 0), 'set_long'] <- -obs_data[which(obs_data$set_long >= 0), 'set_long'] 
obs_data[which(obs_data$up_long >= 0), 'up_long'] <- -obs_data[which(obs_data$up_long >= 0), 'up_long'] 

#Check that they're all negative
which(obs_data$set_long > 0)
which(obs_data$up_long > 0)

#Add columns for avg lat and longs
obs_data$avg_lat <- (obs_data$set_lat + obs_data$up_lat) / 2
obs_data$avg_long <- (obs_data$up_long + obs_data$set_long) / 2

#Add in the d_port and r_port groups
obs_data <- obs_data %>% group_by(d_port_group) %>% mutate(d_port_group_desc = paste0(unique(d_port), collapse = "; ")) %>%
  as.data.frame
obs_data <- obs_data %>% group_by(r_port_group) %>% mutate(r_port_group_desc = paste0(unique(r_port), collapse = "; ")) %>%
  as.data.frame

#Add in time descriptions
obs_data$when <- 'baseline'
obs_data[which(obs_data$dyear >= 2007 & obs_data$dyear < 2011), 'when'] <- 'before'
obs_data[which(obs_data$dyear >= 2011), 'when'] <- 'after'

#Filter out long tows
quantile(obs_data$km_duration, na.rm = T)

obs_data <- obs_data %>% filter(km_duration <= 10)

obs_data$set_year <- as.numeric(obs_data$set_year)

save(comb_data, file = "C://Users//Lewis//Documents//Data//comb_data.Rda")




#---------------------------------------------------------------------------------
#Scrappies
#---------------------------------------------------------------------------------
#Later maybe look into replacing observer tows into logbook data when possible

str(lbk2)

#in obs_data, set_month, set_day, set_year, set_lat, set_long 

#Gotta parse the towdate, 
#into set_month, set_day, set_year, set_lat, set_long


#Combine the date
focus$





#Find hauls that are in the observer data and lbk data
obs_hauls <- as.character(unique(obs_data1$haul_id))
lbk_hauls <- unique(lbk1$haul_id)

obs_vess <- unique(obs_data1$drvid)
lbk_vess <- unique(lbk1$drvid)

sum(lbk_vess %in% obs_vess) == length(lbk_vess)
sum(obs_vess %in% lbk_vess) == length(obs_vess)

lbk_vess[which(lbk_vess %in% obs_vess == FALSE)]

#Look at a single vessel's tows
obsv <- obs_data1 %>% filter(drvid == 645410, dyear == 2009)
lbkv <- lbk1 %>% filter(drvid == 645410, dyear == 2009)

#Try to figure out how they're assigning the haul_id 
head(obs_data[, c('drvid', 'd_date', 'haul_id', 'trip_id', 'haul_num')])
obs_data %>% select(drvid, d_date, haul_id, trip_id, haul_num) %>% head
obs_data %>% select(drvid)

obs_vess <- unique(obs_data$drvid)
lbk_vess <- unique(lbk$drvid)

#What years were vessels not found in both data sets active?
lbk_vess %>% filter(drvid %in% lbk_vess[which(lbk_vess %in% obs_vess == FALSE)])
lbk_vess[which(lbk_vess %in% obs_vess == FALSE)]


#in obs_data, set_month, set_day, set_year, set_lat, set_long 
#Check number of digits in the lat and long values


#---------------------------------------------------------------------------------
#Try to move observer tows into the logbook data, based on set date and lat/long and set time
# obs_data1$unq <- paste(obs_data1$set_lat, obs_data1$set_long, obs_data1$set_month, 
#                        obs_data1$set_day, obs_data1$set_year, obs_data1$set_time)
# lbk1$unq <- paste(lbk1$set_lat, lbk1$set_long, lbk1$set_month, 
#                        lbk1$set_day, lbk1$set_year, lbk1$set_time)
# obs_unqs <- unique(obs_data1$unq)
