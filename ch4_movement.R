#---------------------------------------------------------------------------------
#Movement among clusters

#Load data from ch4_cluster.R
  #tows_clust.R

#---------------------------------------------------------------------------------
#Decisions can be made based on expected average catch or catch proportion of each species
#Accounting for 85% of tows, by filtering so each cluster has at least 50 tows
  
#Filter clusters so that each one has at least 50 tows
# clusts_threshold <- tows_per_clust %>% filter(ntows >= 50) %>% select(unq_clust) 

# filt_clusts <- tows_clust[tows_clust$unq_clust %in% clusts_threshold$unq_clust, ]
# unique(filt_clusts$unq_clust) == unique(clusts_threshold$unq_clust) [order(unique(clusts_threshold$unq_clust))]

# #Calculate the average catch of each species in each cluster
# ###Decide to use hpounds or apounds
# filt_clusts <- filt_clusts %>% group_by(haul_id) %>% mutate(tow_catch_tot = sum(hpounds, na.rm = T)) %>% 
#   group_by(haul_id, species) %>% mutate(tow_catch_spp = sum(hpounds, na.rm = T), 
#     perc_spp = tow_catch_spp / unique(tow_catch_tot))

# #Calculate averages for each species in each cluster
# filt_clusts <- filt_clusts %>% group_by(unq_clust, species) %>% 
#   mutate(clust_catch_avg = mean(tow_catch_spp, na.rm = T), 
#     clust_perc_avg = mean(perc_spp, na.rm = T)) %>% 
#   as.data.frame

# #Add in target and weak species classifications
# filt_clusts$type <- 'other'
# targs <- c("Dover Sole", 'Sablefish', 'Longspine Thornyhead', "Petrale Sole",
#     "Lingcod", "Shortspine Thornyhead")
# weaks <- c("Darkblotched Rockfish", "Pacific Ocean Perch", "Canary Rockfish",
#     "Bocaccio Rockfish", "Yelloweye Rockfish", "Cowcod Rockfish", "Cowcod")
# groundfish <- c('Arrowtooth Flounder', 'English Sole', 'Longnose Skate', 'Yellowtail Rockfish',
#   "Widow Rockfish", 'Chilipepper Rockfish', 'Black Rockfish', "Vermilion Rockfish", 'Greenstriped Rockfish',
#   "Greenspotted Rockfish", 'Bank Rockfish')

# filt_clusts[filt_clusts$species %in% targs, 'type'] <- "targets"
# filt_clusts[filt_clusts$species %in% weaks, 'type'] <- "weaks"
# filt_clusts[filt_clusts$species %in% groundfish, 'type'] <- "groundfish"

# save(filt_clusts, file = 'output/filt_clusts.Rdata')

#---------------------------------------------------------------------------------
#Can load filt_clusts here
load('output/filt_clusts.Rdata')

#Add in the value of each tow
#Load exvessel info
load("output/exvessel_formatted.Rdata")

#Merge and calculate haul values
filt_clusts <- filt_clusts %>% left_join(exvessel, by = c('set_year', 'r_state', 'species'))
filt_clusts <- filt_clusts %>% group_by(haul_id) %>% 
  mutate(haul_value = sum(exval_pound * hpounds, na.rm = T)) 

#Add in distance from port to centroid of average cluster
ports <- read.csv("output/ports.csv", stringsAsFactors = F)
# ggplot(ports) + geom_point(aes(x = d_port_long, y = d_port_lat), col = 'red') + 
#   geom_map(data = states_map, map = states_map, 
#          aes(x = long, y = lat, map_id = region)) + scale_x_continuous(limits = c(-126, -118)) + 
#   scale_y_continuous(limits = c(30, 49))

#Add in the distances
filt_clusts <- filt_clusts %>% left_join(ports, by = 'd_port')
filt_clusts <- filt_clusts %>% group_by(unq_clust) %>% 
  mutate(avg_long_clust = mean(avg_long), avg_lat_clust = mean(avg_lat))

source("R/dist_funcs.R")

#Units are in km
filt_clusts$d_port_clust_dist <- gcd_slc(deg2rad(filt_clusts$avg_long_clust), 
  deg2rad(filt_clusts$avg_lat_clust), deg2rad(filt_clusts$d_port_long), 
  deg2rad(filt_clusts$d_port_lat))


#Look at overlap in tows from each port
filt_clusts %>% distinct(haul_id, .keep_all = T) %>% 
  filter(d_port %in% c("CHARLESTON (COOS BAY)", "NEWPORT", "ASTORIA / WARRENTON")) %>% ggplot() + 
  geom_point(aes(x = set_long, y = set_lat, colour = d_port)) + geom_map(data = states_map, map = states_map, 
         aes(x = long, y = lat, map_id = region)) + scale_x_continuous(limits = c(-126, -118)) + 
  scale_y_continuous(limits = c(30, 49)) + facet_wrap(~ d_port)

#Create a data frame to look at 
#Add in cost information by region
load(file = 'output/costs.Rdata')
names(costs)[2] <- 'ryear'
filt_clusts <- filt_clusts %>% left_join(costs, by = c('d_port', 'ryear'))

#Costs are per day, calculate costs per tow based on averages and see if I can get profits
#to match some of the published numbers
filt_clusts$ddate <- mdy(paste(filt_clusts$dmonth, filt_clusts$dday, filt_clusts$dyear, sep = "-"))
filt_clusts$rdate <- mdy(paste(filt_clusts$rmonth, filt_clusts$rday, filt_clusts$ryear, sep = "-"))
filt_clusts$trip_duration_days <- as.numeric(filt_clusts$rdate - filt_clusts$ddate) + 1

filt_clusts %>% group_by(trip_id) %>% 
  mutate(ntows_per_day = length(unique(haul_id)) / trip_duration_days) %>% 
  group_by(r_port, ryear) %>% 
  mutate(avg_tows_per_day = mean(ntows_per_day)) %>% select(avg_tows_per_day) %>%

  distinct(avg_tows_per_day)




filt_clusts %>% distinct(haul_id, .keep_all = T) %>% group_by(set_year) %>% 
  summarize(tot_value = sum(haul_value),
  value_per_boat = tot_value / length(unique(drvid)), ndays = length(unique))




ff <- filt_clusts[1, ]
days
as.vector(ff$rdate - ff$ddate)
days(ff$rdate - ff$ddate)


#Scale fuel cost based on percentages
filt_clusts <- filt_clusts %>% group_by(d_port) %>%
  mutate(avg_d_port_clust_dist = mean(d_port_clust_dist),
    dist_perc_diff = (d_port_clust_dist - avg_d_port_clust_dist) / avg_d_port_clust_dist,
    scaled_fuel_per_clust = fuel * dist_perc_diff + fuel) 

#Check the profits, with published numbers
filt_clusts %>% distinct(haul_id, .keep_all = TRUE) %>% group_by(ryear) %>%
  summarize(value = sum(haul_value), 
    cost = sum(buyback, captain, crew, observer, scaled_fuel_per_clust, na.rm = TRUE),
    profit = value - cost)

#Calculate number of tows per day to scale the costs
filt_clusts %>% group_by(dport_desc)

unique(nchar(filt_clusts$dyear))
ymd(112004)

filt_clusts[which(filt_clusts$trip_duration == 999), ] %>%
  filter(dyear != ryear) %>%
  select(dmonth, dday, dyear, rmonth, rday, ryear, trip_duration) 


filt_clusts %>% filter(trip_id == '961620') %>% distinct(haul_id, .keep_all = TRUE) %>%
  summarize(value =  sum(haul_value), cost = sum(buyback, captain, crew, observer, 
    scaled_fuel_per_clust))

#Calculate the number proportion of values that overlap?

#Potential port groups
#wa - astoria, ilwaco, bellingham bay, neah bay, newport, westport
#newport-charleston
#monterey - moss landing - san francisco - half moon bay


#---------------------------------------------------------------------------------
#Look at mean and se for haul_value in each cluster
money_clusts <- filt_clusts %>% distinct(haul_id, .keep_all = T) %>%
 group_by(unq_clust) %>% summarize(avg_value = mean(haul_value), sd_value = sd(haul_value), 
  ntows = length(unique(haul_id)), avg_depth = mean(avg_depth), avg_long = mean(avg_long),
  avg_lat = mean(avg_lat))
money_clusts$se_value <- money_clusts$sd_value / sqrt(money_clusts$ntows)  

#
money_clusts %>% ggplot() + geom_point(aes(x = ntows, y = sd_value, fill = avg_depth), 
  pch = 21) + scale_fill_gradient(low = 'white', high = 'red')

#Visualize location
money_clusts %>% ggplot() + geom_point(aes(x = avg_long, y = avg_lat, colour = avg_value)) + 
  scale_colour_gradient(low = 'white', high = 'red') + geom_map(data = states_map, map = states_map, 
         aes(x = long, y = lat, map_id = region)) + scale_x_continuous(limits = c(-126, -118)) + 
  scale_y_continuous(limits = c(30, 49))

save(money_clusts, file = 'output/money_clusts.Rdata')
money_clusts %>% filter(unq_clust %in% 
  money_clusts$unq_clust[which(money_clusts$unq_clust %in% unique(ast$unq_clust))]) %>%
arrange(desc(avg_value)) %>% select(unq_clust) %>% as.data.frame

#Calculate costs of each trip
filt_clusts %>% group_by(trip_id) %>% summarize(nclusts = length(unique(unq_clust)))

#Look at trip 6219
filt_clusts %>% filter(trip_id == 6219) %>% distinct(haul_id, .keep_all = TRUE) %>%
  as.data.frame %>% head


#---------------------------------------------------------------------------------
#Make assumption about:
# number of tows per day
# number of tows per trip

filt_clusts %>% filter(trip_id == '961620') %>% as.data.frame 



#Start Developing movement rules
#cluster 991 consistently catches lots of sablefish

###Need to add in distances from port

#Pick three clusters and see if I can get from the shitty one to the good one
#with certain rules
# filt_clusts %>% filter(d_port == "ASTORIA / WARRENTON") -> ast

#58 good
#265 ok 
#295 shitty

#start in shitty

filt_clusts %>% filter(trip_id == '961620') %>% as.data.frame 






#Add in economic data from 5 year review, 
#cost per day, per port, in each year, fuel scaled by distance traveled,
#like 3-64 of 5yr_review

#Sample a tow in the shitty cluster
first_clust <- filt_clusts %>% filter(unq_clust == 295) %>% 
  select(unq_clust, d_port, haul_id, species, hpounds, apounds, 
    haul_value, d_port_clust_dist, trip_id)

#Sample a trip
trip <- base::sample(unique(first_clust$trip_id), size = 1)

#Look at catch
first_clust %>% filter(trip_id %in% trip) %>% group_by(species) %>%
  summarize(hpounds = sum(hpounds, na.rm = T), haul_value = sum(unique(haul_value), na_rm = T))

filt_clusts[which(filt_clusts$trip_id == '961620'), ] %>% 





#Steps should be:
#1. Check individual quota,
#2. Look at profits
#3. Look at available knowledge
#4. Move depending on how close to your quota constraints you are




















#------------------
#Look at the difference between hpounds and apounds
filt_clusts$ah_diff <- filt_clusts$apounds - filt_clusts$hpounds

filt_clusts[which(is.na(filt_clusts$ah_diff)), 'ah_diff'] <- filt_clusts[which(is.na(filt_clusts$ah_diff)), 'apounds']
filt_clusts[which(is.na(filt_clusts$ah_diff)), 'ah_diff'] <- filt_clusts[which(is.na(filt_clusts$ah_diff)), 'hpounds']


filt_clusts %>% group_by(set_year, species, type) %>% summarize(hpounds = sum(hpounds, na.rm = T), 
  apounds = sum(apounds, na.rm = T), ha_diff = apounds - hpounds) %>% arrange(desc(ha_diff)) %>%
  filter(type == 'weaks', ha_diff != 0) %>% as.data.frame -> ha_diffs

tow_ha_diffs <- filt_clusts %>% group_by(haul_id, species, type) %>% summarize(hpounds = sum(hpounds, na.rm = T), 
  apounds = sum(apounds, na.rm = T), ha_diff = apounds - hpounds) %>% arrange(desc(ha_diff)) 

trip_ha_diffs <- filt_clusts %>% group_by(trip_id, species, type) %>% summarize(hpounds = sum(hpounds, na.rm = T), 
  apounds = sum(apounds, na.rm = T), ha_diff = apounds - hpounds) %>% arrange(desc(ha_diff)) 

trip_ha_diffs %>% filter(type == 'targets') %>% head

   %>%
  filter(type == 'weaks', ha_diff != 0) %>% as.data.frame -> ha_diffs
 
  ah_diff = mean(ah_diff)) %>% 
  arrange(desc(ah_diff)) %>% filter(type == 'weaks') %>% as.data.frame


filt_clusts





