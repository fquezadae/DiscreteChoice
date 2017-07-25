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
# filt_clusts %>% distinct(haul_id, .keep_all = T) %>% 
#   filter(d_port %in% c("CHARLESTON (COOS BAY)", "NEWPORT", "ASTORIA / WARRENTON")) %>% ggplot() + 
#   geom_point(aes(x = set_long, y = set_lat, colour = d_port)) + geom_map(data = states_map, map = states_map, 
#          aes(x = long, y = lat, map_id = region)) + scale_x_continuous(limits = c(-126, -118)) + 
#   scale_y_continuous(limits = c(30, 49)) + facet_wrap(~ d_port)

#Create a data frame to look at 
#Add in cost information by region
load(file = 'output/costs.Rdata')
names(costs)[2] <- 'ryear'
filt_clusts <- filt_clusts %>% left_join(costs, by = c('d_port', 'ryear'))


#Costs are per day, calculate costs per tow based on averages and see if I can get profits
#to match some of the published numbers
filt_clusts$ddate <- mdy(paste(filt_clusts$dmonth, filt_clusts$dday, filt_clusts$dyear, sep = "-"))
filt_clusts$rdate <- mdy(paste(filt_clusts$rmonth, filt_clusts$rday, filt_clusts$ryear, sep = "-"))
filt_clusts$set_date <- mdy(paste(filt_clusts$set_month, filt_clusts$set_day, filt_clusts$set_year, sep = "-"))
filt_clusts$trip_duration_days <- as.numeric(filt_clusts$rdate - filt_clusts$ddate) + 1

#Calculate number tows per day
filt_clusts <- filt_clusts %>% group_by(trip_id) %>% mutate(ntows_per_day = length(unique(haul_id)) /
  unique(trip_duration_days)) 

#Scale the costs by the number of tows per day for each trip
filt_clusts[which(filt_clusts$ntows_per_day < 1), 'ntows_per_day'] <- 1

filt_clusts$buyback_per_tow <- filt_clusts$buyback / filt_clusts$ntows_per_day
filt_clusts$captain_per_tow <- filt_clusts$captain / filt_clusts$ntows_per_day
filt_clusts$crew_per_tow <- filt_clusts$crew / filt_clusts$ntows_per_day
filt_clusts$fuel_per_tow <- filt_clusts$fuel / filt_clusts$ntows_per_day
filt_clusts$observer_per_tow <- filt_clusts$observer / filt_clusts$ntows_per_day

# filt_clusts %>% distinct(haul_id, .keep_all = T) %>% group_by(set_year) %>% 
#   summarize(tot_value = sum(haul_value),
#   value_per_boat = tot_value / length(unique(drvid)), ndays = length(unique))

#Scale fuel cost based on percentages
filt_clusts <- filt_clusts %>% group_by(d_port) %>%
  mutate(avg_d_port_clust_dist = mean(d_port_clust_dist),
    dist_perc_diff = (d_port_clust_dist - avg_d_port_clust_dist) / avg_d_port_clust_dist,
    scaled_fuel_per_clust_per_tow = fuel_per_tow * dist_perc_diff + fuel) 

#---------------------------------------------------------------------------------
#Check the profits, with published numbers
filt_clusts %>% distinct(haul_id, .keep_all = TRUE) %>% group_by(ryear) %>%
  summarize(haul_value = sum(haul_value), unq_haul_value = unique(haul_value),
    cost = sum(buyback_per_tow, captain_per_tow, crew_per_tow, 
      observer_per_tow, scaled_fuel_per_clust_per_tow, na.rm = TRUE),
    profit = haul_value - cost)

haul_profits <- filt_clusts %>% distinct(haul_id, .keep_all = TRUE) %>%
  group_by(haul_id) %>% summarize(haul_value = sum(haul_value), 
    unq_haul_value = unique(haul_value), costs = buyback_per_tow + captain_per_tow +
    crew_per_tow + observer_per_tow + scaled_fuel_per_clust_per_tow,
    haul_profit = haul_value - costs, profit_fuel_only = haul_value - scaled_fuel_per_clust_per_tow)

#Table 3-10 has outputs and inputs in trawl
# haul_profits %>% group_by(set_year) %>% summarize(haul_value = sum(haul_value, na.rm = T) , 
#   haul_profit = sum(haul_profit, na.rm = T), haul_rev_fuel = sum(rev_fuel_only,
#     na.rm = T))

# haul_profits %>% group_by(drvid, ryear) %>% summarize(profits = sum(haul_profit),
#   rev_fuel = sum(rev_fuel_only)) %>%
#   group_by(ryear) %>% summarize(avg_profits = mean(profits, na.rm = T), 
#     avg_rev_fuel = mean(rev_fuel, na.rm = T))

#Because detailed data is unavailable to us, we assume that all the per tow costs
  #are the same but fuel scales variably
filt_clusts$haul_profit_fuel <- filt_clusts$haul_value - filt_clusts$scaled_fuel_per_clust_per_tow

#Add in haul_profits
filt_clusts <- filt_clusts %>% left_join(haul_profits %>% select(-haul_value), 
  by = 'haul_id')

#---------------------------------------------------------------------------------
#Bin the clusters to define the range of clusters a boat can move to
filt_clusts %>% distinct(unq_clust, .keep_all = TRUE) %>% as.data.frame %>% head

bin_clusts <- bin_data(data = filt_clusts %>% distinct(unq_clust, .keep_all = TRUE), 
  x_col = "avg_long_clust", "avg_lat_clust", group = "dyear", 
  grid_size = c(.0909, .11), group_vec = 2007:2014)

#Condense the data frame to remove year values
dim(bin_clusts)
bin_clusts <- bin_clusts %>% group_by(unq) %>% mutate(count = sum(count)) 
bin_clusts <- bin_clusts %>% distinct(unq, .keep_all = TRUE) %>% arrange(unq) %>% as.data.frame
bin_clusts$unq_clust_bin <- 1:nrow(bin_clusts)

#From filt_clusts, make it computationally easier
unique_clusters <- filt_clusts %>% distinct(unq_clust, .keep_all = TRUE) %>% ungroup() %>% select(avg_long_clust,
  avg_lat_clust, unq_clust) %>% as.data.frame 
unique_clusters$unq_clust_bin <- 999

#Use for loop to assign individual clusters to a binned cluster group
for(ii in 1:nrow(bin_clusts)){
  tt <- bin_clusts[ii, ]

  unique_clusters[which(unique_clusters$avg_long_clust >= tt$xmin & 
      unique_clusters$avg_long_clust <= tt$xmax & 
      unique_clusters$avg_lat_clust >= tt$ymin & 
      unique_clusters$avg_lat_clust <= tt$ymax), 'unq_clust_bin'] <- tt$unq_clust_bin
}

#looks like it accounted for all the clusters
unique_clusters %>% group_by(unq_clust_bin) %>% summarize(nclusts = length(unique(unq_clust))) %>%
  select(nclusts) %>% sum()

#Add in the xbin and ybin to unique_clusters
bb <- bin_clusts %>% select(unq_clust_bin, xbin, ybin, unq)
unique_clusters1 <- unique_clusters %>% left_join(bb, by = 'unq_clust_bin') 
unique_clusters1 <- unique_clusters1 %>% select(-avg_long_clust, -avg_lat_clust)

filt_clusts <- filt_clusts %>% left_join(unique_clusters1, by = 'unq_clust')

#---------------------------------------------------------------------------------
#For each cluster, calculate frequency of species encounters, 
  #frequency of species type encounters,
  #species proportions of catch ["clust_perc_avg"], and proportion for each species type

#Add frequency of encounters for species
filt_clusts <- filt_clusts %>% group_by(unq_clust) %>% 
  mutate(nhauls_in_clust = length(unique(haul_id))) %>%
  group_by(unq_clust, species) %>% mutate(spp_nhauls_in_clust = length(unique(haul_id))) %>%
  mutate(prop_hauls_w_spp = spp_nhauls_in_clust / nhauls_in_clust) 

#Add frequency of encounters by type of species
filt_clusts <- filt_clusts %>% group_by(unq_clust, type) %>%   
  mutate(type_nhauls_in_clust = length(unique(haul_id))) %>%
  mutate(prop_hauls_by_type = type_nhauls_in_clust / nhauls_in_clust) 

#Remove Washington State values
filt_clusts <- filt_clusts[-which(filt_clusts$d_port == "WASHINGTON STATE"), ] 

#---------------------------------------------------------------------------------
#See scripts/quota_props.R for details of quota formatting
load('output/quotas.Rdata')

#---------------------------------------------------------------------------------
#Average number of tows per vessel
# filt_clusts %>% filter(type %in% c('targets', 'weaks')) %>% group_by(set_year, species) %>% 
#   summarize(catch = sum(hpounds, na.rm = T)) %>% filter(set_year == 2011)

# filt_clusts %>% filter(set_year == 2011) %>% group_by(drvid) %>% 
#   summarize(ntows = length(unique(haul_id))) %>% arrange(desc(ntows)) 

#---------------------------------------------------------------------------------
#Notes for movement
#I think the profitable clusters are nearby

#Currently there doesn't seem to be an average path for these?
#Think about risk profile, where fishers balance profits with bycatch associated with each cluster

#How to specify data available to make decision about which cluster to move to next
#
#Can move to a different cluster 

#Strategies are the data available
#1. Average of the whole cluster?
#2. Recent catches from nearby vessels
#3. 

#Movement rules
#Risk-Reward based
  #based on profits and probability of catching
#Implement a kind of move on rule - Have to move if there's catch of a target species
#Depends on how close the vessel is to its quota

#Available clusters
#1. All clusters within a port
#2. 

#Move to the most profitable nearby cluster
#Possible clusters are within one cell
#Define the possible clusters based on the unique_bins

#Specify quota pound amount; Base this on the median amount of catch per vessel per year
filt_clusts %>% group_by(drvid, haul_id, set_year) %>% summarize(hpounds = sum(hpounds)) %>%
  group_by(drvid, set_year) %>% summarize(hpounds = sum(hpounds, na.rm = T)) %>%
  group_by(set_year) %>% summarize(med_hpounds = median(hpounds))
qps <- 400000 #get to the point where you can catch at least one pound of yelloweye
quotas$tac <- quotas$tac_prop * qps

#Start with only clusters out of astoria, ten vessels, 
# ast <- filt_clusts %>% filter(d_port == 'ASTORIA / WARRENTON')

#Select different clusters based on proximity to current cluster...

#Start of decision framework function
#58 good
#265 ok 
#295 shitty

#Inputs are 
#Start in a shitty cluster
first_cluster <- ast %>% filter(unq_clust == 295)

#Tools to profile functions
devtools::install_github("hadley/lineprof")
library(lineprof)
library(pryr)
mem_used()

#Can start looking at how these things compare
#Compare catch:TAC, cumulative profits

xx <- fish_trip(ntows = 20, scope = 5, quotas = quotas1, seed = 300, scale = "port")

yy <- fleet_trip(scope = 1, quotas = quotas, seed = 300, scale = 'fleet', prob_type = "type_clust_perc",
  ntows = 20)

#---------------------------------------------------------------------------------
#Figures used to compare things
#can compare straight up profits



#---------------------------------------------------------------------------------
#Test consistency of algorithm by fishing in the best cluster multiple times until
# catch:TAC hit for something; baseline for expectations

#---------------------------------------------------------------------------------
#Specify arguments as a list
in_list <- list(nreps = 6, ncores = 6, ntows = 20, scope = 5, 
  scale = 'scope', the_port = "ASTORIA / WARRENTON", prob_type = "type_clust_perc",
  quotas = quotas)

run_trip_simulation <- function(in_list){
  #Run simulation
  start_time <- Sys.time()
  the_runs <- mclapply(1:in_list$nreps, 
    FUN = function(seeds) fish_trip(ntows = in_list$ntows, scope = in_list$scope, 
      quotas = quotas, seed = seeds, scale = in_list$scale, prob_type = in_list$prob_type,
    the_port = in_list$the_port), 
    mc.cores = in_list$ncores)
  run_time <- Sys.time() - start_time

  #Store the catch results in a list
  catches <- lapply(the_runs, FUN = function(x) x[[1]])
  catches <- list_to_df(catches, ind_name = "rep", 
    col_ind_name = 'rep_id')
  catches$rep_id <- factor(catches$rep_id, 
    levels = unique(catches$rep_id))
  catches$tow <- as.numeric(substr(catches$tow_num, 4, 
    nchar(catches$tow_num)))

  #Store the quotas
  quotas <- lapply(the_runs, FUN = function(x) x[[2]])
  quotas <- list_to_df(quotas, ind_name = "rep", 
    col_ind_name = 'rep_id')
  quotas$rep_id <- factor(quotas$rep_id, 
    levels = unique(quotas$rep_id))

  return(list(catches = catches, quotas = quotas, run_time = run_time))
}


in_list1 <- list(nreps = 24, ncores = 6, ntows = 20, scope = 5, 
  scale = 'port', the_port = "ASTORIA / WARRENTON", prob_type = "type_clust_perc",
  quotas = quotas)

in_list2 <- list(nreps = 24, ncores = 6, ntows = 20, scope = 5, 
  scale = 'port', the_port = "ASTORIA / WARRENTON", prob_type = "type_prop_hauls",
  quotas = quotas)

#Make input list
in_list <- list(in_list1, in_list2)

#Run simulation with many replicates
res <- lapply(1:length(in_list), FUN = function(xx) run_trip_simulation(in_list[[xx]]))

#Compress and compare results with different strategies
catches <- lapply(1:length(res), FUN = function(xx) res[[xx]]$catches)
catches1 <- list_to_df(catches, ind_name = c("spp_perc", "prop_hauls"), col_ind_name = "prob_type") 

catches1 %>% group_by(prob_type, rep_id) %>% summarize(ntows = length(unique(tow_num)), 
  profits = sum(unique(profit_fuel_only)), med = median())

catches1 %>% group_by(prob_type, rep_id) %>% summarize(ntows = length(unique(tow_num)), 
  profits = sum(unique(profit_fuel_only))) %>% ggplot(aes(x = profits)) + 
  geom_histogram() +  
  facet_wrap(~ prob_type, ncol = 1) + geom_vline(aes(xintercept = median(profits)), col = 'red', lty = 2) 



str(res)
res[[1]]$catches %>% distinct(rep_id, haul_id, .keep_all = TRUE) %>% group_by(rep_id) %>%
  summarize(profit_fuel_only = sum(profit_fuel_only))
distinct(rep_id, tow, .keep_all = TRUE)

issa <- res[[1]]$quotas

#compare strategies



#Maybe calculate the proportion below a certain number
issa %>% ggplot() + geom_histogram(aes(x = ratio)) + facet_wrap(~ type)




#Do proportion less than 0.1 and skew
issa %>% group_by(type) %>% summarize(skew = calc_skew(log(catch))) %>% as.data.frame





lapply(res, FUN = function(xx) xx[[3]])







xx <- run_trip_simulation(in_list)

#At the port level, clusters should on average get to the same spot
start_time <- Sys.time()
catch_perc_reps <- mclapply(1:48, FUN = function(seeds) fish_trip(ntows = 20, scope = 10, 
  quotas = quotas, seed = seeds, scale = 'port'), mc.cores = 6)
run_time <- Sys.time() - start_time

#Look at catches
the_catch_lists <- lapply(catch_perc_reps, FUN = function(x) x[[1]])
the_catch_lists <- list_to_df(the_catch_lists, ind_name = "rep", 
  col_ind_name = 'rep_id')
the_catch_lists$rep_id <- factor(the_catch_lists$rep_id, 
  levels = unique(the_catch_lists$rep_id))
the_catch_lists$tow <- as.numeric(substr(the_catch_lists$tow_num, 4, 
  nchar(the_catch_lists$tow_num)))

the_catch_lists %>% distinct(rep_id, tow_num, .keep_all = T) %>% 
  group_by(rep_id) %>% summarize(profits = sum(profit_fuel_only)) %>% 
  ggplot() + geom_histogram(aes(profits))

the_catch_lists %>% distinct(rep_id, tow_num, .keep_all = T) %>% 
  ggplot(aes(x = avg_long, y = avg_lat)) + geom_point(aes(colour = tow)) + 
  facet_wrap(~ rep_id) + geom_map(data = states_map, map = states_map, 
         aes(x = long, y = lat, map_id = region)) + scale_x_continuous(limits = c(-126, -123.5)) + 
  scale_y_continuous(limits = c(45, 48))

the_quotas <- the_catch_lists <- lapply(catch_perc_reps, FUN = function(x) x[[2]])
the_quotas <- list_to_df(the_quotas, ind_name = "rep", 
  col_ind_name = 'rep_id')
the_quotas$rep_id <- factor(the_quotas$rep_id, 
  levels = unique(the_quotas$rep_id))

#Some things go way over
ggplot(the_quotas) + geom_histogram(aes(ratio)) + facet_wrap(~ rep_id)



#----------------------------------------------------------------------
#Repeat for proportion of hauls
start_time <- Sys.time()
prop_reps <- mclapply(1:48, FUN = function(seeds) fish_trip(ntows = 20, scope = 10, 
  quotas = quotas, seed = seeds, scale = 'port', prob_type = "type_prop_hauls"), 
mc.cores = 6)
run_time <- Sys.time() - start_time

#Look at catches
the_catch_lists <- lapply(prop_reps, FUN = function(x) x[[1]])
the_catch_lists <- list_to_df(the_catch_lists, ind_name = "rep", 
  col_ind_name = 'rep_id')
the_catch_lists$rep_id <- factor(the_catch_lists$rep_id, 
  levels = unique(the_catch_lists$rep_id))
the_catch_lists$tow <- as.numeric(substr(the_catch_lists$tow_num, 4, 
  nchar(the_catch_lists$tow_num)))
the_catch_lists %>% distinct(rep_id, tow_num, .keep_all = T) %>% 
  group_by(rep_id) %>% summarize(profits = sum(profit_fuel_only)) %>% 
  ggplot() + geom_histogram(aes(profits))



#Next step is to write this so 

#Which was the most caught thing
the_quotas <- lapply(reps, FUN = function(x) x[[2]])
the_quotas <- list_to_df(the_quotas, ind_name = 'rep', col_ind_name = 'rep_id')

the_quotas %>% filter(ratio >= 1)



the_catch_lists %>% group_by(rep_id) %>% summarize(ntows = length(unique(tow_num)))
 



ggplot()
xx[[1]] %>% head

xx[[2]]

#Manipulate the amount of available clusters

#Start in a dope one
yy <- fish_trip(ntows = 50, scope = 5, start_clust = 58, quotas = quotas)
yys <- yy %>% distinct(haul_id, .keep_all = T)

#
zz <- fish_trip(ntows = 50, scope = 1, start_clust = 835)
zzs <- zz %>% distinct(haul_id, .keep_all = T)

ggplot(xxs, aes(x = avg_long, y = avg_lat))  + geom_point() + geom_line() + 
  geom_point(data = yys, aes(x = avg_long, y = avg_lat), col = 'red') +
  geom_line(data = yys, aes(x = avg_long, y = avg_lat), col = 'red') +
  geom_point(data = zzs, aes(x = avg_long, y = avg_lat), col = 'green') +
  geom_line(data = zzs, aes(x = avg_long, y = avg_lat), col = 'green') 

filt_clusts %>% distinct(haul_id, .keep_all = TRUE) %>% group_by(unq_clust, d_port) %>%
  summarize(avg_prof = mean(profit_fuel_only)) %>% arrange(desc(avg_prof))

#Pull out haul_id
filt_clusts %>% distinct(haul_id, .keep_all = T) %>% filter(haul_id %in% 
  xx$haul_id) %>% ggplot() + geom_point(aes(x = avg_long, y = avg_lat)) + 
geom_line(aes(x = avg_long, y = avg_lat)) + 
geom_point(data = yys, aes(x = avg_long, y = avg_lat), col = 'red') + 
geom_line(data = yys, aes(x = avg_long, y = avg_lat), col = 'red')


unique(xx$set_long)



#Quantify risk based on 
runtime <- lineprof(fish_trip(ntows = 50))




#---------------------------------------------------------------------------------
#Start Developing movement rules
ggplot(filt_clusts) + geom_histogram(aes(x = haul_profit)) + facet_wrap(~ set_year)

#Which are the most profitable clusters?
filt_clusts %>% group_by(unq_clust) %>% summarize(avg_haul_profit = mean(unique(haul_profit),
  na.rm = TRUE), avg_long_clust = unique(avg_long_clust), avg_lat_clust = unique(avg_lat_clust),
avg_haul_value = mean(unique(haul_value), na.rm = T),
avg_haul_profit_fuel = mean(unique(haul_profit_fuel), na.rm = T)) -> prof_clusts

prof_clusts_m <- melt(prof_clusts, id.vars = c("unq_clust", 'avg_long_clust', 
  'avg_lat_clust'))

#Plot 
ggplot(prof_clusts_m) + geom_point(aes(x = avg_long_clust, y = avg_lat_clust, col = value),
  alpha = .5) + 
  scale_colour_gradient2(low = 'blue', mid = 'white', high = 'red') + facet_wrap(~ variable) +
  geom_map(data = states_map, map = states_map, 
         aes(x = long, y = lat, map_id = region)) + scale_x_continuous(limits = c(-126, -118)) + 
  scale_y_continuous(limits = c(30, 49))

# filt_clusts %>% arrange(haul_profit) %>% select(haul_id, haul_profit, d_port_clust_dist) %>% head

# filt_clusts %>% filter(haul_id == 133618) %>% as.data.frame %>% head 

#cluster 991 consistently catches lots of sablefish

###Need to add in distances from port

#Pick three clusters and see if I can get from the shitty one to the good one
#with certain rules
# filt_clusts %>% filter(d_port == "ASTORIA / WARRENTON") -> ast

#58 good
#265 ok 
#295 shitty

#start in shitty
#Add in economic data from 5 year review, 
#cost per day, per port, in each year, fuel scaled by distance traveled,
#like 3-64 of 5yr_review

#Assume perfect knowledge of the three clusters
filt_clusts %>% distinct(unq_clust) %>% group_by(unq_clust) %>% 
  avg_profit

prof_clusts %>% arrange(desc(avg_haul_profit))

#Sample a tow in the shitty cluster
first_clust <- filt_clusts %>% filter(unq_clust == 295) %>% 
  select(unq_clust, d_port, haul_id, species, hpounds, apounds, 
    haul_value, d_port_clust_dist, trip_id, haul_profit, profit_fuel_only)
sample_tow <- base::sample(unique(first_clust$haul_id), size = 1)

#Has knowledge of nearby clusters...
#Bin the clusters to be in 10x10 
#Add cluster group column, can expand the cluster group

first_clust %>% group_by(species) %>% summarize()

filter(haul_id == sample_tow) %>% as.data.frame

#


#Look at catch
first_clust %>% filter(trip_id %in% trip) %>% group_by(species) %>%
  summarize(hpounds = sum(hpounds, na.rm = T), haul_value = sum(unique(haul_value), na_rm = T))

filt_clusts[which(filt_clusts$trip_id == '961620'), ] %>% 





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




#Steps should be:
#1. Check individual quota,
#2. Look at profits
#3. Look at available knowledge
#4. Move depending on how close to your quota constraints you are




















# #------------------
# #Look at the difference between hpounds and apounds
# filt_clusts$ah_diff <- filt_clusts$apounds - filt_clusts$hpounds

# filt_clusts[which(is.na(filt_clusts$ah_diff)), 'ah_diff'] <- filt_clusts[which(is.na(filt_clusts$ah_diff)), 'apounds']
# filt_clusts[which(is.na(filt_clusts$ah_diff)), 'ah_diff'] <- filt_clusts[which(is.na(filt_clusts$ah_diff)), 'hpounds']


# filt_clusts %>% group_by(set_year, species, type) %>% summarize(hpounds = sum(hpounds, na.rm = T), 
#   apounds = sum(apounds, na.rm = T), ha_diff = apounds - hpounds) %>% arrange(desc(ha_diff)) %>%
#   filter(type == 'weaks', ha_diff != 0) %>% as.data.frame -> ha_diffs

# tow_ha_diffs <- filt_clusts %>% group_by(haul_id, species, type) %>% summarize(hpounds = sum(hpounds, na.rm = T), 
#   apounds = sum(apounds, na.rm = T), ha_diff = apounds - hpounds) %>% arrange(desc(ha_diff)) 

# trip_ha_diffs <- filt_clusts %>% group_by(trip_id, species, type) %>% summarize(hpounds = sum(hpounds, na.rm = T), 
#   apounds = sum(apounds, na.rm = T), ha_diff = apounds - hpounds) %>% arrange(desc(ha_diff)) 

# trip_ha_diffs %>% filter(type == 'targets') %>% head

#    %>%
#   filter(type == 'weaks', ha_diff != 0) %>% as.data.frame -> ha_diffs
 
#   ah_diff = mean(ah_diff)) %>% 
#   arrange(desc(ah_diff)) %>% filter(type == 'weaks') %>% as.data.frame


# filt_clusts







#Scarps

# #Calculate number of days per boat
# ndays_fishing <- filt_clusts %>% distinct(trip_id, .keep_all = T) %>% 
#   group_by(ryear, drvid) %>% summarize(ndays_fishing = sum(trip_duration_days)) %>% 
#   group_by(ryear) %>% mutate(avg_days_per_year = mean(ndays_fishing))

# filt_clusts <- filt_clusts %>% left_join(ndays_fishing, by = c('ryear', 'drvid'))

# #Pretty close I think
# filt_clusts %>% distinct(haul_id, .keep_all = T) %>% group_by(drvid, ryear) %>%   
#   summarize(ndays = length(unique(set_date)))

# filt_clusts %>% group_by(drvid, ryear) %>% summarize(ndays = length(unique(set_date)))


# #Calculate profits per boat
# filt_clusts %>% distinct(haul_id, .keep_all = TRUE) %>% group_by(drvid, ryear) %>%
#   mutate(value = sum(haul_value), 
#     cost = sum(buyback_per_tow, captain_per_tow, crew_per_tow, 
#       observer_per_tow, scaled_fuel_per_clust_per_tow, na.rm = TRUE),
#     profit = value - cost) %>% 
#   group_by(ryear) %>% summarize(avg_profit_per_boat = sum(profit) / length(unique(drvid)),
#     avg_profit_per_boat_per_day = avg_profit_per_boat / unique(avg_days_per_year))
  


#   group_by(drvid, ryear, trip_id) %>% mutate(unq_days_per_trip = unique(trip_duration_days)) %>% 
#   group_by(drvid, ryear) %>% mutate(ndays_per_boat = sum(unq_days_per_trip)) %>% 
#   select(ndays_per_boat)
#   group_by(ryear) %>% 
#   summarize(avg_profit_per_boat = sum(profit) / length(unique(drvid)) / )
# filt_clusts %>% distinct(haul_id, .keep_all = TRUE) %>% group_by(drvid, ryear)

