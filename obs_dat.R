#---------------------------------------------------------------------------------
#Specify Computer
setwd('/Users/peterkuriyama/School/Research/ch4')
# setwd('c://Users//Peter//ch4')

# list.files("//udrive.uw.edu//udrive//file_clusts_dist.Rdata")

#---------------------------------------------------------------------------------
#Start of obs_dat, work off this script
#Load Packages
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(devtools)
library(maps)
library(doParallel)
library(lubridate)
library(GGally)
library(nnet)
library(tidyr)
library(mlogit)

#Install ch4 package
devtools::install_github("peterkuriyama/ch4", auth_token = "83f947b716e40172803f0ff798c46f5ff9ca3cd1")
library("ch4")

#Install delta plot functions
devtools::install_github("peterkuriyama/ch2vms/ch2vms")
library(ch2vms)

#States Map
states_map <- map_data("state")

#---------------------------------------------------------------------------------
#Load and format data

#Tows clust, more complete than filt_clusts
# load("output/tows_clust.Rdata")

# #Consolidate species records in tows_clust
# tows_clust <- tows_clust %>% group_by(haul_id, species) %>%
#   mutate(apounds = sum(apounds, na.rm = T), hpounds = sum(hpounds, na.rm = T)) %>%
#   distinct(haul_id, species, .keep_all = T) %>% as.data.frame

# #Replace Princeton to keep consistent
# tows_clust[which(tows_clust$d_port == "PRINCETON (HALF MOON BAY)"), 'd_port'] <- "PRINCETON / HALF MOON BAY"
# tows_clust[which(tows_clust$r_port == "PRINCETON (HALF MOON BAY)"), 'r_port'] <- "PRINCETON / HALF MOON BAY"

# #More formatting in ch4_movement
# # load("//udrive.uw.edu//udrive//file_clusts_dist.Rdata")
# load("output/file_clusts_dist.Rdata")

# filt_clusts <- filt_clusts_dist
# rm(filt_clusts_dist)
# # names(filt_clusts)[grep('hpounds', names(filt_clusts))]
# filt_clusts$hpounds.y <- NULL
# filt_clusts$apounds.y <- NULL

# names(filt_clusts)[grep('hpounds', names(filt_clusts))] <- 'hpounds'
# names(filt_clusts)[grep('apounds', names(filt_clusts))] <- 'apounds'

# load("//udrive.uw.edu//udrive//quotas.Rdata")

# load('output/quotas.Rdata')
# quotas$tac <- quotas$tac_prop * 100000
# filt_clusts <- filt_clusts %>% ungroup

# source('R/dist_funcs.R')
# source("R/rum_probs_nodummy.R")

# #Format 2013 quotas
# q13 <- read.csv('data/quotas_2013.csv', stringsAsFactors = FALSE)
# names(q13) <- c('year', 'permit_id', 'permit_owner', 'species', 'qs_percent', 'tac',
#   'current_qs', 'qs_balance', 'mts')
# unique(q13$species)

# q13$new_species <- c('Arrowtooth Flounder', "Bocaccio Rockfish", "Canary Rockfish", 
#   "Chilipepper Rockfish", "Cowcod Rockfish", "Darkblotched Rockfish", "Dover Sole", 
#   "English Sole", "Lingcod", "Lingcod", "Longspine Thornyhead", "Minor Shelf North",
#   "Minor Shelf South", "Minor Slope North", 'Minor Slope South', "Other Flatfish", "Pacific Cod", 
#   "Pacific Halibut", "Pacific Ocean Perch", "Pacific Whiting", "Petrale Sole", 'Sablefish', "Sablefish",
#   "Shortspine Thornyhead", "Shortspine Thornyhead", "Splitnose Rockfish", "Starry Flounder", 
#   "Widow Rockfish", "Yelloweye Rockfish", "Yellowtail Rockfish")

# tnc_q13 <- q13 %>% filter(permit_owner == "THE NATURE CONSERVANCY")
# tnc_q13$tac <- gsub("\\,", "", tnc_q13$tac)
# tnc_q13$tac <- as.numeric(tnc_q13$tac)
# tnc_q13$species <- tnc_q13$new_species

# quotas_mb <- quotas %>% select(-tac) %>% left_join(tnc_q13 %>% select(species, tac), by = 'species')
# quotas_mb <- quotas_mb %>% group_by(species, type) %>% summarize(tac = sum(tac))

#Format the species names

#---------------------------------------------------------------------------------
#Add in monthly prices

# prices <- read.csv('data/monthly_prices.csv', stringsAsFactors = FALSE)
# names(prices) <- tolower(names(prices))
# names(prices)[2] <- "dport"
# names(prices)[3] <- "species"

# #Check Species names
# unique(prices$species)[which(unique(prices$species) %in% unique(filt_clusts$species) == FALSE)]

# #Change POP and shortspine/longspine thornyheads
# prices[grep("Pacific Ocean Perch", prices$species), 'species'] <- "Pacific Ocean Perch"
# prices[grep("/ Longspine Thorny", prices$species), 'species'] <- "Shortspine/Longspine Thornyhead"

# #Check port names
# unique(prices$dport)[unique(prices$dport) %in% unique(filt_clusts$dport_desc) == FALSE]
# prices[grep("ASTORIA", prices$dport), 'dport'] <- "ASTORIA / WARRENTON"
# prices[grep("Charleston", prices$dport), 'dport'] <- "CHARLESTON (COOS BAY)"

# # #Add it into filt_clusts
# # filt_clusts$exval_pound <- NULL

# # names(prices)[9] <- 'exval_pound'
# prices <- plyr::rename(prices, c("year" = 'ryear', 'month' = 'rmonth', 'dport' = 'r_port', 
#   'price' = 'exval_pound'))

# #Add prices into tows_clust
# #Fill in price data so there are values for each previous month
# prices_expanded <- prices %>% expand(r_port, species, ryear, rmonth)
# prices_expanded <- prices_expanded %>% left_join(prices %>%
#   select(r_port, species, ryear, rmonth, exval_pound), by = c("r_port", "species", 'ryear',
#   'rmonth'))

# #Look at filt_clusts when available
# annual_prices <- filt_clusts %>% ungroup %>% 
#   distinct(r_port, ryear, species, rmonth, exval_pound) %>%
#   as.data.frame
# names(annual_prices)[5] <- 'annual_price'

# prices_expanded <- prices_expanded %>% left_join(annual_prices, by = c("r_port",
#   "ryear", 'species', 'rmonth'))

# #Use annual prices in the case of missing monthly prices
# prices_expanded <- prices_expanded %>% group_by(r_port, species, ryear) %>% fill(annual_price) 

# prices_expanded <- prices_expanded %>% group_by(species) %>% 
#   mutate(species_avg_price = mean(exval_pound, na.rm = T)) %>% 
#   group_by(species, r_port) %>% mutate(species_port_avg_price = mean(exval_pound, na.rm = T)) %>%
#   group_by(species, ryear) %>% mutate(species_year_avg_price = mean(exval_pound, na.rm = T)) %>%
#   as.data.frame

# #Fill in missing exval_pound values with: first
# #1. The annual species average value
# na_ind <- which(is.na(prices_expanded$exval_pound))
# prices_expanded[na_ind, 'exval_pound'] <- prices_expanded[na_ind, 'species_year_avg_price']

# #2. The species average value 
# na_ind <- which(is.na(prices_expanded$exval_pound))
# prices_expanded[na_ind, 'exval_pound'] <- prices_expanded[na_ind, 'species_avg_price']

# #Now add into tows_clust
# prices_expanded <- prices_expanded %>% select(r_port, species, ryear, rmonth, exval_pound)

# tc1 <- tows_clust %>% left_join(prices_expanded, by = c("r_port", "species",
#   'ryear', 'rmonth'))

# tows_clust$haul_spp <- paste(tows_clust$haul_id, tows_clust$species)
# tc1$haul_spp <- paste(tc1$haul_id, tc1$species)

# #Remove the duplicated values
# tows_clust <- tc1 %>% distinct(haul_spp, .keep_all = T) 
# rm(tc1)

# #---------------------------------------------------------------------------------
# #Add in avg_quota price, exval_pound, d_port_long, d_port_lat, set_date
# #Exval_pound added above
# filt_clusts <- filt_clusts %>% ungroup

# ###Add in avg_quota prices
# qps <- filt_clusts %>% ungroup %>% filter(type %in% c('weaks')) %>% distinct(species, avg_quota_price)
# qps <- subset(qps, species != "Cowcod Rockfish")
# tows_clust <- tows_clust %>% left_join(qps, by = 'species')

# ###Add in d_port_long and lats
# port_lat_long <- filt_clusts %>% distinct(d_port, d_port_lat, d_port_long) %>% as.data.frame
# tows_clust <- tows_clust %>% left_join(port_lat_long, by = "d_port")

# ###Add in date
# tows_clust$set_date <- paste(tows_clust$set_year, tows_clust$set_month, tows_clust$set_day, sep = "-")
# tows_clust$set_date <- ymd(tows_clust$set_date)

#---------------------------------------------------------------------------------
#Bin tows_clust, then back assign the tows
# tows_clust_bin <- bin_data(tows_clust, x_col = 'avg_long', y_col = 'avg_lat', group = 'set_year', grid_size = c(.0909, .11),
#   group_vec = 2007:2014)

# #Back assign the clustered values
# unq_bins <- tows_clust_bin %>% distinct(unq, .keep_all = T)

# #Loop through all the unq_bins rows
# tows_clust$unq <- "999"
# tows_clust$bin_x <- "999"
# tows_clust$bin_y <- "999"

# for(ii in 1:nrow(unq_bins)){
#   tb <- unq_bins[ii, ]
#   the_inds <- which(tows_clust$avg_long > tb$xmin & tows_clust$avg_long < tb$xmax &
#     tows_clust$avg_lat > tb$ymin & tows_clust$avg_lat < tb$ymax)
#   tows_clust[the_inds, 'unq'] <- tb$unq
#   tows_clust[the_inds, 'bin_x'] <- tb$x
#   tows_clust[the_inds, 'bin_y'] <- tb$y
# }

# tows_clust$bin_x <- round(as.numeric(tows_clust$bin_x), digits = 4)
# tows_clust$bin_y <- round(as.numeric(tows_clust$bin_y), digits = 4)

# #---------------------------------------------------------------------------------
# #Add in type for tows_clust
# tows_clust$type <- NULL
# # tows_clust$type <- "other"

# the_types <- filt_clusts %>% filter(type != 'other') %>% distinct(species, type) %>% as.data.frame
# tows_clust <- tows_clust %>% left_join(the_types, by = 'species')
# tows_clust[which(is.na(tows_clust$type)), 'type'] <- "other"

# #Some prices exval pound values are still NAs?
# exval_nas <- which(is.na(tows_clust$exval_pound))

# unq_clusts <- tows_clust %>% distinct(dport_desc, clust) %>% mutate(unq_clust = 1:length(clust))
# tows_clust <- tows_clust %>% left_join(unq_clusts, by = c('dport_desc', 'clust'))

# save(tows_clust, file = "/Volumes/udrive/tows_clust_921.Rdata")

# load(file = "/Volumes/udrive/tows_clust_921.Rdata")

# #Add in depth bands to tows_clust
# depths <- data.frame(depth_band = c(1, 2, 3, 4, 5, 6, 7),
#   min_depth = c(0, 50, 100, 150, 200, 300, 500), 
#   max_depth = c(50, 100, 150, 200, 300, 500, 700))

# tows_clust$depth_bin <- 69

# for(dd in 1:nrow(depths)){
#   temp_depth <- depths[dd, ]
#   tows_clust[which(tows_clust$avg_depth >= temp_depth$min_depth & 
#           tows_clust$avg_depth < temp_depth$max_depth), 'depth_bin'] <- temp_depth$depth_band
# }

# unique(tows_clust$depth_bin)

# tows_clust[which(tows_clust$depth_bin == 69), 'avg_depth'] 
save(tows_clust, file = "/Volumes/udrive/tows_clust_925_depth_bin.Rdata")
#---------------------------------------------------------------------------------

load(file = "/Volumes/udrive/tows_clust_925_depth_bin.Rdata")

#Add in depth bins to tows_clust

mb1 <- sampled_rums(data_in = tows_clust, the_port = c("MOSS LANDING", 'SAN FRANCISCO'), min_year = 2011, max_year = 2014,
  risk_coefficient = 1, ndays = 30, focus_year = 2013, 
  nhauls_sampled = 50, seed = 310, ncores = 6)


# names(prices)[names(prices) %in% names(filt_clusts)]

# filt_clusts1 <- filt_clusts %>% left_join(prices %>% select(species, r_port, rmonth, ryear, exval_pound), 
#   by = c('species', 'r_port', 'ryear', 'rmonth'))


# filt_clusts1$dupes <- paste(filt_clusts1$haul_id, filt_clusts1$species)
# filt_clusts1[which(duplicated(filt_clusts1$dupes)), ]

# filt_clusts1 %>% filter(haul_id == '139802', species == 'Shortspine/Longspine Thornyhead') %>% 
#   select(hpounds)

# filt_clusts1 <- filt_clusts1[-which(duplicated(filt_clusts1$dupes)), ]

# #Check that the pounds are the same
# filt_clusts %>% group_by(set_year) %>% summarize(tot_pounds = sum(apounds))
# filt_clusts1 %>% group_by(set_year) %>% summarize(tot_pounds = sum(apounds))

# #Remove the dupes column and save filt_clusts
# filt_clusts <- filt_clusts1

# rm(filt_clusts1)
# save(filt_clusts, file = 'output/filt_clusts_w_monthly_prices.Rdata')

#---------------------------------------------------------------------------------
#Add in the filt_clusts
# load(file = 'output/filt_clusts_w_monthly_prices.Rdata')

#Check that 
filt_clusts$dupes <- NULL

filt_clusts %>% group_by(set_year) %>% summarize(nhauls = length(unique(haul_id)))


#Add dummy variables for previously fished clusters to filt_clusts
# fc_dummy <- filt_clusts %>% distinct(haul_id, .keep_all = T) %>% 
#  select(dport_desc, unq_clust, set_date, haul_id) 

# load(file = 'output/dummy_30.Rdata')
# fc_dummy$dummy_30 <- dummy_30

#Add this into filt_clusts
# filt_clusts <- filt_clusts %>% left_join(fc_dummy %>% select(haul_id, dummy_30), by = 'haul_id')


#---------------------------------------------------------------------------------
#Start of resampling function
ch4_ctl <- function(rc = 1, port, years, ndays1, the_seeds, ncores, rum_func = rum_probs, 
  focus_year = 2013){
  outs <- list(rc = rc, port = port, years = years, 
    ndays1 = ndays1, the_seeds = the_seeds, ncores = ncores, rum_func = rum_func, 
    focus_year = focus_year)
  return(outs)
}

#--------------------------------------------------------------------------------- 
#morro bay runs in like 20 seconds
#Newport takes like 3 minutes to run this

# rc0 <- sampled_rums(data_in = filt_clusts, the_port = 'NEWPORT', min_year = 2011, max_year = 2014,
#   risk_coefficient = 0, ndays = 30, focus_year = 2013, 

#   nhauls_sampled = 50, seed = 310, ncores = 6)

st_time <- Sys.time()
newport1 <- sampled_rums(data_in = filt_clusts, the_port = 'NEWPORT', min_year = 2011, max_year = 2014,
  risk_coefficient = 1, ndays = 30, focus_year = 2013, 
  nhauls_sampled = 50, seed = 310, ncores = 6)
r_time <- Sys.time() - st_time
r_time

start_clust <- Sys.time()
newport1_clust <- sampled_rums_clust(data_in = filt_clusts, the_port = 'NEWPORT', min_year = 2011, max_year = 2014,
  risk_coefficient = 1, ndays = 30, focus_year = 2013, 
  nhauls_sampled = 50, seed = 310, ncores = 6)
run_clust <- Sys.time() - start_clust
run_clust

#--------------------------------------------------------------------------------- 
#Try gettting this to run with multiple ports 
the_port <- c("MOSS LANDING", 'SAN FRANCISCO')

mb1 <- sampled_rums(data_in = filt_clusts, the_port = "MORRO BAY", 
  min_year = 2011, max_year = 2014,
  risk_coefficient = 1, ndays = 30, focus_year = 2013, 
  nhauls_sampled = 50, seed = 310, ncores = 6)

#--------------------------------------------------------------------------------- 

moss_sf_310 <- sampled_rums(data_in = filt_clusts, the_port = c("MOSS LANDING", "SAN FRANCISCO"), 
  min_year = 2010, max_year = 2014,
  risk_coefficient = 1, ndays = 30, focus_year = 2012, 
  nhauls_sampled = 50, seed = 310, ncores = 6)

moss_sf_311 <- sampled_rums(data_in = filt_clusts, the_port = c("MOSS LANDING", "SAN FRANCISCO"), 
  min_year = 2010, max_year = 2014,
  risk_coefficient = 1, ndays = 30, focus_year = 2012, 
  nhauls_sampled = 50, seed = 311, ncores = 6)

moss_sf_309 <- sampled_rums(data_in = filt_clusts, the_port = c("MOSS LANDING", "SAN FRANCISCO"), 
  min_year = 2010, max_year = 2014,
  risk_coefficient = 1, ndays = 30, focus_year = 2012, 
  nhauls_sampled = 50, seed = 309, ncores = 6)
#--------------------------------------------------------------------------------- 
ports <- c("NEWPORT", "ASTORIA / WARRENTON", "EUREKA", "CHARLESTON (COOS BAY)")
rcs <- c(1, 50)

to_run <- expand.grid(ports, rcs)
to_run[, 1] <- as.character(to_run[, 1])
to_run[, 2] <- as.integer(to_run[, 2])
to_run <- as.data.frame(to_run)

names(to_run) <- c('port', "risk_coefficient")

t1 <- Sys.time()
runs <- lapply(1:8, FUN = function(yy){
  thing <- sampled_rums(data_in = filt_clusts, the_port = to_run[yy, 'port'],
    min_year = 2011, max_year = 2014, risk_coefficient = to_run[yy, 'risk_coefficient'],
    ndays = 30, focus_year = 2013, nhauls_sampled = 50, seed = 310, ncores = 6)
  return(thing)
})
t2 <- Sys.time() - t1; t2
save(runs, file = 'output/runs.Rdata')


thing <- sampled_rums(data_in = filt_clusts, the_port = to_run[yy, 'port'],
    min_year = 2011, max_year = 2014, risk_coefficient = to_run[yy, 'risk_coefficient'],
    ndays = 30, focus_year = 2013, nhauls_sampled = 50, seed = 310, ncores = 6)



summary(morro0[[2]])$CoefTable[, 4]

str(coef(morro0[[2]]))

morro1 <- sampled_rums_pc(data_in = filt_clusts, the_port = 'MORRO BAY', min_year = 2011, max_year = 2014,
  risk_coefficient = 1, ndays = 30, focus_year = 2013, 
  nhauls_sampled = 50, seed = 310, ncores = 6)


#Extract coefficients and see how they compare to Dan's 
newport1 <- sampled_rums(data_in = filt_clusts, the_port = 'NEWPORT', min_year = 2011, max_year = 2014,
  risk_coefficient = 1, ndays = 30, focus_year = 2013, 
  nhauls_sampled = 50, seed = 310, ncores = 6)

eureka1 <- sampled_rums(data_in = filt_clusts, the_port = 'EUREKA', min_year = 2010, max_year = 2014,
  risk_coefficient = 1, ndays = 30, focus_year = 2012, 
  nhauls_sampled = 50, seed = 310, ncores = 6)

#--------------------------------------------------------------------------------- 
#Then sample a training data set in proportion to tows in each cluster
#use this to see if on average vessels go towards certain clusters more often than others
start_time <- Sys.time()
# rc_ast <- sampled_rums(data_in = filt_clusts, the_port = 'ASTORIA / WARRENTON', 
  # min_year = 2011, max_year = 2014,
  # risk_coefficient = 1, ndays = 30, focus_year = 2013, 
  # nhauls_sampled = 50, seed = 310)
run_time <- Sys.time() - start_time; run_time

rc1 <- sampled_rums(data_in = filt_clusts, the_port = 'MORRO BAY', min_year = 2011, max_year = 2014,
  risk_coefficient = 1, ndays = 30, focus_year = 2013, 
  nhauls_sampled = 50, seed = 310)

dat_set1 <- training_data(data_in = filt_clusts, the_port = 'MORRO BAY', min_year = 2011, max_year = 2014,
  risk_coefficient = 1, ndays = 30, focus_year = 2013, 
  nhauls_sampled = 50, seed = 301)


#----------------------------------------------------------------------------------------------------
#Sample Tows from sampled_rums results and training data set
#Can sample all 1000 morro bay tows in 1 hour 20 minutes
# 300:(300 + (6 * 10) - 1)

the_seeds <- 300:(300 + (6 * 10) - 1)

start_time <- Sys.time()
replicates <- mclapply(the_seeds, FUN = function(xx){
  temp_samps <- resample_training_data(training_data = dat_set1, rum_results = rc1, seed = xx)
  temp_samps <- temp_samps %>% filter(is.na(tac) == FALSE)
  return(temp_samps)
}, mc.cores = 6)
replicates1 <- list_to_df(replicates, the_seeds, col_ind_name = 'seeds')

run_time <- Sys.time - start_time; run_time

#Plot replicate values
replicates1 %>% distinct(seeds, date_haul_id, .keep_all = T) %>% group_by(seeds) %>%
  summarize(revenues = sum(haul_net_revenue)) %>% ggplot() + geom_histogram(aes(x = revenues))

save(replicates1, file = 'output/morro_bay_rc1.Rdata')

#Run this tonight while eating dinner maybe for 1000


#----------------------------------------------------------------------------------------------------
#Summary Values
#Some tows might not be over the tac
#Filter keeping only the catches that are below the tac
sampled_catches <- sampled_catches %>% filter(is.na(tac) == FALSE)

#Catch:TAC
sampled_catches %>% group_by(species) %>% summarize(tot_catch = sum(hpounds), tac = unique(tac),
  ctac = round(tot_catch / tac, digits = 3)) 

revenues <- sampled_catches %>% distinct(haul_id, .keep_all = TRUE) %>% select(haul_net_revenue) %>% sum

#----------------------------------------------------------------------------------------------------




catch_over <- sampled_catches[which(sampled_catches$cum_catch >= sampled_catches$tac), "date_haul_id"] %>% min

under_catches <- sampled_catches %>% filter(date_haul_id <= catch_over)



#Calculate final catch:TAC
under_catches %>% filter(is.na(tac) == FALSE) %>% group_by(species, type) %>% 
  summarize(tot_catch = sum(hpounds), tac = sum(tac, na.rm = TRUE))


under_catches %>% distinct(species, type, tac)
under_catches %>% group_by(species, type) %>% 

summarize(tot_catch = sum(hpounds))


under_catches %>% filter(date_haul_id == catch_over) %>% mutate(ctac = cum_catch / tac) %>%
  filter(type %in% c('targets', 'weaks')) %>% ggplot() + geom_histogram(aes(x = ctac)) + 
  facet_wrap(~ type)
  


sampled_catches <- sampled_catches %>% as.data.frame









# date_of_hauls <- sampled_catches %>% ungroup %>% distinct(haul_id, .keep_all = T) %>% 
#   select(haul_id, fished_haul_date) %>%
#   mutate(date_haul_id = 1:length(haul_id))


#Calculate the haul values in each  


# clust_samples$fished_haul_date <- clust_samples$set_date





#what is the assumption about order of tows?
#Pick cluster based on revenue expectations and distance
sampled_catches <- sampled_catches %>% left_join(clust_samples %>%
  select(unq_clust, clust_haulnum, fished_haul_date), by = c('unq_clust', 'clust_haulnum'))

#See how much of quota is caught?




sampled_catches <- sampled_catches %>% left_join(date_of_hauls, by = c('haul_id', 'fished_haul_date')) %>%
  as.data.frame


#Find place that a cumulative quota goes over quota
sampled_catches <- sampled_catches %>% filter(is.na(tac) == FALSE)

#some of the date_haul_id values are NA

date_of_hauls %>% filter(fished_haul_date == ymd("2013-03-19"))

sampled_catches %>% filter(is.na(date_haul_id)) %>% select(haul_id, fished_haul_date)


#
sampled_catches[which(sampled_catches$cum_catch >= sampled_catches$tac), ]

catches_under_quota <- 
sampled_ca


sampled_catches[which(sampled_catches$cum_catch >= sampled_catches$tac), "date_haul_id" ]

















#Could sum the probabilities of fishing in each cluster
dat_set1[[1]] %>% group_by(fished_haul, unq_clust) %>% mutate(probs = sum(probs))

#Sample tows to fish in






#--------------------------------------------------------------------------------- 
#Pick it up here
head(dat_set1[[2]])

dat_set1[[1]] 



first_tows1 <- dat_set1[[1]]
second_tows1 <- dat_set1[[2]]

first_tows1 <- lapply(unique(first_tows1$fished_haul), FUN = function(xx){
  temp <- first_tows1 %>% filter(fished_haul == xx)
  probs <- predict(rc100[[3]], temp)
  temp$probs <- probs
  return(temp)
})

first_tows1


one_tow1 <- dat_set1[[1]] %>% filter(fished_haul == 128004)

dat_set100 <- training_data(data_in = filt_clusts, the_port = 'MORRO BAY', min_year = 2011, max_year = 2014,
  risk_coefficient = 100, ndays = 30, focus_year = 2013, 
  nhauls_sampled = 50, seed = 301)

one_tow100 <- dat_set100[[1]] %>% filter(fished_haul == 128004)



predict(rc1[[3]], newdat = one_tow1)
predict(rc100[[3]], newdat = one_tow100)
(set_set1[[1]])

#See how to add probabilities
one_tow100 <- dat_set100[[1]] %>% filter(fished_haul %in% c(128004))
two_tow100 <- dat_set100[[1]] %>% filter(fished_haul %in% c(128004, 128112))
predict(rc100[[3]], newdat = one_tow100)

#Calculate probabilities of fishing
dat_set100[[1]] <- lapply(unique(dat_set100[[1]]$fished_haul), FUN = function(xx){
  temp <- two_tow100 %>% filter(fished_haul == xx)
  probs <- predict(rc100[[3]], temp)
  temp$probs <- probs
  return(temp)
})
two_tow100 <- ldply(two_tow100)

two_tow100 %>% group_by(fished_haul) %>% summarize(probsum = sum(probs))

two_tow100 %>% group_by(fished_haul) %>% do({
  probs <- predict(rc100[[3]], newdat = .)
  # data_frame(., probs)
})


mutate(probs = predict(rc100[[3]], newdat = .))


unique(dat_set100[[1]]$fished_haul)


#--------------------------------------------------------------------------------- 
#Check the probabilities associated with each risk coefficient
#Way to compare the probabilities
ctl_list <- ch4_ctl(rc = 0, port = "MORRO BAY", years = c(2012, 2013), ndays1 = 30,
  the_seeds = 300, ncores = 1, rum_func = rum_probs)

probs <- mclapply(c(0, 1, 100, 200), FUN = function(xx){
  temp_list <- ctl_list
  temp_list$rc <- xx

  temp_outs <- temp_list$rum_func(rc = temp_list$rc, port = temp_list$port,
    years = temp_list$years, fc = filt_clusts, ndays1 = temp_list$ndays1)
  return(temp_outs)
}, mc.cores = 4)

#FIRST tow probabilities in each cluster
first_tow_probs <- list_to_df(lapply(probs, FUN = function(yy) yy[[1]]), ind_name = c(0, 1, 100, 200),
  col_ind_name = "risk_coefficient")
first_tow_probs$probs <- round(first_tow_probs$probs, digits = 3)
first_tow_probs$risk_coefficient <- as.integer(first_tow_probs$risk_coefficient)
first_tow_probs %>% dcast(unq_clust ~ risk_coefficient, value.var = 'probs')

#SECOND tow probabilities in each cluster
second_tow_probs <- list_to_df(lapply(probs, FUN = function(yy) yy[[2]]), ind_name = c(0, 1, 100, 200),
  col_ind_name = "risk_coefficient")
second_tow_probs$probs <- round(second_tow_probs$probs, digits = 3)
second_tow_probs$risk_coefficient <- as.integer(second_tow_probs$risk_coefficient)
second_tow_probs %>% dcast(unq_clust ~ risk_coefficient, value.var = 'probs')


probs[[1]][[6]] %>% head
probs[[2]][[6]] %>% head
probs[[3]][[6]] %>% head

coef(probs[[1]][[4]])[1] / coef(probs[[1]][[4]])[2]

#Dig into one run
ctl_list <- ch4_ctl(rc = 200, port = "MORRO BAY", years = c(2012, 2013),
  ndays1 = 30, the_seeds = 300:305, ncores = 6, rum_func = rum_probs)
the_probs <- ctl_list$rum_func(rc = ctl_list$rc, port = ctl_list$port,
  years = ctl_list$years, fc = filt_clusts, ndays1 = ctl_list$ndays1)

ctl_list <- ch4_ctl(rc = 1, port = "MORRO BAY", years = c(2012, 2013),
  ndays1 = 30, the_seeds = 300:305, ncores = 6, rum_func = rum_probs)
the_probs1 <- ctl_list$rum_func(rc = ctl_list$rc, port = ctl_list$port,
  years = ctl_list$years, fc = filt_clusts, ndays1 = ctl_list$ndays1)

ctl_list <- ch4_ctl(rc = 100, port = "MORRO BAY", years = c(2012, 2013),
  ndays1 = 30, the_seeds = 300:305, ncores = 6, rum_func = rum_probs)





the_probs5 <- ctl_list$rum_func(rc = ctl_list$rc, port = ctl_list$port,
  years = ctl_list$years, fc = filt_clusts, ndays1 = ctl_list$ndays1)



#Compare the data going in
the_probs1[[5]] %>% head(n = 10)
the_probs5[[5]] %>% head(n = 10)

coef(the_probs[[4]])
coef(the_probs1[[4]])
coef(the_probs5[[4]])


#Plot the values for comparison
the_probs[[1]]
the_probs1[[1]]
the_probs5[[1]]

the_probs[[2]]
the_probs1[[2]]
the_probs5[[2]]

filt_clusts %>% filter(haul_id == 128005, type %in% c('targets', 'weaks')) %>% select(species)

%>% select(species, )


the_probs[[6]] %>% head
the_probs1[[6]] %>% head(n = 15)

coef(the_probs[[4]])

coef(the_probs1[[4]])

the_probs[[6]] %>% head
the_probs1[[6]] %>% head


the_probs5[[1]] %>% filter(dummy == 1) %>% ggplot() + geom_histogram(aes(x = revs))

sum(the_probs1[[2]]$probs)

the_probs1[[2]] %>% left_join(the_probs5[[2]], by = 'unq_clust') %>% 
  mutate(diffs = round(probs.x - probs.y, digits = 4))

#---------------------------------------------------------------------------------
#Try some actual runs

diff_risk_values <- function(rc_values, input_ctl){

  #Change rc value
  temp_res <- lapply(rc_values, FUN = function(xx){
    temp_ctl <- input_ctl
    temp_ctl$rc <- xx

    #Run the model now
    temp <- sample_catches(ctl_list = temp_ctl)

    return(temp)
  })

  #Parse the outputs from each thing
  catch_samples <- lapply(temp_res, FUN = function(xx) xx[[1]])
  catch_samples <- list_to_df(catch_samples, ind_name = rc_values, col_ind_name = "risk_coefficient")
  #Filter out bycatch also
  catch_samples <- catch_samples %>% filter(type != 'other')

  run_times <- lapply(temp_res, FUN = function(xx) xx[[2]])
  run_times <- list_to_df(run_times, ind_name = rc_values, col_ind_name = "risk_coefficient")
  
  first_tow_model <- lapply(temp_res, FUN = function(xx) xx[[3]])
  second_tow_model <- lapply(temp_res, FUN = function(xx) xx[[4]])

  first_tow_probs <- lapply(temp_res, FUN = function(xx) xx[[5]])
  first_tow_probs <- list_to_df(first_tow_probs, ind_name = rc_values, col_ind_name = "risk_coefficient")
  
  second_tow_probs <- lapply(temp_res, FUN = function(xx) xx[[6]])
  second_tow_probs <- list_to_df(second_tow_probs, ind_name = rc_values, col_ind_name = "risk_coefficient")

  outs <- list(catch_samples = catch_samples, run_times = run_times, 
    first_tow_model = first_tow_model, second_tow_model = second_tow_model, 
    first_tow_probs = first_tow_probs, second_tow_probs = second_tow_probs)
  return(outs)
}

rcrc <- c(0, 1, 5)

ctl1 <- ch4_ctl(rc = 1, port = "ASTORIA / WARRENTON", years = c(2012, 2013),
  ndays1 = 30, the_seeds = 300, ncores = 1, rum_func = rum_probs)
s1 <- sample_catches(ctl_list = ctl1, the_dat = filt_clusts)


vals <- diff_risk_values(rc_values = rcrc, input_ctl = ctl1)


#Mean catch compositions are nearly identical
vals[[1]] %>% filter(type %in% c('targets', 'weaks')) %>% 
  group_by(risk_coefficient, rep, drvid_id, trip_id, species) %>%
  summarize(catch = sum(hpounds)) %>% dcast(species ~ risk_coefficient, value.var = 'catch', mean)

vals[[1]] %>% filter(type %in% c('targets', 'weaks')) %>% 
  group_by(risk_coefficient, rep, drvid_id, trip_id, species) %>%  summarize(catch = sum(hpounds)) %>%
   ggplot() + geom_histogram(aes(x = catch)) +
  facet_wrap(~ species + risk_coefficient)

vals$first_tow_probs %>% dcast(unq_clust ~ risk_coefficient, value.var = 'probs') 

vals$second_tow_probs %>% dcast(unq_clust ~ risk_coefficient, value.var = 'probs')


vals[[1]] %>% filter(rep == 300, risk_coefficient == 5) %>% distinct(haul_id) 


tt <- ch4_ctl(rc = 5, port = "MORRO BAY", years = c(2012, 2013),
  ndays1 = 30, the_seeds = 300:301, ncores = 1, rum_func = rum_probs)
t0 <- sample_catches(ctl_list = tt)

coef(t0[[3]])
coef(t0[[4]])

t0[[5]]
t0[[6]]



#With dummy
tt <- ch4_ctl(rc = 1, port = "ASTORIA / WARRENTON", years = c(2012, 2013),
  ndays1 = 30, the_seeds = 300:301, ncores = 2, rum_func = rum_probs)
t1 <- sample_catches(ctl_list = tt)

apply(fitted(t0[[3]], outcome = FALSE), MAR = 2, FUN = mean)
apply(fitted(t1[[3]], outcome = FALSE), MAR = 2, FUN = mean)

apply(fitted(t0[[4]], outcome = FALSE), MAR = 2, FUN = mean)
apply(fitted(t1[[4]], outcome = FALSE), MAR = 2, FUN = mean)

fitted(t0[[4]], outcome == FALSE )
t1[[4]] 


tt <- ch4_ctl(rc = 1, port = "ASTORIA / WARRENTON", years = c(2012, 2013),
  ndays1 = 60, the_seeds = 300:311, ncores = 6)
t1 <- sample_catches(ctl_list = tt)

tt <- ch4_ctl(rc = 5, port = "ASTORIA / WARRENTON", years = c(2012, 2013),
  ndays1 = 60, the_seeds = 300:301, ncores = 6)
t5 <- sample_catches(ctl_list = tt)

#Look at the revenues

t0_300 <- t0[[1]] %>% distinct(rep, drvid, trip_tow_id, .keep_all = T) %>% group_by(drvid) %>%
  mutate(med_value = median(haul_value), rc  = 0) 

t1_300 <- t1[[1]] %>% distinct(rep, drvid, trip_tow_id, .keep_all = T) %>% group_by(drvid) %>%
  mutate(med_value = median(haul_value), rc = 1)

t5_300 <- t5[[1]] %>% distinct(rep, drvid, trip_tow_id, .keep_all = T) %>% group_by(drvid) %>%
  mutate(med_value = median(haul_value), rc = 5)

tts <- rbind(t0_300, t1_300, t5_300)

tts %>% ggplot() + geom_histogram(aes(x = haul_value)) + geom_vline(aes(xintercept = med_value), lty = 2) +
  facet_wrap(~ drvid + rc, scales = 'free')

tts %>% distinct(med_value)



t5_300 <- t5[[1]] %>% filter(rep == 300) %>% distinct(trip_tow_id, .keep_all = T) %>% 
  mutate(med_value = median(haul_value)) %>% ggplot() + 
  geom_histogram(aes(x = haul_value)) + geom_vline(aes(xintercept = med_value), lty = 2)


t1[[1]] %>% filter(rep == 300, trip_id <= 3) %>% distinct(trip_tow_id, .keep_all = T) %>% head
t1[[5]]


t5[[1]] %>% filter(rep == 300, trip_id <= 3) %>% distinct(trip_tow_id, .keep_all = T) %>% head
t5[[5]]

set.seed(300)
cc <- t1[[5]] %>% sample_n(size = 1, replace = T, weight = t1[[5]]$probs); cc
tows <- filt_clusts %>% filter(unq_clust == cc$unq_clust) %>% distinct(haul_id) 
base::sample(tows$haul_id, size = 1)


# filt_clusts %>% filter(unq_clust == cc$unq_clust) %>% distinct(haul_id) %>% 
#   sample_n(size = 1, replace = T) 

set.seed(300)
cc <- t5[[5]] %>% sample_n(size = 1, replace = T, weight = t5[[5]]$probs); cc
tows <- filt_clusts %>% filter(unq_clust == cc$unq_clust) %>% distinct(haul_id) 
base::sample(tows$haul_id, size = 1)


filt_clusts %>% filter(unq_clust == cc$unq_clust) %>% distinct(haul_id) %>% 
  sample_n(size = 1, replace = T) 

#------------------------------------------------------------------------------------
#Look at results of tt
load('output/test_run1.Rdata')
runs$catch_samples %>% distinct(rep, trip_tow_id, .keep_all = T) %>% 
  ggplot(aes(x = haul_value)) + geom_histogram()

runs$catch_sample %>% head
runs8$catch_sample %>% head
str(runs)
# runs8 <- runs
runs8$catch_samples %>% distinct(rep, trip_tow_id, .keep_all = T) %>% 
  ggplot(aes(x = haul_value)) + geom_histogram()



#------------------------------------------------------------------------------------
#Arguments
#Arguments seed values, representing different iterations
#number of cores to run wit it
xx <- calc_ctac_rev(quotas = quotas, catches = mb_reps1)

#------------------------------------------------------------------------------------
#Maybe a function to compare samples to empirical values 
#See what actual values were for 2013
emp_values <- filt_clusts %>% filter(set_year == 2013, dport_desc == "MORRO BAY", 
  type %in% c('targets', 'weaks')) 

length(unique(emp_values$haul_id))
length(unique(emp_values$trip_id))

emp_values_summary <- emp_values %>% group_by(species) %>% 
  summarize(emp_catches = sum(hpounds))

resamp_values <- mb_reps1 %>% filter(type %in% c('targets', 'weaks'),
  trip_tow_id <= length(unique(emp_values$haul_id))) %>% 
  group_by(rep, drvid_id, species) %>% summarize(resamp_catches = sum(hpounds)) %>% 
  left_join(emp_values_summary, by = 'species')

resamp_values %>% filter(drvid_id == 1) %>% 
  ggplot(aes(x = emp_catches, y = resamp_catches, colour = species)) + geom_point() + 
  geom_abline(slope = 1)  + facet_wrap(~ rep)


#------------------------------------------------------------------------------------
#Add in NA values for species that weren't caught in the resampling

#Sample catches for each iteration, then determine which ones you keep
#to calculate catch-quota balancing
# mb_reps_orig <- mb_reps1



#Add cumulative catches for each species
# mb_reps1 <- mb_reps1 %>% arrange(rep, drvid_id, trip_id, tow_index) %>% 
#  group_by(rep, species) %>% mutate(cum_catch = cumsum(hpounds)) %>% as.data.frame

#Add in the quotas to compare catches to TAC
# mb_reps1 <- mb_reps1 %>% right_join(quotas, by = c('species', 'type'))

#Add zeroes in for missing values
# mb_reps1[which(is.na(mb_reps1$rep)), 'hpounds'] <- 0
# mb_reps1[which(is.na(mb_reps1$rep)), 'cum_catch'] <- 0

#Figure out points in each replicate  at which cum_catch goes over tac
mb_reps1$catch_tac <- mb_reps1$cum_catch / mb_reps1$tac

#Number tows within each trip
tows_over <- mb_reps1 %>% filter(catch_tac >= 1) %>% group_by(rep, drvid_id) %>% 
  summarize(first_trip_tow_over = min(trip_tow_id))
mb_reps1 <- mb_reps1 %>% left_join(tows_over, by = c('rep', 'drvid_id'))

mb_reps1 %>% filter(rep == 300, drvid_id == 1, trip_tow_id %in% c(6, 7)) %>%
 arrange(trip_tow_id)

unders <- mb_reps1 %>% filter(trip_tow_id <= first_trip_tow_over)

#annual revenues
unders %>% group_by()

final_catch_tac <- unders %>% group_by(rep, drvid, species, type) %>% 
  summarize(catch = sum(hpounds), tac = unique(tac), catch_tac = catch / tac)

final_catch_tac %>% filter(drvid == "610567") %>% ggplot(aes(x = catch_tac)) + 
  geom_histogram() + facet_wrap(~ type)

#End of function
#How to calculate revenues
mb_reps1 %>% distinct(haul_id, .keep_all = TRUE) %>% group_by(rep, drvid_id) %>% 
  ggplot() + geom_histogram(aes(x = haul_value)) + 
  facet_wrap(~ drvid_id + unq_clust, scales = 'free_x')

mb_reps1 %>% filter(drvid_id == 1, rep == 300) %>% distinct(haul_id, .keep_all = T) %>%
  group_by(trip_id) %>% summarize(nhauls = length(haul_id)) %>% as.data.frame

mb_reps1 %>% filter(drvid_id == 1, rep == 300, trip_id == 31) %>% 
  distinct(haul_id, .keep_all = T) %>% ggplot() + geom_point(aes(x = avg_long,
    y = avg_lat)) 


%>% distinct(haul_id, .keep_all = T) %>% 
  group_by(rep, drvid_id) %>% summarize(nhauls = length(unique(haul_id)), 
    revenues = sum(haul_value))

 distinct(haul_value)

   %>% ggplot() + geom_histogram(aes(x = revenues))

mb_reps1 %>% filter(rep == 300) %>% distinct(drvid_id, haul_id, .keep_all = T) %>% head


#Look at how variable revenues are for mb_reps
# eddie <- fish_fleet(fleet_chars = vess_vals, rum_res = the_probs, seed = 300)

ggplot(all_trips) + geom_point(aes(x = avg_long,))


one_trip(p1s = first_probs, p2s = second_probs, data_of_interest = filt_clusts, 
  nhauls = 8)

##Combine with quotas
quotas1 <- catch %>% group_by(species, type) %>% summarize(hpounds = sum(hpounds, na.rm = TRUE)) %>%
  select(species, hpounds) %>% 
  right_join(quotas, by = 'species')
quotas1[is.na(quotas1)] <- 0
quotas1$catch <- quotas1$catch + quotas1$hpounds
stop_samples <- sum(quotas$catch >= quotas$tac)



one_trip(p1s = first_probs, p2s = second_probs, quotas = quotas,
  data_of_interest = filt_clusts)


# first_tow <- sample_n(first_probs, size = 1, weight = first_probs$probs, replace = T)
trip_clusts <- c(first_tow$unq_clust, other_tows$unq_clust)

ntows_in_each_clust <- table(trip_clusts)

the_tows <- lapply(1:length(ntows_in_each_clust), FUN = function(xx){
  temp_hauls <- filt_clusts %>% filter(unq_clust == as.numeric(names(ntows_in_each_clust)[xx])) %>%
    distinct(haul_id) %>% sample_n(size = ntows_in_each_clust[xx], replace = T)
  return(data.frame(unq_clust = as.numeric(names(ntows_in_each_clust)[xx]), 
    haul_id = temp_hauls, index = 1:ntows_in_each_clust[xx]))
})

the_tows <- ldply(the_tows)
the_tows <- plyr::rename(the_tows, c("index" = 'inclust_index'))

#Add in the hauls
trip_clusts <- data_frame(unq_clust = trip_clusts, tow_index = 1:length(trip_clusts))
trip_clusts <- trip_clusts %>% group_by(unq_clust) %>% mutate(inclust_index = 1:n())
trip_clusts <- trip_clusts %>% left_join(the_tows, by = c('unq_clust', "inclust_index"))

catches <- filt_clusts %>% filter(haul_id %in% trip_clusts$haul_id)

#What are the columns to save
catch <- catches %>% group_by(haul_id, species, type) %>% summarize(hpounds = sum(hpounds, na.rm = T),
    haul_value = unique(haul_value),  haul_profit = unique(haul_profit),
    profit_fuel_only = unique(profit_fuel_only), unq_clust_bin = unique(unq_clust_bin),
    xbin = unique(xbin), ybin = unique(ybin), unq = unique(unq), 
    unq_clust = unique(unq_clust), avg_long = unique(avg_long), 
    avg_lat = unique(avg_lat)) %>% as.data.frame
trip_clusts <- ungroup(trip_clusts)
catch <- catch %>% left_join(trip_clusts %>% select(haul_id, tow_index), by = 'haul_id') 
catch <- catch %>% arrange(tow_index)

##Combine with quotas
quotas1 <- catch %>% group_by(species, type) %>% summarize(hpounds = sum(hpounds, na.rm = TRUE)) %>%
  select(species, hpounds) %>% 
  right_join(quotas, by = 'species')
quotas1[is.na(quotas1)] <- 0
quotas1$catch <- quotas1$catch + quotas1$hpounds
stop_samples <- sum(quotas$catch >= quotas$tac)


#---------------------------------------------------------------------------------
#Proportion of hauls that catch each species
filt_clusts %>% filter(type == 'weaks') %>% ggplot() + 
  geom_histogram(aes(x = prop_hauls_w_spp)) + facet_wrap(~ species)

#Histograms of hpound amounts for each species in each haul
filt_clusts %>% filter(type == 'weaks') %>% group_by(species) %>% 
  summarize(max_val = max(hpounds))

filt_clusts %>% filter(type == 'weaks') %>% ggplot() + 
  geom_histogram(aes(x = hpounds)) + xlim(limits = c(0, 15000)) + 
  facet_wrap(~ species)

#---------------------------------------------------------------------------------
#Try fish_trip
#Might have to play with risk_coefficient shit in 

xx <- fish_trip(input = filt_clusts, ntows = 10, start_vess = "618440", seed = 250, 
  scope = 1, quotas = quotas, scale = 'scope', the_port = "ASTORIA / WARRENTON",
 risk_coefficient = 1)

x1 <- fish_trip(input = filt_clusts, ntows = 10, start_vess = "618440", seed = 1, 
  scope = 1, quotas = quotas, scale = 'scope', the_port = "ASTORIA / WARRENTON",
 risk_coefficient = 1)

x2 <- fish_trip(input = filt_clusts, ntows = 10, start_vess = "618440", seed = 2, 
  scope = 1, quotas = quotas, scale = 'scope', the_port = "ASTORIA / WARRENTON",
 risk_coefficient = 1)



#------------------------------------------------------------------------
save(obs_data, file = 'obs_data_920.Rdata')

#---------------------------------------------------------------------------------
#Plot some maps of clusters
filt_clusts %>% distinct(haul_id, .keep_all = T) %>% ggplot() + 
  geom_point(aes(x = avg_long, y = avg_lat, colour = unq_clust), alpha = 0.2) + 
  geom_map(data = states_map, map = states_map, 
         aes(x = long, y = lat, map_id = region)) + scale_x_continuous(limits = c(-126, -118)) + 
  scale_y_continuous(limits = c(30, 49)) + ggsave(file = 'figs/clusts_on_coast.png', width = 5,
    height = 9.3)

bin_clusts <- bin_data(data = filt_clusts %>% distinct(unq_clust, .keep_all = TRUE), 
  x_col = "avg_long_clust", "avg_lat_clust", group = "dyear", 
  grid_size = c(.0909, .11), group_vec = 2007:2014)

bin_clusts %>% ggplot() + geom_tile(aes(x = x, y = y, fill = count)) + geom_map(data = states_map, map = states_map, 
         aes(x = long, y = lat, map_id = region)) + scale_x_continuous(limits = c(-126, -120)) + 
  scale_y_continuous(limits = c(34, 49)) + scale_fill_gradient(low = "white", high = "red") + 
  ggsave(file = "figs/binned_clusts_on_coast.png", width = 5, height = 9)


#---------------------------------------------------------------------------------
#Load Data
# load("C:\\Users\\Lewis\\Documents\\Data\\OBDATA_Barnett_OBProcessed_Catch_Data_2002_2014_2015-10-21.Rda")
# obs_data <- OB.ad2
# rm(OB.ad2)



#Load obs_data

#add in prices
load('output/exvessel_formatted.Rdata')
exvessel$mt <- NULL
exvessel$pounds <- NULL
exvessel$value <- NULL
names(exvessel) <- c('set_year', 'd_state', 'species', 'exval_pound')

#Can find tows_clust in "ch4_cluster.R"

tows_clust <- left_join(tows_clust, exvessel, by = c("set_year", "d_state", "species"))


# load(file = "C://Users//Lewis//Documents//Data//comb_data.Rda")
# obs_data <- comb_data
# rm(comb_data)


#---------------------------------------------------------------------------------
#To Do
#Effort concentration in clusters
#Have looked at individual vessel shifts

#---------------------------------------------------------------------------------
#Code arranged by figures

#---------------------------------------------------------------------------------
#Aggregate measures of effort
agg_effort <- obs_data %>% group_by(set_year) %>% summarize(nvess = length(unique(drvid)), 
                                                            ntows = length(unique(haul_id)),
                                                            avg_depth = mean(avg_depth))
agg_effort$set_year <- as.numeric(agg_effort$set_year)

#-----------------------------------
#By State
#Number of Vessels Decreased
obs_data %>% filter(set_year >= 2007) %>% group_by(set_year, r_state) %>% summarize(nvess = length(unique(drvid))) %>% 
  ggplot() + geom_point(aes(x = set_year, y = nvess)) + geom_line(aes(x = set_year, y = nvess)) + 
  facet_wrap(~ r_state) + geom_vline(xintercept = 2010.5, lty = 2)

#Number of tows decreased
obs_data %>% filter(set_year >= 2007) %>% group_by(set_year, r_state) %>% summarize(ntows = length(unique(haul_id))) %>% 
  ggplot() + geom_point(aes(x = set_year, y = ntows)) + geom_line(aes(x = set_year, y = ntows)) + 
  facet_wrap(~ r_state) + geom_vline(xintercept = 2010.5, lty = 2)

#Time Per Tow
obs_data %>% distinct(haul_id, .keep_all = T) %>% filter(set_year >= 2007) %>% group_by(set_year, r_state) %>%
  summarize(duration = sum(haul_duration), ntows = length(unique(haul_id)), 
            duration_per_tow = duration / ntows ) %>% ggplot(aes(x = set_year, y = duration_per_tow)) + geom_line() +
  geom_point() + facet_wrap(~ r_state) + geom_vline(xintercept = 2010.5, lty = 2) + ylim(limits= c(0, 5.5))

#Effort hours
obs_data %>% distinct(haul_id, .keep_all = T) %>% filter(set_year >= 2007) %>% group_by(set_year, r_state) %>%
  summarize(duration = sum(haul_duration), ntows = length(unique(haul_id)), 
            duration_per_tow = duration / ntows ) %>% ggplot(aes(x = set_year, y = duration)) + geom_line() +
  geom_point() + facet_wrap(~ r_state) + geom_vline(xintercept = 2010.5, lty = 2)




#---------------------------------------------------------------------------------
#look at individual vessel changes in position
ind_changes <- obs_data %>% distinct(haul_id, .keep_all = T) %>% group_by(drvid, set_year, r_state) %>% 
  mutate(ntows = length(unique(haul_id))) %>% group_by(drvid, when, r_state) %>%
  summarize(avg_lat = mean(avg_lat), avg_long = mean(avg_long), avg_ntows = mean(ntows)) %>% 
  filter(when != 'baseline') 

ind_changes <- melt(ind_changes, id.vars = c('drvid', 'when', 'r_state'))
ind_changes <- ind_changes %>% dcast(drvid + r_state ~ when + variable) 
ind_changes$diff_avg_lat <- ind_changes$after_avg_lat - ind_changes$before_avg_lat
ind_changes$diff_avg_long <- ind_changes$after_avg_long - ind_changes$before_avg_long
ind_changes$diff_avg_ntows <- ind_changes$after_avg_ntows - ind_changes$before_avg_ntows
ind_changes$type <- "inc"
ind_changes[which(ind_changes$diff_avg_ntows < 0), 'type'] <- 'dec'

#Delta changes in long and lat
ind_changes %>% ggplot(aes(x = diff_avg_long, y = diff_avg_lat)) + 
  geom_point(aes(size = abs(diff_avg_ntows)), pch = 19, alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2) + geom_vline(xintercept = 0, lty = 2) +
  facet_wrap(~ type + r_state)

#histograms of long and lat changes
ind_changes %>% ggplot(aes(x = diff_avg_long)) + geom_histogram() + 
  geom_vline(xintercept = 0, lty = 2) + 
  facet_wrap(~ type + r_state)

ind_changes %>% ggplot(aes(x = diff_avg_lat)) + geom_histogram() + 
  geom_vline(xintercept = 0, lty = 2) + 
  facet_wrap(~ type + r_state) + coord_flip()

#---------------------------------------------------------------------------------
#Did vessels move between states?
obs_data %>% group_by(set_year, drvid) %>% 
  summarize(nstates = length(unique(r_state)), states = paste0(unique(r_state), collapse = '; ')) %>% 
  as.data.frame %>% filter(nstates > 1)
                                                       
#Vessel 578282
big_move <- obs_data %>% filter(drvid == 578282, set_year >= 2007)  
big_move %>% distinct(haul_id, .keep_all = TRUE) %>% group_by(set_year) %>% summarize(avg_lat = mean(avg_lat),
                                                                                      abg_long = mean(avg_long))
  
  group_by(set_year) %>% distin
  summarize()

obs_data %>% filter(drvid == 578282, set_year >= 2007) %>% group_by(set_year) %>% nports

plot_tows(to_plot = obs_data %>% filter(drvid == 578282, set_year >= 2007), region = 'OR', plot_type = 'facet')

#---------------------------------------------------------------------------------
#Cluster the tows by port group if possible

#---------------------------------------------------------------------------------
#Permutation Tests of signifiance



#---------------------------------------------------------------------------------

#---------------------------------------------------------------------------------
agg_effort %>% filter(set_year != 2001) %>% ggplot(aes(x = set_year, y = ntows, group = 1)) + geom_line() + 
  geom_point() + ylim(limits = c(0, 20000))
  
  geom_line(aes(x = set_year, y = ntows))
+ 
  geom_point(aes(x = set_year, y = ntows))

agg_effort %>% filter(set_year != 2001) %>% ggplot(aes(x = set_year)) + geom_line(aes(y = ntows)) + 
  geom_line(aes(x = set_year, y = ntows)) + 
  ylim(limits = c(0, 20000)) 







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

obs_data1 <- arrange_tows(obs_data)

#---------------------------------------------------------------------------------
#Function to calculate values for delta plots
obs_deltas <- delta_plot(data = obs_data)

ggplot(obs_deltas, aes(x = prop_zero, y = skew)) + geom_point() + facet_wrap(~ year) + 
  geom_text(aes(label = short))
#Should look like this, attached this file in the email

load('output/survey_deltas.Rdata')










#Scraps

#--------------------------
#Format data as mlogit dta; do this on individual tow data

# hd2 <- haul_dat %>% filter(drvid == "516880")
# hd2 <- hd2 %>% group_by(unq_clust) %>% summarize(ntows = length(unique(haul_id)),
#   revenue = mean(revenue), d_port_clust_dist = unique(d_port_clust_dist), 
#   dark = mean(Darkblotched_Rockfish), canary = mean(Canary_Rockfish),
#   pop = mean(Pacific_Ocean_Perch)) %>% as.data.frame

# hd2m_check <- mlogit.data(hd2, shape = 'wide', choice = "unq_clust")
# pairs(hd2)

# outs <- vector('list', length = nrow(hd2))

# for(ii in 1:length(unique(hd2m_check$chid))){
#   hd2m_check[which(hd2m_check$chid == ii), "revenue"] <- hd2$revenue
#   hd2m_check[which(hd2m_check$chid == ii), "d_port_clust_dist"] <- hd2$d_port_clust_dist
#   hd2m_check[which(hd2m_check$chid == ii), "dark"] <- hd2$dark
#   hd2m_check[which(hd2m_check$chid == ii), "canary"] <- hd2$canary
#   hd2m_check[which(hd2m_check$chid == ii), "pop"] <- hd2$pop


#   # temp <- hd2
#   # temp$unq_clust <- FALSE
#   # temp$unq_clust[ii] <- TRUE
#   # row.names(temp) <- paste(row.names(temp), temp$alt, sep = ".")
#   # temp$chid <- ii
#   # outs[[ii]] <- temp
# }
# # hd2m <- ldply(outs)
# # rownames(hd2m) <- paste(hd2m$chid, hd2m$alt, sep = '.')
# # attr(hd2m, 'class') <- c('mlogit.data', 'data.frame')

# m <- mlogit(unq_clust ~ revenue, hd2m_check)


#Move the 
#format revenue data
# revs <- data.frame(t(hd2$revenue))
# names(revs) <- paste("rev", hd2$unq_clust, sep = ".")
# hd2 <- cbind(hd2, revs)

# #format distance data
# dists <- data.frame(t(hd2$d_port_clust_dist))
# names(dists) <- paste("dist", hd2$unq_clust, sep = ".")
# hd2 <- cbind(hd2, revs)





# #----------------------------------------------------------------
# #Compare the deviances of different model formulations
# # null <- multinom(ntows ~ 1, data = haul_dat2)
# # rev_model <- multinom(unq_clust ~ revenue, data = haul_dat2, maxit = 10000)
# # coef(rev_model)
# # rev_model <- multinom(ntows ~ revenue, data = haul_dat2, maxit = 10000)

# # rev_dist_model <- multinom(unq_clust ~ d_port_clust_dist + revenue, 
# #   data = haul_dat2, maxit = 10000)
# rev_dist_model <- multinom(unq_clust ~ revenue + d_port_clust_dist , 
#   data = haul_dat2, maxit = 10000)
# coef(rev_dist_model)
# model.matrix(rev_dist_model)

# rdd_model <- multinom(unq_clust ~ revenue + d_port_clust_dist + dark + pop, 
#   data = haul_dat2, maxit = 10000)

# # deviance(null) - deviance(rev_model)
# # deviance(rev_model) - deviance(rev_dist_model)
# # deviance(rev_dist_model) - deviance(rdd_model)

# #Plot the observations
# preds <- cbind(1, haul_dat2$revenue, haul_dat2$d_port_clust_dist) %*%
#            t(coef(rev_dist_model))
# summary(predict(rev_dist_model))

# #----------------------------------------------------------------
# #Extract probabilities from coefficients
# cc <- coef(rdd_model)

# pp <- function(X, B){
#   U <- cbind(1, exp(X %*% t(B)))
#   s <- U %*% rep(1, ncol(U))
#   apply(U, 2, function(u) u / s)
# }

# X <- model.matrix(rdd_model)

# X[, "pop"] <- X[, "pop"] * 100
# pr <- apply(predict(rdd_model, X,  type = 'prob'), 2, mean); pr
# # pr <- apply(pp(X, B), 2, mean); pr
# round(pr, digits = 2)

# new_dat <- haul_dat2
# new_dat[3, "d_port_clust_dist"] <- new_dat[3, "d_port_clust_dist"] * 100

# predict(rdd_model, new_dat)

# rowSums(predict(rev_dist_model, type = 'prob'))


# ######
# dd <- multinom(Y ~ revenue + d_port_clust_dist + dark, data = haul_dat2)
# coef(dd)

# dd2 <- multinom(ntows ~ revenue + d_port_clust_dist + dark, data = haul_dat2)
# coef(dd2)

# cuse$Y <- as.matrix(cuse[,c("none","ster","other")])
# haul_dat2 %>% dcast(unq_clust ~ ntows, value.var )



# haul_dat2d


# char_res <- multinom(unq_clust_fact ~ revenue + d_port_clust_dist,
#       data = haul_dat, 
#       maxit = 30000, MaxNWts = 1000, trace = T)  
  
# #  

# the_coefs <- coef(char_res)
# exp(the_coefs[, "d_port_clust_dist"])


# the_coefs$unq_clust <- rownames(the_coefs)

# #------------------------------------------------------------------------

# #Predictions for one vessel
# all_preds <- summary(predict(ast_res))
# all_preds <- data.frame(unq_clust = names(all_preds), all_preds = all_preds)
# all_preds$unq_clust <- as.character(all_preds$unq_clust)

# obs <- port_rum_data[[1]] %>% group_by(unq_clust) %>% summarize(obs = length(unique(haul_id))) %>% 
#   as.data.frame
# obs$unq_clust <- as.character(obs$unq_clust)

# all_preds <- left_join(obs, all_preds, by = 'unq_clust', fill = 0)
# all_preds[is.na(all_preds$all_preds), "all_preds"] <- 0

# #Start manipulating shit to see if things change
# #Double distance
# dd <- port_rum_data[[1]]

# ###Manipulate shit here
# # dd$d_port_clust_dist <- dd$d_port_clust_dist * 1.1
# # dd$Darkblotched_Rockfish <- dd$Darkblotched_Rockfish * 100
# # dd$Pacific_Ocean_Perch <- dd$Pacific_Ocean_Perch * 100

# #When working with revenue, look at only one
# dd[dd$unq_clust == 3, 'revenue'] <- dd[dd$unq_clust == 3, 'revenue'] / 100
# # dd[dd$unq_clust == 3, 'Darkblotched_Rockfish'] <- dd[dd$unq_clust == 3, 'Darkblotched_Rockfish'] * 100
# ###

# dd_preds <- summary(predict(ast_res, dd))
# dd_preds <- data.frame(unq_clust = names(dd_preds), dd_preds)
# dd_preds$unq_clust <- as.character(dd_preds$unq_clust)
# dd_preds[order(dd_preds$unq_clust), ]

# all_preds <- all_preds %>% inner_join(dd_preds, by = 'unq_clust', fill = 0, all = T)
# #Now they seem to add up

# #Plot them to see how they compare
# melt(all_preds, id = c('unq_clust', "obs")) %>% ggplot() + geom_point(aes(x = obs, y = value,
#                                                                           colour = variable)) 


# the_coefs %>% filter(unq_clust == 3)
# all_preds %>% filter(unq_clust == 3)

# melt(all_preds, id = c('unq_clust', "obs")) %>% ggplot() + geom_point(aes(x = obs, y = value)) +
#   facet_wrap(~ variable)
  


# all_preds$unq_clustdd_preds$unq_clust
# dd_preds$unq_clust
# sum(all_preds$dd_preds, na.rm = T)

# #Find a cluster of interest that changed a lot
# the_coefs[which(the_coefs$unq_clust == "110"), ]

# #Which clusters had the biggest changes?


# #Try to see if it makes sense why it changed
# #I doubled the distance, so presumably the coefficients suggest that?


# dd %>% filter(unq_clust == "110") %>% ungroup %>% distinct(d_port_clust_dist)
# dd %>% distinct(unq_clust, .keep_all = T) %>% ggplot() + geom_histogram(aes(x = d_port_clust_dist))

# quantile(the_coefs$d_port_clust_dist)

# rum_dat <- rum_data(the_input = filt_clusts %>% filter(drvid %in% c("605206")),
#   risk_aversion = 1, the_quotas = quotas, weak_value = 'prop_hauls_w_spp')

# res1 <- fit_rum(haul_dat = rum_dat, print_trace = T)
# hist(coef(res1)[, "d_port_clust_dist"], breaks = 30)

# #Find sites that have positive coefficients for distance
# coef(res1)[which(coef(res1)[, "d_port_clust_dist"] >= 0), ]

# #Create data frame with coefficients to compare predictions
# coefs <- as.data.frame(coef(res1))
# coefs$unq_clust <- rownames(coefs)


# unique(rum_dat$Canary_Rockfish)
# unique(rum_dat$Darkblotched_Rockfish)
# unique(rum_dat$Pacific_Ocean_Perch)
# unique(rum_dat$Yelloweye_Rockfish)


# rum_dat %>% group_by(unq_clust) %>% summarize(ntows = length(unique(haul_id)))



# #Predict with risk_aversion of 1, then multiply by bigger number to 
# #see if predictions change

# #risk aversion value of 1
# ra1 <- summary(predict(res1, rum_dat %>% filter(drvid == "605206")))
# preds <- data.frame(unq_clust = names(ra1), preds_1 = ra1) 
# preds$unq_clust <- as.character(preds$unq_clust)

# #Multiply some of the expected values to see if I can get different probabilities
# ra100 <- rum_dat %>% filter(drvid == "605206") 
# # ra100$Canary_Rockfish <- ra100$Canary_Rockfish * 100
# ra100$d_port_clust_dist <- ra100$d_port_clust_dist * 1.5
# ra2 <- summary(predict(res1, ra100))
# preds2 <- data.frame(unq_clust = names(ra2), preds_2 = ra2) 
# preds2$unq_clust <- as.character(preds2$unq_clust)

# #Add in prediction twos
# preds <- left_join(preds, preds2, by = 'unq_clust')

# #Where was the biggest difference
# preds$preds_1 - preds$preds_2.y
# which(preds$preds_2.y / sum(preds$preds_2.y, na.rm = T) >= .16)
# preds[56, ]

# rum_dat[rum_dat$unq_clust == 83, ]



# #Do this so that quoa changes





# obs <- rum_dat %>% group_by(unq_clust) %>% summarize(obs = length(unique(haul_id))) 
# obs$unq_clust <- as.character(obs$unq_clust)





# preds %>% left_join(obs, by = 'unq_clust')


#---------------------------------------------------------------------------------
# # #Maybe fit frequency of encounters, not much contrast in the hpound values
# # #Make rum_data for all the ports
# # ports <- unique(filt_clusts$dport_desc)

# # #Remove neah bay
# # ports <- ports[-which(ports %in% c("NEAH BAY", "PRINCETON / HALF MOON BAY", 
# #                                    "BODEGA BAY"))]

# # port_rum_data <- lapply(unique(filt_clusts$dport_desc), FUN = function(x){
# #   temp <- filt_clusts %>% filter(dport_desc == x)

# #   rum_dat <- rum_data(the_input = temp, risk_aversion = 1, the_quotas = quotas,
# #                       weak_value = "hpounds")
# #   print(x)
# #   return(rum_dat)
# # })

# # #Fit model for Astoria
# # #Look at the data
# # ast_res <- fit_rum(haul_dat = port_rum_data[[1]], print_trace = T)

# #----------------------------------------------------------------
# haul_dat <- port_rum_data[[2]]

# #Reformat haul_data
# haul_dat2 <- haul_dat %>% group_by(unq_clust) %>% summarize(ntows = length(unique(haul_id)), 
#   revenue = mean(revenue), d_port_clust_dist = unique(d_port_clust_dist),
#   dark = mean(Darkblotched_Rockfish),
#   pop = mean(Pacific_Ocean_Perch)) %>% as.data.frame

# # haul_dat2 <- haul_dat2[1:10, ]
# # t(haul_dat2[, c('unq_clust', 'ntows')])

# # ntows <- as.data.frame(diag(x = haul_dat2$ntows))
# # names(ntows) <- paste0("clust", haul_dat2$unq_clust)
# # haul_dat2$Y <- as.matrix(ntows)

# head(mb_reps1)


# tows_over <- mb_reps1 %>% filter(catch_tac < 1) %>% group_by(rep, drvid_id, trip_id, species) %>%
#   summarize(first_tow_over = min(tow_index))

# #Go through each 


# mb_reps1 %>% filter(catch_tac >= 1) %>% group_by(rep, drvid_id) %>% 
#   summarize(first_tow_over = min(tow_index))

# mb_reps1 %>% filter(rep == 300, species == 'Chilipepper Rockfish') %>% head

# #Find the tow at which shit goes over
# #Let them go over
# just_under_tows <- mb_reps1 %>% filter(catch_tac < 1) %>% 
#   group_by(rep, drvid_id, trip_id, species) %>% summarize(just_under_tow = max(tow_index))

# just_under_tows %>% group_by(rep, drvid_id, trip_id) %>% summarize(just_under_tow = min(just_under_tow))


# , rep == 300, drvid_id == 1, 
#   trip_id == 1) %>% group_by(species) %>% summarize(max_tow = max(tow_index)) 

# %>% 
# select(max_tow) %>% min


# one_tow <- mb_reps11 %>% filter(rep == 300, drvid_id == 1,
#   trip_id == 1)

# one_tow %>% mutate(catch_tac = round(catch_tac, digits = 3)) %>% filter(tow_index <= 7) %>%
#   group_by(species, type) %>% summarize(total_catch = sum(hpounds), tac = unique(tac)) %>% 
#   mutate(prop = total_catch / tac) %>% ggplot() + geom_histogram(aes(x = prop)) + 
#   facet_wrap(~ type)

# #Check this shit works

# mb_reps %>% filter()

# mb_reps11 <- mb_reps11 %>% arrange(rep, drvid_id, trip_id, tow_index) %>%
#   group_by(rep, drvid_id, trip_id, species)

# # mb_reps11$over <- mb_reps11$cum_catch >= mb_reps11$tac

# mb_reps11 <- mb_reps11 %>% arrange(rep, drvid_id, trip_id, tow_index) %>%
#   group_by(rep, drvid_id, trip_id, species, over) %>% mutate(tow_over = min(tow_index)) %>%
#   as.data.frame

# mb_reps11 %>% filter(over == TRUE) %>% group_by(rep, drvid_id, trip_id) %>% summarize(tow_over = min(tow_over))

# #Filter out the tows that are over
# mb_reps11 %>% filter(rep == 300, drvid_id == 1, trip_id == 1, over == FALSE)


# mb_reps11 %>% filter(over == T) %>% head





#---------------------------------------------------------------------------------
#Try running models for 2012 data 


# the_model <- sampled_rums
# filename <- paste0(tolower(substr(models$ports[1], 1, 3)),
#   "_mod_", models$rcs[1], ".Rdata")


# start_time <- Sys.time()
# rc_ast0 <- sampled_rums(data_in = filt_clusts, the_port = 'NEWPORT', 
#   min_year = 2010, max_year = 2014,
#   risk_coefficient = 1, ndays = 30, focus_year = 2012, 
#   nhauls_sampled = 50, seed = 310, ncores = 6)
# run_time <- Sys.time() - start_time; run_time

# summary(rc_ast0[[2]])

# rc_ast50 <- sampled_rums(data_in = filt_clusts, the_port = 'NEWPORT', 
#   min_year = 2011, max_year = 2014,
#   risk_coefficient = 50, ndays = 30, focus_year = 2013, 
#   nhauls_sampled = 50, seed = 310, ncores = 6)
# summary(rc_ast50[[2]])

# predict(rc_ast50[[2]])
# train <- rc_ast0[[2]]$model[1:51, ]
# predict(train, newdat = rc_ast50[[2]])
# mf <- mFormula(fished ~ prev_days_rev * dummy_first + 
#     distance * dummy_first + prev_days_rev * dummy_not_first +
#     distance * dummy_not_first - distance - prev_days_rev - 1 - 
#     dummy_first - dummy_not_first + dummy_prev_days + dummy_prev_year_days)
# predict(rc_ast0[[2]], newdat = train) - 
#   predict(rc_ast50[[2]], newdat = train)


#---------------------------------------------------------------------------------
#Add dummy variables for previously fished clusters to filt_clusts
 # dummy30
# #Subtract 30 days
# fc_dummy$prev_date <- fc_dummy$set_date - days(30)
# fc_dummy$one_year_set_date <- fc_dummy$set_date - days(365)
# fc_dummy$one_year_prev_date <- fc_dummy$prev_date - days(365)
# fc_dummy <- fc_dummy %>% as.data.frame

# #Port and cluster combinations, used to unlist the intervals
# port_clust_combs <- fc_dummy %>% distinct(dport_desc, unq_clust) %>% as.data.frame

# #Create a list of unlisted intervals
# the_intervals <- lapply(1:nrow(port_clust_combs), FUN = function(xx){
#   temp <- fc_dummy %>% filter(dport_desc == port_clust_combs[xx, "dport_desc"],
#     unq_clust == port_clust_combs[xx, "unq_clust"])
#   temp$interval <- interval(temp$one_year_prev_date, temp$one_year_set_date)
#   return(unlist(temp$interval))
# })

# #Compare each set date to the corresponding intervals
# start_time <- Sys.time()
# dummy_30 <- sapply(1:nrow(fc_dummy), FUN = function(aa){
#   #Find the index in port_clust_combs
#   interval_ind <- which(port_clust_combs$dport_desc %in% fc_dummy[aa, 'dport_desc'] &
#         port_clust_combs$unq_clust %in% fc_dummy[aa, 'unq_clust'])
# # which(fc_dummy[aa, 'set_date'] %within% the_intervals[[interval_ind]]  )
# # the_intervals[[interval_ind]][c(58, 60, 61)]
#   ntimes_int <- sum(fc_dummy[aa, 'set_date'] %within% the_intervals[[interval_ind]])
#   return(if_else(ntimes_int > 0, 1, 0))
# })
# run_time <- Sys.time() - start_time

# dim(fc_dummy)
# fc_dummy$dummy_30 <- dummy_30
# save(dummy_30, file = 'output/dummy_30.Rdata')

#---------------------------------------------------------------------------------
#Astoria Run
# browser()    
# filt_clusts %>% filter(dport_desc == "ASTORIA / WARRENTON", set_year == 2012) %>%
#   select(drvid) %>% unique

#Fort bragg, astoria, newport, charleston, eureka
# ss_time <- Sys.time()
# ports_to_run <- c("ASTORIA / WARRENTON", "NEWPORT", "CHARLESTON (COOS BAY)",
#   "FORT BRAGG", "EUREKA")
# risk_coefficients <- c(1, 50)

# models <- expand.grid(ports_to_run, risk_coefficients)
# models <- data_frame(ports = as.character(models$Var1), 
#   rcs = as.numeric(models$Var2))
# models <- as.data.frame(models)

# for(jj in 1:nrow(models)){
#   start_time <- Sys.time()
#   mod <- sampled_rums(data_in = filt_clusts, the_port = models[jj, "ports"],
#    min_year = 2011, max_year = 2014, risk_coefficient = models[jj, "rcs"],
#    ndays = 30, focus_year = 2013, nhauls_sampled = 50, seed = 310,
#    ncores = 6)
#   run_time <- Sys.time() - start_time
  
#   #Print the run time
#   cat(run_time, units(run_time), "run =" ,jj, '\n')

#   filename <- paste0("output/", tolower(substr(models$ports[jj], 1, 3)),
#     "_mod_", models$rcs[jj], ".Rdata")
#   save(mod, file = filename)
# }

# rr_time <- Sys.time() - ss_time


# load("output/ast_mod_1.Rdata")
# load("output/ast_mod_50.Rdata")


# load("output/new_mod_1.Rdata")

# rc1 <- sampled_rums(data_in = filt_clusts, the_port = 'MORRO BAY', min_year = 2011, max_year = 2014,
#   risk_coefficient = 1, ndays = 30, focus_year = 2013, 
#   nhauls_sampled = 50, seed = 310)


# filt_clusts %>% filter(dport_desc == "MORRO BAY", set_year == 2013) %>% distinct(haul_id, .keep_all = T) %>%
#   select(avg_long, avg_lat, haul_id) %>% as.data.frame %>% head(n = 30)

# rc100 <- sampled_rums(data_in = filt_clusts, the_port = 'MORRO BAY', min_year = 2011, max_year = 2014,
#   risk_coefficient = 100, ndays = 30, focus_year = 2013, 
#   nhauls_sampled = 50)
#---------------------------------------------------------------------------------
# #See how risk affects the
# # the_probs1 <- rum_probs(rc = tt$rc, port = tt$port,
# #     years = tt$years, fc = filt_clusts, ndays1 = tt$ndays1)

# #Just look at the model fits with different risk coefficients

# tt <- ch4_ctl(rc = 5, port = "ASTORIA / WARRENTON", years = c(2012, 2013),
#   ndays1 = 60, the_seeds = 300:305, ncores = 6)
# the_probs10 <- rum_probs(rc = tt$rc, port = tt$port,
#     years = tt$years, fc = filt_clusts, ndays1 = tt$ndays1)

# p1s <- the_probs1[[1]]
# names(p1s)[2] <- 'probs1'

# p2s <- the_probs10[[1]]
# names(p2s)[2] <- 'probs2'

# pcomps <- p1s %>% left_join(p2s, by = 'unq_clust') %>% mutate(pdiffs = probs2 - probs1)


# the_probs1[[1]] %>% left_join(the_probs5[[1]], by = 'unq_clust') %>% 
#   mutate(probs_diff = probs.y - probs.x)


# the_probs[1]

# length(300:407) / 6




