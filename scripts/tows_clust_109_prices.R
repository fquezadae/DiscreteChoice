#Make sure that quota prices are added in correctly

#Use this version of tows_clust
load(file = "/Volumes/udrive/tows_clust_925_depth_bin.Rdata")

#Pull out species of interest
tc_spp <- tows_clust %>% filter(type != 'other') %>% distinct(species)

#-------------------------------------------------------------------------------------
#Add in quota prices
qps <- read.csv("/Users/peterkuriyama/Dropbox/ch4_929/data/quota_prices.csv", 
  stringsAsFactors = FALSE)

qps$X <- NULL
qps$X.1 <- NULL

qps <- melt(qps)
qps <- qps %>% group_by(species) %>% summarize(avg_price = mean(value, na.rm = T)) %>% 
  as.data.frame
qps[which(is.na(qps$avg_price)), 'avg_price'] <- 0

#Check species names
qps$species <- c("Arrowtooth Flounder", "Bocaccio Rockfish", 'Canary Rockfish',
  "Chilipepper Rockfish", 'Cowcod Rockfish', "Darkblotched Rockfish",
  "Dover Sole", "English Sole", "Lingcod", "Lingcod", "Lingcod", 'Longspine Thornyhead',
  "shelf north", 'shelf south', 'slope north', 'slope south', 'other flatfish',
  'Pacific Ocean Perch', "Pacific Cod", "Pacific Halibut", "Pacific Whiting", 
  "Petrale Sole", "Sablefish", 'Sablefish', "Shortspine Thornyhead", 'Shortspine Thornyhead',
  "Splitnose Rockfish", "Starry Flounder", "Widow Rockfish", "Yelloweye Rockfish", 
  "Yellowtail Rockfish")
qps <- qps %>% group_by(species) %>% summarize(avg_price = mean(avg_price))
qps <- qps %>% as.data.frame

#Add these in to 

#-------------------------------------------------------------------------------------
#Make sure adding the right ex vessel values for years 2007-2010, won't really matter except 
#for 2010 years
qps[which(qps$species %in% tc_spp$species == FALSE), 'species']
names(qps)[2] <- 'avg_quota_price'

#Add the 
tows_clust$avg_quota_price <- NULL
tows_clust <- tows_clust %>% left_join(qps, by = 'species')
tows_clust[which(is.na(tows_clust$avg_quota_price)), 'avg_quota_price'] <- 0

#-------------------------------------------------------------------------------------
#Split into two time periods
bef <- tows_clust %>% filter(set_year < 2011)
aft <- tows_clust %>% filter(set_year >= 2011)

bef$exval_pound <- NULL

#Add averaged quota prices for years before 2011
avg_exval_pounds <- tows_clust %>% group_by(species) %>% 
  summarize(exval_pound = mean(exval_pound, na.rm = T)) %>% 
  arrange(exval_pound) %>% as.data.frame
avg_exval_pounds[which(is.na(avg_exval_pounds$exval_pound)), 'exval_pound'] <- 0

bef <- bef %>% left_join(avg_exval_pounds, by = 'species')
bef <- bef[, names(aft)]

tows_clust2 <- rbind(bef, aft)
tows_clust <- tows_clust2

save(tows_clust, file = "/Volumes/udrive/tows_clust_109_prices.Rdata")



#Add in the ports also


# tows_clust %>% filter(haul_id %in% c(109126)) %>% distinct(haul_id, .keep_all = T)