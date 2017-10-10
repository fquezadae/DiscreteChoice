load(file = "/Volumes/udrive/tows_clust_109_prices.Rdata")

#Add in fleet names
dan_ports <- read.csv(file = 'data/dan_port_assignments.csv', stringsAsFactors = FALSE)
dan_ports <- dan_ports[, c(1, 2)]
tows_clust1 <- tows_clust %>% left_join(dan_ports, by = 'drvid')

fltz <- data.frame(fleet_num = c(8, 9, 10, 12, 13, 14, 16, 17, 18),
  fleet_name = c('FORT BRAGG', "EUREKA", "CRESCENT CITY", "BROOKINGS", "CHARLESTON (COOS BAY)", 
    "NEWPORT", "ASTORIA / WARRENTON", "ILWACO/CHINOOK", "WESTPORT"))
tows_clust1 <- tows_clust1 %>% left_join(fltz, by = 'fleet_num')

tows_clust <- tows_clust1
tows_clust[which(tows_clust$dport_desc == "ILWACO"), "dport_desc"] <- "ILWACO/CHINOOK"

#Update departure port lat and long
port_locz <- tows_clust %>% distinct(dport_desc, d_port_long, d_port_lat)
names(port_locz) <- c('fleet_name', 'fleet_name_long', 'fleet_name_lat')
port_locz <- port_locz %>% filter(is.na(fleet_name_long) == FALSE)

tows_clust <- tows_clust %>% left_join(port_locz, by = 'fleet_name') 

#---------------------------
#Calculate net prices for tows
tows_clust$net_price <- tows_clust$exval_pound - tows_clust$avg_quota_price

#Calculate gross revenue
tows_clust$gross_rev <- tows_clust$hpounds * tows_clust$exval_pound

#Calculate net price
# tows_clust$quota_val <- tows_clust$hpounds * tows_clust$avg_quota_price

#calculate haul revenue
# tows_clust  %>% group_by(fleet_name, set_year) %>% 
#   summarize(gross_rev = sum(gross_rev, na.rm = T), quota_val = sum(quota_val, na.rm = T),
#     net_rev = gross_rev - quota_val) %>% filter(set_year == 2012) %>% select(gross_rev,
#     quota_val) 
  
# tows_clust %>% filter(haul_id == 67434, type != 'other') %>% 
#   select(haul_id, species, type, set_date, hpounds, exval_pound, avg_quota_price, gross_rev,
#     quota_val) %>% group_by(haul_id)

#---------------------------

# weak_inds <- which(tows_clust$type == 'weaks')
# tows_clust[weak_inds, 'net_price'] <- tows_clust[weak_inds, "exval_pound"] - tows_clust[weak_inds, 
#   'avg_quota_price']

# tows_clust$spp_value <- tows_clust$hpounds * (tows_clust$net_price)

# tows_clust <- tows_clust %>% group_by(haul_id) %>% 
#   mutate(haul_value = sum(spp_value, na.rm = T)) %>% 
#   as.data.frame

#add column for the quota value of weak stock species
weak_quota_value <- tows_clust %>% filter(type == 'weaks') %>% group_by(haul_id, type) %>% 
  summarize(weak_quota_value = sum(quota_val, na.rm = T))
tows_clust <- tows_clust %>% left_join(weak_quota_value %>% select(-type), by = 'haul_id')

#add column for the quota value of target and groundfish species
tg_rev <- tows_clust %>% filter(type %in% c('targets', 'groundfish')) %>% group_by(haul_id) %>%
  summarize(tg_rev = sum(gross_rev, na.rm = T))
tows_clust <- tows_clust %>% left_join(tg_rev, by = 'haul_id')

tgo_rev <- tows_clust %>% filter(type %in% c('targets', 'groundfish', 'other')) %>% group_by(haul_id) %>%
  summarize(tgo_rev = sum(gross_rev, na.rm = T))
tows_clust <- tows_clust %>% left_join(tgo_rev, by = 'haul_id')

tgow_rev <- tows_clust %>% group_by(haul_id) %>%
  summarize(tgow_rev = sum(gross_rev, na.rm = T))
tows_clust <- tows_clust %>% left_join(tgow_rev, by = 'haul_id')
tows_clust[is.na(tows_clust$weak_quota_value), 'weak_quota_value'] <- 0

save(tows_clust, file = "/Volumes/udrive/tows_clust_1010.Rdata")