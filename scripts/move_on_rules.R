#---------------------------------------------------------------------------------
#Move on Rules

obs_data <- obs_data %>% arrange(trip_id, haul_num)

#Calculate distance between tows 
dists <- obs_data %>% distinct(trip_id, haul_num, .keep_all = T) %>%
  select(trip_id, haul_num, up_long, up_lat, set_long, set_lat) %>% 
  arrange(trip_id, haul_num) %>% mutate(haul_num_diff = haul_num - lag(haul_num))

#add zeroes in for NAs
dists[1, "haul_num_diff"] <- 0

obs_data$set_date <- paste(obs_data$set_year, obs_data$set_month, obs_data$set_day, obs_data$set_time,
                           sep = "-")
obs_data$set_date <- ymd_hm(obs_data$set_date)

#---------------------------------------------------------------------------------
#Calculate distances between trips for each vessel
time_diffs <- obs_data %>% distinct(haul_id, .keep_all = T) %>% arrange(drvid, set_date) %>%
  group_by(drvid) %>% 
  mutate(day_diff = set_date - lag(set_date),
         dist_btw_tows = gcd_slc(long1 = deg2rad(lag(up_long)),
                                 lat1 = deg2rad(lag(up_lat)), long2 = deg2rad(set_long), 
                                 lat2 = deg2rad(up_lat))) %>% as.data.frame
         
time_diffs$day_diff <- time_diffs$day_diff / 60 / 60 / 24
time_diffs$day_diff <- as.numeric(time_diffs$day_diff)

time_diffs <- time_diffs %>% select(haul_id, dist_btw_tows, day_diff)

obs_data1 <- obs_data %>% left_join(time_diffs, by = "haul_id")

#---------------------------------------------------------------------------------
#Bin the Data
#filter to only be 2011-2014 with 100% observer coverage
obs_data1 <- obs_data1 %>% filter(set_year >= 2011, set_year <= 2014)  

obs_binned <- bin_data(data = obs_data1, x_col = "avg_long", y_col = 'avg_lat',
                       group = "set_year", group_vec = 2011:2014, grid_size = c(0.0909, .11))

usa <- map_data("state")

ggplot(obs_binned) + geom_tile(aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient(low = 'white', high = 'red') + 
  geom_map(data = usa, map = usa, aes(x = long, y = lat, 
                                      map_id = region, group = group), fill = 'gray', colour = 'gray70') + 
  # geom_polygon(data = usa, aes(x = long, y = lat), fill = NA, colour = 'black') +
  scale_x_continuous(limits = c(-126.5, -120.5)) + 
  scale_y_continuous(limits = c(34, 48)) + coord_fixed(1.3) + theme_bw() + facet_grid(~ year)

#---------------------------------------------------------------------------------
#Go through to back assign tow values to clusters
the_bins <- obs_binned %>% distinct(unq, .keep_all = T) 

obs_data1$unq <- "999"

for(ii in 1:nrow(the_bins)){
  temp <- the_bins[ii, ]
  obs_data1[which(obs_data1$avg_long > temp$xmin & obs_data1$avg_lon < temp$xmax &
                    obs_data1$avg_lat > temp$ymin & obs_data1$avg_lat < temp$ymax), 'unq'] <- temp$unq
}

#---------------------------------------------------------------------------------
#Look at tow distances immediately after catching a weak stock species
spps <- read.csv('C://Users//Lewis//Desktop//spps.csv', stringsAsFactors = FALSE)
names(spps)[2] <- "spp_type"

# obs_data1$spp_type <- 'other'
obs_data1 <- obs_data1 %>% left_join(spps, by = 'species')
obs_data1[which(is.na(obs_data1$spp_type)), 'spp_type'] <- "other"

#Add id value for tow by
haulz <- obs_data1 %>% distinct(drvid, haul_id, set_date) %>% group_by(drvid) %>% arrange(set_date) %>%
  mutate(vess_haul_id = 1:length(unique(haul_id)))

obs_data1 <- obs_data1 %>% left_join(haulz %>% ungroup %>% select(haul_id, vess_haul_id), by = "haul_id" )

#add vess_haul_comb
obs_data1$vess_haul_comb <- paste(obs_data1$drvid, obs_data1$vess_haul_id)

weaks <- obs_data1 %>% filter(spp_type == 'weaks')
weaks$after_vess_haul_comb <- paste(weaks$drvid, weaks$vess_haul_id + 1)

weaks_after <- obs_data1 %>% filter(vess_haul_comb %in% unique(weaks$after_vess_haul_comb))

#Cast the catch of each species in each haul
weaks_catches <- weaks %>% group_by(haul_id, species) %>% summarize(apounds = sum(apounds)) %>% 
  dcast(haul_id ~ species, value.var = 'apounds', fill = 0)
names(weaks_catches) <- gsub(" ", "_", names(weaks_catches))
  
#Add into obs_data1
obs_data2 <- obs_data1 %>% left_join(weaks_catches, by = "haul_id")
obs_data2$haul_type <- "later tow"
obs_data2[which(obs_data2$haul_num == 1), "haul_type"] <- "first tow"

#add weaks hauls
weak_hauls <- weaks %>% distinct(haul_id)
obs_data2[obs_data2$haul_id %in% weak_hauls$haul_id, 'haul_type'] <- 'caught weak'

weaks_after_hauls <- weaks_after %>% distinct(haul_id)
obs_data2[obs_data2$haul_id %in% weaks_after_hauls$haul_id, 'haul_type'] <- 'after weak'

#---------------------------------------------------------------------------------
#Start plotting the distributions maybe
obs_data2 <- obs_data2 %>% group_by(haul_id, spp_type) %>% mutate(sum_catch_type = sum(apounds)) %>%
  as.data.frame

#Add column with summed weak stock catches
obs_data2 <- obs_data2 %>% group_by(haul_id) %>% mutate(tot_weaks = sum(Bocaccio_Rockfish, Canary_Rockfish,
                                                           Cowcod_Rockfish, Darkblotched_Rockfish, Pacific_Ocean_Perch,
                                                           Yelloweye_Rockfish, na.rm = T))

#Add in the weaks catch correctly
obs_data2 %>% filter(spp_type == 'weaks') %>% head


obs_data2$tot_weaks <- 0
obs_data2[which(obs_data2$spp_type == 'weaks'), 'tot_weaks'] <- 
  obs_data2[which(obs_data2$spp_type == 'weaks'), 'sum_catch_type']

#Add in the previous tow weak stock catches
obs_data3 <- obs_data2 %>% distinct(drvid, vess_haul_id, .keep_all = T)

obs_data3 <- obs_data3 %>% group_by(drvid) %>% mutate(prev_tot_weaks = lag(tot_weaks),
                                                                    prev_Bocaccio_Rockfish = lag(Bocaccio_Rockfish),
                                                                    prev_Canary_Rockfish = lag(Canary_Rockfish),
                                                                    prev_Cowcod_Rockfish = lag(Cowcod_Rockfish),
                                                                    prev_Darkblotched_Rockfish = lag(Darkblotched_Rockfish),
                                                                    prev_Pacific_Ocean_Perch = lag(Pacific_Ocean_Perch),
                                                                    prev_Yelloweye_Rockfish = lag(Yelloweye_Rockfish)) %>%
  as.data.frame

library(tidyr)

obs_data3 <- obs_data3 %>% tidyr::replace_na(list(prev_tot_weaks = 0, prev_Bocaccio_Rockfish = 0, 
                                      prev_Canary_Rockfish = 0, prev_Cowcod_Rockfish = 0, prev_Darkblotched_Rockfish = 0,
                                      prev_Pacific_Ocean_Perch = 0, prev_Yelloweye_Rockfish = 0))

obs_data3 %>% filter(prev_tot_weaks != 0) %>% ggplot() + 
  geom_point(aes(x = tot_weaks, y = dist_btw_tows), pch = '.') + facet_wrap(~ haul_type)

#---------------------------------------------------------------------------------
save(obs_data3, file = 'obs_data3.Rdata')


#---------------------------------------------------------------------------------
#ANOVA for tows after weak species caught and tows that caught weak species
#Run for all ports

#Add in column for after weak or not after weak
obs_data3$haul_type2 <- obs_data3$haul_type
obs_data3[which(obs_data3$haul_type2 == 'caught weak'), 'haul_type2'] <- 'later tow'

#two types of hauls
two_types <- obs_data3 %>% filter(haul_type2 != "first tow")

#Change zero distances to very small numbers
two_types[which(two_types$dist_btw_tows == 0), "dist_btw_tows"] <- .0000001
two_types$dist_btw_tows_log <- log(two_types$dist_btw_tows)

two_types <- two_types %>% filter(dist_btw_tows != 0)

the_ports <- list(c("MOSS LANDING", "SAN FRANCISCO"),
                  "FORT BRAGG",
                  "EUREKA",
                  c("CRESCENT CITY", "BROOKINGS"),
                  "CHARLESTON (COOS BAY)",
                  "NEWPORT", 
                  "ASTORIA / WARRENTON", 
                  c("ILWACO/CHINOOK", "WESTPORT"))

port_anovas <- lapply(1:length(the_ports), FUN = function(xx){
  print(xx)
  temp <- two_types %>% filter(d_port %in% the_ports[[xx]])
  aov_res <- aov(dist_btw_tows_log ~ haul_type2, data = temp)
  return(aov_res)
})

summary(port_anovas[[1]])
summary(port_anovas[[2]])
summary(port_anovas[[3]])
summary(port_anovas[[4]])
summary(port_anovas[[5]])
summary(port_anovas[[6]])
summary(port_anovas[[7]])
summary(port_anovas[[8]])

#See sample sizes of each kind
two_types %>% group_by(d_port, haul_type2) %>% summarize(nvals = length(unique(haul_id))) %>%
  dcast(d_port ~ haul_type2, value.var = "nvals")

#Results of significance are kind of mixed.
#Is it related to the type or amount of weak stocks caught?

#---------------------------------------------------------------------------------
#How soon do they return to places with catches of weak stock species
two_types <- two_types %>% arrange(set_date) %>% group_by(drvid, unq) %>% 
  mutate(return_time = set_date - lag(set_date)) %>% as.data.frame
two_types$return_time <- seconds_to_period(two_types$return_time)
two_types$return_time_hr <- as.numeric(two_types$return_time, "hours")

#Look and see return times in response to yelloweye
return_times <- two_types %>% 
  select(haul_id, d_port, haul_num, drvid, set_date, r_port, dist_btw_tows, day_diff, haul_type, trip_id,
         prev_tot_weaks, prev_Bocaccio_Rockfish, prev_Canary_Rockfish,
         prev_Cowcod_Rockfish, prev_Darkblotched_Rockfish, prev_Pacific_Ocean_Perch,
         prev_Yelloweye_Rockfish, haul_type2, dist_btw_tows_log, return_time_hr, unq) %>% 
  melt(id.vars = c('haul_id', "trip_id", "haul_num", "drvid" , "set_date", "d_port", 'r_port', 'dist_btw_tows', 'day_diff',
                   'haul_type', 'prev_tot_weaks', 'haul_type2', 'dist_btw_tows_log', 
                   'unq', 'return_time_hr'))


# return_times[which(return_times$return_time_hr == max(return_times$return_time_hr, na.rm = T)), ]
# return_times %>% filter(trip_id == '13291') %>% distinct(haul_id, .keep_all = T)

#######
#######
#Look at Astoria Only
return_times_ast <- return_times %>% filter(d_port == "ASTORIA / WARRENTON")

#Frequented locations are those greater than the median
return_times_ast$frequent <- "yes"
return_times_ast[which(return_times_ast$nhauls <= quantile(ast_quants$nhauls)[3]), 'frequent'] <- 'no'
return_times_ast$return_time_hr_log <- log(return_times_ast$return_time_hr)

obs_data3 %>% group_by(set_year) %>% summarize(nhauls = length(unique(haul_id)))


return_times_ast %>% distinct(haul_id, .keep_all = T) %>%
  ggplot() + geom_point(aes(x = prev_tot_weaks, y = return_time_hr)) + facet_wrap(~ frequent)

#Filter out hauls that caught zero weak stocks
return_times_ast %>% distinct(haul_id, .keep_all = T) %>% filter(prev_tot_weaks != 0) %>%
  ggplot() + geom_point(aes(x = prev_tot_weaks, y = return_time_hr, colour = drvid), pch = '.') + facet_wrap(~ drvid)

#Really it's for vessel though, some might be more responsive to catch events



#Look at most frequented locations
return_times_ast <- return_times_ast %>% group_by(unq) %>% mutate(nhauls = length(unique(haul_id))) %>%
  as.data.frame
ast_quants <- return_times_ast %>% distinct(unq, .keep_all = T) %>% as
quantile(ast_quants$nhauls)[3]


#top10 clusters
top10 <- return_times_ast %>% distinct(unq, .keep_all = T) %>% arrange(desc(nhauls)) %>% 
  select(unq) %>% top_n(10)


return_times_ast %>% filter(variable == "prev_Pacific_Ocean_Perch", unq %in% top10$unq) %>%
  filter(is.na(return_time_hr) == FALSE) %>% ggplot() + geom_point(aes(x = value, y = return_time_hr,
                                                                       colour = unq)) +
  facet_wrap(~ unq, scales = 'free')

return_times_ast %>% filter(unq == top10$unq[1]) %>% ggplot() + 
  geom_point(aes(x = set_date, y = value)) + facet_wrap(~ variable, scales = 'free')






plot(return_times_ast$set_date, return_times_ast$haul_id )


# return_times_ast %>% filter(variable == "prev_Pacific_Ocean_Perch",
#                             is.na(return_time_hr) == FALSE) %>% ggplot() + 
#   geom_point(aes(x = value, y = return_time_hr, colour = unq), 
#              pch = '.') + facet_wrap(~ frequent) + xlim(c(0, 1000))



res <- lm(return_time_hr_log ~ value, 
          data = return_times_ast %>% filter(variable == 'prev_Darkblotched_Rockfish', frequent == 'yes',
                                             return_time_hr != 0))
hist(resid(res))


data = 



return_times_ast %>% ggplot(aes(x = nhauls, y = value)) + geom_point() + facet_wrap(~variable)



hist(ast_quants$nhauls, breaks = 50)






return_times_ast %>% filter(is.na(return_time_hr) == FALSE, value != 0, 
                            variable == "prev_Darkblotched_Rockfish", unq == "19 112") %>%
  ggplot() + geom_point(aes(x = value, y = return_time_hr, colour = unq)) + facet_wrap(~ variable)





return_times %>% filter(is.na(return_time_hr) == FALSE, value != 0) %>% 
  filter(d_port == "ASTORIA / WARRENTON") %>% 
  ggplot() + 
  geom_point(aes(x = value, y = return_time_hr)) + facet_wrap(~ variable)






#---------------------------------------------------------------------------------
#ANOVA breaking up groups into species
#See if responses are different based on type of species caught
dist_by_spp  <- two_types %>% select(haul_id, d_port, r_port, dist_btw_tows, day_diff, haul_type, 
                                     prev_tot_weaks, prev_Bocaccio_Rockfish, prev_Canary_Rockfish,
                                     prev_Cowcod_Rockfish, prev_Darkblotched_Rockfish, prev_Pacific_Ocean_Perch,
                                     prev_Yelloweye_Rockfish, haul_type2, dist_btw_tows_log) %>%
  melt(id.vars = c('haul_id', "d_port", 'r_port', 'dist_btw_tows', 'day_diff',
                   'haul_type', 'prev_tot_weaks', 'haul_type2', 'dist_btw_tows_log'))

dist_by_spp <- dist_by_spp %>% group_by(variable) %>% mutate(max_value = max(value))

#Look at catches in previous tow
dist_by_spp %>% filter(value != 0, value < max_value) %>% ggplot() + geom_histogram(aes(x = value), bins = 100) + 
  facet_wrap(~ variable, scales = 'free') 



dist_by_spp %>% filter(d_port == "ASTORIA / WARRENTON") %>% filter(dist_btw_tows < 500) %>% ggplot() + 
  geom_boxplot(aes(x = haul_type2, y = dist_btw_tows))

summary(aov(dist_btw_tows ~ haul_type2 + variable, 
            data = dist_by_spp %>% filter(d_port %in% the_ports[[8]])))


dist_by_spp %>% filter(d_port == "ASTORIA / WARRENTON") %>% ggplot() + 
  geom_boxplot(aes(x = variable, y = dist_btw_tows)) + facet_wrap(~ haul_type2)

#---------------------------------------------------------------------------------
#Formulate move on rules based on species caught
#ANOVAs based on species categories

#1.How far do they move?
#2.How soon do they return?







# obs_data3 %>% filter(d_port == "ASTORIA / WARRENTON", haul_type2 != "first tow") %>% ggplot() + 
#   geom_histogram(aes(x = dist_btw_tows)) + facet_wrap(~ haul_type2, nrow = 3, ncol = 1, scales = 'free')

#Astoria ANOVA
ast <- obs_data3 %>% filter(d_port == "ASTORIA / WARRENTON", haul_type2 != "first tow")
ast <- ast %>% filter(dist_btw_tows != 0)
ast$dist_btw_tows_log <- log(ast$dist_btw_tows)

ast %>% ggplot() + geom_histogram(aes(x = dist_btw_tows), bins = 100) + facet_wrap(~ haul_type2, nrow = 2)


ast_aov <- aov(dist_btw_tows_log ~ haul_type2, data = ast)

hist(resid(ast_aov))


ast %>% group_by(haul_type2) %>% summarize(avg_dist = mean(dist_btw_tows),
                                           var_dist = var(dist_btw_tows))




#---------------------------------------------------------------------------------
#See if there are any responses to specific species
after_weaks <- obs_data3 %>% filter(haul_type == "after weak") %>% select(haul_id, d_port, 
                                                                          set_year, set_month, dist_btw_tows, prev_tot_weaks, prev_Bocaccio_Rockfish,
                     prev_Canary_Rockfish, prev_Cowcod_Rockfish, prev_Darkblotched_Rockfish,
                     prev_Pacific_Ocean_Perch, prev_Yelloweye_Rockfish) 

after_weaks <- after_weaks %>% melt(id.vars = c("haul_id", 'd_port',
                                                'set_year', 'set_month', 'dist_btw_tows'))

#Plot distances between tows
after_weaks %>% filter(d_port == "ASTORIA / WARRENTON") %>% 
  ggplot() + geom_point(aes(x = value, y = dist_btw_tows)) + facet_wrap(~ variable,
                                                                        scales = 'free')

#Fit a linear model to see what effect catches of particular species have on move on distance
yell_ast <- after_weaks %>% filter(d_port == "ASTORIA / WARRENTON", variable == 'prev_Yelloweye_Rockfish') 
yell_ast <- yell_ast %>% filter(value != 0)
  
ya <- lm(dist_btw_tows ~ value - 1, data = yell_ast)
summary(ya)

#Plot the results


yell_ast$preds <- predict(ya)

predict(ya)[order(predict(ya))]
 
plot(yell_ast$value, yell_ast$dist_btw_tows, pch = 19)
lines(yell_ast[order(yell_ast$preds), c("value", 'preds')], col = 'red', lwd = 2)


#Plot histograms for each previous thing

after_weaks %>% ggplot() + geom_histogram(aes(x = dist_btw_tows)) + facet_wrap(~ variable, 
                                                                               scales = 'free')
  

  


  
  
#scraps
  
  #Remove tows with difference greater than 1, negative values mean it's start of another trip
  # dists <- dists %>% filter(haul_num_diff <= 1)
# 
# dists <- dists %>% group_by(trip_id) %>% 
#   mutate(dist_btw_tows = gcd_slc(long1 = deg2rad(lag(up_long)),
#                                  lat1 = deg2rad(lag(up_lat)), long2 = deg2rad(set_long), 
#                                  lat2 = deg2rad(up_lat))) 
#   
#Merge back into the obs_dat  
# obs_data1 <- obs_data %>% left_join(dists %>% select(trip_id, haul_num, haul_num_diff,
#                                                     dist_btw_tows), by = c("trip_id", 'haul_num'))
# 
# 
# 
# weaks_after %>% distinct(haul_id, .keep_all = TRUE) %>% group_by(drvid, d_port) %>% 
#   summarize(mean_movement = mean(dist_btw_tows))
# 
# %>% ggplot() + geom_histogram(aes(x = mean_movement)) + 
#   facet_wrap(~ d_port)
# 
# 
# #add in weaks to see the after effects
# weaks_after <- rbind(weaks %>% select(-after_vess_haul_comb), weaks_after)
# weaks_after <- weaks_after %>% arrange(drvid, vess_haul_id)
# 
#   
#   
# 
# 
# 
# obs_data1 %>% filter(avg_long > temp$xmin, avg_long < temp$xmax,
#                      avg_lat > temp$ymin, avg_lat < temp$ymax) %>% dim
# 
# 
# 
# head(ob)
# 
# 
# 
# 
# 
# 
# 
# ggplot(fig1_dat) + geom_tile(aes(x = x, y = y, fill = count)) +
#   geom_map(data = usa, map = usa, aes(x = long, y = lat, 
#                                       map_id = region, group = group), fill = 'gray', colour = 'gray70') + 
#   # geom_polygon(data = usa, aes(x = long, y = lat), fill = NA, colour = 'black') +
#   scale_x_continuous(limits = c(-126.5, -120.5)) + 
#   scale_y_continuous(limits = c(34, 50)) + scale_fill_gradient2(low = 'blue', high = 'red') + 
#   facet_wrap(~ year) + theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
#   coord_fixed(1.3) + xlab('Longitude') + ylab("Latitude") + labs(fill = "# VMS Pings") + 
# 
# 
# 
# obs_binned %>% group_by(unq) %>% mutate(tot_cou)
# 
# obs_binned %>% filter(unq == "56 6") %>% 
# obs_binned %>% filter(unq == "54 5") %>% dim
# 
# 
# first_five <- first_five %>% distinct(trip_id, haul_num, .keep_all = T) %>%
#   select(trip_id, haul_num, up_long, up_lat, set_long, set_lat) %>% 
#   arrange(trip_id, haul_num)
# 
# %>% group_by(trip_id, haul_num) %>% 


