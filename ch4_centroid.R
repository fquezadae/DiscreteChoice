#Straight effort, no filtering based on hpounds/apounds catch compositions


#Weighted mean stuff
#Weigh by 
wc_data_orig

morn <- c(62, 67, 71, 74, 76, 77, 78, 79, 79, 80, 80, 81, 81, 82, 83, 84, 86, 89, 93, 98)
afte <- c(81, 82, 83, 84, 85, 86, 87, 87, 88, 88, 89, 89, 89, 90, 90, 
  90, 90, 91, 91, 91, 92, 92, 93, 93, 94, 95, 96, 97, 98, 99)

#Weight by number of students
((mean(morn) * length(morn)) + (mean(afte) * length(afte)) ) / (length(morn) + length(afte))

#Do this as a convex combination
(length(morn) / (length(morn) + length(afte))) * mean(morn) + 
  (length(afte) / (length(morn) + length(afte))) * mean(afte)

c(length(morn), length(afte)) / (length(morn) + length(afte))

#Example with weighted.mean function
weighted.mean(c(mean(morn), mean(afte)), 
  c(length(morn), length(afte)) / (length(morn) + length(afte)))


#Port centroids

#Figure out which ports
wc_data_orig$when <- 'before'
wc_data_orig[which(wc_data_orig$dyear > 2010), 'when'] <- 'after'
wc_data_orig$when <- factor(wc_data_orig$when, levels = c('before', 'after'))

#Filter the data for years
wc_data_focus <- subset(wc_data_orig, dyear >= 2008 & dyear <= 2013)

#--------------------------------------------------------------------------------------------------
wc_data_focus <- obs_data

#Unweighted Averages
#calculate average before after coordinates
wc_data_focus %>% group_by(dport_desc, when) %>% mutate(tot_avg_lat = mean(avglat),
  tot_avg_long = mean(avglong)) %>% select(tot_avg_lat, tot_avg_long) %>%
  distinct(dport_desc, when, .keep_all = T) %>% melt %>% 
  dcast(dport_desc ~ variable + when) -> port_avg_coords

wc_data_focus <- left_join(wc_data_focus, port_avg_coords, by = 'dport_desc')

#Vessel averages
wc_data_focus %>% group_by(drvid, when) %>% mutate(vess_tot_avg_lat = mean(avglat),
  vess_tot_avg_long = mean(avglong)) %>% 
  select(vess_tot_avg_lat, vess_tot_avg_long) %>%
  distinct(drvid, when, .keep_all = T) %>% melt %>% 
  dcast(drvid ~ variable + when) -> vess_avg_coords

wc_data_focus <- left_join(wc_data_focus, vess_avg_coords, by = 'drvid')

#Add filter for if vessel was there before and after
wc_data_focus <- wc_data_focus %>% group_by(drvid) %>% mutate(vess_both = length(unique(when))) %>%
  as.data.frame
unique(wc_data_focus$vess_both)
wc_data_focus <- wc_data_focus %>% group_by(when) %>% mutate(nunq_hauls = mean(length(unique(drvid)))) %>%
  as.data.frame

#Add in the direction of the shift for unique vessels
wc_data_focus$direc <- 999

wc_data_focus[which(-wc_data_focus$vess_tot_avg_long_before >= 
                    -wc_data_focus$vess_tot_avg_long_after &
              wc_data_focus$vess_tot_avg_lat_before >= 
              wc_data_focus$vess_tot_avg_lat_after), "direc"] <- "SW"

wc_data_focus[which(-wc_data_focus$vess_tot_avg_long_before <= 
                    -wc_data_focus$vess_tot_avg_long_after &
              wc_data_focus$vess_tot_avg_lat_before >= 
              wc_data_focus$vess_tot_avg_lat_after), "direc"] <- "SE"

wc_data_focus[which(-wc_data_focus$vess_tot_avg_long_before <= 
                    -wc_data_focus$vess_tot_avg_long_after &
              wc_data_focus$vess_tot_avg_lat_before <= 
              wc_data_focus$vess_tot_avg_lat_after), "direc"] <- "NE"

wc_data_focus[which(-wc_data_focus$vess_tot_avg_long_before >= 
                    -wc_data_focus$vess_tot_avg_long_after &
              wc_data_focus$vess_tot_avg_lat_before <= 
              wc_data_focus$vess_tot_avg_lat_after), "direc"] <- "NW"

#Check to see all missing ones just had NAs for one of the coordinates

#-------------------------------
# Washington
wash <- wc_data_focus %>% filter(agid == "W", dport_desc != "dont know ORE")

#Raw numbers of tows
wash %>% group_by(dport_desc, dyear) %>% 
  summarize(ntows = length(unique(haul_id))) %>% 
  ggplot(aes(x = dyear, y = ntows, colour = dport_desc,
    group = dport_desc)) + geom_line() + geom_point() + theme_bw() + 
  geom_vline(xintercept = 2010.5, lty = 2) + xlab("Year") + 
  ylab("Number of Unique Tows") + ggsave(file = 'figs/wash_tows.png',
    width = 6.9, height = 5)

#Spatial shifts
wash %>% filter(gr_sector == "TRAWL") %>%
  ggplot() + 
  geom_segment(aes(x = -set_long, xend = -up_long, y = set_lat, yend = up_lat,
    colour = dport_desc), 
     arrow = arrow(type = 'open', length = unit(.1, 'cm'))) +
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = -tot_avg_long_before, xend = -tot_avg_long_after,
    y = tot_avg_lat_before, yend = tot_avg_lat_after), arrow = arrow(type = 'open',
      length = unit(.3, 'cm'))) +
  scale_x_continuous(limits = c(-125.5, -123.5)) + scale_y_continuous(limits = c(43.5, 48)) +
  facet_grid(dport_desc ~ when) + 
  ggsave('figs/washington_shift_unweighted.png', width = 7, height = 7)

#Vessel Shifts
wash %>%
 ggplot() + geom_segment(aes(x = -set_long, xend = -up_long, y = set_lat, yend = up_lat),
    colour = 'gray', arrow = arrow(type = 'open', length = unit(.1, 'cm'))) +
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = -vess_tot_avg_long_before, xend = -vess_tot_avg_long_after,
    y = vess_tot_avg_lat_before, yend = vess_tot_avg_lat_after, colour = drvid), arrow = arrow(type = 'open',
      length = unit(.4, 'cm'))) + scale_x_continuous(limits = c(-125.5, -123.5)) + 
    scale_y_continuous(limits = c(43, 48))  + facet_wrap(~ vess_both) + 
  ggsave(width = 9.5, height = 6, file = 'figs/wa_vess_shift.png')

#Look at only shifts in centroid, characterize shift in direction and number
  #of values
wash %>% distinct(drvid, .keep_all = T) %>% filter(direc != 999) %>% ggplot() + 
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = -vess_tot_avg_long_before, xend = -vess_tot_avg_long_after,
    y = vess_tot_avg_lat_before, yend = vess_tot_avg_lat_after, colour = drvid), arrow = arrow(type = 'open',
      length = unit(.4, 'cm'))) + scale_x_continuous(limits = c(-125.5, -123.5)) + 
  scale_y_continuous(limits = c(43, 48)) + geom_point(aes(x = -vess_tot_avg_long_before,
  y = vess_tot_avg_lat_before, size = nunq_hauls, colour = drvid)) + 
  facet_wrap(~ direc) + 
  ggsave(width = 8.5, height = 6, file = 'figs/wa_vess_shift_dir.png')


   
#-------------------------------
#Oregon
oregon <- wc_data_focus %>% filter(agid == "O", dport_desc != "dont know",
  dport_desc != "WASHINGTON STATE")
 
#Number of tows
oregon %>% group_by(dport_desc, dyear) %>% 
  summarize(ntows = length(unique(haul_id))) %>% 
  ggplot(aes(x = dyear, y = ntows, colour = dport_desc,
    group = dport_desc)) + geom_line() + geom_point() + theme_bw() + 
  geom_vline(xintercept = 2010.5, lty = 2) + xlab("Year") + 
  ylab("Number of Unique Tows") + ggsave(file = 'figs/oregon_tows.png',
    width = 6.9, height = 5)

#Map of shifts
oregon %>% filter(gr_sector == "TRAWL") %>%
  ggplot() + 
  geom_segment(aes(x = -set_long, xend = -up_long, y = set_lat, yend = up_lat,
    colour = dport_desc), 
     arrow = arrow(type = 'open', length = unit(.1, 'cm'))) +
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = -tot_avg_long_before, xend = -tot_avg_long_after,
    y = tot_avg_lat_before, yend = tot_avg_lat_after), arrow = arrow(type = 'open',
      length = unit(.3, 'cm'))) +
  scale_x_continuous(limits = c(-125.5, -123.5)) + scale_y_continuous(limits = c(43.5, 48)) +
  facet_grid(dport_desc ~ when) + 
  ggsave('figs/oregon_shift_unweighted.png', width = 7, height = 7)

#Vessel shifts
oregon %>%
 ggplot() + geom_segment(aes(x = -set_long, xend = -up_long, y = set_lat, yend = up_lat),
    colour = 'gray', arrow = arrow(type = 'open', length = unit(.1, 'cm'))) +
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = -vess_tot_avg_long_before, xend = -vess_tot_avg_long_after,
    y = vess_tot_avg_lat_before, yend = vess_tot_avg_lat_after, colour = drvid), 
      show.legend = F, arrow = arrow(type = 'open',
      length = unit(.4, 'cm'))) + scale_x_continuous(limits = c(-125.5, -123.5)) + 
    scale_y_continuous(limits = c(40, 48))  + facet_wrap(~ vess_both) +
    ggsave(width = 10.2, height = 6.8, file = 'figs/oregon_vess_shift.png')


#Vessels shifts
oregon %>% distinct(drvid, .keep_all = T) %>% filter(direc != 999) %>% ggplot() + 
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = -vess_tot_avg_long_before, xend = -vess_tot_avg_long_after,
    y = vess_tot_avg_lat_before, yend = vess_tot_avg_lat_after, colour = drvid), arrow = arrow(type = 'open',
      length = unit(.4, 'cm'))) + scale_x_continuous(limits = c(-125.5, -123.5)) + 
    scale_y_continuous(limits = c(40, 48)) +
  geom_point(aes(x = -vess_tot_avg_long_before,
  y = vess_tot_avg_lat_before, size = nunq_hauls, colour = drvid)) + 
  facet_wrap(~ direc) + ggsave(width = 8.3, height = 9.6, file = 'figs/oregon_vess_shift_dir.png')
    
#-------------------------------
calif <- wc_data_focus %>% filter(agid == "C") 

#Number of tows
calif %>% group_by(dport_desc, dyear) %>% 
  summarize(ntows = length(unique(haul_id))) %>% 
  ggplot(aes(x = dyear, y = ntows, colour = dport_desc,
    group = dport_desc)) + geom_line() + geom_point() + theme_bw() + 
  geom_vline(xintercept = 2010.5, lty = 2) + xlab("Year") + 
  ylab("Number of Unique Tows") + ggsave(file = 'figs/ca_tows.png',
    width = 6.9, height = 5)

#Map of shifts
calif %>% filter(gr_sector == "TRAWL") %>% 
  filter(dport_desc %in% c("SAN FRANCISCO", "NORTH OF CALIFORNIA",
    "FORT BRAGG", "EUREKA", "CRESCENT CITY")) %>%
  ggplot() + 
  geom_segment(aes(x = -set_long, xend = -up_long, y = set_lat, yend = up_lat,
    colour = dport_desc), 
     arrow = arrow(type = 'open', length = unit(.1, 'cm'))) +
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = -tot_avg_long_before, xend = -tot_avg_long_after,
    y = tot_avg_lat_before, yend = tot_avg_lat_after), arrow = arrow(type = 'open',
      length = unit(.3, 'cm'))) +
  scale_x_continuous(limits = c(-125.5, -121.5)) + scale_y_continuous(limits = c(35, 43)) +
  facet_grid(dport_desc ~ when) + 
  ggsave('figs/ca_shift_unweighted.png', width = 5.5, height = 7)


calif %>%
 ggplot() + geom_segment(aes(x = -set_long, xend = -up_long, y = set_lat, yend = up_lat),
    colour = 'gray', arrow = arrow(type = 'open', length = unit(.1, 'cm'))) +
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = -vess_tot_avg_long_before, xend = -vess_tot_avg_long_after,
    y = vess_tot_avg_lat_before, yend = vess_tot_avg_lat_after, colour = drvid), 
      show.legend = F, arrow = arrow(type = 'open',
      length = unit(.4, 'cm'))) + scale_x_continuous(limits = c(-125.5, -121.5)) + 
    scale_y_continuous(limits = c(35, 43))  + facet_wrap(~ vess_both) +
      ggsave('figs/ca_vess_shift.png', width = 10.3, height = 6.85)

#Vessels shifts
calif %>% distinct(drvid, .keep_all = T) %>% filter(direc != 999) %>% ggplot() + 
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = -vess_tot_avg_long_before, xend = -vess_tot_avg_long_after,
    y = vess_tot_avg_lat_before, yend = vess_tot_avg_lat_after, colour = drvid), arrow = arrow(type = 'open',
      length = unit(.4, 'cm'))) + scale_x_continuous(limits = c(-125.5, -121.5)) + 
    scale_y_continuous(limits = c(35, 43)) +
  geom_point(aes(x = -vess_tot_avg_long_before,
  y = vess_tot_avg_lat_before, size = nunq_hauls, colour = drvid)) + 
  facet_wrap(~ direc) + ggsave(width = 8.3, height = 9.6, file = 'figs/ca_vess_shift_dir.png')
#--------------------------------------------------------------------------------------------------
#Look at individual vessel shifts

wash %>% group_by(drvid, when) %>% mutate(vess)


geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat, colour = clust)) + 
  facet_wrap(~ dyear)



#Individual vessel centroids by year
wc_data_orig %>% group_by(drvid, dyear) %>% summarize(avg_set_lat = mean(set_lat),
  avg_set_long = mean(set_long), avg_up_lat = mean(up_lat), avg_up_long = mean(up_long)) %>%
  filter(drvid == 1037785) %>% ggplot() + geom_segment(aes(x = avg_set_long, xend = avg_up_long, 
  y = avg_set_lat, yend = avg_up_lat, col = dyear))

  

   + 
  facet_wrap(~ dyear)

#Do this by port, weigh by number of rows




#Need to calculate relative weights for each catgeory and supply the mean values









