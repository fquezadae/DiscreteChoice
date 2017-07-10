#Straight effort, no filtering based on hpounds/apounds catch compositions

#Weighted mean stuff
#Weigh by 
wc_data_orig

#Port centroids

#Figure out which ports
# wc_data_orig$when <- 'before'
# wc_data_orig[which(wc_data_orig$dyear > 2010), 'when'] <- 'after'
# wc_data_orig$when <- factor(wc_data_orig$when, levels = c('before', 'after'))
# 
# #Filter the data for years
# wc_data_focus <- subset(wc_data_orig, dyear >= 2008 & dyear <= 2013)

#--------------------------------------------------------------------------------------------------
wc_data_focus <- obs_data

#Unweighted Averages
#calculate average before after coordinates
wc_data_focus <- wc_data_focus %>% filter(when != 'baseline')

port_avg_coords <- wc_data_focus %>% group_by(d_port, when) %>% mutate(tot_avg_lat = mean(avg_lat),
  tot_avg_long = mean(avg_long)) %>% select(tot_avg_lat, tot_avg_long) %>%
  distinct(d_port, when, .keep_all = T) %>% melt %>% 
  dcast(d_port ~ variable + when)

wc_data_focus <- left_join(wc_data_focus, port_avg_coords, by = 'd_port')

#Vessel averages
wc_data_focus %>% group_by(drvid, when) %>% mutate(vess_tot_avg_lat = mean(avg_lat),
  vess_tot_avg_long = mean(avg_long)) %>% 
  select(vess_tot_avg_lat, vess_tot_avg_long) %>%
  distinct(drvid, when, .keep_all = T) %>% melt %>% 
  dcast(drvid ~ variable + when) -> vess_avg_coords

wc_data_focus <- left_join(wc_data_focus, vess_avg_coords, by = 'drvid')

#Add filter for if vessel was there before and after
wc_data_focus <- wc_data_focus %>% group_by(drvid) %>% mutate(vess_both = length(unique(when))) %>%
  as.data.frame
unique(wc_data_focus$vess_both)
wc_data_focus <- wc_data_focus %>% group_by(drvid, when) %>% 
  mutate(nunq_hauls = mean(length(unique(haul_id)))) %>%
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

#Make wc_data_focus$when a factor
wc_data_focus$when <- factor(wc_data_focus$when, levels = c('before','after'))

#-------------------------------
# Washington
wash <- wc_data_focus %>% filter(d_state == "WA")

#Raw numbers of tows
wash %>% group_by(d_port, dyear) %>% 
  summarize(ntows = length(unique(haul_id))) %>% 
  ggplot(aes(x = dyear, y = ntows, colour = d_port,
    group = d_port)) + geom_line() + geom_point() + theme_bw() + 
  geom_vline(xintercept = 2010.5, lty = 2) + xlab("Year") + 
  ylab("Number of Unique Tows") + ggsave(file = 'figs/wash_tows.png',
    width = 6.9, height = 5)

#Spatial shifts
wash %>% group_by(d_port) %>% mutate(bef_aft = length(unique(when))) %>% filter(bef_aft == 2) %>%
  ggplot() + 
  geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat,
    colour = d_port), 
     arrow = arrow(type = 'open', length = unit(.1, 'cm'))) +
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = tot_avg_long_before, xend = tot_avg_long_after,
    y = tot_avg_lat_before, yend = tot_avg_lat_after), arrow = arrow(type = 'open',
      length = unit(.3, 'cm'))) +
  scale_x_continuous(limits = c(-125.5, -123.5)) + scale_y_continuous(limits = c(43.5, 48)) +
  facet_grid(d_port ~ when) +
  ggsave('figs/washington_shift_unweighted.png', width = 7, height = 7)

#Vessel Shifts
wash %>%
 ggplot() + geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat),
    colour = 'gray', arrow = arrow(type = 'open', length = unit(.1, 'cm'))) +
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = vess_tot_avg_long_before, xend = vess_tot_avg_long_after,
    y = vess_tot_avg_lat_before, yend = vess_tot_avg_lat_after, colour = drvid), arrow = arrow(type = 'open',
      length = unit(.4, 'cm'))) + scale_x_continuous(limits = c(-125.5, -123.5)) + 
    scale_y_continuous(limits = c(43, 48))  + facet_wrap(~ vess_both) + 
  ggsave(width = 9.5, height = 6, file = 'figs/wa_vess_shift.png')

#Look at only shifts in centroid, characterize shift in direction and number
  #of values
wash %>% distinct(drvid, .keep_all = T) %>% filter(direc != 999) %>% ggplot() + 
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = vess_tot_avg_long_before, xend = vess_tot_avg_long_after,
    y = vess_tot_avg_lat_before, yend = vess_tot_avg_lat_after, colour = drvid), arrow = arrow(type = 'open',
      length = unit(.4, 'cm'))) + scale_x_continuous(limits = c(-125.5, -123.5)) + 
  scale_y_continuous(limits = c(43, 48)) + geom_point(aes(x = vess_tot_avg_long_before,
  y = vess_tot_avg_lat_before, size = nunq_hauls, colour = drvid)) + 
  facet_wrap(~ direc) + 
  ggsave(width = 8.5, height = 6, file = 'figs/wa_vess_shift_dir.png')


   
#-------------------------------
#Oregon
oregon <- wc_data_focus %>% filter(d_state == "OR")
 
#Number of tows
oregon %>% group_by(d_port, dyear) %>% 
  summarize(ntows = length(unique(haul_id))) %>% 
  ggplot(aes(x = dyear, y = ntows, colour = d_port,
    group = d_port)) + geom_line() + geom_point() + theme_bw() + 
  geom_vline(xintercept = 2010.5, lty = 2) + xlab("Year") + 
  ylab("Number of Unique Tows") + ggsave(file = 'figs/oregon_tows.png',
    width = 6.9, height = 5)

#Map of shifts
oregon %>% group_by(d_port) %>% mutate(bef_aft = length(unique(when))) %>% filter(bef_aft == 2) %>%
  ggplot() + 
  geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat,
    colour = d_port), 
     arrow = arrow(type = 'open', length = unit(.1, 'cm'))) +
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = tot_avg_long_before, xend = tot_avg_long_after,
    y = tot_avg_lat_before, yend = tot_avg_lat_after), arrow = arrow(type = 'open',
      length = unit(.3, 'cm'))) +
  scale_x_continuous(limits = c(-125.5, -123.5)) + scale_y_continuous(limits = c(43.5, 48)) +
  facet_grid(d_port ~ when) + 
  ggsave('figs/oregon_shift_unweighted.png', width = 7, height = 7)

#Vessel shifts
oregon %>%
 ggplot() + geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat),
    colour = 'gray', arrow = arrow(type = 'open', length = unit(.1, 'cm'))) +
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = vess_tot_avg_long_before, xend = vess_tot_avg_long_after,
    y = vess_tot_avg_lat_before, yend = vess_tot_avg_lat_after, colour = drvid), 
      show.legend = F, arrow = arrow(type = 'open',
      length = unit(.4, 'cm'))) + scale_x_continuous(limits = c(-125.5, -123.5)) + 
    scale_y_continuous(limits = c(40, 48))  + facet_wrap(~ vess_both) +
    ggsave(width = 10.2, height = 6.8, file = 'figs/oregon_vess_shift.png')


#Vessels shifts
oregon %>% distinct(drvid, .keep_all = T) %>% filter(direc != 999) %>% ggplot() + 
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = vess_tot_avg_long_before, xend = vess_tot_avg_long_after,
    y = vess_tot_avg_lat_before, yend = vess_tot_avg_lat_after, colour = drvid), arrow = arrow(type = 'open',
      length = unit(.4, 'cm'))) + scale_x_continuous(limits = c(-125.5, -123.5)) + 
    scale_y_continuous(limits = c(40, 48)) +
  geom_point(aes(x = vess_tot_avg_long_before,
  y = vess_tot_avg_lat_before, size = nunq_hauls, colour = drvid)) + 
  facet_wrap(~ direc) + ggsave(width = 8.3, height = 9.6, file = 'figs/oregon_vess_shift_dir.png')
    
#-------------------------------
calif <- wc_data_focus %>% filter(d_state == "CA") 

#Number of tows
calif %>% group_by(d_port, dyear) %>% 
  summarize(ntows = length(unique(haul_id))) %>% 
  ggplot(aes(x = dyear, y = ntows, colour = d_port,
    group = d_port)) + geom_line() + geom_point() + theme_bw() + 
  geom_vline(xintercept = 2010.5, lty = 2) + xlab("Year") + 
  ylab("Number of Unique Tows") + ggsave(file = 'figs/ca_tows.png',
    width = 6.9, height = 5)

#Map of shifts
calif %>% 
  filter(d_port %in% c("SAN FRANCISCO", "NORTH OF CALIFORNIA",
    "FORT BRAGG", "EUREKA", "CRESCENT CITY")) %>%
  ggplot() + 
  geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat,
    colour = d_port), 
     arrow = arrow(type = 'open', length = unit(.1, 'cm'))) +
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = tot_avg_long_before, xend = tot_avg_long_after,
    y = tot_avg_lat_before, yend = tot_avg_lat_after), arrow = arrow(type = 'open',
      length = unit(.3, 'cm'))) +
  scale_x_continuous(limits = c(-125.5, -121.5)) + scale_y_continuous(limits = c(35, 43)) +
  facet_grid(d_port ~ when) + 
  ggsave('figs/ca_shift_unweighted.png', width = 5.5, height = 7)


calif %>%
 ggplot() + geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat),
    colour = 'gray', arrow = arrow(type = 'open', length = unit(.1, 'cm'))) +
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = vess_tot_avg_long_before, xend = vess_tot_avg_long_after,
    y = vess_tot_avg_lat_before, yend = vess_tot_avg_lat_after, colour = drvid), 
      show.legend = F, arrow = arrow(type = 'open',
      length = unit(.4, 'cm'))) + scale_x_continuous(limits = c(-125.5, -121.5)) + 
    scale_y_continuous(limits = c(35, 43))  + facet_wrap(~ vess_both) +
      ggsave('figs/ca_vess_shift.png', width = 10.3, height = 6.85)

#Vessels shifts
calif %>% distinct(drvid, .keep_all = T) %>% filter(direc != 999) %>% ggplot() + 
  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region)) + 
  geom_segment(aes(x = vess_tot_avg_long_before, xend = vess_tot_avg_long_after,
    y = vess_tot_avg_lat_before, yend = vess_tot_avg_lat_after, colour = drvid), arrow = arrow(type = 'open',
      length = unit(.4, 'cm'))) + scale_x_continuous(limits = c(-125.5, -121.5)) + 
    scale_y_continuous(limits = c(35, 43)) +
  geom_point(aes(x = vess_tot_avg_long_before,
  y = vess_tot_avg_lat_before, size = nunq_hauls, colour = drvid)) + 
  facet_wrap(~ direc) + ggsave(width = 8.3, height = 9.6, file = 'figs/ca_vess_shift_dir.png')

#-------------------------------
#Individual vessel plot
ind_vess_changes <- obs_data %>% group_by(drvid, set_year) %>% mutate(ntows = length(unique(haul_id))) %>%
  group_by(drvid, when) %>% summarize(avg_long = mean(avg_long), avg_lat = mean(avg_lat), 
                                      avg_ntows = mean(ntows)) %>% filter(when != 'baseline') %>%
  melt(., id.vars = c('drvid', 'when')) %>% dcast(., drvid ~ when + variable)

#Shifts in individual vessels after catch shares
ind_vess_changes$diff_avg_long <- ind_vess_changes$after_avg_long - ind_vess_changes$before_avg_long
ind_vess_changes$diff_avg_lat <- ind_vess_changes$after_avg_lat - ind_vess_changes$before_avg_lat
ind_vess_changes$diff_avg_ntows <- ind_vess_changes$after_avg_ntows - ind_vess_changes$before_avg_ntows

ind_vess_changes$lab <- "no"

ind_vess_changes[which(abs(ind_vess_changes$diff_avg_long) >= 0.2), 'lab'] <- 'yes'
ind_vess_changes[which(abs(ind_vess_changes$diff_avg_lat) >= 2.5), 'lab'] <- 'yes'


#Add in transparent colors
ggplot(ind_vess_changes, aes(x = diff_avg_long, y = diff_avg_lat)) + 
  geom_point(aes(fill = diff_avg_ntows, size = abs(diff_avg_ntows)), pch = 21, alpha = .5) + 
  theme_bw() + geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + ylab("Change in latitude") + 
  xlab("Change in Longitude") + 
  geom_text(data = ind_vess_changes %>% filter(lab == 'yes'), 
            aes(x = diff_avg_long, y = diff_avg_lat, label = drvid), nudge_y = .3) + 
  ggsave("figs/ind_vess_latlog_shifts_drvid.png", width = 9, height = 7)

ind_vess_changes %>% filter(lab == 'yes') %>% select(drvid)

#-------------------------------
#Vessels that moved the most or had the biggest increases...

big_changes <- ind_vess_changes[which(abs(ind_vess_changes$diff_avg_ntows) > 85), 'drvid']
big_changes <- obs_data %>% filter(drvid %in% big_changes)

big_changes %>% group_by(set_year, drvid, d_state) %>% summarize(ntows = length(unique(haul_id))) %>%
  ggplot(aes(x = set_year, y = ntows)) + geom_point(aes(group = drvid)) + 
  geom_line(aes(group = drvid)) + facet_wrap(~ d_state)

big_changes <- big_changes %>% group_by(set_year, drvid, d_state) %>% summarize(ntows = length(unique(haul_id))) 

big_changes %>%
  ggplot(aes(x = set_year, y = ntows)) + geom_point(aes(group = drvid)) + 
  geom_line(aes(group = drvid)) + facet_wrap(~ d_state) + geom_vline(xintercept = 2010.5, lty = 2)

#Look at one vessel specifically
big_changes <- big_changes %>% group_by(set_year, drvid) %>% mutate(ntows = length(unique(haul_id))) %>%
  as.data.frame

big_changes %>% filter(d_state == 'CA', max(ntows) > 200) %>% arrange(desc(ntows))

big_changes %>% filter(drvid == '610567') %>% ggplot(aes(x = set_year, y = ntows)) +
  geom_line() + geom_point()

vess <- ind_vess_changes %>% filter(lab == 'yes')
vess <- vess$drvid

# 610567; morro bay
pdf(file = 'figs/drvid_big_changes.pdf', width = 9.8, height = 6.5)

for(ii in 1:length(vess)){
  one_vess <- obs_data %>% filter(drvid == vess[ii])  
  pp <- one_vess %>% distinct(haul_id, .keep_all = TRUE) %>% ggplot(aes(x = avg_long, y = avg_lat)) + 
    geom_line() + 
    geom_point(aes(colour = set_month)) + facet_wrap(~ set_year) + 
    geom_map(data = states_map, map = states_map, aes(x = long, y = lat, map_id = region)) + 
    scale_x_continuous(limits = range(one_vess$avg_lon)) + 
    scale_y_continuous(limits = range(one_vess$avg_lat)) + ggtitle(unique(one_vess$drvid))
  print(pp)
}

dev.off()


big_changes %>% filter(d_state == 'CA', max(ntows) > 200)



