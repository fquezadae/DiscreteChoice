#------------------------------------------------------------------------
#Figure 4
#------------------------------------------------------------------------
#Vessel shifts north or south

tows_clust$when <- 'before'
tows_clust[which(tows_clust$set_year >= 2011), 'when'] <- 'after'

#Distinct tows
tc_tows <- tows_clust %>% distinct(haul_id, .keep_all = T)

#Look at average shifts of each vessel
vess_shifts <- tc_tows %>% group_by(drvid, set_year, d_port) %>% summarize(avg_long = mean(avg_long),
  avg_lat = mean(avg_lat), nhauls = length(unique(haul_id)))
vess_shifts$when <- 'before'
vess_shifts[which(vess_shifts$set_year >= 2011), 'when'] <- 'after'

vess_shifts <- vess_shifts %>% group_by(drvid, when, d_port) %>% summarize(avg_long = mean(avg_long), avg_lat = mean(avg_lat),
  avg_hauls = mean(nhauls))
vess_shifts <- vess_shifts %>% arrange(drvid, when)

shifts <- vess_shifts %>% group_by(drvid, d_port) %>% summarize(delt_long = avg_long[1] - avg_long[2],
  delt_lat = avg_lat[1] - avg_lat[2], delt_hauls = avg_hauls[1] - avg_hauls[2])

#------------------------------------------------------------------------
#Run ANOVAs on latitude and longitude for each vessel

vessels <- unique(tc_tows$drvid)
lat_long_anova <- vector('list', length = length(vessels))

for(ii in 1:length(vessels)){
  
  vess1 <- tc_tows %>% filter(drvid == vessels[ii])
  
  if(length(unique(vess1$when)) != 2) next
  #Longitude
  res_long <- aov(avg_long ~ when, vess1)
  p_long <- summary(res_long)[[1]][[5]][1]
  sig_long <- ifelse(p_long < .05, TRUE, FALSE )
  
  #Latitude  
  res_lat <- aov(avg_lat ~ when, vess1)
  p_lat <- summary(res_lat)[[1]][[5]][1]
  sig_lat <- ifelse(p_lat < .05, TRUE, FALSE )

  outs <- data.frame(drvid = vessels[ii], p_long = p_long, sig_long = sig_long, p_lat = p_lat,
    sig_lat = sig_lat)
  lat_long_anova[[ii]] <- outs
}

lat_long_anova <- ldply(lat_long_anova)

#Numbers of significant vessels
both_sig <- lat_long_anova %>% filter(sig_long == TRUE, sig_lat == TRUE)
both_sig$sig <- 'both'

long_sig <- lat_long_anova %>% filter(sig_long == TRUE, sig_lat == FALSE)
long_sig$sig <- 'long'
lat_sig <- lat_long_anova %>% filter(sig_long == FALSE, sig_lat == TRUE)
lat_sig$sig <- 'lat'
none_sig <- lat_long_anova %>% filter(sig_long == FALSE, sig_lat == FALSE)
none_sig$sig <- 'none'

lat_long_anova <- rbind(both_sig, long_sig, lat_sig, none_sig)

shifts <- shifts %>% left_join(lat_long_anova %>% select(drvid, sig), by = 'drvid')

#------------------------------------------------------------------------

#------------------------------------------------------------------------
#Shorter trips, shorter hauls?
tows_clust$d_date <- paste(tows_clust$dyear, tows_clust$dmonth, tows_clust$dday, sep = "-")
tows_clust$d_date <- ymd(tows_clust$d_date)

tows_clust$r_date <- paste(tows_clust$ryear, tows_clust$rmonth, tows_clust$rday, sep = "-")
tows_clust$r_date <- ymd(tows_clust$r_date)

tows_clust$trip_length <- tows_clust$r_date - tows_clust$d_date
tows_clust$trip_length <- as.numeric(tows_clust$trip_length)

tows_clust %>% distinct(trip_id, .keep_all = T) %>% group_by(drvid, dyear) %>%
  summarize(avg_trip = mean(trip_length))

tows_clust %>% distinct(trip_id, .keep_all = T) %>% group_by(dyear) %>%
  summarize(avg_trip = mean(trip_length))

#Average tow duration
tows_clust %>% distinct(haul_id, .keep_all = T) %>% group_by(set_year) %>%
  summarize(avg_haul = mean(haul_duration))




tows_clust %>% group_by(drvid, dyear) %>% summarize(avg_trip = mean(trip_duration, na.rm = T))



#------------------------------------------------------------------------
#Shifts for fleets in each state






#------------------------------------------------------------------------
shifts <- left_join(shifts, tows_clust %>% distinct(d_port, d_state), by = 'd_port')
shifts <- shifts %>% as.data.frame

#Table of values
table3_1 <- shifts %>% group_by(d_state) %>% summarize(avg_delt_long = mean(delt_long, na.rm = T),
  avg_delt_lat = mean(delt_lat, na.rm = T), avg_delt_hauls = mean(delt_hauls, na.rm = T))

table3 <- shifts %>% group_by(d_state, sig) %>% summarize(nvess = length(unique(drvid))) %>%
  dcast(d_state ~ sig, value.var = "nvess")


names(table3) <- c("State", "Both", "Latitude", "Longitude", "No Change", 'Exited')
table3[is.na(table3) ] <- 0
table3 <- table3[c(3, 2, 1), ]

#Add in average shift for each

# write.csv(table3)

positives <- shifts %>% filter(delt_hauls >= 0)
negatives <- shifts %>% filter(delt_hauls < 0)

# negatives[which(negatives$delt_lat < -4), ]

# tows_clust %>% filter(drvid == 589114) %>% 
#   distinct(haul_id, .keep_all = T) %>% distinct(dport_desc, set_year)

#    %>% ggplot() + geom_point(aes(x = avg_long, y = avg_lat)) + 
#   facet_wrap(~ set_year)
# tows_clust 

positives %>% ggplot() + geom_point(aes(x = delt_long, y = delt_lat), alpha = .2) + facet_wrap(~ sig)

negatives %>% ggplot() + geom_point(aes(x = delt_long, y = delt_lat), alpha = .2) +
  facet_wrap(~ sig)

#table by state



#------------------------------------------------------------------------


# #------------------------------------------------------------------------
# par(mfcol = c(1, 2), mar = c(0, 0, 0, 0), oma = c(3, 3, 1, 1))
# #Plot a
# plot(positives$delt_long, positives$delt_lat, pch = 21, xlim = c(-.5, .5), ylim = c(-5.5, 5), ann = F,
#   axes = F, xaxs = 'i', yaxs = 'i', col = "")
# axis(side = 1, mgp = c(0, .5, 0))
# axis(side = 2, mgp = c(0, .5, 0), las = 2)
# abline(v = 0, lty = 2)
# abline(h = 0, lty = 2)
# box()
# mtext(adj = .02, text = "a.) Increases", line = -1)

# #Plot b
# plot(negatives$delt_long, negatives$delt_lat, pch = 21, xlim = c(-.5, .5), ylim = c(-5.5, 5), ann = F,
#   axes = F, xaxs = 'i', yaxs = 'i')
# axis(side = 1, mgp = c(0, .5, 0))
# # axis(side = 2, mgp = c(0, .5, 0), las = 2)
# abline(v = 0, lty = 2)
# abline(h = 0, lty = 2)
# box()
# mtext(adj = .02, text = "b.) Decreases", line = -1)

#Before after skew, detect changes in targeting

#Do ANOVA to test significance of these changes for each vessel.

