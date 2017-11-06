#------------------------------------------------------------------------
#Vessel Shifts

#------------------------------------------------------------------------
#Calculate average depth changes
deltas <- tows_clust %>% distinct(haul_id, .keep_all = TRUE) %>% 
  group_by(drvid, when) %>% summarize(avg_lat = mean(avg_lat), 
  avg_long = mean(avg_long), avg_depth = mean(avg_depth))

d_lat <- deltas %>% filter(when != 'baseline') %>% 
  dcast(drvid ~ when, value.var = "avg_lat") %>% group_by(drvid) %>%
  mutate(delta_lat = after - before) %>% select(drvid, delta_lat)

d_long <- deltas %>% filter(when != 'baseline') %>% 
  dcast(drvid ~ when, value.var = "avg_long") %>% group_by(drvid) %>%
  mutate(delta_long = after - before) %>% select(drvid, delta_long)

d_depth <- deltas %>% filter(when != 'baseline') %>% 
  dcast(drvid ~ when, value.var = "avg_depth") %>% group_by(drvid) %>%
  mutate(delta_depth = after - before) %>% select(drvid, delta_depth)

deltas <- d_lat %>% left_join(d_depth, by = 'drvid')

#For the vessels that remained, what were the shifts in effort
deltas <- deltas[which(is.na(deltas$delta_lat) == FALSE), ]

diff_ports <- deltas %>% filter(delta_lat >= 1 | delta_lat <= -1)
deltas <- deltas %>% filter(delta_lat < 1, delta_lat > -1) %>% as.data.frame


tc_tows <- tows_clust %>% distinct(haul_id, .keep_all = T)

#------------------------------------------------------------------------
#Run ANOVA on depths and latitude
vessels <- unique(tc_tows$drvid)
lat_depth_anova <- vector('list', length = length(vessels))
 
for(ii in 1:length(vessels)){
   
  vess1 <- tc_tows %>% filter(drvid == vessels[ii])
   
  if(length(unique(vess1$when)) != 2) next
  #Depth
  res_depth <- aov(avg_depth ~ when, vess1)
  p_depth <- summary(res_depth)[[1]][[5]][1]
  sig_depth <- ifelse(p_depth < .05, TRUE, FALSE )
   
  #Latitude  
  res_lat <- aov(avg_lat ~ when, vess1)
  p_lat <- summary(res_lat)[[1]][[5]][1]
  sig_lat <- ifelse(p_lat < .05, TRUE, FALSE )
 
  outs <- data.frame(drvid = vessels[ii], p_long = p_depth, sig_depth = sig_depth, p_lat = p_lat,
    sig_lat = sig_lat)
  lat_depth_anova[[ii]] <- outs
}
 
lat_depth_anova <- ldply(lat_depth_anova)
 
#Numbers of significant vessels
both_sig <- lat_depth_anova %>% filter(sig_depth == TRUE, sig_lat == TRUE)
both_sig$sig <- 'both'
 
depth_sig <- lat_depth_anova %>% filter(sig_depth == TRUE, sig_lat == FALSE)
depth_sig$sig <- 'depth'
lat_sig <- lat_depth_anova %>% filter(sig_depth == FALSE, sig_lat == TRUE)
lat_sig$sig <- 'lat'
none_sig <- lat_depth_anova %>% filter(sig_depth == FALSE, sig_lat == FALSE)
none_sig$sig <- 'none'
 
lat_depth_anova <- rbind(both_sig, depth_sig, lat_sig, none_sig)

save(lat_depth_anova, file = 'output/lat_depth_anova.Rdata') 
save(deltas, file = 'output/lat_depth_delta.Rdata') 

#------------------------------------------------------------------------
#Run ANOVAs for each vessel that remained

# load("output/lat_depth_anova.Rdata")

# deltas <- deltas %>% left_join(lat_depth_anova, by = 'drvid')
# # add significant long color of transparent blue
# deltas$sig_depth_color <- adjustcolor('black', alpha.f = 0)
# deltas[which(deltas$sig_depth == T), "sig_depth_color"] <- adjustcolor('red', alpha.f = .5)

# deltas$sig_lat_color <- adjustcolor('black', alpha.f = 0)
# deltas[which(deltas$sig_lat == T), "sig_lat_color"] <- adjustcolor('blue', alpha.f = .5)

#------------------------------------------------------------------------
#Table
# table(deltas$sig) / sum(table(deltas$sig))



# #------------------------------------------------------------------------
# #Figure

# png(width = 7, height = 7, units = 'in', res = 200, file = 'figs/ch4_vess_shifts.png')

# matlay <- matrix(c(2, 0, 1, 3), ncol = 2, byrow = T)
# layout(matlay, widths = c(5/7, 2/7), heights = c(2/7, 5/7))

# par(mar = c(0, 0, 0, 0), oma = c(3, 4, 1, 1), mgp = c(0, .5, 0))

# #------------------------
# #Plot 1
# plot(deltas$delta_long, deltas$delta_lat, pch = 19, 
#   col = deltas$sig_depth_color, cex = 1, ann = F, axes = F,
#   xaxs = 'i', yaxs = 'i', xpd = T, ylim = c(-1, 1),
#   xlim = c(-.5, .5))
# points(deltas$delta_long, deltas$delta_lat, pch = 19, col = deltas$sig_lat_color, cex = 1)

# #add nonsignificant points, open no color
# nonsigs <- deltas[which(deltas$sig == 'none'), ]
# points(nonsigs$delta_long, nonsigs$delta_lat, pch = 21, bg = adjustcolor('red', alpha.f = 0), 
#   col = 'black')

# abline(v = 0, lty = 2)
# abline(h = 0, lty = 2)
# axis(side = 1, cex = 1.5)
# axis(side = 2, las = 2)

# #------------------------
# #Plot 2
# hist1 <- hist(deltas$delta_long, breaks = seq(-.5, .5, by = .05), plot = F)

# barplot(hist1$density, space = 0, ylim = c(0, max(hist1$density)), axes = F, 
#   xlim = c(0.5, length(hist1$density) - .5), col = adjustcolor('red', alpha.f = .5))
# # box()
# # axis(side = 1)

# #------------------------
# #plot 3
# hist2 <- hist(deltas$delta_lat, breaks = seq(-1, 1, by = .05), plot = F)
# barplot(hist2$density, space = 0, xlim = c(0, max(hist2$density)), axes = F, horiz = T,
#   ylim = c(0.5, length(hist2$density) - .5), col = adjustcolor('blue', alpha.f = .5))

# mtext(side = 1, outer = T, "Change in longitude", adj = .3, line = 1.7, cex = 1.2)
# mtext(side = 2, outer = T, "Change in latitude", adj = .3, line = 2.2, cex = 1.2)


# dev.off()