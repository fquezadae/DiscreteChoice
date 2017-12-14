# #------------------------------------------------------------------------
# #Vessel Shifts

# deltas <- tows_clust %>% group_by(drvid, when) %>% summarize(avg_lat = mean(avg_lat), 
#   avg_long = mean(avg_long), avg_depth = mean(avg_depth))

# d_lat <- deltas %>% filter(when != 'baseline') %>% 
#   dcast(drvid ~ when, value.var = "avg_lat") %>% group_by(drvid) %>%
#   mutate(delta_lat = after - before) %>% select(drvid, delta_lat)

# d_long <- deltas %>% filter(when != 'baseline') %>% 
#   dcast(drvid ~ when, value.var = "avg_long") %>% group_by(drvid) %>%
#   mutate(delta_long = after - before) %>% select(drvid, delta_long)

# d_depth <- deltas %>% filter(when != 'baseline') %>% 
#   dcast(drvid ~ when, value.var = "avg_depth") %>% group_by(drvid) %>%
#   mutate(delta_depth = after - before) %>% select(drvid, delta_depth)

# deltas <- d_lat %>% left_join(d_depth, by = 'drvid')

# #For the vessels that remained, what were the shifts in effort
# deltas <- deltas[which(is.na(deltas$delta_lat) == FALSE), ]

# diff_ports <- deltas %>% filter(delta_lat >= 1 | delta_lat <= -1)
# deltas <- deltas %>% filter(delta_lat < 1, delta_lat > -1)
# deltas <- deltas %>% as.data.frame

table(deltas$sig) / sum(table(deltas$sig))


#------------------------------------------------------------------------
#Run ANOVAs for each vessel that remained
load('output/diff_ports_deltas.Rdata')
load("output/lat_depth_anova.Rdata")
load('output/lat_depth_delta.Rdata')

diff_ports <- as.data.frame(diff_ports)
deltas <- rbind(deltas, diff_ports)

deltas <- deltas %>% left_join(lat_depth_anova, by = 'drvid')

#Change outlier values to maximum plot values
deltas$delta_depth >= 150
deltas[which(deltas$delta_depth <= -150), 'delta_depth'] <- -149
deltas[which(deltas$delta_lat >= 1), 'delta_lat'] <- .99
deltas[which(deltas$delta_lat <= -1), 'delta_lat'] <- -.99

# add significant long color of transparent blue
deltas$sig_delta_color <- adjustcolor('black', alpha.f = 0)
deltas[which(deltas$sig_depth == T), "sig_depth_color"] <- adjustcolor('red', alpha.f = .5)

deltas$sig_lat_color <- adjustcolor('black', alpha.f = 0)
deltas[which(deltas$sig_lat == T), "sig_lat_color"] <- adjustcolor('blue', alpha.f = .5)

dd <- deltas %>% filter(sig_depth == TRUE) %>% select(delta_depth) 

#------------------------------------------------------------------------
#Add ports to each vessel
deltas <- tows_clust %>% distinct(drvid, .keep_all = T) %>% select(drvid, fleet_name) %>% 
  right_join(deltas, by = 'drvid') 

deltas %>% filter(sig_depth == TRUE, sig_lat == TRUE)
deltas %>% filter(sig_depth == FALSE, sig_lat == TRUE) %>% nrow / nrow(deltas)
deltas %>% filter(sig_depth == TRUE, sig_lat == FALSE) %>% nrow / nrow(deltas)
deltas %>% filter(sig_depth == FALSE, sig_lat == FALSE) %>% nrow / nrow(deltas)

depths <- deltas %>% group_by(fleet_name) %>% filter(sig_depth == TRUE) %>% summarize(avg_depth = mean(delta_depth) * 1.8)
lats <- deltas %>% group_by(fleet_name) %>% filter(sig_lat == TRUE) %>% summarize(avg_lat = mean(delta_lat))

depths %>% left_join(lats, by = 'fleet_name')

deltas$drvid

hist(dd$delta_depth)

#------------------------------------------------------------------------
#Figure

png(width = 7, height = 7, units = 'in', res = 200, file = 'figs/ch4_fig3.png')

matlay <- matrix(c(2, 0, 1, 3), ncol = 2, byrow = T)
layout(matlay, widths = c(5/7, 2/7), heights = c(2/7, 5/7))

par(mar = c(0, 0, 0, 0), oma = c(3, 4, 1, 1), mgp = c(0, .5, 0))
#------------------------
#Plot 1
plot(deltas$delta_depth, deltas$delta_lat, pch = 19, 
  col = deltas$sig_depth_color, cex = 1, ann = F, axes = F,
  xaxs = 'i', yaxs = 'i', xpd = T, ylim = c(-1.0, 1.0),
  xlim = c(-150, 150))
points(deltas$delta_depth, deltas$delta_lat, pch = 19, col = deltas$sig_lat_color, 
  cex = 1, xpd = TRUE)

#add nonsignificant points, open no color
nonsigs <- deltas[which(deltas$sig == 'none'), ]
points(nonsigs$delta_depth, nonsigs$delta_lat, pch = 21, 
  bg = adjustcolor('red', alpha.f = 0), 
  col = 'black', xpd = T)

abline(v = 0, lty = 2)
abline(h = 0, lty = 2)
axis(side = 1, cex = 1.5, at = c(-150, -100, -50, 0, 50, 100, 150),
  labels = c(-247, -183, -91, 0, 91, 183, 247))
axis(side = 2, las = 2)

#------------------------
#Plot 2
hist1 <- hist(deltas$delta_depth, breaks = seq(-150, 150, by = 5), plot = F)
par(xpd = T)
barplot(hist1$density, space = 0, axes = F, 
  col = adjustcolor('red', alpha.f = .5), xaxs = 'i', yaxs = 'i',
  xpd = T)
# ylim = c(0, max(hist1$density)),
#------------------------
#plot 3
hist2 <- hist(deltas$delta_lat, breaks = seq(-1, 1, by = .05), plot = F)
barplot(hist2$density, space = 0, xlim = c(0, max(hist2$density)), axes = F, horiz = T,
  # ylim = c(0.5, length(hist2$density) - .5), 
  col = adjustcolor('blue', alpha.f = .5),
  xaxs = 'i', yaxs= 'i')

mtext(side = 1, outer = T, "Change in depth (meters)", adj = .3, line = 1.7, cex = 1.2)
mtext(side = 2, outer = T, "Change in latitude", adj = .3, line = 2.2, cex = 1.2)


dev.off()