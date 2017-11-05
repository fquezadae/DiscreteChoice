#------------------------------------------------------------------------
#Vessel Shifts

deltas <- tows_clust %>% group_by(drvid, when) %>% summarize(avg_lat = mean(avg_lat), 
  avg_long = mean(avg_long))

d_lat <- deltas %>% filter(when != 'baseline') %>% 
  dcast(drvid ~ when, value.var = "avg_lat") %>% group_by(drvid) %>%
  mutate(delta_lat = after - before) %>% select(drvid, delta_lat)

d_long <- deltas %>% filter(when != 'baseline') %>% 
  dcast(drvid ~ when, value.var = "avg_long") %>% group_by(drvid) %>%
  mutate(delta_long = after - before) %>% select(drvid, delta_long)


deltas <- d_lat %>% left_join(d_long, by = 'drvid')

#For the vessels that remained, what were the shifts in effort
deltas <- deltas[which(is.na(deltas$delta_lat) == FALSE), ]

diff_ports <- deltas %>% filter(delta_lat >= .5 | delta_lat <= -.5)
deltas <- deltas %>% filter(delta_lat < .5, delta_lat > -.5)
deltas <- deltas %>% as.data.frame

#------------------------------------------------------------------------
#Run ANOVAs for each vessel that remained

one_vess <- tows_clust %>% filter(drvid == 1037785)
load("output/lat_long_anova.Rdata")
head(deltas)

deltas <- deltas %>% left_join(lat_long_anova, by = 'drvid')
# add significant long color of transparent blue
deltas$sig_long_color <- adjustcolor('black', alpha.f = 0)
deltas[which(deltas$sig_long == T), "sig_long_color"] <- adjustcolor('red', alpha.f = .5)

deltas$sig_lat_color <- adjustcolor('black', alpha.f = 0)
deltas[which(deltas$sig_lat == T), "sig_lat_color"] <- adjustcolor('blue', alpha.f = .5)
#------------------------------------------------------------------------
#Figure

png(width = 7, height = 7, units = 'in', res = 200, file = 'figs/ch4_vess_shifts.png')

matlay <- matrix(c(2, 0, 1, 3), ncol = 2, byrow = T)
layout(matlay, widths = c(5/7, 2/7), heights = c(2/7, 5/7))

par(mar = c(0, 0, 0, 0), oma = c(3, 4, 1, 1), mgp = c(0, .5, 0))

#------------------------
#Plot 1
plot(deltas$delta_long, deltas$delta_lat, pch = 19, 
  col = deltas$sig_long_color, cex = 1, ann = F, axes = F,
  xaxs = 'i', yaxs = 'i', xpd = T, ylim = c(-.5, .5),
  xlim = c(-.4, .4))
points(deltas$delta_long, deltas$delta_lat, pch = 19, col = deltas$sig_lat_color, cex = 1)

#add nonsignificant points, open no color
nonsigs <- deltas[which(deltas$sig == 'none'), ]
points(nonsigs$delta_long, nonsigs$delta_lat, pch = 21, bg = adjustcolor('red', alpha.f = 0), 
  col = 'black')

abline(v = 0, lty = 2)
abline(h = 0, lty = 2)
axis(side = 1, cex = 1.5)
axis(side = 2, las = 2)
# abline(v = .37)
# /box()

#------------------------
#Plot 2
hist1 <- hist(deltas$delta_long, breaks = seq(-.4, .4, by = .05), plot = F)

barplot(hist1$density, space = 0, ylim = c(0, max(hist1$density)), axes = F, 
  xlim = c(0.5, 15.5), col = adjustcolor('red', alpha.f = .5))
# box()
# axis(side = 1)

#------------------------
#plot 3
hist2 <- hist(deltas$delta_lat, breaks = seq(-.5, .5, by = .05), plot = F)
barplot(hist2$density, space = 0, xlim = c(0, max(hist2$density)), axes = F, horiz = T,
  ylim = c(0.5, 17.5), col = adjustcolor('blue', alpha.f = .5))

mtext(side = 1, outer = T, "Change in longitude", adj = .3, line = 1.7, cex = 1.2)
mtext(side = 2, outer = T, "Change in latitude", adj = .3, line = 2.2, cex = 1.2)
dev.off()