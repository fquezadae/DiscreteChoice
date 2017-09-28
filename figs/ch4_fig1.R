#----------------------------------------------------------------------------------------v
# agg_effort <- obs_data %>% group_by(set_year) %>% summarize(nvess = length(unique(drvid)), 
#                                                             ntows = length(unique(haul_id)),
#                                                             avg_depth = mean(avg_depth))
agg_effort <- tows_clust %>% group_by(set_year) %>% summarize(nvess = length(unique(drvid)), 
                                                            ntows = length(unique(haul_id)),
                                                            avg_depth = mean(avg_depth))


agg_effort$set_year <- as.numeric(agg_effort$set_year)

agg_effort1 <- agg_effort

#----------------------------------------------------------------------------------------v
#Figure 1
png(width = 7, height = 7, units = 'in', res = 200, file = 'figs/ch4_fig1.png')
par(mfrow = c(2, 1), mgp = c(.2, .5, 0), mar = c(.5, 1, 1, 0), oma = c(3, 3, 0, 0))

#Number of vessels
plot(agg_effort1$set_year, agg_effort1$nvess, ylim = c(0, 125), pch = 19, type = 'b', axes = F,
     ann = F, xaxs = 'i', yaxs = 'i', xlim = c(2006.5, 2014.5))
axis(side = 2, las = 2)
axis(side = 1, at = 2007:2014, lwd.tick = 0, ann = F, labels = F)

# axis(side = 1, at = c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014), labels = rep("", 8))
abline(v = 2010.5, lty = 2)
mtext(side = 2, "Number of Vessels", line = 3, cex = 1.2)
mtext(side = 3, "a.)", outer = F, adj = .01, line = -1)

#Number of tows
plot(agg_effort1$set_year, agg_effort1$ntows, ylim = c(0, 20500), pch = 19, type = 'b',
     col = 'black', ann = F, axes = F, xaxs = 'i', yaxs = 'i', xlim = c(2006.5, 2014.5))
axis(side = 2, las = 2)
axis(side = 1, at = c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014))
abline(v = 2010.5, lty = 2)
mtext(side = 2, "Number of Tows", line = 3, cex = 1.2)
mtext(side = 3, "b.)", outer = F, adj = .01, line = -1)
mtext(side = 1, "Year", outer = T, line = 1.5, cex = 1.2)
dev.off()
