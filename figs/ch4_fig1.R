#----------------------------------------------------------------------------------------v
# agg_effort <- obs_data %>% group_by(set_year) %>% summarize(nvess = length(unique(drvid)), 
#                                                             ntows = length(unique(haul_id)),
#                                                             avg_depth = mean(avg_depth))


agg_effort <- tows_clust %>% group_by(set_year) %>% summarize(nvess = length(unique(drvid)), 
                                                            ntows = length(unique(haul_id)),
                                                            avg_depth = mean(avg_depth))
save(agg_effort, file = 'output/agg_effort.Rdata')

#----------------------------------------------------------------------------------------v
devtools::install_github("peterkuriyama/ch4", auth_token = "83f947b716e40172803f0ff798c46f5ff9ca3cd1")
library("ch4")
library(RColorBrewer)

#Number of vessels
load(file = "output/agg_effort.Rdata")

agg_effort$when <- 'before'
agg_effort[which(agg_effort$set_year >= 2011), 'when'] <- 'after'

(mean(agg_effort$nvess[5:8]) - mean(agg_effort$nvess[1:4]))
(mean(agg_effort$nvess[5:8]) - mean(agg_effort$nvess[1:4])) / mean(agg_effort$nvess[1:4])

mean(agg_effort$ntows[1:4])
mean(agg_effort$ntows[5:8])
(mean(agg_effort$ntows[5:8]) - mean(agg_effort$ntows[1:4]))
(mean(agg_effort$ntows[5:8]) - mean(agg_effort$ntows[1:4])) / mean(agg_effort$ntows[1:4])

agg_effort$set_year <- as.numeric(agg_effort$set_year)

agg_effort1 <- agg_effort

#----------------
#Number of vessels in each fleet
fleet_vess <- tows_clust %>% group_by(fleet_name, set_year) %>% summarize(nvess = length(unique(drvid))) %>% 
	as.data.frame
save(fleet_vess, file = "output/fleet_vess.Rdata")
#----------------

load(file = 'output/fleet_vess.Rdata')

fleet_vess$plot_fleet <- fleet_vess$fleet_name
fleet_vess[which(fleet_vess$fleet_name %in% c("ASTORIA / WARRENTON", "CRESCENT CITY", 'EUREKA',
	'CHARLESTON (COOS BAY)', 'BROOKINGS', 'FORT BRAGG', 'NEWPORT') == FALSE), 'plot_fleet'] <- 'other'
fleet_vess[which(fleet_vess$fleet_name %in% c("CRESCENT CITY", 'BROOKINGS')), 'plot_fleet'] <- 
	"CRESCENT CITY AND BROOKINGS"

fleet_vess <- fleet_vess %>% group_by(set_year, plot_fleet) %>% summarize(nvess = sum(nvess)) %>% 
	arrange(plot_fleet) %>% as.data.frame
#----------------------------------------------------------------------------------------v
#Run permutation to calculate the

vess_sig <- simple_permute(input = agg_effort %>% as.data.frame, perm_column = 'nvess')
tows_sig <- simple_permute(input = agg_effort %>% as.data.frame, perm_column = 'ntows')

hist(vess_sig[[2]])

# #Look at distinct hauls
# unq_tows <- tows_clust %>% distinct(haul_id, .keep_all = T)
# seed <- 300
# 
# unq_tows$samp_year <- sample(unq_tows$set_year, replace = F)
# unq_tows$samp_when <- 'before'
# unq_tows[which(unq_tows$samp_year >= 2011), 'samp_when'] <- 'after'
# 
# unq_tows %>% group_by(samp_year) %>% summarize(nvess = length(unique(drvid)), 
#   ntows = length(unique(haul_id)))
# 
# sample(unq_tows$set_year, replace = F) %>% head
# perms <- tows_clust %>% select(when, haul_id, drvid)


#----------------------------------------------------------------------------------------v
#Figure 1
png(width = 5.7, height = 8.6, units = 'in', res = 200, file = 'figs/ch4_fig1.png')

par(mfrow = c(3, 1), mgp = c(.2, .5, 0), mar = c(.5, 1, 1, 0), oma = c(3, 4, 0, 0))

#-------------------------------------------
#Number of vessels
plot(agg_effort1$set_year, agg_effort1$nvess, ylim = c(0, 125), pch = 19, type = 'b', axes = F,
     ann = F, xaxs = 'i', yaxs = 'i', xlim = c(2006.5, 2014.5))
axis(side = 2, las = 2)
axis(side = 1, at = 2007:2014, lwd.tick = 0, ann = F, labels = F)
abline(v = 2010.5, lty = 2)
mtext(side = 2, "Number of Vessels", line = 3, cex = 1.2)
mtext(side = 3, "a)", outer = F, adj = .01, line = -1)

#-------------------------------------------
#Vessels in each port
fltz <- unique(fleet_vess$plot_fleet)
fltz_colors <- brewer.pal(7,"Dark2")
fltz_colors <- adjustcolor(fltz_colors, alpha.f = .7)

#Format legend
ff <- data_frame(fltz, fltz_colors)
max_vess <- fleet_vess %>% group_by(plot_fleet) %>% summarize(max_nvess = max(nvess)) %>%
	arrange(desc(max_nvess))
names(max_vess)[1] <- "fltz"
ff <- ff %>%  left_join(max_vess, by = 'fltz') %>% arrange(desc(max_nvess)) %>% as.data.frame
ff$plot_val <- c('Other', 'Astoria', "Charleston", "Newport", 'Eureka', "Brookings & Crescent City",
	"Fort Bragg")

#Actual plot
plot(fleet_vess$set_year, fleet_vess$nvess, ann = F, axes = F, xlim = c(2006.5, 2014.5), type = 'n',
	ylim = c(0, 60))
for(ii in 1:length(fltz)){
  temp <- subset(fleet_vess, plot_fleet == fltz[ii])
  points(temp$set_year, temp$nvess, pch = 19, type = 'o', col = fltz_colors[ii])
  # lines(temp$set_year, temp$nvess, pch = 19)
}
axis(side = 1, at = 2007:2014, lwd.tick = 0, ann = F, labels = F) 
axis(side = 2, las = 2 ) 
abline(v = 2010.5, lty = 2)
legend('topright', legend = ff$plot_val, pch = 19, bty = 'n', col = ff$fltz_colors)
mtext(side = 3, "b)", outer = F, adj = .01, line = -1)
mtext(side = 2, "Number of Vessels", line = 3, cex = 1.2)

#-------------------------------------------
#Number of tows
plot(agg_effort1$set_year, agg_effort1$ntows, ylim = c(0, 20500), pch = 19, type = 'b',
     col = 'black', ann = F, axes = F, xaxs = 'i', yaxs = 'i', xlim = c(2006.5, 2014.5))
axis(side = 2, las = 2)
axis(side = 1, at = c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014))
abline(v = 2010.5, lty = 2)
mtext(side = 2, "Number of Tows", line = 3, cex = 1.2)
mtext(side = 3, "c)", outer = F, adj = .01, line = -1)
mtext(side = 1, "Year", outer = T, line = 1.5, cex = 1.2)

dev.off()


#----------------------------------------------------------------------------------------
# Look at number of vessels changes

# 
# tows_clust %>% group_by(fleet_name, set_year) %>% summarize(nvess = length(unique(drvid))) %>% 
#   filter(fleet_name %in% c("ASTORIA / WARRENTON", 'BROOKINGS', 'CHARLESTON (COOS BAY)', 
#     'CRESCENT CITY', 'EUREKA', 'NEWPORT'))
#   ggplot() + geom_line(aes(x = set_year, y = nvess, group = fleet_name)) + facet_wrap(~ fleet_name)
# 
# 









