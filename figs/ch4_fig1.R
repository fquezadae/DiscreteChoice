# <<<<<<< HEAD
#----------------------------------------------------------------------------------------v
#Keep only 
dd <- tows_clust %>% distinct(haul_id, .keep_all = T)
dd <- dd %>% group_by(drvid) %>% mutate(nn = length(unique(haul_id))) %>%
  as.data.frame
dd <- dd %>% filter(nn > 125)

dd %>% group_by(ryear, r_port) %>% summarize(nvess = length(unique(drvid))) %>%
  ggplot(aes(x = ryear, y = nvess)) + 
  geom_line() + facet_wrap(~r_port)


dd %>% group_by(ryear, fleet_name) %>% summarize(nvess = length(unique(drvid))) %>%
  ggplot(aes(x = ryear, y = nvess)) + 
  geom_point() + facet_wrap(~fleet_name)

unassigned_drvid <- dd %>% filter(is.na(fleet_name)) %>% distinct(drvid) %>%
  pull(drvid)



dd %>% filter(drvid %in% unassigned_drvid) %>% group_by(drvid, r_port, ryear) %>%
  summarize(ntows = length(unique(haul_id))) %>% ggplot() +
  geom_point(aes(x = ryear, y = ntows, shape = r_port)) + facet_wrap(~ drvid)



dd %>% group_by(drvid, r_port) %>% 
  mutate(nn1 = length(unique(haul_id)), min_yr = min(ryear),
    max_yr = max(ryear)) %>% 
  group_by(drvid) %>%
  filter(nn1 == max(nn1)) %>% distinct(drvid, .keep_all = T) %>%
  select(drvid,  fleet_name, ryear, r_port, min_yr, max_yr) %>% 
  arrange(fleet_name, r_port) %>% as.data.frame 
  
tows_clust %>% filter(drvid %in% unique(dd$drvid) == FALSE) %>% 
  distinct(drvid, fleet_name) 


tows_clust %>% filter(drvid %in% unique(dd$drvid) == FALSE) %>% 
  group_by(drvid) %>% summarize(ntows = length(unique(haul_id))) %>%
  arrange(desc(ntows)) %>% as.data.frame


top_port <- dd %>% group_by(drvid, ryear, r_port) %>% 
  summarize(nhauls = length(unique(haul_id))) %>% group_by(drvid, r_port) %>%
  summarize(avg_hauls_port = mean(nhauls)) %>% group_by(drvid) %>%
  filter(avg_hauls_port == max(avg_hauls_port)) %>% as.data.frame
top_port <- top_port %>% select(drvid, r_port) %>% as.data.frame
names(top_port)[2] <- 'top_port'

dd <- dd %>% left_join(top_port, by = 'drvid')


dd %>% group_by(top_port, ryear) %>% summarize(nvess = length(unique(drvid))) %>%
  ggplot(aes(x = ryear, y = nvess)) + facet_wrap(~ top_port) + 
  geom_point() + geom_line()


#----------------------------------------------------------------------------------------
#New figure 2
#Add new name categories


plota <- dd %>% group_by(ryear) %>% summarize(nvess = length(unique(drvid))) %>%
  as.data.frame
agg_effort1 <- plota 

#Configure plotb
pb_names <- data_frame(top_port = unique(dd$top_port)) %>% as.data.frame
pb_names$fleet_name <- pb_names$top_port
pb_names[which(pb_names$fleet_name %in% unique(dd$fleet_name) == F), 
  "fleet_name"] <- "other"
pb_names[which(pb_names$fleet_name %in% c("BROOKINGS", 'CRESCENT CITY')), 
  "fleet_name"] <- "BROOKINGS & CRESCENT CITY"
pb_names[which(pb_names$top_port == "WESTPORT"), 'fleet_name'] <- 'other'

dd1 <- dd %>% left_join(pb_names, by = 'top_port')
dd1[which(dd1$fleet_name.x == "ILWACO/CHINOOK"), "fleet_name.y"] <- "other"

#By top port
dd1 %>% filter(fleet_name.y == 'other') %>%
  group_by(top_port, ryear) %>% summarize(nvess = length(unique(drvid))) %>%
  ggplot(aes(x = ryear, y = nvess)) + geom_point() + geom_line() +
  facet_wrap(~ top_port)

#By fleet name (includes other)
# dd1 %>% group_by(fleet_name.y, ryear) %>% summarize(nvess = length(unique(drvid))) %>%
#   ggplot(aes(x = ryear, y = nvess)) + geom_point() + geom_line() +
#   facet_wrap(~ fleet_name.y)

plotc <- dd %>% group_by(ryear) %>% summarize(nhauls = length(unique(haul_id))) %>%
  as.data.frame

#-------------------------------------------
#Save as tiff that is 85 mm wide

tiff(width = 85, height = 128, units = 'mm', res = 300, 
  file = 'figs/fig1.tiff')

par(mfrow = c(3, 1), mgp = c(.2, .5, 0), mar = c(.5, 1, 1, 0), 
  oma = c(3, 3, 0, .5))

#-------------------------------------------
#Number of vessels
plot(agg_effort1$ryear, agg_effort1$nvess, ylim = c(0, 120), 
  pch = 19, type = 'b', axes = F,
     ann = F, xaxs = 'i', yaxs = 'i', xlim = c(2006.5, 2014.5))
axis(side = 2, las = 2)
axis(side = 1, at = 2007:2014, lwd.tick = 0, ann = F, labels = F)
abline(v = 2010.5, lty = 2)
mtext(side = 2, "Number of vessels", line = 2, cex = .8)
mtext(side = 3, "a)", outer = F, adj = .01, line = -.5, cex = .8)

#-------------------------------------------
#Vessels in each port
fltz <- unique(dd1$fleet_name.y)
fltz_colors <- brewer.pal(length(fltz),"Dark2")
fltz_colors <- adjustcolor(fltz_colors, alpha.f = .7)

plotb <- dd1 %>% group_by(ryear, fleet_name.y) %>% summarize(nvess = length(unique(drvid))) %>%
  as.data.frame

#Format legend
ff <- data_frame(fltz, fltz_colors)
max_vess <- plotb %>% group_by(fleet_name.y) %>% summarize(max_nvess = max(nvess)) %>%
  arrange(desc(max_nvess))

names(max_vess)[1] <- "fltz"
ff <- ff %>%  left_join(max_vess, by = 'fltz') %>% arrange(desc(max_nvess)) %>% as.data.frame
ff$plot_val <- c("Astoria", "other", "Newport", "Charleston",
                 "Brookings & C.C.", "Eureka", 
                 "Fort Bragg")
# ff$plot_val <- c('Other', 'Astoria', "Charleston", "Newport", 'Eureka', "Brookings & Crescent City",
#   "Fort Bragg")

#Actual plot
plot(plotb$ryear, plotb$nvess, ann = F, axes = F, xlim = c(2006.5, 2014.5), 
  type = 'n', ylim = c(0, 30))
for(ii in 1:length(fltz)){
  temp <- subset(plotb, fleet_name.y == fltz[ii])
  points(temp$ryear, temp$nvess, pch = 19, type = 'o', col = fltz_colors[ii])
  # lines(temp$set_year, temp$nvess, pch = 19)
}
axis(side = 1, at = 2007:2014, lwd.tick = 0, ann = F, labels = F) 
axis(side = 2, las = 2 ) 
abline(v = 2010.5, lty = 2)
par(xpd = T)

legend(x = 2010.75, y = 34, legend = ff$plot_val[1:5], pch = 19, 
  bty = 'n', col = ff$fltz_colors[1:5], y.intersp = .8)
legend(x = 2012.5, y = 34, legend = ff$plot_val[6:8], pch = 19, 
  bty = 'n', col = ff$fltz_colors[6:8], y.intersp = .8)
mtext(side = 3, "b)", outer = F, adj = .01, line = -1,
  cex = .8)
mtext(side = 2, "Number of vessels", line = 2, cex = .8)

#-------------------------------------------
#Number of tows
plot(plotc$ryear, plotc$nhauls / 1000, ylim = c(0, 20), pch = 19, type = 'b',
     col = 'black', ann = F, axes = F, xaxs = 'i', yaxs = 'i', xlim = c(2006.5, 2014.5))
axis(side = 2, las = 2)
axis(side = 1, at = c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014))
abline(v = 2010.5, lty = 2)
mtext(side = 2, "Number of tows (1000s)", line = 2, cex = .8)
mtext(side = 3, "c)", outer = F, adj = .01, line = -1, cex = .8)
mtext(side = 1, "Year", outer = T, line = 1.5, cex = .8)

dev.off()






#-------------------------------------------
#Paper calculations
# (mean(plota$nvess[1:4]) - mean(plota$nvess[5:8])) / mean(plota$nvess[1:4]) * 100
# mean(plota$nvess[1:4]); mean(plota$nvess[5:8])
# (73.5 - 58.5) / 73.5 * 100

# #Plot b
# ast <- plotb %>% filter(fleet_name.y == "ASTORIA / WARRENTON") %>% select(nvess)
# (mean(ast$nvess[1:4]) - mean(ast$nvess[5:8])) / mean(mean(ast$nvess[1:4]))

# ast <- plotb %>% filter(fleet_name.y == "NEWPORT") %>% select(nvess)
# (mean(ast$nvess[1:4]) - mean(ast$nvess[5:8])) / mean(mean(ast$nvess[1:4]))

# #Plot c metrics
# mean(plotc$nhauls[1:4])
# mean(plotc$nhauls[5:8])
# (mean(plotc$nhauls[1:4]) - mean(plotc$nhauls[5:8])) / 
#   mean(plotc$nhauls[1:4]) * 100





#-------------------------------------------

# dd %>% group_by(ryear, r_port) %>% summarize(nvess = length(unique(drvid))) %>%
#   ggplot(aes(x = ryear, y = nvess)) + geom_point() + 
#   facet_wrap(~ r_port) 


# dd %>% group_by(ryear, fleet_name) %>% summarize(nvess = length(unique(drvid))) %>%
#   ggplot(aes(x = ryear, y = nvess)) + geom_point() + 
#   facet_wrap(~ fleet_name) 


# dd %>% filter(is.na(fleet_name)) %>% group_by(ryear, r_port) %>%
#   summarize(nvess = length(unique(drvid))) %>%
#   ggplot(aes(x = ryear, y = nvess)) + geom_point() + 
#   facet_wrap(~ r_port)
# dd %>% filter(is.na(fleet_name))


# dd %>% filter(drvid == "923632") %>% ggplot() + geom_point(aes(x = avg_long,
#   y = avg_lat))

#   %>% filter(drvid == 640718) %>% distinct(r_port) == FALSE) %>%


# dd %>% group_by(drvid, r_port) %>%

# #Find the most common return port


# dd %>% filter(drvid =)

# dd %>% filter(drvid == 244706) %>% distinct(ryear)
# dd %>% filter(drvid == 640718) %>% distinct(r_port)







# dd %>% filter(r_port == "ASTORIA / WARRENTON") %>% distinct(fleet_name)
# dd %>% filter(fleet_name == "ASTORIA / WARRENTON") %>% distinct(dport_desc)


# dd %>% fleet_name
# dd 

# ## REMOVE DUPLICATE TOWS and isolate years of interest
# LBK_clean = LBK[!duplicated(LBK$HAUL_ID),]
# LBK_clean = LBK_clean %>% filter(RYEAR < 2015, RYEAR > 2006)
# LBK_clean$DRVID = as.character(LBK_clean$DRVID)

# #### summarize participation by vessel, calculate total tows per vessel over all years
# N_VESSEL_LBK = as.data.frame(table(LBK_clean$DRVID))
# hist(N_VESSEL_LBK$Freq) # played around with # breaks and looks like 500 is tows is a good distinction

# # Find vessel IDs with more than 500 tows overall

# N_VESSEL_LBK_MAJOR <- N_VESSEL_LBK %>% filter(Freq > 500)
# # N_VESSEL_LBK_MAJOR <-  filter(N_VESSEL_LBK, Freq > 500)

# # filter dataset to vessels with more than 500 tows overall
# LBK_clean_major = filter(LBK_clean, DRVID %in% N_VESSEL_LBK_MAJOR$Var1)

# # number of vessels per year
# Vessel_yr_LBK = summarise(group_by(LBK_clean_major, RYEAR), N_VESSEL_LBK_YR = length(unique(DRVID)))

# # mean number of vessels before catch shares
# mean(Vessel_yr_LBK$N_VESSEL_LBK_YR[1:4]) # 72.5
# # after
# mean(Vessel_yr_LBK$N_VESSEL_LBK_YR[5:8]) # 57


# #----------------------------------------------------------------------------------------v
# # agg_effort <- obs_data %>% group_by(set_year) %>% summarize(nvess = length(unique(drvid)), 
# #                                                             ntows = length(unique(haul_id)),
# #                                                             avg_depth = mean(avg_depth))


# agg_effort <- tows_clust %>% group_by(set_year) %>% summarize(nvess = length(unique(drvid)), 
#                                                             ntows = length(unique(haul_id)),
#                                                             avg_depth = mean(avg_depth))

# #Number of vessels
# (mean(agg_effort$nvess[5:8]) - mean(agg_effort$nvess[1:4]))
# (mean(agg_effort$nvess[5:8]) - mean(agg_effort$nvess[1:4])) / mean(agg_effort$nvess[1:4])

# mean(agg_effort$ntows[1:4])
# mean(agg_effort$ntows[5:8])
# (mean(agg_effort$ntows[5:8]) - mean(agg_effort$ntows[1:4]))
# (mean(agg_effort$ntows[5:8]) - mean(agg_effort$ntows[1:4])) / mean(agg_effort$ntows[1:4])

# mean(agg_effort[1:4, 'nvess'], na.rm = T)

# agg_effort$set_year <- as.numeric(agg_effort$set_year)

# agg_effort1 <- agg_effort

# #Number of vessels in each fleet
# fleet_vess <- tows_clust %>% group_by(fleet_name, set_year) %>% summarize(nvess = length(unique(drvid))) %>% 
# 	as.data.frame
# fleet_vess$plot_fleet <- fleet_vess$fleet_name
# fleet_vess[which(fleet_vess$fleet_name %in% c("ASTORIA / WARRENTON", "CRESCENT CITY", 'EUREKA',
# 	'CHARLESTON (COOS BAY)', 'BROOKINGS', 'FORT BRAGG', 'NEWPORT') == FALSE), 'plot_fleet'] <- 'other'
# fleet_vess[which(fleet_vess$fleet_name %in% c("CRESCENT CITY", 'BROOKINGS')), 'plot_fleet'] <- 
# 	"CRESCENT CITY AND BROOKINGS"

# fleet_vess <- fleet_vess %>% group_by(set_year, plot_fleet) %>% summarize(nvess = sum(nvess)) %>% 
# 	arrange(plot_fleet) %>% as.data.frame
# #----------------------------------------------------------------------------------------v
# #Run permutation to calculate the

# vess_sig <- simple_permute(input = agg_effort %>% as.data.frame, perm_column = 'nvess')
# tows_sig <- simple_permute(input = agg_effort %>% as.data.frame, perm_column = 'ntows')

# hist(vess_sig[[2]])

# #Look at distinct hauls
# unq_tows <- tows_clust %>% distinct(haul_id, .keep_all = T)
# seed <- 300

# unq_tows$samp_year <- sample(unq_tows$set_year, replace = F)
# unq_tows$samp_when <- 'before'
# unq_tows[which(unq_tows$samp_year >= 2011), 'samp_when'] <- 'after'

# unq_tows %>% group_by(samp_year) %>% summarize(nvess = length(unique(drvid)), 
#   ntows = length(unique(haul_id)))

# sample(unq_tows$set_year, replace = F) %>% head
# perms <- tows_clust %>% select(when, haul_id, drvid)

# agg_effort$when <- 'before'
# agg_effort[which(agg_effort$set_year >= 2011), 'when'] <- 'after'

# #----------------------------------------------------------------------------------------v
# #Figure 1
# png(width = 5.7, height = 8.6, units = 'in', res = 200, file = 'figs/ch4_fig1.png')

# par(mfrow = c(3, 1), mgp = c(.2, .5, 0), mar = c(.5, 1, 1, 0), oma = c(3, 4, 0, 0))

# #-------------------------------------------
# #Number of vessels
# plot(agg_effort1$set_year, agg_effort1$nvess, ylim = c(0, 125), pch = 19, type = 'b', axes = F,
#      ann = F, xaxs = 'i', yaxs = 'i', xlim = c(2006.5, 2014.5))
# axis(side = 2, las = 2)
# axis(side = 1, at = 2007:2014, lwd.tick = 0, ann = F, labels = F)
# abline(v = 2010.5, lty = 2)
# mtext(side = 2, "Number of Vessels", line = 3, cex = 1.2)
# mtext(side = 3, "a)", outer = F, adj = .01, line = -1)

# #-------------------------------------------
# #Vessels in each port
# fltz <- unique(fleet_vess$plot_fleet)
# fltz_colors <- brewer.pal(7,"Dark2")
# fltz_colors <- adjustcolor(fltz_colors, alpha.f = .7)

# #Format legend
# ff <- data_frame(fltz, fltz_colors)
# max_vess <- fleet_vess %>% group_by(plot_fleet) %>% summarize(max_nvess = max(nvess)) %>%
# 	arrange(desc(max_nvess))
# names(max_vess)[1] <- "fltz"
# ff <- ff %>%  left_join(max_vess, by = 'fltz') %>% arrange(desc(max_nvess)) %>% as.data.frame
# ff$plot_val <- c('Other', 'Astoria', "Charleston", "Newport", 'Eureka', "Brookings & Crescent City",
# 	"Fort Bragg")

# #Actual plot
# plot(fleet_vess$set_year, fleet_vess$nvess, ann = F, axes = F, xlim = c(2006.5, 2014.5), type = 'n',
# 	ylim = c(0, 60))
# for(ii in 1:length(fltz)){
#   temp <- subset(fleet_vess, plot_fleet == fltz[ii])
#   points(temp$set_year, temp$nvess, pch = 19, type = 'o', col = fltz_colors[ii])
#   # lines(temp$set_year, temp$nvess, pch = 19)
# }
# axis(side = 1, at = 2007:2014, lwd.tick = 0, ann = F, labels = F) 
# axis(side = 2, las = 2 ) 
# abline(v = 2010.5, lty = 2)
# legend('topright', legend = ff$plot_val, pch = 19, bty = 'n', col = ff$fltz_colors)
# mtext(side = 3, "b)", outer = F, adj = .01, line = -1)
# mtext(side = 2, "Number of Vessels", line = 3, cex = 1.2)

# #-------------------------------------------
# #Number of tows
# plot(agg_effort1$set_year, agg_effort1$ntows, ylim = c(0, 20500), pch = 19, type = 'b',
#      col = 'black', ann = F, axes = F, xaxs = 'i', yaxs = 'i', xlim = c(2006.5, 2014.5))
# axis(side = 2, las = 2)
# axis(side = 1, at = c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014))
# abline(v = 2010.5, lty = 2)
# mtext(side = 2, "Number of Tows", line = 3, cex = 1.2)
# mtext(side = 3, "c)", outer = F, adj = .01, line = -1)
# mtext(side = 1, "Year", outer = T, line = 1.5, cex = 1.2)


# dev.off()


# #----------------------------------------------------------------------------------------
# # Look at number of vessels changes


# tows_clust %>% group_by(fleet_name, set_year) %>% summarize(nvess = length(unique(drvid))) %>% 
#   filter(fleet_name %in% c("ASTORIA / WARRENTON", 'BROOKINGS', 'CHARLESTON (COOS BAY)', 
#     'CRESCENT CITY', 'EUREKA', 'NEWPORT'))
#   ggplot() + geom_line(aes(x = set_year, y = nvess, group = fleet_name)) + facet_wrap(~ fleet_name)











# =======
# #----------------------------------------------------------------------------------------v
# # agg_effort <- obs_data %>% group_by(set_year) %>% summarize(nvess = length(unique(drvid)), 
# #                                                             ntows = length(unique(haul_id)),
# #                                                             avg_depth = mean(avg_depth))


# agg_effort <- tows_clust %>% group_by(set_year) %>% summarize(nvess = length(unique(drvid)), 
#                                                             ntows = length(unique(haul_id)),
#                                                             avg_depth = mean(avg_depth))
# save(agg_effort, file = 'output/agg_effort.Rdata')

# #----------------------------------------------------------------------------------------v
# devtools::install_github("peterkuriyama/ch4", auth_token = "83f947b716e40172803f0ff798c46f5ff9ca3cd1")
# library("ch4")
# library(RColorBrewer)

# #Number of vessels
# load(file = "output/agg_effort.Rdata")

# agg_effort$when <- 'before'
# agg_effort[which(agg_effort$set_year >= 2011), 'when'] <- 'after'

# (mean(agg_effort$nvess[5:8]) - mean(agg_effort$nvess[1:4]))
# (mean(agg_effort$nvess[5:8]) - mean(agg_effort$nvess[1:4])) / mean(agg_effort$nvess[1:4])

# mean(agg_effort$ntows[1:4])
# mean(agg_effort$ntows[5:8])
# (mean(agg_effort$ntows[5:8]) - mean(agg_effort$ntows[1:4]))
# (mean(agg_effort$ntows[5:8]) - mean(agg_effort$ntows[1:4])) / mean(agg_effort$ntows[1:4])

# agg_effort$set_year <- as.numeric(agg_effort$set_year)

# agg_effort1 <- agg_effort

# #----------------
# #Number of vessels in each fleet
# fleet_vess <- tows_clust %>% group_by(fleet_name, set_year) %>% summarize(nvess = length(unique(drvid))) %>% 
# 	as.data.frame
# save(fleet_vess, file = "output/fleet_vess.Rdata")
# #----------------

# load(file = 'output/fleet_vess.Rdata')

# fleet_vess$plot_fleet <- fleet_vess$fleet_name
# fleet_vess[which(fleet_vess$fleet_name %in% c("ASTORIA / WARRENTON", "CRESCENT CITY", 'EUREKA',
# 	'CHARLESTON (COOS BAY)', 'BROOKINGS', 'FORT BRAGG', 'NEWPORT') == FALSE), 'plot_fleet'] <- 'other'
# fleet_vess[which(fleet_vess$fleet_name %in% c("CRESCENT CITY", 'BROOKINGS')), 'plot_fleet'] <- 
# 	"CRESCENT CITY AND BROOKINGS"

# fleet_vess <- fleet_vess %>% group_by(set_year, plot_fleet) %>% summarize(nvess = sum(nvess)) %>% 
# 	arrange(plot_fleet) %>% as.data.frame
# #----------------------------------------------------------------------------------------v
# #Run permutation to calculate the

# vess_sig <- simple_permute(input = agg_effort %>% as.data.frame, perm_column = 'nvess')
# tows_sig <- simple_permute(input = agg_effort %>% as.data.frame, perm_column = 'ntows')

# hist(vess_sig[[2]])

# # #Look at distinct hauls
# # unq_tows <- tows_clust %>% distinct(haul_id, .keep_all = T)
# # seed <- 300
# # 
# # unq_tows$samp_year <- sample(unq_tows$set_year, replace = F)
# # unq_tows$samp_when <- 'before'
# # unq_tows[which(unq_tows$samp_year >= 2011), 'samp_when'] <- 'after'
# # 
# # unq_tows %>% group_by(samp_year) %>% summarize(nvess = length(unique(drvid)), 
# #   ntows = length(unique(haul_id)))
# # 
# # sample(unq_tows$set_year, replace = F) %>% head
# # perms <- tows_clust %>% select(when, haul_id, drvid)


# #----------------------------------------------------------------------------------------v
# #Figure 1
# png(width = 5.7, height = 8.6, units = 'in', res = 200, file = 'figs/ch4_fig1.png')

# par(mfrow = c(3, 1), mgp = c(.2, .5, 0), mar = c(.5, 1, 1, 0), oma = c(3, 4, 0, 0))

# #-------------------------------------------
# #Number of vessels
# plot(agg_effort1$set_year, agg_effort1$nvess, ylim = c(0, 125), pch = 19, type = 'b', axes = F,
#      ann = F, xaxs = 'i', yaxs = 'i', xlim = c(2006.5, 2014.5))
# axis(side = 2, las = 2)
# axis(side = 1, at = 2007:2014, lwd.tick = 0, ann = F, labels = F)
# abline(v = 2010.5, lty = 2)
# mtext(side = 2, "Number of Vessels", line = 3, cex = 1.2)
# mtext(side = 3, "a)", outer = F, adj = .01, line = -1)

# #-------------------------------------------
# #Vessels in each port
# fltz <- unique(fleet_vess$plot_fleet)
# fltz_colors <- brewer.pal(7,"Dark2")
# fltz_colors <- adjustcolor(fltz_colors, alpha.f = .7)

# #Format legend
# ff <- data_frame(fltz, fltz_colors)
# max_vess <- fleet_vess %>% group_by(plot_fleet) %>% summarize(max_nvess = max(nvess)) %>%
# 	arrange(desc(max_nvess))
# names(max_vess)[1] <- "fltz"
# ff <- ff %>%  left_join(max_vess, by = 'fltz') %>% arrange(desc(max_nvess)) %>% as.data.frame
# ff$plot_val <- c('Other', 'Astoria', "Charleston", "Newport", 'Eureka', "Brookings & Crescent City",
# 	"Fort Bragg")

# #Actual plot
# plot(fleet_vess$set_year, fleet_vess$nvess, ann = F, axes = F, xlim = c(2006.5, 2014.5), type = 'n',
# 	ylim = c(0, 60))
# for(ii in 1:length(fltz)){
#   temp <- subset(fleet_vess, plot_fleet == fltz[ii])
#   points(temp$set_year, temp$nvess, pch = 19, type = 'o', col = fltz_colors[ii])
#   # lines(temp$set_year, temp$nvess, pch = 19)
# }
# axis(side = 1, at = 2007:2014, lwd.tick = 0, ann = F, labels = F) 
# axis(side = 2, las = 2 ) 
# abline(v = 2010.5, lty = 2)
# legend('topright', legend = ff$plot_val, pch = 19, bty = 'n', col = ff$fltz_colors)
# mtext(side = 3, "b)", outer = F, adj = .01, line = -1)
# mtext(side = 2, "Number of Vessels", line = 3, cex = 1.2)

# #-------------------------------------------
# #Number of tows
# plot(agg_effort1$set_year, agg_effort1$ntows, ylim = c(0, 20500), pch = 19, type = 'b',
#      col = 'black', ann = F, axes = F, xaxs = 'i', yaxs = 'i', xlim = c(2006.5, 2014.5))
# axis(side = 2, las = 2)
# axis(side = 1, at = c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014))
# abline(v = 2010.5, lty = 2)
# mtext(side = 2, "Number of Tows", line = 3, cex = 1.2)
# mtext(side = 3, "c)", outer = F, adj = .01, line = -1)
# mtext(side = 1, "Year", outer = T, line = 1.5, cex = 1.2)

# dev.off()


# #----------------------------------------------------------------------------------------
# # Look at number of vessels changes

# # 
# # tows_clust %>% group_by(fleet_name, set_year) %>% summarize(nvess = length(unique(drvid))) %>% 
# #   filter(fleet_name %in% c("ASTORIA / WARRENTON", 'BROOKINGS', 'CHARLESTON (COOS BAY)', 
# #     'CRESCENT CITY', 'EUREKA', 'NEWPORT'))
# #   ggplot() + geom_line(aes(x = set_year, y = nvess, group = fleet_name)) + facet_wrap(~ fleet_name)
# # 
# # 









# # >>>>>>> 60f18a0663453a379fbc6814a12e7bea80d936d0
