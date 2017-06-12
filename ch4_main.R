#Working script for Chapter 4
#Evaluating catch-quota balancing under different TAC configurations in a catch
#share fishery

setwd('/Users/peterkuriyama/School/Research/ch4')

library(devtools)
devtools::install_github('peterkuriyama/ch4')
      
#Make sure that packages are loaded
library(ch4)
library(plyr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(parallel)
library(lubridate)

#---------------------------------------------------------------------------------
#Start of Analysis
#Load the Tow Data
# wc_data <- load_tow_data() #Data should have spatial ids with years and without years

# #Data Processing initially
# #Try sampling of data to stay within the TAC of multiple species
# wc_data <- wc_data[-grep("\\.", wc_data$row_name), ]
# save(wc_data, file = 'output/unique_wc_tows.Rdata')

#Load Unique tows
load('output/unique_wc_tows.Rdata')

#Load states map
states_map <- map_data("state")
  
#Plot clusters function
# plot_clusts()

#Do the bin2d thing
names(wc_data)
write.csv(names(wc_data), row.names = FALSE, file = 'output/logbook_names.csv')

#---------------------------------------------------------------------------------
#Filter so that only tows with ha_ratio between 0.6 and 1.2 are kept
wc_data$ha_ratio <- wc_data$hpounds / wc_data$apounds

#Lose about half of the tows
wc_data_orig <- wc_data

#---------------------------------------------------------------------------------
load("/Users/peterkuriyama/School/Research/ch2_vms_old/data/LBKDATA_Barnett_Logbook_Data_2002_2014_2015-11-24.Rda")
lewis <- LBK.out
names(lewis) <- tolower(names(lewis))


lewis <- lewis %>% filter(ryear >= 2008 & ryear <= 2013)

paste(c(1:5), collapse = ' ')

peter_logbook <- lewis %>% group_by(agid, ryear) %>% summarize(peter_nunq_tows = length(unique(haul_id)),
  nunq_vessels = length(unique(drvid)), avg_duration = round(mean(duration, na.rm = T), digits = 1), 
  avg_uplat = round(mean(up_lat, na.rm = T), digits = 1), 
  avg_uplong = round(mean(up_long, na.rm = TRUE), digits = 1), avg_depth1 = round(mean(depth1), digits = 1),
  targets = paste0(unique(target), collapse = " ")) %>%
  as.data.frame 
names(peter_logbook) <- c('agid', 'ryear', 'peter_nunq_tows', 'nunq_vessels', "peter_avg_duration",
'peter_avg_uplat', 'peter_avg_uplong', 'peter_avg_depth1', 'targets')

targs <- peter_logbook$targets
peter_logbook$targets <- NULL

#Read in Dan's data
dan_logbook <- read.csv('output/dan_logbook.csv')
names(dan_logbook) <- c('agid', 'ryear', 'dan_nunq_tows', 'sumofduration', 'dan_avg_duration', 'dan_avg_uplat',
  "dan_avg_uplong", 'dan_avg_depth1')

comp <- left_join(peter_logbook, dan_logbook, by = c('agid', 'ryear'))

#Calculate differences
comp$diff_nunq_tows <- comp$peter_nunq_tows - comp$dan_nunq_tows
comp$diff_avg_duration <- comp$peter_avg_duration - comp$dan_avg_duration
comp$diff_avg_uplat <- comp$peter_avg_uplat - comp$dan_avg_uplat
comp$diff_avg_uplong <- comp$peter_avg_uplong - comp$dan_avg_uplong
comp$diff_avg_depth1 <- comp$peter_avg_depth1 - comp$dan_avg_depth1

#Organize the column by metric
comp1 <- comp[, c(1, 2, 4, grep('nunq_tows', names(comp)),
  c(5, 11, 16),  
  grep('uplat', names(comp)),
  grep('uplong', names(comp)),
  grep('depth1', names(comp)))]
comp1$targets <- targs  

write.csv(comp1, 'output/dan_peter_comparison.csv', row.names = FALSE)

#Combine the two


#Check numbers with dan holland
wc_data_orig %>% group_by(agid, ryear) %>% summarize(count = n(), nunq_tos = length(unique(haul_id)),
  avgduration = mean(duration), avguplat = mean(up_lat), avguplong = mean(up_long), avgdepth = mean(depth1))

unique(lewis$gr_sector)



#Filter data based on ratios, removes about 50% of the data
# filt_dat <- wc_data %>% filter(ha_ratio >= 0.6 & ha_ratio <= 1.2) 
# input <- filt_dat

#---------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
#Analaysis

#Data are now filtered so hailed and adjusted pounds in relative agreement
#ch4_format_data pulls only the top100 clusters (by number of tows)

#takes 50 seconds for top100, 1.3 minutes for unfiltereed
top100 <- ch4_format_data(filt_dat, top100 = TRUE) 

# clusts <- ch4_format_data(filt_dat, top100 = FALSE) 

#Add date
top100_clusts$date <- paste0(top100_clusts$dyear, "-", top100_clusts$dmonth, "-",
  top100_clusts$dday)
top100_clusts$date <- ymd(top100_clusts$date)

#Table of significance; slightly different, look into at some point
top100_clusts %>% distinct(unq_clust, sig_ntows) %>% nrow
top100_clusts %>% distinct(unq_clust, sig_ntows) %>% group_by(sig_ntows) %>% 
  summarize(sigtows = length(unique(unq_clust)) / 100) 
top100_clusts %>% distinct(unq_clust, sig_nvess) %>% group_by(sig_nvess) %>% 
  summarize(sigtows = length(unique(unq_clust)) / 100) 

#Add on species category percentages
top100_clusts <- top100_clusts %>% group_by(dyear, unq_clust, type) %>% 
  mutate(avg_perc = mean(perc)) %>% as.data.frame

top100_clusts <- ch4_perm_test(input = top100_clusts, column = 'avg_perc', ndraws = 1000, 
  gb = "dyear, unq_clust, type", clust_cat = 'unq_clust, type', summ = "mean(avg_perc)",
  crit = "<" )

start_time <- Sys.time()
top100_clusts <- ch4_perm_test(input = top100_clusts, column = "hpounds", ndraws = 1000,
  gb = "dyear, unq_clust, species", clust_cat = 'unq_clust, species', 
  summ = "hpounds", crit = "<", annual = FALSE, save_resamps = TRUE)
runtime <- Sys.time() - start_time
(runtime)
#took 26 minutes

save(top100_clusts, file = 'output/top100_clusts.Rdata')

#Remove old duplicated rows
top100_clusts[, c(47, 48)] <- NULL

top100_clusts %>% filter(sig_hpounds == 'sig increase', type == 'targets') %>%
  ggplot(aes())


#---------------------------------------------------------------------------------------------
#Which clusters had increases in vessels and target catches?
top100_clusts %>% filter(sig_nvess == 'sig increase')
top100_clusts %>% group_by(sig_nvess) %>% summarize(nclusts = length(unique(unq_clust)))
top100_clusts %>% group_by(sig_ntows) %>% summarize(nclusts = length(unique(unq_clust)))


unique(top100_clusts$sig_ntows)


top100_clusts %>% filter(sig_nvess == 'sig increase')


#Which clusters had significant increases in target species
top100_clusts %>% filter(sig_hpounds == 'sig increase', type == 'targets') %>% 
  ggplot() + geom_segment(aes(x = -set_long, xend = -up_long, y = set_lat, 
    yend = up_lat, colour = unq_clust)) + 
  facet_wrap(~ species)

top100_clusts %>% filter(sig_hpounds == 'sig increase', type == 'targets')

#Load the data here to save time

unique(top100_clusts$sig_hpounds)
#-------------------------------------------------
#-------------------------------------------------

# gb <- "dyear, unq_clust, type"
# summ <- "unique(avg_perc)"
# clust_cat <- 'unq_clust, type'

#Summarize significance
#Check that things are working right
temp <- top100_clusts %>% filter(sig_avg_perc == 'sig increase', type == 'weaks') 

#-------------------------------------------------
top100_clusts %>% filter(species == 'Yelloweye Rockfish')
top100_clusts %>% filter(species == 'Widow Rockfish')

#-------------------------------------------------
#Increase in catch percentages
dec_targets <- top100_clusts %>% filter(sig_avg_perc == 'sig decrease', type == 'targets')
dec_weaks <- top100_clusts %>% filter(sig_avg_perc == 'sig decrease', type == 'weaks')

dec_targets %>% distinct(unq_clust, sig_ntows) %>% group_by(sig_ntows) %>
dec_targets %>% distinct(unq_clust, sig_ntows) %>% group_by(sig_ntows) %>% 
  summarize(sigs = length(unique(unq_clust))) %>%


#Histograms of catch category percentages by port
top100_clusts %>% filter(species == 'Petrale Sole') %>% ggplot(aes(x = perc)) + 
  geom_histogram() + facet_wrap(~ dport_desc)

#Constraining Species

#Were certain ports more selective
top100_clusts %>% filter(type %in% c('targets', 'weaks')) %>% 
  ggplot(aes(x = prop_zero, y = skew)) + geom_point(aes(colour = type)) + 
  facet_wrap(~ dport_desc)

#Target species only
top100_clusts %>% filter(type %in% c('targets')) %>%
  ggplot(aes(x = prop_zero, y = skew)) + geom_point(aes(colour = dport_desc)) + 
  facet_wrap(~ species + dport_desc)

#Weak stock species
top100_clusts %>% filter(type %in% c('weaks')) %>%
  ggplot(aes(x = prop_zero, y = skew)) + geom_point(aes(colour = dport_desc)) + 
  facet_wrap(~ species + dport_desc)


ggplot(aes(x = prop_zero, y = skew)) + geom_point(aes(colour))

top100_clusts %>% filter(species == 'Sablefish') %>% 
  ggplot(aes(x = prop_zero, y = skew)) + geom_point(aes(colour = type)) + 
  facet_wrap(~ dport_desc)


ggplot(top100_clusts, aes(x = prop_zero, y = skew)) + geom_point()



top100_clusts %>% group_by(unq_clust, species) %>% mutate(avg_perc = mean(perc)) %>%
  select(dport_desc, type, perc, avg_perc) %>% filter(dport_desc %in% ports$dport_desc,
    type %in% c('targets', 'weaks')) %>%
  ggplot() + geom_histogram(aes(x = perc)) + facet_wrap(~ type + dport_desc, ncol = 2)

%>% summarize(avg_perc = mean(perc))


dec_ <- plot_clusts(dat = dcatch, show_fig = F, specify_lims = T)
dd + facet_wrap(~ type)

itow_plot <- pl

plot_clusts(dat = temp, show_fig = T, specify_lims = F)

  select(unq_clust, dyear, type, when, avg_perc) %>% distinct() %>%
  arrange(unq_clust, dyear)

temp

# ch4_perm_test(input = temp, column = 'avg_perc', ndraws = 1000, 
#   gb = 'dyear, unq_clust, type', summ = 'mean(avg_perc)', 
#   clust_cat = 'unq_clust, type', crit = "<")

  arrange(unq_clust, dyear) %>% 
  ggplot(aes(x = dyear, y = avg_perc)) + geom_line(aes(colour = unq_clust, group = unq_clust))

#Resample these tows






  dcast(unq_clust + type ~ when, value.var = "avg_perc")

temp %>% filter(unq_clust == 2, type == 'targets') %>% distinct() %>% head

dcast(unq_clust, )

plot_clusts(dat = temp, show_fig = TRUE, specify_lims = TRUE)


#--------------------------------------------------------------------------------
#Plot clusters on a map
xx <- plot_clusts(dat = top100_clusts, show_fig = TRUE, specify_lims = TRUE)




#--------------------------------------------------------------------------------
#Sampling in clusters, way to move among them
#pick port
astoria <- top100_clusts %>% filter(dport_desc == 'ASTORIA')

astoria %>% geom

#Come up with rules to move among clusters
#Need to look into how individual vessels move among clusters, 
# is there a fixed order for each vessel?
tt <- astoria %>% filter(drvid == '626614')
tt <- tt %>% arrange(date)


#Did effort for individual vessels contract or exapnd? Is there a metric for this?
#Pinsky centroid stuff

geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat, colour = clust)) + 
  facet_wrap(~ dyear)

#how much variability is there in sablefish catches, for example
png(width = 13, height = 10, file = 'figs/cluster_delta_plots.png', units = 'in', 
  res = 200)
top100_clusts %>% filter(type %in% c('targets', 'weaks')) %>% ggplot() + 
  geom_point(aes(x = prop_zero, y = skew, colour = unq_clust)) + facet_wrap(~ species) 
dev.off()

#Did individual vessels fish in fewer clusters over time due to catch shares?
top100_clusts %>% group_by(drvid, dyear) %>% summarize(nclusts = length(unique(unq_clust))) %>%
  ggplot(aes(x = dyear, y = nclusts)) + geom_line(aes(colour = drvid))

#Write function to run a permutation test for different columns
  #Could be number of clusters per vessel, number of vessels per cluster, number of tows 
  #per cluster for example





#--------------------------------------------------------------------------------
#Way to look at species interactions
#top
high <- top100_clusts %>% filter(high_weak_perc == max(top100_clusts$high_weak_perc),
  type %in% c('targets', 'weaks')) 

pp <- high %>% select(perc, species, haul_id) %>% dcast(haul_id ~ species, value.var = 'perc')
#Replace all NAs with 0
pp <- melt(pp)
pp[is.na(pp$value), 'value'] <- 0

pp <- dcast(pp, haul_id ~ variable, value.var = 'value')
pp$haul_id <- factor(pp$haul_id)

pairs(pp[, c(3, 4, 7, 8)])

ggplot(high) + geom_point(aes(x = date, y = perc)) + facet_wrap(~ species)


  

high %>% select(perc, species) %>% pairs

pairs(high[c("perc", "species")])



%>% 
  ggplot() + geom_point(aes(x = date, y = perc)) + facet_wrap(~ species)


ggplot(top100_clusts, )

#Add temporal values
top100_clusts %>% filter(high_weak_perc >= .001) %>% 
  arrange(dyear, dmonth, dday) %>% filter(unq_clust == 1073) %>%
  ggplot() + 


length(unique(top100_clusts$unq_clust))
#Number of tows histogram
top100_clusts %>% select(unq_clust, ntows) %>% distinct() %>% ggplot() + 
  geom_histogram(aes(x = ntows))

#Number of vessels in each cluster histogram
top100_clusts %>% select(unq_clust, nvess) %>% distinct() %>% ggplot() + 
  geom_histogram(aes(x = nvess))


#Look into clusters that high bycatch percentages of weak stock,


top100_clusts %>% select(unq_clust, high_weak_perc) %>% distinct() %>% ggplot() + 
  geom_histogram(aes(x = high_weak_perc))
hist

#Avoidance of certain locations after large haul of overfished species?

top100_clusts %>% filter(perc == )






#--------------------------------------------------------------------------------
#Figures

#Plot of vessels fishing in clusters
clust_tows %>% group_by(drvid, dyear) %>% summarize(nclusts = length(unique(unq_clust))) %>% 
  arrange(desc(nclusts)) %>% group_by(drvid) %>% mutate(avg_nclusts = mean(nclusts)) %>% 
  arrange(desc(avg_nclusts)) %>% ggplot(aes(x = dyear, y = nclusts)) + geom_line(aes(colour = drvid))


#General plot of tows
clust_tows %>% filter(drvid == 511697) %>% ggplot() + 
  geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat, colour = clust)) + 
  facet_wrap(~ dyear)

#look at order of tows after catch of weak stock
top100_clusts <- top100_clusts %>% arrange(desc(high_weak_perc))
top100_clusts %>% filter(unq_clust == 37, type == 'weaks', perc >= 0.5)

top100_clusts %>% filter(unq_clust == 37, dyear == 2012, type == 'weaks', perc >= 0.5) 

top100_clusts %>% filter(drvid == "511697", dyear == 2012) %>% ggplot() + 
  geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat)) + 
    facet_wrap(~ dmonth)



#--------------------------------------------------------------------------------
#Do certain tows catch a higher proportion of certain species?
#Is this due to catch shares?

#Look at median and mean values of certain species catch in each cluster
# temp <- top100_clusts %>% filter(unq_clust == 5)

top100_clusts %>% group_by(when, unq_clust, species) %>% mutate(mn = mean(hpounds, na.rm = T),
  med = median(hpounds)) 
med_mean_spp <- top100_clusts %>% group_by(unq_clust, when, species) %>% mutate(med = median(hpounds))
  ,
  mn = mean(hpounds))

   %>% select(med, mn, type)

#Median values
meds <- med_mean_spp %>% dcast(unq_clust + species ~ when, value.var = 'med')
meds$diffs <- meds$after - meds$before

#Mean values
means <- med_mean_spp %>% dcast(unq_clust + species ~ when, value.var = 'mn')
means$diffs <- means$after - means$before

hist(meds$diffs, breaks = 30)
hist(means$diffs, breaks = 30)

#Relationship between number of tows and proportion of tows with bycatch?




  type == 'weaks', perc >= 0.5)

top100_clusts %>% filter(unq_clust == 37)


#Test hypothesis, 

#
#Fishers move towards areas with low bycatch


#Relationship between number of tows and proportion of tows with bycatch?
top100_clusts %>% distinct() %>% ggplot(aes(x = ntows, y = high_weak_perc)) + geom_point() +
  facet_wrap(~ sig_vess)

#Look at delta plots for tows with sig increase in number of 

top100_clusts %>% filter(sig_vess == 'sig increase', type == 'targets') %>% 
  ggplot(aes(x = hpounds)) + geom_histogram() + facet_wrap(~ species + when, ncol = 2)

  ggplot(aes(x = prop_zero, y = skew)) + geom_point() + facet_wrap(~ species)


#Which
top100_clusts %>% filter(type == 'targets', dport_desc == "ASTORIA") %>% 
  ggplot(aes(x = prop_zero, y = skew)) + 
  geom_point(aes(size = ntows, colour = high_weak_perc)) + facet_wrap(~ species)

ggplot(top100_clusts, aes(x = prop_zero, y = skew)) + geom_point(aes(size = high_weak_perc))

top100_clusts %>% arrange(desc(high_weak_perc)) %>% head



top100_clusts$high_weak_perc <- top100_clusts$nhigh / top100_clusts$ntows


  ,
  high_weak_perc = nhigh/ntows)
  ,
  high_weak_perc = nhigh / ntows) %>% select(high_weak_perc)

top100_clusts$weak_perc <- 

temp <- top100_clusts %>% filter(unq_clust == 5)

#What percentage of the tows caught a lot of a weak stock species?
top100_clusts %>% filter(unq_clust == 5, type == 'targets', high_perc == 'no', 
  haul_id == '10906516') 



top100_clusts %>% group_by(haul_id, type, high_perc) %>% mutate(prop = length(high_perc) / ntows) %>%
  select(prop)


length(unique(temp$haul_id))
temp %>% filter(type == 'weaks', high_perc == 'yes') %>% nrow / length(unique(temp$haul_id))

select(high_perc) %>% filter == 'yes'


top100_clusts %>% group_by(unq_clust) %>% mutate(numrow = length(perc)) %>% 
  group_by(unq_clust, type) %>% mutate(perc_high = sum(which(high_perc == 'yes')) ) %>%
  select(perc_high)


select(numrow)


#Calculate frequency of high proportion of weaks
top100_clusts <- top100_clusts %>% group_by(clust, type) %>% mutate(nn = length(perc >= 0.5), freq_high = length(perc >= 0.5) / ntows) %>% 
  as.data.frame

temp <- top100_clusts %>% filter(clust == 5)


top100_clusts %>% distinct(species, prop_zero, skew) %>% ggplot(aes(x = prop_zero, y = skew, label = species)) + 
  geom_point() + geom_text(hjust = -.05)

#Rule to be delta plot targets is less than 0.5 prop_zero and lower than 0 skew

#--------------------------------------------------------------------------------
#Was there a change in catch compositions before and after in sig decrease clusters?
#Connect back with 


temp %>% group_by(species, when) %>% summarize(pounds = sum(hpounds)) %>% 
  arrange(desc(pounds)) %>% dcast(species ~ when, value.var = 'pounds')

temp %>% filter(species == "Dover Sole") %>% ggplot(aes(x = hpounds)) + geom_histogram() +
  facet_wrap(~ when)



#Look at differences in species-specific histograms



#Look at delta plots







#--------------------------------------------------------------------------------
#Find which clusters are visited the most
clust_tows %>% 
# summarize(nclusts = length(unique(clust))) %>% 
  group_by(drvid, dport_desc, clust) %>% mutate(nhauls_per_clust = length(unique(haul_id))) %>% 
  as.data.frame -> xx

#tc for test cluster
tc <- xx %>% filter(dport_desc == 'ASTORIA' & clust == 41) 
tc %>% group_by(species) %>% summarize(tot_hpounds = sum(hpounds), ntows = length(unique(haul_id))) %>% 
  arrange(desc(tot_hpounds)) %>% as.data.frame

tc %>% filter(species == 'Sablefish') %>% ggplot(aes(x = hpounds)) + geom_histogram() + 
  facet_wrap(~ dyear)

#Do Delta plots for each cluster, without time
deltas <- xx %>% group_by(dport_desc, clust, species) %>% mutate(prop_zero = 1 - (length(hpounds) / unique(ntows)),
  skew = calc_skew(log(hpounds))) %>% as.data.frame()

#Delta plots for all species
ggplot(deltas, aes(x = prop_zero, y = skew)) + geom_point(aes(colour = clust)) + 
  facet_wrap(~ species)

#Have to filter these by species,
  

   %>% ggplot(aes(prop_zero, y = skew)) + geom_point(aes(colour = species)) + 
  geom_label()

  1 - (length(hpounds) / ntows))


#Is it the total pounds that are consistent
#Or the proportion of certain target species?


#Pick vessel 629368 and dig into clusters that it visited every year
clust_tows %>% filter(drvid == 629368) %>% distinct(dport_desc)

clust_tows %>% filter(drvid == 511697) %>% group_by(clust) %>% 
  summarize(nyears = length(unique(dyear))) %>% arrange(desc(nyears))



clust_tows %>% filter(drvid == 511697, clust) %>% ggplot() + 
  geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat, colour = clust)) + 
  facet_wrap(~ dyear)





