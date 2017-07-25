#---------------------------------------------------------------------------------
#Start of obs_dat, work off this script
setwd('/Users/peterkuriyama/School/Research/ch4')

#Load Packages
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(devtools)
library(maps)
library(doParallel)

#Install ch4 package
devtools::install_github("peterkuriyama/ch4", auth_token = "83f947b716e40172803f0ff798c46f5ff9ca3cd1")
library("ch4")

#Install delta plot functions
devtools::install_github("peterkuriyama/ch2vms/ch2vms")
library(ch2vms)

#States Map
states_map <- map_data("state")
#---------------------------------------------------------------------------------



#---------------------------------------------------------------------------------
#Load Data
# load("C:\\Users\\Lewis\\Documents\\Data\\OBDATA_Barnett_OBProcessed_Catch_Data_2002_2014_2015-10-21.Rda")
# obs_data <- OB.ad2
# rm(OB.ad2)



#Load obs_data

#add in prices
load('output/exvessel_formatted.Rdata')
exvessel$mt <- NULL
exvessel$pounds <- NULL
exvessel$value <- NULL
names(exvessel) <- c('set_year', 'd_state', 'species', 'exval_pound')

#Can find tows_clust in "ch4_cluster.R"

tows_clust <- left_join(tows_clust, exvessel, by = c("set_year", "d_state", "species"))

# load(file = "C://Users//Lewis//Documents//Data//comb_data.Rda")
# obs_data <- comb_data
# rm(comb_data)


#---------------------------------------------------------------------------------
#To Do
#Effort concentration in clusters
#Have looked at individual vessel shifts

#---------------------------------------------------------------------------------
#Code arranged by figures

#---------------------------------------------------------------------------------
#Aggregate measures of effort
agg_effort <- obs_data %>% group_by(set_year) %>% summarize(nvess = length(unique(drvid)), 
                                                            ntows = length(unique(haul_id)),
                                                            avg_depth = mean(avg_depth))
agg_effort$set_year <- as.numeric(agg_effort$set_year)

#-----------------------------------
#By State
#Number of Vessels Decreased
obs_data %>% filter(set_year >= 2007) %>% group_by(set_year, r_state) %>% summarize(nvess = length(unique(drvid))) %>% 
  ggplot() + geom_point(aes(x = set_year, y = nvess)) + geom_line(aes(x = set_year, y = nvess)) + 
  facet_wrap(~ r_state) + geom_vline(xintercept = 2010.5, lty = 2)

#Number of tows decreased
obs_data %>% filter(set_year >= 2007) %>% group_by(set_year, r_state) %>% summarize(ntows = length(unique(haul_id))) %>% 
  ggplot() + geom_point(aes(x = set_year, y = ntows)) + geom_line(aes(x = set_year, y = ntows)) + 
  facet_wrap(~ r_state) + geom_vline(xintercept = 2010.5, lty = 2)

#Time Per Tow
obs_data %>% distinct(haul_id, .keep_all = T) %>% filter(set_year >= 2007) %>% group_by(set_year, r_state) %>%
  summarize(duration = sum(haul_duration), ntows = length(unique(haul_id)), 
            duration_per_tow = duration / ntows ) %>% ggplot(aes(x = set_year, y = duration_per_tow)) + geom_line() +
  geom_point() + facet_wrap(~ r_state) + geom_vline(xintercept = 2010.5, lty = 2) + ylim(limits= c(0, 5.5))

#Effort hours
obs_data %>% distinct(haul_id, .keep_all = T) %>% filter(set_year >= 2007) %>% group_by(set_year, r_state) %>%
  summarize(duration = sum(haul_duration), ntows = length(unique(haul_id)), 
            duration_per_tow = duration / ntows ) %>% ggplot(aes(x = set_year, y = duration)) + geom_line() +
  geom_point() + facet_wrap(~ r_state) + geom_vline(xintercept = 2010.5, lty = 2)




#---------------------------------------------------------------------------------
#look at individual vessel changes in position
ind_changes <- obs_data %>% distinct(haul_id, .keep_all = T) %>% group_by(drvid, set_year, r_state) %>% 
  mutate(ntows = length(unique(haul_id))) %>% group_by(drvid, when, r_state) %>%
  summarize(avg_lat = mean(avg_lat), avg_long = mean(avg_long), avg_ntows = mean(ntows)) %>% 
  filter(when != 'baseline') 

ind_changes <- melt(ind_changes, id.vars = c('drvid', 'when', 'r_state'))
ind_changes <- ind_changes %>% dcast(drvid + r_state ~ when + variable) 
ind_changes$diff_avg_lat <- ind_changes$after_avg_lat - ind_changes$before_avg_lat
ind_changes$diff_avg_long <- ind_changes$after_avg_long - ind_changes$before_avg_long
ind_changes$diff_avg_ntows <- ind_changes$after_avg_ntows - ind_changes$before_avg_ntows
ind_changes$type <- "inc"
ind_changes[which(ind_changes$diff_avg_ntows < 0), 'type'] <- 'dec'

#Delta changes in long and lat
ind_changes %>% ggplot(aes(x = diff_avg_long, y = diff_avg_lat)) + 
  geom_point(aes(size = abs(diff_avg_ntows)), pch = 19, alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2) + geom_vline(xintercept = 0, lty = 2) +
  facet_wrap(~ type + r_state)

#histograms of long and lat changes
ind_changes %>% ggplot(aes(x = diff_avg_long)) + geom_histogram() + 
  geom_vline(xintercept = 0, lty = 2) + 
  facet_wrap(~ type + r_state)

ind_changes %>% ggplot(aes(x = diff_avg_lat)) + geom_histogram() + 
  geom_vline(xintercept = 0, lty = 2) + 
  facet_wrap(~ type + r_state) + coord_flip()

#---------------------------------------------------------------------------------
#Did vessels move between states?
obs_data %>% group_by(set_year, drvid) %>% 
  summarize(nstates = length(unique(r_state)), states = paste0(unique(r_state), collapse = '; ')) %>% 
  as.data.frame %>% filter(nstates > 1)
                                                       
#Vessel 578282
big_move <- obs_data %>% filter(drvid == 578282, set_year >= 2007)  
big_move %>% distinct(haul_id, .keep_all = TRUE) %>% group_by(set_year) %>% summarize(avg_lat = mean(avg_lat),
                                                                                      abg_long = mean(avg_long))
  
  group_by(set_year) %>% distin
  summarize()

obs_data %>% filter(drvid == 578282, set_year >= 2007) %>% group_by(set_year) %>% nports

plot_tows(to_plot = obs_data %>% filter(drvid == 578282, set_year >= 2007), region = 'OR', plot_type = 'facet')

#---------------------------------------------------------------------------------
#Cluster the tows by port group if possible

#---------------------------------------------------------------------------------
#Permutation Tests of signifiance



#---------------------------------------------------------------------------------

#---------------------------------------------------------------------------------
agg_effort %>% filter(set_year != 2001) %>% ggplot(aes(x = set_year, y = ntows, group = 1)) + geom_line() + 
  geom_point() + ylim(limits = c(0, 20000))
  
  geom_line(aes(x = set_year, y = ntows))
+ 
  geom_point(aes(x = set_year, y = ntows))

agg_effort %>% filter(set_year != 2001) %>% ggplot(aes(x = set_year)) + geom_line(aes(y = ntows)) + 
  geom_line(aes(x = set_year, y = ntows)) + 
  ylim(limits = c(0, 20000)) 







#---------------------------------------------------------------------------------
#Remove seattle values from obs_data
obs_data <- obs_data %>% filter(dport_desc != "SEATTLE")

obs_data1 <- ch4_format_data(obs_data, top100 = FALSE) 
#Save this once and never run again hopefully
save(obs_data1, file = "Data/obs_data1.Rdata")

rm(obs_data)

#Compare the two data types, also filtered to be 2007-2014 only*****
obs_data %>% group_by(dport_desc) %>% summarize(nports = n()) %>% arrange(desc(nports))
obs_data1 %>% group_by(dport_desc) %>% summarize(nports = n()) %>% arrange(desc(nports))
head(obs)

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#Load processed data
load(file = "C:\\Users\\Lewis\\Documents\\Data\\obs_data1.Rdata")

obs_data <- obs_data1
rm(obs_data1)

#---------------------------------------------------------------------------------
obs_data %>% group_by(dport_desc) %>% summarize
obs_data %>% select(ntows, nvess, unq_clust) %>% head(n = 30)

#---------------------------------------------------------------------------------
#Aggregate measures
#See ch4_centroid

obs_data %>% group_by(dyear) %>% summarize(ntows = length(unique(haul_id)),
  avg_dist = mean(dist_slc_km, na.rm = T))


#Rename columns
obs_data <- plyr::rename(obs_data, c('lb' = 'hpounds', 
  'dmonth' = 'tow_month', 'dyear' = 'tow_year',
  'dday' = 'tow_day', 'set_depth' = 'depth1',
  'haul_duration' = 'duration'))

obs_data1 <- arrange_tows(obs_data)

#---------------------------------------------------------------------------------
#Function to calculate values for delta plots
obs_deltas <- delta_plot(data = obs_data)

ggplot(obs_deltas, aes(x = prop_zero, y = skew)) + geom_point() + facet_wrap(~ year) + 
  geom_text(aes(label = short))
#Should look like this, attached this file in the email

load('output/survey_deltas.Rdata')




