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

#---------------------------------------------------------------------------------
#Filter so that only tows with ha_ratio between 0.6 and 1.2 are kept
wc_data$ha_ratio <- wc_data$hpounds / wc_data$apounds

#Lose about half of the tows
wc_data %>% filter(ha_ratio >= 0.6 & ha_ratio <= 1.2) %>% nrow / nrow(wc_data)
wc_data_orig <- wc_data

wc_data <- wc_data %>% filter(ha_ratio >= 0.6 & ha_ratio <= 1.2)

#Data are now filtered so hailed and adjusted pounds in relative agreement

#---------------------------------------------------------------------------------
##Fishing opportunities
#Orient tows in the same direction
wc_data <- arrange_tows(dat = wc_data)

#--------------------------------------------------------------------------------
#Cluster the Data by location at each port
#Filter data so that only values from 2008-2013
#Do this once for each port

#Parallelize and cluster by year and port

#First filter so that ha_ratio is between 0.6 and 1.1
wc_data <- wc_data %>% filter(ha_ratio >= 0.6 & ha_ratio <= 1.1)

#Find years for each unique port
unq_ports <- unique(wc_data$dport_desc)

#Remove "dont" and newport beach values from u_ports
unq_ports <- unq_ports[-grep("dont", unq_ports)]
unq_ports <- unq_ports[-grep("NEWPORT BEACH", unq_ports)]

#Clustering occurs on unique tows only...
clust_tows <- mclapply(1:length(unq_ports), mc.cores = 6,
  FUN = function(x) clust_by_port(port = unq_ports[x], cut_point = NA))
clust_tows <- ldply(clust_tows)

####I may have to change the cluster numbers to be port specific
#Clusters are port specific, add unique clusters numbers 
unq_clusts <- clust_tows %>% select(dport_desc, clust) %>% distinct()
unq_clusts$unq_clust <- 1:nrow(unq_clusts)
clust_tows <- inner_join(clust_tows, unq_clusts, by = c('dport_desc', 'clust'))

#Select columns to keep
clust_tows <- clust_tows %>% select(dday, dmonth, dyear, rtime, rday, rmonth, ryear, 
  drvid, trip_id, townum, set_lat, set_long, duration, up_lat, up_long, depth1, target, 
  hpounds, apounds, gr_sector, haul_id, grouping, species, duration_min, dport_desc, ha_ratio, clust, ntows,
  unq_clust)

#--------------------------------------------------------------------------------
#CHANGES IN TOWS IN CLUSTERS
#before after changes with permutation test

#look at top 100 clusters; ntows column is number of unique hauls
clust_tows <- clust_tows %>% arrange(desc(ntows))
top100 <- unique(clust_tows$unq_clust)[1:100]
ntow_perm <- clust_tows %>% group_by(dyear, unq_clust) %>% summarize(ntows = length(species)) 

p_vals <- rep(999, 100)

for(ii in 1:100){
  temp <- ntow_perm %>% filter(unq_clust == top100[ii], dyear > 2007)
  if(length(temp$dyear) != 6) next
  bef <- mean(temp$ntows[1:3])
  aft <- mean(temp$ntows[4:6])
  emp_out <- aft - bef
  
  resamp <- sapply(1:1000, FUN = function(x){
    draw <- sample(temp$ntows, replace = F)
    bef <- mean(draw[1:3])
    aft <- mean(draw[4:6])
    out <- aft - bef
    return(out)
  })

  #To look at specific distributions
  # hist(resamp)  
  # abline(v = emp_out, lwd = 2, lty = 2, col = 'red')  

  #p_value of emp_out, which things had decreases?
  p_vals[ii] <- length(which(resamp <= emp_out)) / length(resamp)
}

sigs <- data.frame(p_vals = p_vals, sig = "999")
sigs$sig <- as.numeric(sigs$sig)
sigs$sig <- 'no change'
sigs[(sigs$p_vals <= .05), "sig"] <- 'sig decrease'
sigs[(sigs$p_vals >= .95), "sig"] <- 'sig increase'
sigs[(sigs$p_vals == 999), "sig"] <- 'not enough years'
sigs$unq_clust <- top100

#Combine with clust_tows
top100_clusts <- inner_join(clust_tows, sigs, by = c('unq_clust'))
top100_clusts <- plyr::rename(top100_clusts, c("p_vals" = 'p_vals_tows', 'sig' = 'sig_tows'))

#Check plots
ggplot(top100_clusts) + 
  geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat, colour = clust)) + 
  facet_wrap(~ sig_tows)

#--------------------------------------------------------------------------------
#CHANGES IN UNIQUE VESSELS IN CLUSTERS

#look at top 100 clusters, keep everything in top100_clusts
top100_clusts <- top100_clusts %>% group_by(unq_clust) %>% mutate(nvess = length(unique(drvid))) %>%
  as.data.frame

# top100 <- unique(clust_tows$unq_clust)[1:100]
clust_vess2 <- top100_clusts %>% group_by(dyear, unq_clust) %>% summarize(nvess = length(unique(drvid))) %>% 
  as.data.frame

# nvess_perm <- clust_tows %>% group_by(dyear, unq_clust) %>% summarize(ntows = length(species)) 
p_vals <- rep(999, 100)

for(ii in 1:100){
  temp <- clust_vess2 %>% filter(unq_clust == top100[ii], dyear > 2007)
  if(length(temp$dyear) != 6) next
  bef <- mean(temp$nvess[1:3])
  aft <- mean(temp$nvess[4:6])
  emp_out <- aft - bef
  
  resamp <- sapply(1:1000, FUN = function(x){
    draw <- sample(temp$nvess, replace = F)
    bef <- mean(draw[1:3])
    aft <- mean(draw[4:6])
    out <- aft - bef
    return(out)
  })

  #To look at specific distributions
  # hist(resamp)  
  # abline(v = emp_out, lwd = 2, lty = 2, col = 'red')  

  #p_value of emp_out, which things had decreases?
  p_vals[ii] <- length(which(resamp <= emp_out)) / length(resamp)
}

sigs <- data.frame(p_vals = p_vals, sig = "999")
sigs$sig <- as.numeric(sigs$sig)
sigs$sig <- 'no change'
sigs[(sigs$p_vals <= .05), "sig"] <- 'sig decrease'
sigs[(sigs$p_vals >= .95), "sig"] <- 'sig increase'
sigs[(sigs$p_vals == 999), "sig"] <- 'not enough years'

#Combine with clust_tows
sigs$unq_clust <- top100
top100_clusts <- inner_join(top100_clusts, sigs, by = c('unq_clust'))
top100_clusts <- plyr::rename(top100_clusts, c("p_vals" = 'p_vals_vess', 'sig' = 'sig_vess'))

ggplot(top100_clusts) + 
  geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat, colour = clust)) + 
  facet_wrap(~ sig_vess + sig_tows)

#Add in before/after catch shares column
top100_clusts$when <- 'before'
top100_clusts[which(top100_clusts$dyear > 2010), 'when'] <- 'after'
top100_clusts$when <- factor(top100_clusts$when, levels = c('before', 'after'))

#Add category for species

top100_clusts$type <- "not important"
unique(top100_clusts$species)[unique(top100_clusts$species) %>% order]

#Targets, weak, managed
weaks <- c("Darkblotched Rockfish", "Pacific Ocean Perch", "Canary Rockfish",
  "Bocaccio Rockfish", "Yelloweye Rockfish", "Cowcod Rockfish", "Cowcod")
top100_clusts[which(top100_clusts$species %in% weaks), 'type'] <- 'weaks'
targs <- c("Dover Sole", 'Sablefish', 'Longspine Thornyhead', "Petrale Sole",
  "Lingcod", "Shortspine Thornyhead")
top100_clusts[which(top100_clusts$species %in% targs), 'type'] <- 'targets'
groundfish <- c('Arrowtooth Flounder', 'English Sole', 'Longnose Skate', 'Yellowtail Rockfish',
  "Widow Rockfish", 'Chilipepper Rockfish', 'Black Rockfish', "Vermilion Rockfish", 'Greenstriped Rockfish',
  "Greenspotted Rockfish", 'Bank Rockfish')
top100_clusts[which(top100_clusts$species %in% groundfish), 'type'] <- 'groundfish'
top100_clusts %>% filter(type == 'not important') %>% distinct(species) %>% arrange()

#Remove "clust" and only keep unq_clust
top100_clusts$clust <- NULL



#--------------------------------------------------------------------------------
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

#----Analyze changes for catch compositions, by tow and trip
#Do this on temp first, then expand to top100_clusts

#Done for hpounds
top100_clusts %>% group_by(haul_id) %>% mutate(tot = sum(hpounds)) %>% 
  group_by(haul_id, species) %>% mutate(perc = hpounds / tot ) %>% as.data.frame -> top100_clusts

#Histograms across all species of catch percentages

top100_clusts %>% filter(type == 'targets') %>% ggplot(aes(x = perc)) + 
  geom_histogram() + facet_wrap(~ species + when, ncol = 2)
top100_clusts %>% filter(type == 'weaks') %>% ggplot(aes(x = perc)) + 
  geom_histogram() + facet_wrap(~ species + when, ncol = 2)

#Correlation between total pounds and catch percentage of species

#Add in proportion of zero and skew for each  species

#Load calc_skew function
calc_skew <- function(input){
  #Remove NAs
# browser()  
  nn <- length(input)
  
  xhat <- mean(input)
  num <- nn * sum((input - xhat) ^ 3)
  denom <- (nn - 1) * (nn - 2) * sd(input) ^ 3
  skew <- num / denom
  #Equation is from this website...
  #http://www.real-statistics.com/descriptive-statistics/symmetry-skewness-kurtosis/

  return(skew)
}

top100_clusts <- top100_clusts %>% group_by(unq_clust, species) %>% mutate(ntows_with = length(unique(haul_id)), 
  prop_zero = 1 - (ntows_with / ntows), skew = calc_skew(log(hpounds))) %>% as.data.frame

#ID selected species, and frequency of high bycatch
top100_clusts %>% filter(species == 'Petrale Sole') %>% ggplot(aes(x = prop_zero, 
  y = skew)) + geom_point(aes(colour = unq_clust))

top100_clusts$selected <- "no"
top100_clusts[which(top100_clusts$prop_zero <= 0.5 & top100_clusts$skew <= 0), 'selected'] <- "yes"

#ID frequency of high bycatch
top100_clusts$high_perc <- 'no'
top100_clusts[which(top100_clusts$perc >= 0.5 & top100_clusts$type == 'weaks'), 
  'high_perc'] <- "yes"
top100_clusts <- top100_clusts %>% group_by(unq_clust) %>% 
  mutate(nhigh = length(which(high_perc == 'yes')), 
    high_weak_perc = unique(nhigh / ntows)) %>% as.data.frame %>% select(-nhigh, -high_perc)

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



#Check dimensions
dim(wc_data)
dim(clust_tows)

cc <- clust_tows %>% filter(dport_desc == "NEWPORT") 
cc %>% filter(clust == 2) %>% ggplot


quantile(cc$ntows)
hist(cc$ntows, breaks = 30)

ggplot(clust_tows %>% filter(dport_desc == "NEWPORT")) + 
  geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat, 
    colour = clust))


pouts <- vector('list', length = length(unq_ports))
for(ii in 1:length(unq_ports)){
  print(unq_ports[ii])
  
  pouts[[ii]] <- clust_by_port(port = unq_ports[ii])
}



dat <- summ_clust(dat)


#--------------------------------------------------------------------------------
#Look at histograms of species compositions




#--------------------------------------------------------------------------------

#Delta plot for each cluster in each year or something??
#in each cluster in each year do a delta plot for species of interest

#Format species in clust_tows

rr <- clust_tows %>% filter(dport_desc == 'ASTORIA', clust == 1)


delta_plot(data = rr, year_col = 'dyear')


#Calculate skew and 




  

#---------------------------------------------------------------------------------------------------
#Look at before and after changes in catch compositions for each cluster
dat$when <- '999'

#Remove 
dat[which(dat$dyear < 2011), 'when'] <- 'before'
dat[which(dat$dyear >= 2011), 'when'] <- 'after'









#Keep only certain columns
tows <- wc_data %>% select(trip_id, haul_id, hpounds, apounds, species, rport_desc,
  agid, id_by_year, id_no_year)

#Set TACs for a particular species
tows$species <- tolower(tows$species)

#Classify Species
targets <- c('dover sole', 'sablefish', 'petrale sole',
  'longspine thornyhead', 'lingcod', 'shortspine thornyhead')
constraining <- c('yelloweye rockfish', 'cowcod rockfish',
  'bocaccio rockfish', 'canary rockfish', 'pacific ocean perch',
  'darkblotched rockfish')

tows$category <- 'other'
tows[tows$species %in% targets, 'category'] <- 'targets'
tows[tows$species %in% constraining, 'category'] <- 'constraining'

#Find all the tows that have a particular species 
#and look at their compositions
# of_interest <- tows %>% filter(species == 'sablefish') %>% 
#   arrange(desc(apounds))

########################################################################################
#Try it for only sablefish in WA
of_interest <- tows %>% filter(species == 'sablefish' & agid == 'O')

#look at the 
out <- calc_bycatch(nreps = 100, nsamps = 100, tac = 50000, of_interest = of_interest)
pairs <- spp_corrs(tows = tows, output = out, two_species = c('dover sole', 'longspine thornyhead'))
hist(pairs$corrs$r2, breaks = 50)

########################################################################################
#Calculate catch correlations between two species 

#Not so much to calculate the correlation between catch amounts
#Should calculate the consistency of catch amounts...




# ##DOVER SOLE##

# #Find number of tows in each site
# ntows_sites <- wc_data %>% filter(species == 'Dover Sole') %>% 
#   group_by(id_no_year) %>% summarize(ntows = length(unique(haul_id))) %>%
#                  arrange(desc(ntows)) %>% as.data.frame 



# ntows_sites$r2 <- 'none'
# two_species <- c('dover sole', 'sablefish')


# for(mm in 1:nrow(ntows_sites)){  
#   of_interest <- tows %>% filter(id_no_year == ntows_sites[mm, 1])
#   out <- calc_bycatch(nreps = 1, nsamps = ntows_sites[mm, 2], of_interest = of_interest)
#   if(sum(out[[1]][[1]]$species %in% two_species) != 2){
#     ntows_sites[mm, 'r2'] <- 0
#     next
#   }
  
#   pairs <- spp_corrs(tows = tows, output = out, two_species = two_species)
#   ntows_sites[mm, 'r2'] <- round(pairs$corrs$r2, digits = 5)
#   print(mm)
# }
# ntows_sites$r2 <- as.numeric(ntows_sites$r2)

# hist(ntows_sites$r2, breaks = 50)
# plot(ntows_sites$ntows, ntows_sites$r2, pch = 19, xlab = 'Number of Tows in Each Site',
#   ylab = "R2 value", main = 'Correlation between ')


# #ultimately come up with a map that plots correlations between species 1 and 2


# apply(ntows_sites, MAR = 1, FUN = function(x){
#   # print(x[1])
#   of_interest <- tows %>% filter(id_no_year == x[1])
#   out <- calc_bycatch(nreps = 1, nsamps = x[2], of_interest = of_interest)
#   pairs <- spp_corrs(tows, output = out, two_species = c('yelloweye rockfish', 'dover sole'))
# })


# tows %>% filter(haul_id == 108937014)

# #Find species in each site
# of_interest <- tows %>% filter(id_no_year == '701') 


# out <- calc_bycatch(nreps = 1, nsamps = nrow(of_interest), tac = 50000, of_interest = of_interest)
# pairs <- spp_corrs(tows, output = out, two_species = c('dover sole', 'sablefish'))

# plot(pairs$pairs$spp1, pairs$pairs$spp2, pch = 19)



# ########################################################################################
# #To Do:
# #Parallelize and see how high bycatch rates are in certain places



# pairs_orig <- pairs
# pairs1 <- pairs %>% filter(replicate == 1)

# res <- lm(pairs1[, 4] ~ pairs1[, 3])


# pairs1$fits <- fitted(res)

# pairs1 <- pairs1[order(pairs1$fits), ]
# plot(pairs1[, 3], pairs1[, 4], pch = 19)
# lines(pairs1[, 3], pairs1$fits)

# summary(res)$r.squared


# fitted(res)[order(fitted(res))]

# lines(res$fitted.values)
# lines()

# coef(res)
# plot(res)
# #Calcluate bycatch correlations?

# #Plot the pair catch amounts 

# plot(pairs[, 3], pairs[, 4], pch = 19, xlab = names(pairs)[3], ylab = names(pairs)[4],
#   xaxs = 'i', yaxs = 'i', xpd = TRUE)

# plot(pairs[, 3], pairs[, 4])
# ggplot(pairs)

# plot_pairs(tows = tows, output = out)
# #Look at Interactions between species
# #Plot histograms of bycatch
# bycatch <- melt(out[[1]])
# names(bycatch)[3] <- 'tot_apounds'
# names(bycatch)[4] <- 'iteration'

# #Find Medians


# bycatch$category <- '999'
# bycatch[bycatch$species %in% targets, 'category'] <- 'targets'
# bycatch[bycatch$species %in% constraining, 'category'] <- 'constraining'

# #Violin Plot
# ggplot(bycatch, aes(factor(species), value)) + geom_violin() + facet_wrap(~ category, scales = 'free')

# ###_--


# which(out[[3]][1] %in% tows$haul_id)
# which(tows$haul_id %in% out[[3]][1])

# tows[out[[3]][1], 'haul_id']

# #Histograms for target species
# targs <- 

# #Look at pairs of target and constraining species, i.e. which tows caught dover and sablefish?
# out$hauls

# sab_dov <- bycatch %>% filter(species %in% c('sablefish', 'dover sole'))



# ggplot(bycatch, aes(value)) + geom_histogram() + facet_wrap(~ species + category, scales = 'free') + 
#   theme_bw()

 
# #Plot as violin plots
# ggplot(bycatch, aes(factor(species), value)) + geom_boxplot()
# ggplot(bycatch, aes(factor(species), value)) + geom_violin() + facet_wrap(~ species)

# p <- ggplot(mtcars, aes(factor(cyl), mpg))
# p + geom_violin()


# #Plot the Distribution of fishing locations


# ggplot(out[[2]], )

# bh <- ggplot(data, aes(x = longitude, y = latitude, group = year)) +
#     stat_bin2d(binwidth = bw)
# bw = c(0.0909, 0.11)

# #Plot histograms of bycatch from sampled tows
# bycatch <- melt(out[[1]])

# mm <- bycatch %>% filter(species == 'sablefish')
# hist(mm$tot_apounds, breaks = 50)

# hist(bycatch %>% filter(species == 'sablefish') )

# ggplot(bycatch) + geom_histogram(aes(x = tot_apounds), binwidth = 50) + 
#   facet_wrap(~ species) + theme_bw()




# #
# res <- sample_tows(10, nsamps = 50)
# median(res[[2]])
# hist(res[[2]], breaks = 100)

# lapply(res[[3]], FUN = function(x){
#   hauls <- of_interest[x, 'haul_id']
#   tows$haul_id
#   return(hauls)
# })

# res[[3]]

# #Look at all the other tows
# for(rr in 1:length(res[[3]])){

# }



# of_interest[res[[3]], 'haul_id']

# llply(res[[3]], FUN = function(xx){
#   hauls <- of_interest[xx, 'haul_id']
#   return(hauls)
# })





# #Loop over nsamples to evaluate the medians associated with each
# samp_vec <- seq(500, 10000, by = 500)
# outs <- vector('list', length = length(samp_vec))

# for(jj in 1:length(samp_vec)){
#   outs[[jj]] <- sample_tows(5000, nsamp = samp_vec[jj])$tot_catch
#   print(jj)
# }



# plot(samp_vec, unlist(lapply(outs, FUN = median)), pch = 19)
# for_gg <- melt(outs)

# ggplot() + geom_histogram(data = for_gg, aes(x = value), binwidth = 1000) + facet_grid(L1 ~ .)




# sample_tows(5000, nsamps)


# #How to set the TACs?













# perc_samples <- vector('list', length = 5000)
# tc_samples <- vector('list', length = 5000)

# for(ii in 1:length(perc_samples)){
#   samp_rows <- sample(1:nrow(of_interest), 50, replace = FALSE)  
#   sampled <- of_interest[samp_rows, ] %>% arrange(desc(apounds))

#   perc_samples[[ii]] <- sampled$apounds / tac 
#   tc_samples[[ii]] <- sum(sampled$apounds)
# }

# hist(unlist(tc_samples), breaks = 100)
# ggp

# #Get this thing to move a little bit?

# #Draw 50
# samp_rows <- sample(1:nrow(of_interest), 50, replace = FALSE)
# sampled <- of_interest[samp_rows, ] %>% arrange(desc(apounds))

# #How far from TAC?

# sampled$apounds / tac






# of_interest



# #Did 



# tows_of_interest <- tows[unique(of_interest$haul_id) %in% 
#   tows$haul_id, ]



# tows$haul_id %in% of_in
# of_interest$haul_id

# tows_of_interest <- tows %>% filter(haul_id,)

# sables <- tows %>% filter()

# wc_data








