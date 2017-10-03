#------------------------------------------------------------------------
#Start plotting changes in bycatch,
#Spatial bycatches

# tows_clust %>% group_by(unq_clust) %>% summarize(ntows = length(unique(haul_id))) %>%
# 	arrange(desc(ntows))

#add number of tows in each year in each clust
tows_clust <- tows_clust %>% group_by(unq_clust, set_year) %>% 
	mutate(tows_each_year = length(unique(haul_id)))  %>% as.data.frame

tows_clust <- tows_clust %>% group_by(unq_clust, set_year, species) %>% 
	mutate(tows_with_species = length(unique(haul_id)), prop = tows_with_species / tows_each_year) %>%
	as.data.frame

#Number of tows is for each fishing cluster, determined by fishing opportunity 
#algorithm

#------------------------------------------------------------------------
tc_unq_hauls <- tows_clust %>% distinct(haul_id, .keep_all = T)

#Look at slopies 1 from ch4_fig3.R
unq_clusters <- slopies %>% distinct(unq, .keep_all = T)

inds <- mclapply(1:nrow(unq_clusters), FUN = function(xx){
	temp <- unq_clusters[xx, ]
	inds <- which(tows_clust$avg_lat >= temp$ymin & tows_clust$avg_lat <= temp$ymax &
		tows_clust$avg_depth >= temp$xmin & tows_clust$avg_depth <= temp$xmax)
	return(inds)
	}, mc.cores = 6)

tows_clust$sig <- 999
tows_clust$slope <- 999
tows_clust$unq_ll <- tows_clust$unq #for lat and long only
tows_clust$count <- 999

for(ii in 1:length(inds)){
	temp <- unq_clusters[ii, ]
	tows_clust[inds[[ii]], "sig"] <- temp$sig
	tows_clust[inds[[ii]], "slope"] <- temp$slope
	tows_clust[inds[[ii]], "unq"] <- temp$unq
	tows_clust[inds[[ii]], "count"] <- temp$count
}

#------------------------------------------------------------------------
#Figure out why some of the slopes are still 999; I think it's because these
#were the locations that didn't have suffcient data to estiamte a slope

#Group things by unq, rather than unq_clust
tows_clust <- tows_clust %>% group_by(unq, set_year) %>% mutate(unq_tows = length(unique(haul_id))) %>%
	group_by(unq, set_year, species) %>% mutate(unq_tows_w_spp = length(unique(haul_id)), 
		unq_perc = unq_tows_w_spp / unq_tows) %>% as.data.frame
tows_clust <- tows_clust %>% group_by(unq, species) %>% mutate(avg_catch = mean(apounds, na.rm = T))
tows_clust <- as.data.frame(tows_clust)

#See what percentile things catches in location are for each species
clust_bycatch_spp <- c("Dover Sole", "Petrale Sole", "Sablefish", "Canary Rockfish",
	"Darkblotched Rockfish", "Pacific Ocean Perch")

clust_bycatch <- mclapply(clust_bycatch_spp, FUN = function(yy){
  all_pops <- tows_clust %>% filter(species == yy) %>% distinct(unq, .keep_all = T)
  all_pops$percentile <- 999
  # print(yy)
  for(ii in 1:nrow(all_pops)){
		all_pops[ii, 'percentile'] <- sum(all_pops[ii, 'avg_catch'] <= all_pops$avg_catch) / nrow(all_pops)
  }
  return(all_pops)
}, mc.cores = 6)
clust_bycatch <- ldply(clust_bycatch)

#Add more descriptive names for percentile

clust_bycatch <- plyr::rename(clust_bycatch, c('percentile' = 'percent_above'))
clust_bycatch$percent_below <- 1 - clust_bycatch$percent_above

#------------------------------------------------------------------------
#Clust bycatch
dat <- clust_bycatch %>% distinct(unq, species, .keep_all = T)
dat$sig_type <- 'none'
dat[which(dat$sig == 'yes' & dat$slope > 0), 'sig_type'] <- "inc"
dat[which(dat$sig == 'yes' & dat$slope < 0), 'sig_type'] <- "dec"

ggplot() + geom_histogram(aes(x = avg_catch), data = dat) + 
	geom_histogram(aes(x = avg_catch), data = subset(dat, sig == 'yes' & slope > 0), fill = 'red', 
		alpha = .8) + facet_wrap(~ species, scales = 'free')

#Look at average catches
ggplot() + geom_histogram(aes(x = avg_catch), data = subset(dat, sig == 'yes' & slope > 0)) + 
	geom_histogram(aes(x = avg_catch), data = subset(dat, sig == 'yes' & slope < 0), fill = 'red', 
		alpha = .8) + facet_wrap(~ species, scales = 'free')

#Look at Percent caught
ggplot() + geom_histogram(aes(x = unq_perc), data = subset(dat, sig == 'yes' & slope > 0)) + 
	geom_histogram(aes(x = unq_perc), data = subset(dat, sig == 'yes' & slope < 0), fill = 'red', 
		alpha = .8) + facet_wrap(~ species, scales = 'free')

#Plot percent encounter vs avg catch
ggplot(dat %>% filter(sig == 'yes')) + 
	geom_point(aes(x = unq_perc, y = avg_catch, size = unq_tows, col = sig_type), alpha = .5) + 
	facet_wrap(~ species, scales = 'free')



#------------------------------------------------------------------------
#Look at encounter rates and average catch amounts
clust_bycatch %>% distinct(unq, species, .keep_all = T) %>% ggplot() + geom_histogram(aes(x = avg_catch)) + 
	facet_wrap(~ species, scales = 'free')

clust_bycatch %>% distinct(unq, species, .keep_all = T) %>% filter(sig == 'yes', slope > 0) %>%
  ggplot() + geom_histogram(aes(x = avg_catch)) + 
	facet_wrap(~ species, scales = 'free')

clust_bycatch %>% distinct(unq, species, .keep_all = T) %>% filter(sig == 'yes', slope < 0) %>%
  ggplot() + geom_histogram(aes(x = avg_catch)) + 
	facet_wrap(~ species, scales = 'free')


clust_bycatch %>% filter(sig == 'yes') %>% ggplot() + geom_histogram(aes(x = avg_catch)) + 
	facet_wrap(~ species, scales = 'free')



clust_bycatch %>% filter(sig == 'yes') %>% ggplot() + geom_histogram(aes(x = unq_tows_w_spp)) + 
	facet_wrap(~ species, scales = 'free')

	select(unq, species, type, slope, sig, percent_above, percent_below) 
bp$percent <- bp$percent_above
bp[which(bp$type == 'weaks'), 'percent'] <- bp[which(bp$type == 'weaks'), 'percent_below']

#Locations with increases
bp[bp$slope > 0, ] %>% ggplot() + geom_histogram(aes(x = percent_below)) + facet_wrap(~ species)

#Locations with decreases
bp[bp$slope < 0, ] %>% ggplot() + geom_histogram(aes(x = percent_below)) + facet_wrap(~ species)

#------------------------------------------------------------------------
#Break tows up into quantiles based on amounts of catch of each species



bp$spp_short <- bp$species

bp$species <- substr(tolower(bp$species), 1, 3)

bp <- bp %>% dcast(unq + slope + sig ~ species, value.var = 'percent_above', fill = 0) 


pairs(bp[, 4:9])





clust_bycatch %>% filter(species == "Dover Sole") %>% ggplot() + geom_point(aes(x = percentile, 
	y = percentile))






tows_clust %>% filter(unq == '1 37', species == "Dover Sole") 


%>% distinct(ntows)


tows_clust %>% filter(species == "Dover Sole") %>% dim



#------------------------------------------------------------------------
#Look at average catches for everything in each unq_cluster
tows_clust <- tows_clust %>% group_by(unq, species) %>% mutate(avg_catch = mean(apounds)) %>%
	as.data.frame

tows_clust[which(tows_clust$slope == 999), ] %>% select(avg_depth, avg_lat) %>% head

length(which(tows_clust$slope == 999))

#Target species only
tows_clust %>% filter(type %in% c('targets'), slope != 999, sig == 'yes',
	slope > 0) %>% ggplot() + 
	geom_point(aes(x = avg_catch, y = slope)) + facet_wrap(~ species, scales = 'free')

#Weak species only
tows_clust %>% filter(type %in% c('weaks'), slope != 999, sig == 'yes',
	slope > 0) %>% ggplot() + 
	geom_point(aes(x = avg_catch, y = slope)) + facet_wrap(~ species, scales = 'free')


#------------------------------------------------------------------------
#Where do the average catches in the significant slopes locations relate to the average
	#catch expectations coastwide?
#Look at specific target and weak stock species interactions
clust_bycatch_spp <- c("Dover Sole", "Petrale Sole", "Sablefish", "Canary Rockfish",
	"Darkblotched Rockfish", "Pacific Ocean Perch")

clust_bycatch <- lapply(clust_bycatch_spp, FUN = function(yy){
  all_pops <- tows_clust %>% filter(species == yy) %>% distinct(unq, .keep_all = T)
  all_pops$percentile <- 999
  print(yy)
  for(ii in 1:nrow(all_pops)){
	  all_pops[ii, 'percentile'] <- sum(all_pops[ii, 'avg_catch'] <= all_pops$avg_catch) / nrow(all_pops)
  }
  return(all_pops)
})

tows_clust %>% filter(species == "Dover Sole", unq == "1 37")
tows_


#Why did certain clusters have significant increases in effort?
names(clust_bycatch) <- substr(tolower(clust_bycatch_spp), 1, 3)
clust_bycatch <- ldply(clust_bycatch)
clust_bycatch <- plyr::rename(clust_bycatch, c(".id" = 'spp'))

clust_bycatch %>% filter(unq == c('4 53')) %>% distinct(tows_each_year)

#Have to break up the proportions and average cat

#Percentile of expected catches
#Look at 
clust_bycatch %>% select(spp, set_year, unq, bin_x, bin_y, unq_clust, tows_each_year,
	tows_with_species, prop, sig, slope, unq_ll, count, avg_catch, percentile) %>% 
dcast(unq + set_year + bin_x + bin_y + unq_clust + tows_each_year +
		tows_with_species + prop + sig + slope + unq_ll + count + avg_catch ~ spp, value.var = 'percentile') %>% arrange(unq, set_year) %>% 
filter(unq == '1 37')


unique(clust_bycatch$sig)


pairs()


#Look at POP for now

all_pops %>% filter(percentile <= .05)










