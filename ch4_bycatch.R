#------------------------------------------------------------------------
#Start plotting changes in bycatch,
#Spatial bycatches



tows_clust %>% group_by(unq_clust) %>% summarize(ntows = length(unique(haul_id))) %>%
	arrange(desc(ntows))

#add number of tows in each year in each clust
tows_clust <- tows_clust %>% group_by(unq_clust, set_year) %>% 
	mutate(tows_each_year = length(unique(haul_id)))  %>% as.data.frame
tows_clust <- tows_clust %>% group_by(unq_clust, set_year, species) %>% 
	mutate(tows_with_species = length(unique(haul_id)), prop = tows_with_species / tows_each_year) %>%
	as.data.frame

#------------------------------------------------------------------------
tc_unq_hauls <- tows_clust %>% distinct(haul_id, .keep_all = T)

#Look at slopies 1 from ch4_fig3.R
unq_clusters <- slopies %>% distinct(unq, .keep_all = T)

inds <- mclapply(1:nrow(unq_clusters), FUN = function(xx){
	temp <- unq_clusters[xx, ]
	inds <- which(tows_clust$avg_lat >= temp$ymin & tows_clust$avg_lat <= temp$ymax &
		tows_clust$avg_depth >= temp$xmin & tows_clust$avg_depth <= temp$xmax)
	return(inds)
	},
	mc.cores = 6)

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
#Look at POP for now
all_pops <- tows_clust %>% filter(species == "Pacific Ocean Perch") %>% distinct(unq, .keep_all = T)
all_pops$percentile <- 999

for(ii in 1:nrow(all_pops)){
	all_pops[ii, 'percentile'] <- sum(all_pops[ii, 'avg_catch'] <= all_pops$avg_catch) / nrow(all_pops)
}

all_pops %>% filter(percentile <= .05)










