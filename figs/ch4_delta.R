#Delta plots

# Something with shortspines
# thornys <- tows_clust[grep("Thornyhead", tows_clust$species), ]
# thornys %>% group_by(set_year, species) %>% summarize(apounds = sum(apounds)) %>% arrange(species) %>%
#   as.data.frame


#Do this with tows clust
spp_years <- tows_clust %>% filter(type != 'other') %>% distinct(set_year, species)

delts <- mclapply(1:nrow(spp_years), FUN = function(mm) {
  outs <- year_spp_delta(year = spp_years[mm, "set_year"], 
    spp = spp_years[mm, 'species'])
  return(outs)
}, mc.cores = 6)

delts <- ldply(delts)
delts <-  delts %>% arrange(species, year)
delts$when <- 'before'
delts[which(delts$year >= 2011), 'when'] <- 'after'

ggplot(delts) + geom_point(aes(x = prop_zero, y = skew, colour = when)) + facet_wrap(~ species)

#Plot targets
types <- tows_clust %>% filter(type != 'other') %>% distinct(type, species)
delts <- delts %>% left_join(types, by = "species")

targs <- filter(delts, type == 'targets')
ggplot(targs) + geom_point(aes(x = prop_zero, y = skew, colour = when)) + facet_wrap(~ species)
tspp <- unique(targs$species) #target species

#Targets
source("figs/ch4_targ_delta.R")

weaks <- filter(delts, type == 'weaks')
wspp <- unique(weaks$species) #target species

#Weaks 
source('figs/ch4_weak_delta.R')

ggplot(weaks) + geom_point(aes(x = prop_zero, y = skew, colour = when)) + facet_wrap(~ species)





