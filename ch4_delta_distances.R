
#-------------------------------------------------
#Do vessels group together?
#ID vessles that most often in the same clusters
ast <- top100_clusts %>% filter(dport_desc == "ASTORIA")

#Top Astoria Vessels
ast %>% group_by(drvid) %>% summarize(numrows = length(selected) ) %>% arrange(desc(numrows)) 

#
#Number of vessels in each cluster
ast %>% group_by(dyear, unq_clust) %>% summarize(nvess = length(unique(drvid))) %>% 
  ggplot(aes(x = dyear, y = nvess)) + geom_line(aes(group = unq_clust, 
    colour = unq_clust))

ggplot(ast, aes(x = dmonth, y = unq_clust)) + geom_point()

#Which vessels fished the most together?
ast %>% filter(drvid %in% c('511697', "503182"), dyear == 2010) %>% arrange(date) %>% 
  ggplot(aes(x = -set_long, y = set_lat)) + geom_path(aes(colour = drvid)) + 
  geom_point(aes(colour = drvid)) + 
  facet_wrap(~ dmonth)

#Table of cluster and vessel combinations
#Fill in number of tows for each vessel pair 
ct <- ast %>% select(unq_clust, drvid, haul_id) %>% group_by(drvid, unq_clust) %>% 
  summarize(ntows = length(unique(haul_id))) %>% as.data.frame

#Clusters for all Astoria.
xx <- lapply(unique(ct$unq_clust), FUN = function(x){
  tempct <- ct %>% filter(unq_clust == x)

  #Fill in the combinations
  #Called e for no reason
  ee <- expand.grid(1:nrow(tempct), 1:nrow(tempct))
  ee <- ee[which(ee$Var1 > ee$Var2), ]
  
  ntows1 <- tempct[ee[, 1], 'ntows']
  ntows2 <- tempct[ee[, 2], 'ntows']
  
  ee[, 1] <- tempct[ee[, 1], 'drvid']
  ee[, 2] <- tempct[ee[, 2], 'drvid']
  
  ee$ntows1 <- ntows1
  ee$ntows2 <- ntows2
  
  names(ee)[1:2] <- c('vess1', 'vess2')
  ee$clust <- x
  
  #Add unq column for each vessel combination
  ee$unq <- paste(ee$vess1, ee$vess2)
  return(ee)
})

xx <- ldply(xx)

#See which are the most common combinations
#Within each cluster, how many boats are there fishing together?
#maybe summarize with plot
boat_combs_per_clust <- xx %>% group_by(clust) %>% summarize(ncombs = length(unique(unq)), 
  avg_comb_tows = mean(ntows1 + ntows2), low_comb_tows = quantile(ntows1 + ntows2, .05),
  high_comb_tows = quantile(ntows1 + ntows2, .95)) %>% arrange(desc(ncombs))


#For each combination, which was the most consistent across clusters?
#Sort of proxy for information sharing
#That is which vessels seemed to share the most information?
#And how similar were their catches within each cluster?

#Construct a data frame that takes each vessel combination within each cluster
#calculates the vessel-specific, species-specific skew and propzero,
  #and makes a data frame that can be used to plot stuff

#vessel combinations
vess_combs <- xx




peter <- calc_comb_deltas(vess_combs[2, ])

#Parallelize this among 5 clusters
# n_comp_clusts <- 3

ind <- 1:nrow(vess_combs)
inds <- split(ind, ceiling(seq_along(ind) / (nrow(vess_combs) / 5)))

#Only look at 1:2
start_time <- Sys.time()

deltas <- mclapply(1:length(inds), mc.cores = length(inds), FUN = function(yy){
            forloop_inds <- inds[[yy]]
            forloop_out <- vector("list", length = length(forloop_inds))
          
            for(jj in 1:length(forloop_inds)){
              forloop_out[[jj]] <- calc_comb_deltas(vess_combs[forloop_inds[jj], ])
            }
          
            forloop_out <- ldply(forloop_out)
            return(forloop_out)
          })

run_time <- Sys.time() - start_time

deltas <- ldply(deltas)

deltas %>% filter(drvid == "220086", unq_clust == 1) %>% head

subset(vess_combs, unq == '503182 220086')
subset(deltas, unq == '503182 220086')




#Delta and proportion zero for each cluster, drvid, species combination
clust16 <- top100_clusts %>% filter(drvid %in% c(618440, 570945), unq_clust == 16) %>%
  group_by(drvid, species) %>% mutate(vess_clust_skew = calc_skew(log(hpounds))) %>%
  group_by(drvid) %>% mutate(nhauls = length(unique(haul_id))) %>%
  group_by(drvid, species) %>% mutate(nspphauls = length(unique(haul_id)),
    vess_clust_propzero = 1 - (nspphauls / nhauls)) %>% select(-nhauls, -nspphauls) %>%
  as.data.frame


clust16 %>% filter(type %in% c('targets', 'weaks')) %>%
 group_by(unq_clust, drvid, species) %>% distinct(.keep_all = T) %>% 
  select(species, drvid, unq_clust, vess_clust_propzero, vess_clust_skew) %>%
  plyr::rename(c("vess_clust_propzero" = "x", "vess_clust_skew" = "y")) %>% 
  melt(id.vars = c('species', 'drvid', 'unq_clust')) %>% 
  dcast(species + unq_clust ~ variable + drvid) -> calc_dists

#Need some way to hold keep the pasted drvids
names(calc_dists)[3:6] <- c('x1', 'x2', 'y1', 'y2')
calc_dists$dist <- sapply(1:nrow(calc_dists), FUN = function(x){
  temp <- calc_dists[x, ]
  dist <- sqrt((temp$x1 - temp$x2) ^ 2 + (temp$y1 - temp$y2) ^ 2)
})

mm <- melt(calc_dists, id.vars = c('species', 'unq_clust', 'dist')) %>% arrange(variable) 
mm$vess <- c(rep(1, 8), rep(2, 8), rep(1, 8), rep(2, 8))
mm$variable <- c(rep("x", 16), rep("y", 16))
mmt <- dcast(mm, species + unq_clust + vess ~ variable, value.var = 'value')
mmt <- mmt %>% arrange(vess)

ggplot(mmt, aes(x = x, y = y)) + geom_point(aes(colour = vess, size = dist)) + 
  facet_wrap(~ species)

ggplot(mmt, aes(x = ))

calc_dists %>% ggplot() 






calc_dists$dist <- sqrt()



  %>% do({
    x
  })



  melt(id.vars = c('species', 'drvid'))

  %>% melt(id.vars = )
  dcast(species + unq_clust ~ drvid, value.var = 'vess_clust_propzero')


data1 <- melt(data, id.vars = c("x", "y"))
dcast(data1, x ~ variable + y)


dcast(clust16, species + unq_clust ~ drvid, value.var = 'vess_clust_propzero')) %>% head

clust16 %>% 



clust16 %>% filter(type %in% c('targets', 'weaks')) %>% 
  ggplot(aes(x = vess_clust_propzero, y = vess_clust_skew)) + 
    geom_point(aes(colour = species)) +
    facet_wrap(~ drvid)

#Calculate the distance between each of the points
#Distance between the points correlated with number of combinations
#Vessels that fish near each other often, have more similar catch compositions
    #of target species

dcast(clust16, species + unq_clust ~ drvid, value.var = 'vess_clust_skew') %>% head



top100_clusts %>% filter(drvid %in% c(618440, 570945), unq_clust == 16, 
  species == 'Sablefish') %>% 
  ggplot() + geom_histogram(aes(hpounds)) + facet_wrap(~ dyear + drvid, ncol = 2)


head

length(.[, "unq"])
#105 rows...
hist(a)
aa <- xx %>% filter(clust == 1)
hist(aa$ntows1 + aa$ntows2)
quantile(aa$ntows1 + aa$ntows2, .95)

#See which are the most common combinations
most_common <- xx %>% group_by(unq) %>% mutate(ntogether = length(unique(clust))) %>%
         arrange(desc(ntogether)) %>% as.data.frame

most_common %>% filter(clust == 1) %>% ggplot() + geom_histogram(aes(x = ntogether))

ggplot(most_common) + geom_histogram(aes(x = ntogether)) + facet_wrap(~ clust)

most_common$clust

hist(unique(most_common$ntogether), breaks = 30)



write.csv(ast %>% dcast(unq_clust ~ drvid), file = 'output/clust_vess.csv', row.names = F)




#Two vessels that fished the most together
top100_clusts %>% filter(drvid %in% c(626614, 511697)) %>% ggplot() + 
  geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat, colour = drvid)) + 
  facet_wrap(~ dyear)

#Look at months and years, 
  # dev.size width = 19, height = 12
top100_clusts %>% filter(drvid %in% c(626614, 511697)) %>% ggplot() + 
  geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat, colour = drvid)) + 
  facet_wrap(~ dyear + dmonth, ncol = 12)

#Example plot
