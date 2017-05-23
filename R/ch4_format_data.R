#' Format the data based on whatever the input is

#' Format the data, adding columns required for analysis. Can input any data frame, unfiltered data, 
#' data filtered to be 0.6-1.1 ratio between hailed and adjusted pounds for example.

#' @param input Data frame input
#' @param top100 Switch to filter to top100 clusters by tow

#' @export

#---------------------------------------------------------------------------------
##Fishing opportunities
#---------------------------------------------------------------------------------
ch4_format_data <- function(input, top100 = FALSE){
  #Orient tows in the same direction
  input <- arrange_tows(dat = input)
  
  #--------------------------------------------------------------------------------
  #Cluster the Data by location at each port
  #Filter data so that only values from 2008-2013
  input <- filter(input, dyear >= 2008)
  
  #Parallelize and cluster by year and port
  
  #Find years for each unique port
  unq_ports <- unique(input$dport_desc)
  
  #Remove "dont" and newport beach values from u_ports
  unq_ports <- unq_ports[-grep("dont", unq_ports)]
  unq_ports <- unq_ports[-grep("NEWPORT BEACH", unq_ports)]
  
  #Clustering occurs on unique tows only...
  clust_tows <- mclapply(1:length(unq_ports), mc.cores = 6,
    FUN = function(x) clust_by_port(port = unq_ports[x], cut_point = NA, input = input))
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
  
  #Can filter to top100 clusters
  if(top100 == TRUE){    
    top100 <- clust_tows %>% group_by(unq_clust) %>% summarize(ntows = length(unique(haul_id))) %>%
      as.data.frame %>% arrange(desc(ntows))
    clust_tows <- clust_tows %>% filter(unq_clust %in% top100$unq_clust[1:100])
    
  } 

  #--------------------------------------------------------------------------------
  #Permutation tests in each cluster
  #Group values by year and cluster

  #---------------------------------------
  #Add number of vessels
  clust_tows <- clust_tows %>% group_by(dyear, unq_clust) %>% mutate(nvess = length(unique(drvid))) %>% 
    as.data.frame
  
  #---------------------------------------
  #Run permutation test in parallel
  cols <- c('ntows', 'nvess')
browser()
  clust_tows_out <- mclapply(cols, mc.cores = 2, 
    FUN = function(x) ch4_perm_test(input = clust_tows, column = x, ndraws = 1000))

  #Combine the two parallelized things
  c1 <- clust_tows_out[[1]]
  c2 <- clust_tows_out[[2]] %>% select(dday, dmonth, dyear, trip_id, haul_id, species, 
    p_vals_nvess, sig_nvess)

  clust_tows <- left_join(c1, c2, by = c('dday', 'dmonth', 'dyear', 'trip_id', 'haul_id',
    'species'))
  
  # clust_tows <- ch4_perm_test(input = clust_tows, column = 'ntows', ndraws = 1000)

  # clust_tows <- ch4_perm_test(input = clust_tows, column = 'nvess', ndraws = 1000)
    #--------------------------------------------------------------------------------
  #Add in before/after catch shares column
  clust_tows$when <- 'before'
  clust_tows[which(clust_tows$dyear > 2010), 'when'] <- 'after'
  clust_tows$when <- factor(clust_tows$when, levels = c('before', 'after'))
  
  #Add category for species
  clust_tows$type <- "not important"
  # unique(clust_tows$species)[unique(clust_tows$species) %>% order]
  
  #Targets, weak, managed
  weaks <- c("Darkblotched Rockfish", "Pacific Ocean Perch", "Canary Rockfish",
    "Bocaccio Rockfish", "Yelloweye Rockfish", "Cowcod Rockfish", "Cowcod")
  clust_tows[which(clust_tows$species %in% weaks), 'type'] <- 'weaks'
  targs <- c("Dover Sole", 'Sablefish', 'Longspine Thornyhead', "Petrale Sole",
    "Lingcod", "Shortspine Thornyhead")
  clust_tows[which(clust_tows$species %in% targs), 'type'] <- 'targets'
  groundfish <- c('Arrowtooth Flounder', 'English Sole', 'Longnose Skate', 'Yellowtail Rockfish',
    "Widow Rockfish", 'Chilipepper Rockfish', 'Black Rockfish', "Vermilion Rockfish", 'Greenstriped Rockfish',
    "Greenspotted Rockfish", 'Bank Rockfish')
  clust_tows[which(clust_tows$species %in% groundfish), 'type'] <- 'groundfish'
  clust_tows %>% filter(type == 'not important') %>% distinct(species) %>% arrange()
  
  #Remove "clust" and only keep unq_clust
  clust_tows$clust <- NULL
  
  #---------------------------------------------------------------------------------
  #----Analyze changes for catch compositions, by tow and trip
  
  #Done for hpounds
  clust_tows %>% group_by(haul_id) %>% mutate(tot = sum(hpounds)) %>% 
    group_by(haul_id, species) %>% mutate(perc = hpounds / tot ) %>% as.data.frame -> clust_tows
  
  #Histograms across all species of catch percentages
  # clust_tows %>% filter(type == 'targets') %>% ggplot(aes(x = perc)) + 
  #   geom_histogram() + facet_wrap(~ species + when, ncol = 2)
  # clust_tows %>% filter(type == 'weaks') %>% ggplot(aes(x = perc)) + 
  #   geom_histogram() + facet_wrap(~ species + when, ncol = 2)
  
  #Correlation between total pounds and catch percentage of species
  
  #Add in proportion of zero and skew for each  species
  clust_tows <- clust_tows %>% group_by(unq_clust, species) %>% mutate(ntows_with = length(unique(haul_id)), 
    prop_zero = 1 - (ntows_with / ntows), skew = calc_skew(log(hpounds))) %>% as.data.frame
  
  #ID selected species, and frequency of high bycatch
  # clust_tows %>% filter(species == 'Petrale Sole') %>% ggplot(aes(x = prop_zero, 
  #   y = skew)) + geom_point(aes(colour = unq_clust))
  
  clust_tows$selected <- "no"
  clust_tows[which(clust_tows$prop_zero <= 0.5 & clust_tows$skew <= 0), 'selected'] <- "yes"
  
  #ID frequency of high bycatch
  clust_tows$high_perc <- 'no'
  clust_tows[which(clust_tows$perc >= 0.5 & clust_tows$type == 'weaks'), 
    'high_perc'] <- "yes"
  clust_tows <- clust_tows %>% group_by(unq_clust) %>% 
    mutate(nhigh = length(which(high_perc == 'yes')), 
      high_weak_perc = unique(nhigh / ntows)) %>% as.data.frame %>% select(-nhigh, -high_perc)
  
  return(clust_tows)  

}
