#' Format the data based on whatever the input is

#' Format the data, adding columns required for analysis. Can input any data frame, unfiltered data, 
#' data filtered to be 0.6-1.1 ratio between hailed and adjusted pounds for example.

#' @param input Data frame input
#' @param top100 Switch to filter to top100 clusters by tow

#' @export

#---------------------------------------------------------------------------------
##Fishing opportunities
#---------------------------------------------------------------------------------
ch4_format_data <- function(input, top100 = FALSE, nncores = 6){
  #Orient tows in the same direction
  input <- arrange_tows(dat = input)

  #--------------------------------------------------------------------------------
  #Cluster the Data by location at each port
  #Filter data so that only values from 2008-2013
  input <- filter(input, dyear >= 2007)

  #Parallelize and cluster by year and port
  
  #Find years for each unique port
  unq_ports <- unique(input$dport_desc)
  
  #Remove "dont" and newport beach values from u_ports
  # unq_ports <- unq_ports[-grep("dont", unq_ports)]
  # unq_ports <- unq_ports[-grep("NEWPORT BEACH", unq_ports)]
  
  #Single iteration of clust_by_port
  # tows_clust <- clust_by_port(port = unq_ports[1], cut_point = NA,
  #   input = input)

  if(length(unq_ports) == 1){
    print('only one port')
    ii <- unq_ports[1]
    tows_clust <- clust_by_port(port = ii, cut_point = NA, input = input)
  }

  if(length(unq_ports) > 1){
    tows_clust_list <- vector('list', length = length(unq_ports))
    
    for(ii in 1:length(unq_ports)){
      # clusters <- parallel::makeCluster(nncores)
      # doParallel::registerDoParallel(clusters)  
      print(unq_ports[ii])
      tows_clust_list[[ii]] <- clust_by_port(port = unq_ports[ii], cut_point = NA, input = input)

      # stopCluster(clusters)
    }
    # browser()
    # tows_clust <- foreach(ii = unq_ports,
    #   .packages = c('plyr', 'dplyr', 'reshape2', 'ch4')) %dopar% {
    #     clust_by_port(port = ii, cut_point = NA, input = input)   
    # } 
    
    tows_clust <- ldply(tows_clust_list)

  }

  #Clustering occurs on unique tows only...
  # tows_clust <- mclapply(1:length(unq_ports), mc.cores = 6,
  #   FUN = function(x) clust_by_port(port = unq_ports[x], cut_point = NA, input = input))
  # tows_clust <- ldply(tows_clust)
  
  ####I may have to change the cluster numbers to be port specific
  #Clusters are port specific, add unique clusters numbers 
  unq_clusts <- tows_clust %>% select(dport_desc, clust) %>% distinct()
  unq_clusts$unq_clust <- 1:nrow(unq_clusts)
  tows_clust <- inner_join(tows_clust, unq_clusts, by = c('dport_desc', 'clust'))
  
  #Select columns to keep
  # tows_clust <- tows_clust %>% select(dday, dmonth, dyear, rtime, rday, rmonth, ryear, 
  #   drvid, trip_id, townum, set_lat, set_long, duration, up_lat, up_long, depth1, target, 
  #   hpounds, apounds, gr_sector, haul_id, grouping, species, duration_min, dport_desc, ha_ratio, clust, ntows,
  #   unq_clust)
  
  #Can filter to top100 clusters
  if(top100 == TRUE){    
    top100 <- tows_clust %>% group_by(unq_clust) %>% summarize(ntows = length(unique(haul_id))) %>%
      as.data.frame %>% arrange(desc(ntows))
    tows_clust <- tows_clust %>% filter(unq_clust %in% top100$unq_clust[1:100])    
  } 

  #--------------------------------------------------------------------------------
  #Permutation tests in each cluster
  #Group values by year and cluster

  #---------------------------------------
  #Add number of vessels
  tows_clust <- tows_clust %>% group_by(dyear, unq_clust) %>% mutate(nvess = length(unique(drvid))) %>% 
    as.data.frame
  
  #---------------------------------------
  #Run permutation test in parallel
  cols <- c('ntows', 'nvess')

  #permuation test for number of tows
  tows_clust_out <- ch4_perm_test(input = tows_clust, column = cols[1], ndraws = 1000,
    crit = "<")

  #Permutation test for vessels
  tows_clust_out <- ch4_perm_test(input = tows_clust_out, column = cols[2], ndraws = 1000,
    crit = "<")
# browser()  
  # tows_clust_out <- mclapply(cols, mc.cores = nncores, 
  #   FUN = function(x) ch4_perm_test(input = tows_clust, column = x, ndraws = 1000, 
  #     crit = "<" ))

  # #Combine the two parallelized things
  # c1 <- tows_clust_out[[1]]
  # c2 <- tows_clust_out[[2]] %>% select(dday, dmonth, dyear, trip_id, haul_id, species, 
  #   p_vals_nvess, sig_nvess)

  # tows_clust <- left_join(c1, c2, by = c('dday', 'dmonth', 'dyear', 'trip_id', 'haul_id',
  #   'species'))
  
  # tows_clust <- ch4_perm_test(input = tows_clust, column = 'ntows', ndraws = 1000)

  # tows_clust <- ch4_perm_test(input = tows_clust, column = 'nvess', ndraws = 1000)
    #--------------------------------------------------------------------------------
  #Remove shit because running out of memory
  rm(input)
  rm(tows_clust)
  
  #Add in before/after catch shares column
  tows_clust_out$when <- 'before'
  tows_clust_out[which(tows_clust_out$dyear > 2010), 'when'] <- 'after'
  tows_clust_out$when <- factor(tows_clust_out$when, levels = c('before', 'after'))
  
  #Add category for species
  tows_clust_out$type <- "not important"
  # unique(tows_clust$species)[unique(tows_clust$species) %>% order]
  
  #Targets, weak, managed
  weaks <- c("Darkblotched Rockfish", "Pacific Ocean Perch", "Canary Rockfish",
    "Bocaccio Rockfish", "Yelloweye Rockfish", "Cowcod Rockfish", "Cowcod")
  tows_clust_out[which(tows_clust_out$species %in% weaks), 'type'] <- 'weaks'
  targs <- c("Dover Sole", 'Sablefish', 'Longspine Thornyhead', "Petrale Sole",
    "Lingcod", "Shortspine Thornyhead")
  tows_clust_out[which(tows_clust_out$species %in% targs), 'type'] <- 'targets'
  groundfish <- c('Arrowtooth Flounder', 'English Sole', 'Longnose Skate', 'Yellowtail Rockfish',
    "Widow Rockfish", 'Chilipepper Rockfish', 'Black Rockfish', "Vermilion Rockfish", 'Greenstriped Rockfish',
    "Greenspotted Rockfish", 'Bank Rockfish')
  tows_clust_out[which(tows_clust_out$species %in% groundfish), 'type'] <- 'groundfish'
  # tows_clust_out %>% filter(type == 'not important') %>% distinct(species) %>% arrange()
  
  #Remove "clust" and only keep unq_clust
  tows_clust_out$clust <- NULL
  
  #---------------------------------------------------------------------------------
  #----Analyze changes for catch compositions, by tow and trip
  
  #Done for lb
  tows_clust_out %>% group_by(haul_id) %>% mutate(tot = sum(hpounds)) %>% 
    group_by(haul_id, species) %>% mutate(perc = hpounds / tot ) %>% as.data.frame -> tows_clust_out
  
  #Histograms across all species of catch percentages
  # tows_clust %>% filter(type == 'targets') %>% ggplot(aes(x = perc)) + 
  #   geom_histogram() + facet_wrap(~ species + when, ncol = 2)
  # tows_clust %>% filter(type == 'weaks') %>% ggplot(aes(x = perc)) + 
  #   geom_histogram() + facet_wrap(~ species + when, ncol = 2)
  
  #Correlation between total pounds and catch percentage of species
  
  #Add in proportion of zero and skew for each  species
  tows_clust_out <- tows_clust_out %>% group_by(unq_clust, species) %>% mutate(ntows_with = length(unique(haul_id)), 
    prop_zero = 1 - (ntows_with / ntows), skew = calc_skew(log(hpounds))) %>% as.data.frame
  
  #ID selected species, and frequency of high bycatch
  # tows_clust %>% filter(species == 'Petrale Sole') %>% ggplot(aes(x = prop_zero, 
  #   y = skew)) + geom_point(aes(colour = unq_clust))
  
  tows_clust_out$selected <- "no"
  tows_clust_out[which(tows_clust_out$prop_zero <= 0.5 & tows_clust_out$skew <= 0), 'selected'] <- "yes"
  
  #ID frequency of high bycatch
  tows_clust_out$high_perc <- 'no'
  tows_clust_out[which(tows_clust_out$perc >= 0.5 & tows_clust_out$type == 'weaks'), 
    'high_perc'] <- "yes"
  tows_clust_out <- tows_clust_out %>% group_by(unq_clust) %>% 
    mutate(nhigh = length(which(high_perc == 'yes')), 
      high_weak_perc = unique(nhigh / ntows)) %>% as.data.frame %>% select(-nhigh, -high_perc)
  
  return(tows_clust_out)  

}
