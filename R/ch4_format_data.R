#' Format the data based on whatever the input is

#' Format the data, adding columns required for analysis. Can input any data frame, unfiltered data, 
#' data filtered to be 0.6-1.1 ratio between hailed and adjusted pounds for example.

#' @param input Data frame input

#' @export

#---------------------------------------------------------------------------------
##Fishing opportunities
#---------------------------------------------------------------------------------
ch4_format_data <- function(input){
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
  # ggplot(top100_clusts) + 
  #   geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat, colour = clust)) + 
  #   facet_wrap(~ sig_tows)
  
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
  
  # ggplot(top100_clusts) + 
  #   geom_segment(aes(x = set_long, xend = up_long, y = set_lat, yend = up_lat, colour = clust)) + 
  #   facet_wrap(~ sig_vess + sig_tows)
  
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
  
  #---------------------------------------------------------------------------------
  #----Analyze changes for catch compositions, by tow and trip
  
  #Done for hpounds
  top100_clusts %>% group_by(haul_id) %>% mutate(tot = sum(hpounds)) %>% 
    group_by(haul_id, species) %>% mutate(perc = hpounds / tot ) %>% as.data.frame -> top100_clusts
  
  #Histograms across all species of catch percentages

  # top100_clusts %>% filter(type == 'targets') %>% ggplot(aes(x = perc)) + 
  #   geom_histogram() + facet_wrap(~ species + when, ncol = 2)
  # top100_clusts %>% filter(type == 'weaks') %>% ggplot(aes(x = perc)) + 
  #   geom_histogram() + facet_wrap(~ species + when, ncol = 2)
  
  #Correlation between total pounds and catch percentage of species
  
  #Add in proportion of zero and skew for each  species
  top100_clusts <- top100_clusts %>% group_by(unq_clust, species) %>% mutate(ntows_with = length(unique(haul_id)), 
    prop_zero = 1 - (ntows_with / ntows), skew = calc_skew(log(hpounds))) %>% as.data.frame
  
  #ID selected species, and frequency of high bycatch
  # top100_clusts %>% filter(species == 'Petrale Sole') %>% ggplot(aes(x = prop_zero, 
  #   y = skew)) + geom_point(aes(colour = unq_clust))
  
  top100_clusts$selected <- "no"
  top100_clusts[which(top100_clusts$prop_zero <= 0.5 & top100_clusts$skew <= 0), 'selected'] <- "yes"
  
  #ID frequency of high bycatch
  top100_clusts$high_perc <- 'no'
  top100_clusts[which(top100_clusts$perc >= 0.5 & top100_clusts$type == 'weaks'), 
    'high_perc'] <- "yes"
  top100_clusts <- top100_clusts %>% group_by(unq_clust) %>% 
    mutate(nhigh = length(which(high_perc == 'yes')), 
      high_weak_perc = unique(nhigh / ntows)) %>% as.data.frame %>% select(-nhigh, -high_perc)
  
  return(top100_clusts)  

}
