#' Function to conduct a fishing trip

#' Function to conduct a fishing trip

#' @param input Overall input to the function; defaults to filt_clusts which is the data with clusters
#' filtered to include clusters with the most tows
#' @param ntows Number of tows in the fishing trip
#' @param start_vess Vessel to sample with
#' @param seed Seed for random sampling
#' @param scope Scope of available information; probably will be taken out
#' @param quotas Data frame of quota for target and weak stock species
#' @param scale Scale of movement; if scale == 'port', specify a d_port; if scale == 'scope', 
#' specify scale
#' @param prob_type Probability type; type_prop_hauls -- frequency of encounter 
#' or type_clust_perc -- proportion of catch in each cluster

#' @export

#Things to add:
#Could be many ways of calculating the probabilities;
  #penalties for each cluster based on the expected catches, proportions of species
  #group categories
 
#To do for the probabilities:
  #Also have to include how close you are to quota
  #How to weight the percentage of profits and costs?
  #Want to fish in the place with the highest profits and biggest difference between targets
    #and weaks

#Maybe add in a penalty if the cluster with the most profit has lots of bycatch
#Perfect knowledge of all clusters in port is its own function

#Cluster, number of samples, catch_list, 
fish_trip <- function(input = filt_clusts, ntows = 10, start_vess = 295, seed = 300,
  scope = 1, quotas, scale = "scope", the_port = "ASTORIA / WARRENTON",
  catch_type = "type_clust_perc", prof_type = "avg_profit_fuel_only", risk_coefficient = 2){
  
  #-----------------------------------------------------------------------------------
  #Data Formatting
  #subset data for particular vessels
  the_dat <- input %>% filter(drvid == start_vess)

  #Calculate average number of trips and tows
  ntrips <- the_dat %>% group_by(dyear) %>% summarize(ntrips = length(unique(trip_id))) %>% 
    mutate(avg_ntrips = round(mean(ntrips), digits = 0))
  
  #Calculate average number of tows
  ntows <- the_dat %>% group_by(drvid, trip_id) %>% summarize(ntows = length(unique(haul_id))) %>%
    group_by(drvid) %>% summarize(ntows = round(mean(ntows), digits = 0)) 

  #Format the data for a rum
  weaks <- the_dat %>% filter(type == 'weaks') %>% group_by(species, unq_clust) %>%
    summarize(hpounds = mean(hpounds, na.rm = T)) %>% dcast(unq_clust ~ species,
      value.var = 'hpounds', fill = 0)
  names(weaks)[-1] <- gsub(" ", "_", names(weaks)[-1])
  
  the_dat$revenue <- the_dat$hpounds * (the_dat$exval_pound - the_dat$avg_quota_price)

  #-----------------------------------------------------------------------------------
  #Fit Random Utility Model to calculate probabilities
  rum_dat <- the_dat %>% group_by(unq_clust) %>% 
    summarize(ntows = length(unique(haul_id)), revenue = mean(revenue, na.rm = T),
      d_port_clust_dist = unique(d_port_clust_dist), dport_desc = unique(dport_desc)) %>% 
    left_join(weaks, by = 'unq_clust') 

  missing_clusts <- unique(the_dat$unq_clust)[which(unique(the_dat$unq_clust) %in% 
    weaks$unq_clust == FALSE)]
  
  #Replace NA values in these rows
  miss <- which(rum_dat$unq_clust %in% missing_clusts)
  for(mm in miss){
    rum_dat[mm, which(is.na(rum_dat[mm, ]))] <- 0
  }

  rum_dat$unq_clust_fact <- factor(rum_dat$unq_clust, levels = unique(rum_dat$unq_clust))

  #Fit RUM 
  rum_res <- multinom(unq_clust_fact ~ revenue + d_port_clust_dist + Canary_Rockfish +
      Darkblotched_Rockfish + Pacific_Ocean_Perch + Yelloweye_Rockfish,
      data = rum_dat, 
      maxit = 30000, MaxNWts = 1000, trace = T)  

  #-----------------------------------------------------------------------------------
  #Calculate probabilities in each cluster based on risk_coefficient
  X <- model.matrix(rum_res)
  X[, 4:7] <- X[, 4:7] * risk_coefficient
  clust_probs <- apply(predict(rum_res, X, type = 'probs'), 2, mean)

  #-----------------------------------------------------------------------------------
  #Start first fishing trip
  
  #Sample first cluster
  set.seed(seed)
  first_cluster <- rum_dat %>% sample_n(1, weight = clust_probs) %>% select(unq_clust)

  #Find number of tows to sample; either avg number of tows or number of tows in cluster
##Replacement is FALSE or TRUE?    
#Prevent oversampling the data
  
  nhauls <- the_dat %>% filter(unq_clust == first_cluster$unq_clust) %>% group_by(unq_clust) %>%
    mutate(nhauls = length(unique(haul_id))) %>% distinct(nhauls)
  avg_ntows <- ntows$ntows
  ntows_to_sample <- min(nhauls$nhauls, avg_ntows)

  first_trip <- one_trip(filt_data = the_dat, cluster_value = first_cluster$unq_clust, 
    num_hauls = ntows_to_sample, unq_bin_scope = 1, quotas = quotas)

  #Pick a new cluster based on catches
browser()



  #Define catch list
  # catch_list <- vector('list', length = unique(ntrips$avg_ntrips))
 
  #Compare catches to quotas
  # quotas <- quotas %>% left_join(catch_list[[1]] %>% select(species, hpounds), 
  #                                by = 'species')


  #------------------------------------------------------------
  #Pick next cluster
  #Nearby clusters only
  if(scale == "scope"){
    poss_clusts <- clust_scope(catch_input = catch_list[[1]], input_cs = input, clust_scope = scope)    
    if(scope == 0) poss_clusts <- start_clust
  }

  if(scale == "port"){
    poss_clusts <- input %>% filter(d_port == the_port) %>% 
      distinct(d_port, unq_clust)    
    poss_clusts <- unique(poss_clusts$unq_clust)
  }

  #Start by looking at the average catch proportions of weak stock species
  #Maybe function to process the cluster informa
  poss_clusts <- values_for_probs(poss_clusts = poss_clusts, input_vfb = input)

  #values for probabilities
  probs <- calc_probs(poss_clusts1 = poss_clusts, catch_type = catch_type, prof_type = prof_type,
    objective = objective, in_cp_name = "poss_clusts1")
  
  #make sure that the unq_clusts are the same
  probs <- probs %>% filter(unq_clust %in% unique(poss_clusts$unq_clust))

  #Next Cluster
  next_clust <- probs %>% sample_n(1, weight = probs)
  next_clust <- next_clust$unq_clust
  
  #------------------------------------------------------------
  #Start of for loop
  for(ii in 2:ntows){

    next_cluster <- input %>% filter(unq_clust == next_clust)
    next_tow <- base::sample(unique(next_cluster$haul_id), size = 1)

    catch_list[[ii]] <- summarize_catch(clust = next_cluster, haul_id1 = next_tow)

    #------------------------------------------------------------
    #Compare catches to quotas
    quotas <- quotas %>% left_join(catch_list[[ii]] %>% select(species, hpounds), 
                                   by = 'species')
    quotas[is.na(quotas$hpounds), 'hpounds'] <- 0
    quotas$catch <- quotas$catch + quotas$hpounds
    quotas$hpounds <- NULL

    #If quota exceeded, exit
    if(sum(quotas$catch > quotas$tac) != 0) break 

    #------------------------------------------------------------
    #Pick next cluster
    if(scale == "scope"){
      poss_clusts <- clust_scope(catch_input = catch_list[[1]], input_cs = input, clust_scope = scope)    
    }
    
    if(scale == "port"){
      poss_clusts <- input %>% filter(d_port == the_port) %>% 
        distinct(d_port, unq_clust)    
      poss_clusts <- unique(poss_clusts$unq_clust)
    }

# if(ii == 2) browser()    
    poss_clusts <- values_for_probs(poss_clusts = poss_clusts, input_vfb = input)
    probs <- calc_probs(poss_clusts1 = poss_clusts, catch_type = catch_type, prof_type = prof_type,
      objective = objective, in_cp_name = "poss_clusts1")
    #make sure that the unq_clusts are the same
    probs <- probs %>% filter(unq_clust %in% unique(poss_clusts$unq_clust))

    #Pick next cluster
    next_clust <- probs %>% sample_n(1, weight = probs)
    next_clust <- next_clust$unq_clust  

  }

  names(catch_list) <- paste0("tow", 1:ntows)
  catch_list <- ldply(catch_list)
  names(catch_list)[1] <- 'tow_num'

  #Add catch:quota 
  quotas$ratio <- round(quotas$catch / quotas$tac, digits = 3)

  return(list(catch_list = catch_list, quotas = quotas))
}