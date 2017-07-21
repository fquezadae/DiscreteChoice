#' Function to conduct a fishing trip

#' Function to conduct a fishing trip

#' @param input Overall input to the function; defaults to filt_clusts which is the data with clusters
#' filtered to include clusters with the most tows
#' @param ntows Number of tows in the fishing trip
#' @param start_clust Starting cluster to fish in
#' @param seed Seed for random sampling
#' @param movement Probably going to be taken out
#' @param scope Scope of available information; probably will be taken out
#' @param quotas Data frame of quota for target and weak stock species
#' @param scale Scale of movement; if scale == 'port', specify a d_port; if scale == 'scope', 
#' specify scale

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
fish_trip <- function(input = filt_clusts, ntows = 10, start_clust = 295, seed = 300,
  movement = "profit", scope = 1, quotas, scale = "scope", the_port = "ASTORIA / WARRENTON"){

  #Define initial cluster
  set.seed(seed)
  first_cluster <- input %>% filter(unq_clust == start_clust)
  first_tow <- base::sample(unique(first_cluster$haul_id), size = 1)

  #Define catch list
  catch_list <- vector('list', length = ntows)
 
  catch_list[[1]] <- summarize_catch(clust = first_cluster, haul_id1 = first_tow)

  #Compare catches to quotas
  quotas <- quotas %>% left_join(catch_list[[1]] %>% select(species, hpounds), 
                                 by = 'species')
  quotas[is.na(quotas$hpounds), 'hpounds'] <- 0
  quotas$catch <- quotas$catch + quotas$hpounds
  quotas$hpounds <- NULL

  #------------------------------------------------------------
  #Pick next cluster
  #Nearby clusters only
  if(scale == "scope"){
    poss_clusts <- clust_scope(catch_input = catch_list[[1]], input_cs = input, clust_scope = scope)    
  }

  if(scale == "port"){
    poss_clusts <- input %>% filter(d_port == the_port) %>% 
      distinct(d_port, unq_clust)    
    poss_clusts <- unique(poss_clusts$unq_clust)
  }

  #Start by looking at the average catch proportions of weak stock species
  #Maybe function to process the cluster informa
  poss_clusts <- values_for_probs(poss_clusts = poss_clusts, input_vfb = input)

  #Start with proportion of hauls, go to the place with the biggest difference between 
  #targets and weaks; change value.var to use a different column
  probs <- poss_clusts %>% 
    distinct(type, unq_clust, type_clust_catch, type_clust_perc, type_prop_hauls,
      avg_profit_fuel_only) %>%
    dcast(unq_clust +  avg_profit_fuel_only ~ type, 
      value.var = 'type_prop_hauls') %>% mutate(targ_weak_diff = targets - weaks) %>%
    select(-targets, - weaks)

  #Remove any NA values from targ_weak_diff
  probs <- probs %>% filter(is.na(targ_weak_diff) == FALSE)

  #Transform the values so that there are no negative numbers
  probs[, 2] <- probs[, 2] + abs(min(probs[, 2])) + 1
  probs[, 3] <- probs[, 3] + abs(min(probs[, 3])) + 1
  probs$probs <- (probs[, 2] / sum(probs[, 2]) + probs[, 3] / sum(probs[, 3])) / 2

  #Next Cluster
  next_clust <- base::sample(probs$unq_clust, prob = probs$probs, size = 1, replace = T)
   
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

    poss_clusts <- values_for_probs(poss_clusts = poss_clusts, input_vfb = input)

    #Pick next cluster
    poss_profits <- poss_clusts %>% distinct(unq_clust, prob)
    
    next_clust <- sample(poss_profits$unq_clust, prob = poss_profits$prob, size = 1)    
  }

  names(catch_list) <- paste0("tow", 1:ntows)
  catch_list <- ldply(catch_list)
  names(catch_list)[1] <- 'tow_num'

  #Add catch:quota 
  quotas$ratio <- round(quotas$catch / quotas$tac, digits = 3)

  return(list(catch_list = catch_list, quotas = quotas))
}