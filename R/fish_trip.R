#' Function to conduct a fishing trip

#' Function to conduct a fishing trip

#' @param input Overall input to the function; defaults to filt_clusts which is the data with clusters
#' filtered to include clusters with the most tows
#' @param ntows Number of tows in the fishing trip
#' @param start_clust Starting cluster to fish in
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
fish_trip <- function(input = filt_clusts, ntows = 10, start_clust = 295, seed = 300,
  scope = 1, quotas, scale = "scope", the_port = "ASTORIA / WARRENTON",
  catch_type = "type_clust_perc", prof_type = "avg_profit_fuel_only", objective){

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
  probs <- calc_probs(poss_clusts = poss_clusts, catch_type = catch_type, prof_type = prof_type,
    objective = objective, in_cp_name = "poss_clusts")

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

    poss_clusts <- values_for_probs(poss_clusts = poss_clusts, input_vfb = input)
    probs <- calc_probs(poss_clusts = poss_clusts, catch_type = catch_type, prof_type = prof_type,
      objective = objective, in_cp_name = "poss_clusts")

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