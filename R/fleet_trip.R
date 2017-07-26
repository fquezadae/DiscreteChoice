#' Function to start some number of vessels in a number of clusters

#' Stop at each time step and allow vessels to share recent catch information
#Need to track catches for each vessel,
#Vessels can be under a risk pool, or each have their own quota
#' @param input Input; default to filt_clusts
#' @param clusts Starting clusters; equal to the number of vessels
#' @param seed Seed to get reproducible results
#' @param quotas Quotas; can be a list for each vessel or a single data frame if risk pool
#' @param risk_pool If TRUE, quotas can only be a data frame and everything held to the same standard
#' @param scale If scale == "fleet"; can only look at clusters that others are fishing in 
#' @param scope Scope of movement
#' @param prob_type Type of probability to use; type_clust_perc (proportion of tows with species group)
#' or type_clust_catch (proportion of catches)
#' @param ntows Number of tows

# clusts <- c(295, 58, 265)
# quotas1 <- quotas

# #Define quotas as list of three things
# quotas <- list(quotas1, quotas1, quotas1)
# scope <- 1
# prob_type = "type_clust_perc"

fleet_trip <- function(input = filt_clusts, clusts = c(295, 58, 265), seed = 300, quotas,
  scale = "fleet", scope = 1, prob_type = "type_clust_perc", ntows){

  #------------------------------------------------------------
  #Define first catches
  set.seed(seed)
  tows <- ldply(lapply(1:length(clusts), FUN = function(x){
          input %>% filter(unq_clust %in% clusts[x]) %>% ungroup %>% distinct(haul_id, unq_clust) %>% 
            sample_n(1)
        }))
  # tows <- input %>% filter(unq_clust %in% clusts) %>% group_by(unq_clust) %>% distinct(haul_id) %>% sample_n(1)  %>%
  #   as.data.frame
  # #Keep order of tows same order as clusts 
  # tows <- tows[match(clusts, tows$unq_clust), ]

  #Track catches by vessel
  catch_list <- lapply(1:length(clusts), FUN = function(xx) summarize_catch(clust = input %>% 
    filter(clust == tows[xx, "unq_clust"]), haul_id1 = tows[xx, "haul_id"]))
  catch_list <- lapply(catch_list, FUN = function(xx){
    temp <- xx
    temp$tow <- "tow1"
    return(temp)
  })

  #------------------------------------------------------------
  #update quotas
  quotas <- lapply(1:length(clusts), FUN = function(xx){
    temp <- quotas[[xx]] %>% left_join(catch_list[[xx]] %>% select(species, hpounds), by = "species")
    temp[is.na(temp$hpounds), 'hpounds'] <- 0
    temp$catch <- temp$catch + temp$hpounds
    temp$hpounds <- NULL
    return(temp)
  })  

  #------------------------------------------------------------
  #Pick next cluster
  #Nearby clusters only
  if(scale == "scope"){
    poss_clusts <- lapply(1:length(clusts), FUN = function(xx){
      clust_scope(catch_input = catch_list[[xx]], input_cs = input, clust_scope = scope)
    })
    poss_clusts <- values_for_probs(poss_clusts = poss_clusts, input_vfb = input)
  }

  if(scale == "port"){
    poss_clusts <- input %>% filter(d_port == the_port) %>% 
      distinct(d_port, unq_clust)    
    poss_clusts <- unique(poss_clusts$unq_clust)
    poss_clusts <- values_for_probs(poss_clusts = poss_clusts, input_vfb = input)
  }

  if(scale == "fleet"){
    poss_clusts <- unique(ldply(catch_list)$unq_clust)
    haul_specific_input <- input %>% filter(haul_id %in% unique(ldply(catch_list)$haul_id))
    poss_clusts <- values_for_probs(poss_clusts = poss_clusts, input_vfb = haul_specific_input)
  }
  #Start by looking at the average catch proportions of weak stock species
  
  #Start with proportion of hauls, go to the place with the biggest difference between 
  #targets and weaks; change value.var to use a different column
  
  #Catch composition; where do you catch most targets and least weak stock
  probs <- poss_clusts %>% 
    distinct(type, unq_clust, type_clust_catch, type_clust_perc, type_prop_hauls,
      avg_profit_fuel_only) %>%
    dcast(unq_clust +  avg_profit_fuel_only ~ type, 
      value.var = prob_type) %>% mutate(targ_weak_diff = targets - weaks) %>%
    select(-targets, -weaks)

  #Encounter frequency; how often do you catch a target and a weak stock species
  # probs <- poss_clusts %>% 
  #   distinct(type, unq_clust, type_clust_catch, type_clust_perc, type_prop_hauls,
  #     avg_profit_fuel_only) %>%
  #   dcast(unq_clust +  avg_profit_fuel_only ~ type, 
  #     value.var = 'type_prop_hauls') %>% mutate(targ_weak_diff = targets - weaks) %>%
  #   select(-targets, - weaks)

  #Remove any NA values from targ_weak_diff
  probs <- probs %>% filter(is.na(targ_weak_diff) == FALSE)

  #Transform the values so that there are no negative numbers
  probs[, 2] <- probs[, 2] + abs(min(probs[, 2])) + 1
  probs[, 3] <- probs[, 3] + abs(min(probs[, 3])) + 1
  
  probs$probs <- (probs[, 2] / sum(probs[, 2]) + probs[, 3] / sum(probs[, 3])) / 2

  #Next Cluster
  next_clusts <- base::sample(probs$unq_clust, prob = probs$probs, size = length(clusts), replace = T)
  # next_clust <- base::sample(probs$unq_clust, prob = probs$probs, size = 1, replace = T)
   
  #------------------------------------------------------------
  #Start of for loop
  for(ii in 2:ntows){
    tows <- ldply(lapply(1:length(next_clusts), FUN = function(x){
          input %>% filter(unq_clust %in% next_clusts[x]) %>% ungroup %>% distinct(haul_id, unq_clust) %>% 
            sample_n(1)
        }))
      
    #Track catches by vessel
    catch_list_new <- lapply(1:length(clusts), FUN = function(xx) summarize_catch(clust = input %>% 
      filter(clust == tows[xx, "unq_clust"]), haul_id1 = tows[xx, "haul_id"]))
    catch_list_new <- lapply(catch_list_new, FUN = function(xx){
      temp <- xx
      temp$tow <- paste0("tow", ii)
      return(temp)
    })

    #Add in the new catches
    catch_list <- lapply(1:length(clusts), FUN = function(xx){
      temp <- rbind(catch_list[[xx]], catch_list_new[[xx]])
      return(temp)
    })

    #------------------------------------------------------------
    #update quotas
    quotas <- lapply(1:length(clusts), FUN = function(xx){
      temp <- quotas[[xx]] %>% left_join(catch_list_new[[xx]] %>% select(species, hpounds), by = "species")
      temp[is.na(temp$hpounds), 'hpounds'] <- 0
      temp$catch <- temp$catch + temp$hpounds
      temp$hpounds <- NULL
      return(temp)
    })  
  
    #------------------------------------------------------------
    #Pick next cluster
    #Nearby clusters only
    if(scale == "scope"){
      poss_clusts <- lapply(1:length(clusts), FUN = function(xx){
        clust_scope(catch_input = catch_list[[xx]], input_cs = input, clust_scope = scope)
      })
      poss_clusts <- values_for_probs(poss_clusts = poss_clusts, input_vfb = input)
    }
  
    if(scale == "port"){
      poss_clusts <- input %>% filter(d_port == the_port) %>% 
        distinct(d_port, unq_clust)    
      poss_clusts <- unique(poss_clusts$unq_clust)
      poss_clusts <- values_for_probs(poss_clusts = poss_clusts, input_vfb = input)
    }
  
    if(scale == "fleet"){
      poss_clusts <- unique(ldply(catch_list)$unq_clust)
      haul_specific_input <- input %>% filter(haul_id %in% unique(ldply(catch_list)$haul_id))
      poss_clusts <- values_for_probs(poss_clusts = poss_clusts, input_vfb = haul_specific_input)
    }
    #Start by looking at the average catch proportions of weak stock species
    
    #Start with proportion of hauls, go to the place with the biggest difference between 
    #targets and weaks; change value.var to use a different column

    #Catch composition; where do you catch most targets and least weak stock
    probs <- poss_clusts %>% 
      distinct(type, unq_clust, type_clust_catch, type_clust_perc, type_prop_hauls,
        avg_profit_fuel_only) %>%
      dcast(unq_clust +  avg_profit_fuel_only ~ type, 
        value.var = prob_type) %>% mutate(targ_weak_diff = targets - weaks) %>%
      select(-targets, - weaks)
  
    #Encounter frequency; how often do you catch a target and a weak stock species
    # probs <- poss_clusts %>% 
    #   distinct(type, unq_clust, type_clust_catch, type_clust_perc, type_prop_hauls,
    #     avg_profit_fuel_only) %>%
    #   dcast(unq_clust +  avg_profit_fuel_only ~ type, 
    #     value.var = 'type_prop_hauls') %>% mutate(targ_weak_diff = targets - weaks) %>%
    #   select(-targets, - weaks)
  
    #Remove any NA values from targ_weak_diff
    probs <- probs %>% filter(is.na(targ_weak_diff) == FALSE)
  
    #Transform the values so that there are no negative numbers
    probs[, 2] <- probs[, 2] + abs(min(probs[, 2])) + 1
    probs[, 3] <- probs[, 3] + abs(min(probs[, 3])) + 1
    
    probs$probs <- (probs[, 2] / sum(probs[, 2]) + probs[, 3] / sum(probs[, 3])) / 2
  
    #Next Cluster
    next_clusts <- base::sample(probs$unq_clust, prob = probs$probs, size = length(clusts), replace = T)    
  }
  
  names(catch_list) <- paste0("vessel", 1:length(catch_list))
  catch_list <- ldply(catch_list)
  names(catch_list)[1] <- 'vessel'

  #Add catch:quota 
  names(quotas) <- paste0("vessel", 1:length(quotas))
  quotas <- ldply(quotas)
  names(quotas)[1] <- 'vessel'
  quotas$ratio <- round(quotas$catch / quotas$tac, digits = 3)

  #Return output
  return(list(catches = catch_list, quotas = quotas))
}