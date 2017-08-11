#' Function to conduct a fishing trip

#' Function to conduct a fishing trip

#' @param input Overall input to the function; defaults to filt_clusts which is the data with clusters
#' filtered to include clusters with the most tows
#' @param start_vess Vessel to sample with, number of tows calculated based on average for each vessel
#' @param seed Seed for random sampling
## ' @param scope Scope of available information; probably will be taken out
#' @param quotas Data frame of quota for target and weak stock species
##' @param scale Scale of movement; if scale == 'port', specify a d_port; if scale == 'scope', 
##' specify scale
##' @param prob_type Probability type; type_prop_hauls -- frequency of encounter 
##' or type_clust_perc -- proportion of catch in each cluster
#' @param the_port Specify the port; currently deprecated but might include for boats that moved a lot
#' @param risk_coefficient Risk coefficient; used in adjust_probs function
#'risk coefficients;
#' -1 -- blind fishing
#' 0-- only use original data values
#' 1--take catches into account; will eventually get towards 10
#' 10 -- avoid risk fishing

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
fish_trip <- function(input = filt_clusts, start_vess = 295, seed = 300,
  quotas, the_port = "ASTORIA / WARRENTON", risk_coefficient = 2){
  
  #-----------------------------------------------------------------------------------
  input$revenue <- input$hpounds * (input$exval_pound - input$avg_quota_price)
  
  #Data Formatting
  #subset data for particular vessels
  vess_dat <- input %>% filter(drvid == start_vess)

  #Calculate average number of trips and tows
  ntrips <- vess_dat %>% group_by(dyear) %>% summarize(ntrips = length(unique(trip_id))) %>% 
    mutate(avg_ntrips = round(mean(ntrips), digits = 0))
  
  #Calculate average number of tows
  ntows <- vess_dat %>% group_by(drvid, trip_id) %>% summarize(ntows = length(unique(haul_id))) %>%
    group_by(drvid) %>% summarize(ntows = round(mean(ntows), digits = 0)) 

  #Format the data for a rum
  weaks <- vess_dat %>% filter(type == 'weaks') %>% group_by(species, unq_clust) %>%
    summarize(hpounds = mean(hpounds, na.rm = T)) %>% dcast(unq_clust ~ species,
      value.var = 'hpounds', fill = 0)
  names(weaks)[-1] <- gsub(" ", "_", names(weaks)[-1])
  
  # the_dat$revenue <- the_dat$hpounds * (the_dat$exval_pound - the_dat$avg_quota_price)

  #-----------------------------------------------------------------------------------
  #Fit Random Utility Model to calculate probabilities
  rum_dat <- vess_dat %>% group_by(unq_clust) %>% 
    summarize(ntows = length(unique(haul_id)), revenue = mean(revenue, na.rm = T),
      d_port_clust_dist = unique(d_port_clust_dist), dport_desc = unique(dport_desc)) %>% 
    left_join(weaks, by = 'unq_clust') 

  missing_clusts <- unique(vess_dat$unq_clust)[which(unique(vess_dat$unq_clust) %in% 
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
      maxit = 30000, MaxNWts = 1000, trace = F)  

  #-----------------------------------------------------------------------------------
  #Calculate probabilities in each cluster based on risk_coefficient
  X <- model.matrix(rum_res)
  X[, 4:ncol(X)] <- X[, 4:ncol(X)] * risk_coefficient
  start_probs <- apply(predict(rum_res, X, type = 'probs'), 2, mean)

  #-----------------------------------------------------------------------------------
  #Define catch and quota lists
  trips <- vector('list', length = unique(ntrips$avg_ntrips))

  #-----------------------------------------------------------------------------------
  #Start first fishing trip
  #Sample first cluster
  set.seed(seed)
  first_cluster <- rum_dat %>% sample_n(1, weight = start_probs) %>% select(unq_clust)  

  #Define sampling values
  #Find number of tows to sample; either avg number of tows or number of tows in cluster
  nhauls <- input %>% filter(unq_clust == first_cluster$unq_clust) %>% group_by(unq_clust) %>%
    mutate(nhauls = length(unique(haul_id))) %>% distinct(nhauls)
  avg_ntows <- ntows$ntows
  ntows_to_sample <- min(nhauls$nhauls, avg_ntows)

  #-----------------------------------------------------------------------------------  
  #Use input to use all the data in an area  
  first_trip <- one_trip(filt_data = input, cluster_value = first_cluster$unq_clust, 
    num_hauls = ntows_to_sample, unq_bin_scope = 1, quotas = quotas)
  
  probs <- adjust_probs(rum1 = rum_res, quotas1 = first_trip[[2]], rc1 = risk_coefficient)

  #Save the data, the probabilities will be the probs for the next cluster
  trips[[1]] <- list(catches = first_trip[[1]], quotas = probs[[2]], probs = probs[[1]])

  #-----------------------------------------------------------------------------------
  #Keep sampling if no quotas are exceeded
#Need to write code to find point at quota exceeded and remove subsequent tows
  #-----------------------------------------------------------------------------------
  #Pick new clusters if not over the quotas yet
  for(ii in 2:unique(ntrips$avg_ntrips)){
    #Don't fish if quota exceeded
    if(sum(trips[[ii - 1]]$quotas$prop >= 1) > 0) break
# print(ii)    
    #Pick cluster from previous
    cluster <- rum_dat %>% sample_n(1, weight = trips[[ii - 1]]$probs) %>% select(unq_clust)
    trip <- one_trip(filt_data = input, cluster_value = cluster$unq_clust,
      num_hauls = ntows_to_sample, quotas = trips[[ii - 1]]$quotas, unq_bin_scope = 1)
    probs <- adjust_probs(rum1 = rum_res, quotas1 = trip[[2]], rc1 = risk_coefficient)

    trips[[ii]] <- list(catches = trip[[1]], quotas = probs[[2]], probs = probs[[1]])

  }

  #------------------------------------------------------------
  #Format outputs

  #Catches
  catches <- lapply(trips, FUN = function(xx) xx[[1]])
  catches <- list_to_df(catches, ind_name = "trip", col_ind_name = 'trip_id')

  #Quotas
  quotas <- lapply(trips, FUN = function(xx) xx[[2]])
  quotas <- list_to_df(quotas, ind_name = "trip", col_ind_name = 'trip_id')

  #Probabilities
  probs <- lapply(trips, FUN = function(xx) xx[[3]])
  probs <- list_to_df(probs, ind_name = 'trip', col_ind_name = 'trip_id')
  #Add in the initial probabilities
  probs <- rbind(start_probs, probs)
  probs[1, 1] <- 'start'

  #------------------------------------------------------------
  #Which was the tow that exceed one quota?
  last_trip <- unique(catches$trip_id)[length(unique(catches$trip_id))]
  species_over <- quotas %>% filter(trip_id == last_trip, prop >= 1)
  species_over <- plyr::rename(species_over, c("catch" = "final_catch"))
  
  find_catches <- catches %>% filter(trip_id == last_trip, species %in% species_over$species) %>%
    left_join(species_over %>% select(species, final_catch, tac), by = 'species')

  tows_over <- lapply(unique(find_catches$species), FUN = function(x){
    tt <- subset(find_catches, species == x)
    tt$cumulative_catch <- cumsum(tt$hpounds)
    tt$final_minus_cumulative <- (tt$final_catch - tt$cumulative_catch)
    tt$cumulative_catch <- tt$cumulative_catch + tt[which(tt$tow_num == max(tt$tow_num)), 
      "final_minus_cumulative"]
    tt$running_prop <- tt$cumulative_catch / tt$tac
    return(tt[min(which(tt$running_prop >= 1)), 'tow_num'])
  })
  tows_over <- unlist(tows_over)

  #Remove tows that are over
  catches <- catches[-which(catches$trip_id == last_trip & 
      catches$tow_num > min(tows_over)), ] 
  final_quotas <- quotas %>% filter(trip_id == last_trip)

  final_quotas <- catches %>% group_by(species) %>% summarize(hpounds = sum(hpounds)) %>% 
    right_join(final_quotas, by = 'species') %>% as.data.frame
  final_quotas[which(is.na(final_quotas$hpounds)), 'hpounds'] <- 0
  final_quotas$prop <- final_quotas$hpounds / final_quotas$tac

  #------------------------------------------------------------
  #Revenues
  #Haul Revenues
  haul_rev <- catches %>% group_by(trip_id, tow_num) %>% summarize(revenue = unique(haul_value))
  #Trip Revenues
  trip_rev <- haul_rev %>% group_by(trip_id) %>% summarize(trip_revenue = sum(revenue))
  #Annual Revenues
  annual_rev <- sum(trip_rev$trip_revenue)

#summarize revenues also
  outs <- list(catches = catches, quotas = final_quotas, haul_revenue = haul_rev,
    trip_revenue = trip_rev, annual_revenue = annual_rev)
  return(outs)

}






#------------------------------------------------------------------------------------------------------------------------
#Scraps
  #------------------------------------------------------------
  # #Pick next cluster
  # #Nearby clusters only
  # if(scale == "scope"){
  #   poss_clusts <- clust_scope(catch_input = catch_list[[1]], input_cs = input, clust_scope = scope)    
  #   if(scope == 0) poss_clusts <- start_clust
  # }

  # if(scale == "port"){
  #   poss_clusts <- input %>% filter(d_port == the_port) %>% 
  #     distinct(d_port, unq_clust)    
  #   poss_clusts <- unique(poss_clusts$unq_clust)
  # }

  # #Start by looking at the average catch proportions of weak stock species
  # #Maybe function to process the cluster informa
  # poss_clusts <- values_for_probs(poss_clusts = poss_clusts, input_vfb = input)

  # #values for probabilities
  # probs <- calc_probs(poss_clusts1 = poss_clusts, catch_type = catch_type, prof_type = prof_type,
  #   objective = objective, in_cp_name = "poss_clusts1")
  
  # #make sure that the unq_clusts are the same
  # probs <- probs %>% filter(unq_clust %in% unique(poss_clusts$unq_clust))

  # #Next Cluster
  # next_clust <- probs %>% sample_n(1, weight = probs)
  # next_clust <- next_clust$unq_clust
  
  #------------------------------------------------------------
  #Start of for loop
#   for(ii in 2:ntows){

#     next_cluster <- input %>% filter(unq_clust == next_clust)
#     next_tow <- base::sample(unique(next_cluster$haul_id), size = 1)

#     catch_list[[ii]] <- summarize_catch(clust = next_cluster, haul_id1 = next_tow)

#     #------------------------------------------------------------
#     #Compare catches to quotas
#     quotas <- quotas %>% left_join(catch_list[[ii]] %>% select(species, hpounds), 
#                                    by = 'species')
#     quotas[is.na(quotas$hpounds), 'hpounds'] <- 0
#     quotas$catch <- quotas$catch + quotas$hpounds
#     quotas$hpounds <- NULL

#     #If quota exceeded, exit
#     if(sum(quotas$catch > quotas$tac) != 0) break 

#     #------------------------------------------------------------
#     #Pick next cluster
#     if(scale == "scope"){
#       poss_clusts <- clust_scope(catch_input = catch_list[[1]], input_cs = input, clust_scope = scope)    
#     }
    
#     if(scale == "port"){
#       poss_clusts <- input %>% filter(d_port == the_port) %>% 
#         distinct(d_port, unq_clust)    
#       poss_clusts <- unique(poss_clusts$unq_clust)
#     }

# # if(ii == 2) browser()    
#     poss_clusts <- values_for_probs(poss_clusts = poss_clusts, input_vfb = input)
#     probs <- calc_probs(poss_clusts1 = poss_clusts, catch_type = catch_type, prof_type = prof_type,
#       objective = objective, in_cp_name = "poss_clusts1")
#     #make sure that the unq_clusts are the same
#     probs <- probs %>% filter(unq_clust %in% unique(poss_clusts$unq_clust))

#     #Pick next cluster
#     next_clust <- probs %>% sample_n(1, weight = probs)
#     next_clust <- next_clust$unq_clust  

#   }

    # trips[[ii - 1]]$probs

    # clust <- rum_dat %>% sample_n(1, weight = clust_probs) %>% select(unq_clust)
    # trip <- one_trip(filt_data = input, cluster_value = clust$unq_clust, 
    #   num_hauls = ntows_to_sample, unq_bin_scope = 1, quotas = quotas)
    # temp_quotas <- trip[[2]]
    # temp_quotas$prop <- round(temp_quotas$catch / temp_quotas$tac, digits = 4)
  
    # #Reformat temp_quotas to adjust probabilities up or down; only adjust for weak stock species
    # temp_quotas$adj_prop <- temp_quotas$prop * 10
    # #maximum amount of risk aversion is 10
    # temp_quotas$adj_prop <- sapply(temp_quotas$adj_prop, FUN = function(xx) min(10, xx))