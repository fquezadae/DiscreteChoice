#' Values for probability calculations

#' Function to format values for calculating movement probabilities

#' @param poss_clust Vector of possible clusters
#' @param thing_vfb Input (filt_clusts essentailly)
#' @export

#Pull profits per cluster, and proportions of catch and proprtions of tows given a set of clusters
values_for_probs <- function(poss_clusts, input_vfb){

  #--------------------------------------------------------  
  focus_clusts <- input_vfb %>% filter(unq_clust %in% poss_clusts)

  #--------------------------------------------------------  
  #Profits
  poss_profits <- focus_clusts %>% 
    distinct(haul_id, .keep_all = T) %>% group_by(unq_clust) %>%
    summarize(avg_haul_profit = mean(haul_profit, na.rm = T), 
        avg_profit_fuel_only = mean(profit_fuel_only, na.rm = TRUE))

  poss_profits$trans_val <- poss_profits$avg_profit_fuel_only + 
      abs(min(poss_profits$avg_profit_fuel_only)) + 1
  poss_profits$prob <- poss_profits$trans_val / sum(poss_profits$trans_val)

  #--------------------------------------------------------  
  #Species specific values
  spp_vals <- focus_clusts %>% distinct(species, clust_catch_avg, clust_perc_avg, prop_hauls_w_spp) %>% 
    as.data.frame 

  #Filter to only include targets or weaks  
  spp_vals <- spp_vals %>% filter(type %in% c('weaks', 'targets'))
  names(spp_vals)[4:6] <- c('spp_clust_catch', "spp_clust_perc", "spp_prop_hauls")

  #--------------------------------------------------------  
  # Values for each type
  type_avgs <- spp_vals %>% group_by(type, unq_clust) %>% 
    summarize(type_clust_catch = mean(spp_clust_catch),
    type_clust_perc = mean(spp_clust_perc))

  #Proportions for each type
  type_props <- focus_clusts %>% filter(type %in% c('weaks', 'targets')) %>%
    distinct(prop_hauls_by_type) 
  names(type_props)[3] <- 'type_prop_hauls'

  types <- inner_join(type_avgs, type_props, by = c("type", 'unq_clust'), all = TRUE )

  #--------------------------------------------------------  
  #Start combining everything
  outs <- spp_vals %>% left_join(types, by = c("type", "unq_clust"))
  outs <- outs %>% left_join(poss_profits, by = "unq_clust")

  return(outs)
}

  