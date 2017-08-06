#' Format Data for Random Utility Model
#' Function to format data for random utility model. Each row should be a cluster
#' and columns represent arguments for each coefficient

#' @param the_input Data for input
#' @param the_quotas Quota amounts, used to determine how close to quota a vessel is
#' @param risk_aversion The extent to which vessel avoids weak stock species

#' @export

rum_data <- function(the_input, the_quotas = quotas, risk_aversion){  
  
  #Filter data
  rum_dat <- the_input %>% select(unq_clust, species, hpounds, d_port_clust_dist,
    type, exval_pound, drvid, haul_id, prop_hauls_w_spp, prop_hauls_by_type,
    avg_quota_price, haul_num) %>% 
    as.data.frame
  
  #------------------------------------------------------
  #Calculate targets and groundfish revenues for each haul
  #Calculate Revenues based on exvessel price - quota pound costs
  rum_dat$revenue <- rum_dat$hpounds * (rum_dat$exval_pound - rum_dat$avg_quota_price)
  revenues <- rum_dat %>% group_by(unq_clust, haul_id, type, drvid) %>% 
    summarize(revenue = sum(revenue, na.rm = T)) %>% 
    filter(type %in% c('targets', 'groundfish')) %>% group_by(unq_clust, haul_id,
      drvid) %>%
    summarize(revenue = sum(revenue, na.rm = T))

  #------------------------------------------------------ 
  #Quantify bycatch in each haul 
# quotas[quotas$species == "Canary Rockfish", 'catch'] <- 30
  #calculate bycatch expectations
  quotas$available <- quotas$tac - quotas$catch
  
  bycatch <- rum_dat %>% left_join(quotas %>% select(species, available, tac), 
    by = 'species')
  bycatch <- bycatch %>% filter(type == 'weaks')
  
  #add underscore to species names
  bycatch$species <- gsub(" ", "_", bycatch$species)

  bycatch <- bycatch %>% group_by(unq_clust, species) %>% 
    mutate(avg_hpounds = mean(hpounds, na.rm = T)) %>% 
    distinct(unq_clust, .keep_all = T) %>% as.data.frame
  bycatch$quota_over <- bycatch$avg_hpounds - bycatch$available  
  bycatch[which(bycatch$quota_over < 0), 'quota_over'] <- 0  
  bycatch <- bycatch %>% filter(quota_over != 0)
  
  #Adjust quota over amounts
  #Adjust by risk_aversion value
  bycatch$quota_over <- bycatch$quota_over * risk_aversion

  bycatch <- bycatch %>% dcast(unq_clust + haul_id ~ species, value.var = 'quota_over', 
    fill = 0) 

  # rum_dat$quota_over <- rum_dat$hpounds - rum_dat$available
  # rum_dat$more_quota_cost <- (rum_dat$hpounds - rum_dat$available) * rum_dat$avg_quota_price
  # rum_dat[which(rum_dat$more_quota_cost < 0), 'more_quota_cost'] <- 0
  
  #----------------------------------------------------------------  
  #Create data set for model
  haul_dat <- left_join(revenues, bycatch, by = c('unq_clust', "haul_id"), fill = 0)

  #Add in distances
  haul_dat <- left_join(haul_dat, rum_dat %>% distinct(unq_clust, d_port_clust_dist), 
    by = 'unq_clust')

  #Convert unq_clust to a factor
  haul_dat$unq_clust_fact <- factor(haul_dat$unq_clust)
  spps <- names(bycatch)[3:length(names(bycatch))]
  
  #Fill NA values with zeroes  
  haul_dat[is.na(haul_dat)] <- 0
  return(haul_dat)

}
