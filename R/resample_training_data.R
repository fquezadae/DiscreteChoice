#' Function to resample tows given a training data set 

#' Take models and training data set. Resample to see what average revenues/ catch:TAC are.

#' @param training_data Set of training data
#' @param rum_results Results from one of the RUM models
#' @param seed Seed for running replicates

#' @export

resample_training_data <- function(training_data = dat_set1, rum_results = rc1, seed = 300, 
  quotas = quotas_mb){

  #--------------------------------------------------------------------------------- 
  #Calculate probabilities of fishing in each location
  dat_set1 <- add_probs(train_dat = training_data, the_model = rum_results)

  #convert list to data frame
  dat_set1 <- list_to_df(dat_set1, ind_name = c("first", "later" ), col_ind_name = "tow_type")
  dat_set1 <- dat_set1 %>% group_by(fished_haul, unq_clust) %>% mutate(clust_prob = sum(probs)) %>%
    as.data.frame

  set.seed(seed)

  #--------------------------------------------------------------------------------- 
  #Sample the individual hauls
  clust_samples <- dat_set1 %>% group_by(tow_type, fished_haul) %>% distinct(unq_clust, .keep_all = T) %>% 
    sample_n(weight = clust_prob, size = 1) %>% group_by(unq_clust, fished_haul) %>% 
    summarize(nvals = length(unq_clust)) 
  clust_samples <- clust_samples %>% group_by(unq_clust) %>% mutate(clust_haulnum = 1:length(nvals))
  
  #Creat the table that is then used to sample individual hauls
  clust_samples_tbl <-  clust_samples %>% group_by(unq_clust) %>% summarize(nhauls = length(fished_haul))
    
  ######PUll set
  #Filter the data to be from the specific port and only data from 2011-2014
  hauls <- filt_clusts %>% filter(set_year >= 2011, set_year <= 2014, dport_desc == "MORRO BAY") %>%
    distinct(haul_id, .keep_all = T) %>% select(unq_clust, haul_id, set_date)
  
  #Sample individual hauls
  sampled_hauls <- lapply(1:nrow(clust_samples_tbl), FUN = function(xx){
    tc <- clust_samples_tbl[xx, ]
    temp_hauls <- hauls %>% filter(unq_clust == tc$unq_clust) %>% sample_n(size = tc$nhauls, replace = T)
    temp_hauls$clust_haulnum <- 1:nrow(temp_hauls)
    return(temp_hauls)
  })
  sampled_hauls <- ldply(sampled_hauls)

  #--------------------------------------------------------------------------------- 
  #Summarize catches by type
  sampled_catches <- summarize_catch(sampled_hauls1 = sampled_hauls, rc = 1)
  sampled_catches <- sampled_catches %>% filter(type != 'other') %>% arrange(unq_clust, clust_haulnum) 
  
  #Add in the fished_clust value
  sampled_catches <- sampled_catches %>% left_join(clust_samples %>% select(-nvals), 
    by = c('unq_clust', 'clust_haulnum'))
  
  #Add in the dates of fished_haul
  fished_hauls <- hauls
  names(fished_hauls)[2] <- 'fished_haul'
  fished_hauls <- fished_hauls %>% select(-unq_clust)
  sampled_catches$set_date <- NULL
  
  #Add in set_date to clust_samples
  sampled_catches <- sampled_catches %>% left_join(fished_hauls, by = 'fished_haul')

  #--------------------------------------------------------------------------------- 
  #Order the tows by date and haul number
  #Add in date_haul ID Values
  date_of_hauls <- sampled_catches %>% ungroup %>% distinct(fished_haul, .keep_all = T) %>%
    select(fished_haul, haul_id, set_date) %>% mutate(date_haul_id = 1:length(fished_haul))
  
  sampled_catches <- sampled_catches %>% left_join(date_of_hauls %>% select(fished_haul, date_haul_id),
    by = 'fished_haul')

  #--------------------------------------------------------------------------------- 
  #Calculate the cumulative quota amounts
  sampled_catches <- sampled_catches %>% group_by(haul_id, species) %>% mutate(catch = sum(hpounds)) %>% 
    arrange(set_date) %>% group_by(species) %>% mutate(cum_catch = cumsum(catch)) 
  
  #Merge in with quotas
  sampled_catches <- sampled_catches %>% left_join(quotas_mb %>% select(species, tac), by = 'species')
  sampled_catches <- sampled_catches %>% as.data.frame

  return(sampled_catches)
}




# filt_clusts %>% filter(dport_desc == "MORRO BAY", set_year == 2013) %>% distinct(haul_id, .keep_all = TRUE) %>%
#   select(haul_net_revenue)









