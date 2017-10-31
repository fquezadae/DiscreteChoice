#' Generate Choice Set

#' Generate Choice Set for Predictions
#' @param data_in Data going in to the function; default is filt_clusts
#' @param trip_dists Distances covered by each trip
#' @param the_port Port of focus; Default is Astoria
#' @param min_year Minimum year used to filter the data
#' @param max_year Maximum year used to filter the data, also RUM data is filtered to be from the max_year
#' @param risk_coefficient Coefficient to adjust the quota prices up or down, feeding into net revenue calculations
#' @param ndays Number of previous days data to use in revenue expectations
#' @param focus_year Year to focus on for the models
#' @param nhauls_sampled Number of hauls to sample from the full data set
#' @param seed Seed for sampling tows
#' @param ncores Number of cores to use
#' @param rev_scale Scale the revenue by this factor
#' @param net_cost Type of netting of costs;
#' @param habit_distance Distance of spatiotemporal filter
#' 
#' @export

get_choice_set <- function(data_in = filt_clusts, the_port = "ASTORIA / WARRENTON",
  min_year = 2010, max_year = 2012, risk_coefficient = 1,
  ndays = 60, focus_year = 2012, nhauls_sampled = 50, seed = 300, ncores, rev_scale,
  model_type = 'no_bycatch', net_cost, habit_distance){
#Start by sampling 50 tows within the same fleet  
#Figure out how close the different clusters are
  #---------------------------------------------------------------
  ##Filter the data
  dat <- data_in %>% filter(set_year >= min_year, set_year <= max_year, 
    fleet_name %in% the_port)

  #---------------------------------------------------------------
  #Modify the weak stock catches to see if 
  dat$weak_quota_value <- dat$weak_quota_value * risk_coefficient

  #---------------------------------------------------------------
  #Calculate net revenues for each haul
  dat$net_revenue <- 999

  dat <- dat %>% group_by(haul_id) %>% 
      mutate(haul_net_revenue = sum(net_revenue, na.rm = T))  

  #---------------------------------------------------------------
  #Create data set, for each tow
  dist_hauls <- dat %>% distinct(haul_id, .keep_all = T) %>% select(haul_id, unq_clust, set_month, 
    drvid, trip_id, set_day, set_year, haul_net_revenue, set_long, set_lat, 
    haul_num, avg_long, avg_lat, avg_depth, depth_bin, unq, up_long, up_lat, 
    weak_quota_value, tg_rev, tgo_rev, tgow_rev) %>% as.data.frame  
  
  dist_hauls_catch_shares <- dist_hauls %>% filter(set_year >= min_year)

  #For each tow in the focus year, sample other tows
  #Hauls in focus year
  hauls <- dist_hauls %>% filter(set_year == focus_year) %>% arrange(trip_id, haul_num)
  hauls$prev_haul_num <- hauls$haul_num - 1
  
  #Data frame of previous haul locations
  # prev_hauls <- hauls %>% select(trip_id, haul_num, avg_long, avg_lat)
  prev_hauls <- hauls %>% select(trip_id, haul_num, up_long, up_lat)

  #add in zero haul_num values
  zero_hauls <- prev_hauls %>% distinct(trip_id)  
  zero_hauls$haul_num <- 0

  port_locs <- dat %>% ungroup %>% distinct(trip_id, d_port_long, d_port_lat)
  zero_hauls <- zero_hauls %>% left_join(port_locs, by = "trip_id")
  zero_hauls <- plyr::rename(zero_hauls, c("d_port_long" = "up_long", 
    "d_port_lat" = 'up_lat'))

  # zero_hauls$avg_long <- unique(dat$d_port_long)
  # zero_hauls$avg_lat <- unique(dat$d_port_lat)
  
  #Add into previous hauls data frame
  prev_hauls <- rbind(prev_hauls, zero_hauls) %>% arrange(trip_id, haul_num)
  names(prev_hauls)[2:4] <- c('prev_haul_num', "prev_up_long", 'prev_up_lat')
  
  #Add this into the hauls data frame
  hauls <- hauls %>% left_join(prev_hauls, by = c('trip_id', 'prev_haul_num'))

  #Calculate depth bin proportions
  dbp <- dist_hauls_catch_shares %>% filter(depth_bin != 69) %>%
    group_by(depth_bin) %>% summarize(nvals = length(unique(haul_id))) %>%
    mutate(tot_nvals = sum(nvals), prop = nvals / tot_nvals)
  dbp <- as.data.frame(dbp)
  
  #Add number of values to sample
  dbp$n_samp <- dbp$prop * nhauls_sampled
  
  #Round the values to integers
  dbp$n_samp <- round(dbp$n_samp)
  
  #Top off the highest value
  max_dbp <- which(dbp$prop == max(dbp$prop))
  dbp[max_dbp, 'n_samp'] <- dbp[max_dbp, 'n_samp'] + (nhauls_sampled - sum(round(dbp$n_samp)))
 
  #-----------------------------------------------------------------------------
  #Sample Hauls  
  #Set seed
  set.seed(seed)
  seedz <- sample(1:1e7, size = nrow(hauls))

  #Sample hauls and calculate distances
  #For each haul in the focus year, sample nhauls_sampled tows
  cl <- makeCluster(ncores)
  registerDoParallel(cl)

  sampled_hauls <- foreach::foreach(ii = 1:nrow(hauls), 
    .export = c("sample_hauls"), 
    .packages = c("dplyr", 'plyr', 'lubridate')) %dopar% 
      sample_hauls(xx = ii, hauls1 = hauls, 
        dist_hauls_catch_shares1 = dist_hauls_catch_shares, nhauls_sampled1 = nhauls_sampled,
        depth_bin_proportions = dbp, the_seed = seedz[ii])
  
  print("Done sampling hauls")  
  sampled_hauls <- plyr::ldply(sampled_hauls)

  #-----------------------------------------------------------------------------
  #Calculate revenues from each period
  sampled_hauls$prev_days_date <- sampled_hauls$set_date - days(ndays)
  sampled_hauls$prev_year_set_date <- sampled_hauls$set_date - days(365)
  sampled_hauls$prev_year_days_date <- sampled_hauls$prev_days_date - days(365)

  #Add in the vessel that's doing the fishing
  fd <- sampled_hauls %>% filter(fished == TRUE) %>% distinct(fished_haul, drvid)
  fd <- plyr::rename(fd, c("drvid" = 'fished_drvid'))
  # names(fd)[1] <- 'fished_drvid'

  sampled_hauls <- sampled_hauls %>% left_join(fd, by = "fished_haul")

  #What were the average revenues in each location
  tow_dates <- sampled_hauls %>% 
    select(haul_id, drvid, unq_clust, set_date, prev_days_date, prev_year_set_date, prev_year_days_date,
      avg_long, avg_lat, set_lat, set_long, up_lat, up_long, avg_depth, depth_bin, unq, fished_drvid)
  
  #Look at the unique dates and clusters only
  # td1 <- tow_dates %>% distinct(unq_clust, set_date)
  tow_dates$days_inter <- interval(tow_dates$prev_days_date, tow_dates$set_date)
  tow_dates$prev_year_days_inter <- interval(tow_dates$prev_year_days_date, tow_dates$prev_year_set_date)

  #add in the fleet name
# paste(the_port, collapse = "_")
  paste_port <- paste(the_port, collapse = "_")
  tow_dates$fleet_name <- paste_port
  
  # td1 <- tow_dates %>% distinct(unq_clust, set_date, .keep_all = T)
  td1 <- tow_dates
  return(tow_dates)
}
