#' Format RUM data base on resampled tows

#' Function calls mlogit

#' @param data_in Data going in to the function; default is filt_clusts
##' @param trip_dists Distances covered by each trip
#' @param the_port Port of focus; Default is Astoria
#' @param min_year Minimum year used to filter the data
#' @param max_year Maximum year used to filter the data, also RUM data is filtered to be from the max_year
#' @param risk_coefficient Coefficient to adjust the quota prices up or down, feeding into net revenue calculations
#' @param ndays Number of previous days data to use in revenue expectations
#' @param focus_year Year to focus on for the models
#' @param nhauls_sampled Number of hauls to sample from the full data set
#' @param seed Seed for sampling tows

#' @export

sampled_rums <- function(data_in = filt_clusts, the_port = "ASTORIA / WARRENTON",
  min_year = 2011, max_year = 2012, risk_coefficient = 1,
  ndays = 60, focus_year = 2012, nhauls_sampled = 50, seed = 300, ncores){
#Start by sampling 50 tows within the same fleet  
#Figure out how close the different clusters are

  ##Filter the data
  dat <- data_in %>% filter(dport_desc %in% the_port, set_year >= min_year,
    set_year <= max_year)
  
  #---------------------------------------------------------------
  #Calculate haul revenues
  #Turn NA prices into zeroes
  dat[which(is.na(dat$avg_quota_price)), 'avg_quota_price'] <- 0
  
  #Adjust the net prices based on risk coefficient
  dat$rc <- risk_coefficient

  # print("only weak stock species adjusted for risk")  

  dat$net_price <- dat$exval_pound
  weak_inds <- which(dat$type == 'weaks')
  dat[weak_inds, 'net_price'] <- dat$exval_pound[weak_inds] - dat$rc[weak_inds] * 
    dat$avg_quota_price[weak_inds]

  # dat$net_price <- (dat$exval_pound - dat$rc * dat$avg_quota_price)
  dat$net_revenue <- dat$net_price * dat$hpounds

  #Sum the haul revenues
  dat <- dat %>% group_by(haul_id) %>% 
    mutate(haul_net_revenue = sum(net_revenue, na.rm = T))
  
  #Create data set, for each tow
dist_hauls <- dat %>% distinct(haul_id, .keep_all = T) %>% select(haul_id, unq_clust, set_month, 
    drvid, trip_id, set_day, set_year, haul_net_revenue,
    haul_num, avg_long, avg_lat, avg_depth, unq) %>% as.data.frame  
  # dist_hauls <- dat %>% distinct(haul_id, .keep_all = T) %>% select(haul_id, unq_clust, set_month, 
  #   drvid, trip_id, set_day, set_year, haul_net_revenue, avg_long_clust, avg_lat_clust,
  #   haul_num, avg_long, avg_lat, avg_depth, unq, unq_clust_bin) %>% as.data.frame
  dist_hauls_catch_shares <- dist_hauls %>% filter(set_year >= 2011)
  
  set.seed(seed)
  
  #For each tow in the focus year, sample other tows
  #Hauls in focus year
  hauls <- dist_hauls %>% filter(set_year == focus_year) %>% arrange(trip_id, haul_num)
  hauls$prev_haul_num <- hauls$haul_num - 1
  
  #Data frame of previous haul locations
  prev_hauls <- hauls %>% select(trip_id, haul_num, avg_long, avg_lat)
  
  #add in zero haul_num values
  zero_hauls <- prev_hauls %>% distinct(trip_id)  
  zero_hauls$haul_num <- 0
  
  port_locs <- dat %>% ungroup %>% distinct(trip_id, d_port_long, d_port_lat)
  zero_hauls <- zero_hauls %>% left_join(port_locs, by = "trip_id")
  zero_hauls <- plyr::rename(zero_hauls, c("d_port_long" = "avg_long", 
    "d_port_lat" = 'avg_lat'))

  # zero_hauls$avg_long <- unique(dat$d_port_long)
  # zero_hauls$avg_lat <- unique(dat$d_port_lat)
  
  #Add into previous hauls data frame
  prev_hauls <- rbind(prev_hauls, zero_hauls) %>% arrange(trip_id, haul_num)
  names(prev_hauls)[2:4] <- c('prev_haul_num', "prev_avg_long", 'prev_avg_lat')
  
  #Add this into the hauls data frame
  hauls <- hauls %>% left_join(prev_hauls, by = c('trip_id', 'prev_haul_num'))
  
  #-----------------------------------------------------------------------------
  #Sample hauls and calculate distances
  #For each haul in the focus year, sample nhauls_sampled tows
  cl <- makeCluster(ncores)
  registerDoParallel(cl)

  sampled_hauls <- foreach::foreach(ii = 1:nrow(hauls), 
    .export = c("sample_hauls"), 
    .packages = c("dplyr", 'lubridate')) %dopar% 
      sample_hauls(xx = ii, hauls1 = hauls, 
        dist_hauls_catch_shares1 = dist_hauls_catch_shares, nhauls_sampled1 = nhauls_sampled)
  
  print("Done sampling hauls")  
  sampled_hauls <- ldply(sampled_hauls)

  #-----------------------------------------------------------------------------
  #Calculate revenues from each period
  sampled_hauls$prev_days_date <- sampled_hauls$set_date - days(ndays)
  sampled_hauls$prev_year_set_date <- sampled_hauls$set_date - days(365)
  sampled_hauls$prev_year_days_date <- sampled_hauls$prev_days_date - days(365)
  
  #What were the average revenues in each location
  tow_dates <- sampled_hauls %>% 
    select(unq_clust, set_date, prev_days_date, prev_year_set_date, prev_year_days_date,
      avg_long, avg_lat, avg_depth, unq)
  
  #Look at the unique dates and clusters only
  # td1 <- tow_dates %>% distinct(unq_clust, set_date)
  tow_dates$days_inter <- interval(tow_dates$prev_days_date, tow_dates$set_date)
  tow_dates$prev_year_days_inter <- interval(tow_dates$prev_year_days_date, tow_dates$prev_year_set_date)
  
  td1 <- tow_dates %>% distinct(unq_clust, set_date, .keep_all = T)

  #-----------------------------------------------------------------------------  
  dummys <- foreach::foreach(ii = 1:nrow(td1), 
    # .export = c('dat', 'td1'),
    .packages = c("dplyr", 'lubridate')) %dopar% 
      process_dummys(xx = ii, td2 = td1, dat1 = dat)
  stopCluster(cl)

  print("Done calculating dummys and revenues")    
  dummys1 <- ldply(dummys)

  #Change values to be 0 and 1 for dummy variables
  # tow_dates <- cbind(tow_dates, dummys1)
  td1 <- cbind(td1, dummys1)

# td1 %>% ggplot(aes(x = prev_days_rev)) + geom_histogram() + facet_wrap(~ unq_clust)
  
  #converte set_date to character string to merge with the sampled_hauls
  tow_dates$set_date_chr <- as.character(tow_dates$set_date)
  td1$set_date_chr <- as.character(td1$set_date)
  
  #Add in charcater set_date for sampled_hauls
  sampled_hauls$set_date_chr <- as.character(sampled_hauls$set_date)
  td1 <- td1 %>% select(unq_clust, set_date_chr, dummy_prev_days, dummy_prev_year_days, 
    dummy_miss, miss_rev) 
  
  sampled_hauls <- sampled_hauls %>% left_join(td1, by = c("unq_clust", "set_date_chr"))
  
  #Add in dummy variable for first tow
  first_hauls <- sampled_hauls %>% filter(fished == TRUE, haul_num == 1) %>% select(fished_haul)
  sampled_hauls$dummy_first <- 0
  sampled_hauls[which(sampled_hauls$fished_haul %in% first_hauls$fished_haul), 'dummy_first'] <- 1
  sampled_hauls$dummy_not_first <- sampled_hauls$dummy_first

  sampled_hauls[which(sampled_hauls$dummy_not_first == 1), "dummy_not_first"] <- 2
  sampled_hauls[which(sampled_hauls$dummy_not_first == 0), "dummy_not_first"] <- 1
  sampled_hauls[which(sampled_hauls$dummy_not_first == 2), "dummy_not_first"] <- 0
  
  #Make sure that missing values have dummy variable value of 1 for prev_days
  sampled_hauls[which(sampled_hauls$dummy_prev_days != 0), 'dummy_prev_days'] <- 1
  sampled_hauls[which(sampled_hauls$dummy_prev_year_days != 0), 'dummy_prev_year_days'] <- 1

  sampled_hauls[which(sampled_hauls$miss_rev != 0), 'dummy_miss'] <- 0
  sampled_hauls[which(sampled_hauls$miss_rev == 0), 'dummy_miss'] <- 1
  
  #-----------------------------------------------------------------------------
  #Format as mlogit.data
  rdo <- sampled_hauls %>% select(haul_id, unq_clust, haul_num, distance, fished, fished_haul, 
    dummy_prev_days, dummy_prev_year_days, dummy_miss, miss_rev,
    dummy_first, dummy_not_first)
  
  rdo <- rdo %>% group_by(fished_haul) %>% mutate(alt_tow = 1:length(haul_id)) %>% as.data.frame 
  
  #-----------------------------------------------------------------------------
  #Fit mlogit models returning the coefficients, the models, and the data going into the 
  # models

  #Filter out tows with missing values for distance
  rdo <- rdo %>% filter(is.na(distance) == FALSE)

#Fit the model for everything at once  
  the_tows <- mlogit.data(rdo, shape = 'long', choice = 'fished', alt.var = 'alt_tow',
    chid.var = 'fished_haul')

  mf <- mFormula(fished ~ miss_rev * dummy_first + 
    distance * dummy_first + miss_rev * dummy_not_first +
    distance * dummy_not_first - distance - miss_rev - 1 - 
    dummy_first - dummy_not_first + dummy_prev_days + dummy_prev_year_days + dummy_miss)
  
  res <- mlogit(mf, the_tows)

  #List coefficients and rename to align with jeem paper
  coefs <- coef(res)
  coefs <- plyr::rename(coefs, c('dummy_prev_days' = 'dum30', 
    "dummy_prev_year_days" = "dum30y", "miss_rev:dummy_first" = "rev1",
    "dummy_first:distance" = 'dist1', "miss_rev:dummy_not_first" = "rev",
    "distance:dummy_not_first" = 'dist', "dummy_miss" = "dmiss"))
  
  coefs <- data.frame(coefs = round(coefs[c('dist', 'dist1', 'rev', 'rev1', 'dmiss', 'dum30', 'dum30y')],
    digits = 5))

  ps <- summary(res)$CoefTable[, 4]

  ps <- plyr::rename(ps, c('dummy_prev_days' = 'dum30', 
    "dummy_prev_year_days" = "dum30y", "miss_rev:dummy_first" = "rev1",
    "dummy_first:distance" = 'dist1', "miss_rev:dummy_not_first" = "rev",
    "distance:dummy_not_first" = 'dist', "dummy_miss" = "dmiss"))
  
  # ps <- plyr::rename(ps, c('dummy_prev_days' = 'dum30', 
  #   "dummy_prev_year_days" = "dum30y", "prev_days_rev:dummy_first" = "rev1",
  #   "dummy_first:distance" = 'dist1', "prev_days_rev:dummy_not_first" = "rev",
  #   "distance:dummy_not_first" = 'dist'))
  
  ps <- ps[c('dist', 'dist1', 'rev', 'rev1', 'dmiss','dum30', 'dum30y')]
  
  #Add significance values
  coefs$p_values <- round(ps, digits = 5)
  coefs$significance <- " "
  coefs[which(coefs$p_values <= .10), 'significance'] <- "."
  coefs[which(coefs$p_values <= .05), 'significance'] <- "*"
  coefs[which(coefs$p_values <= .01), 'significance'] <- "**"
  coefs[which(coefs$p_values <= .001), 'significance'] <- "***"

  outs <- list(coefs = coefs, mod = res)
  return(outs)

}




