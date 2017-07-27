#' Run Fish Trip Simulation

#' Function to run fish_trip simulations in parallel

#' @param nreps Number of replicates; each nrep corresponds to a seed
#' @param ncores Number of cores; used in mclapply
#' @param ntows Number of tows; start of input into fish_trip function
#' @param scope Scope of movement
#' @param input Overall input to the function; defaults to filt_clusts which is the data with clusters filtered to include clusters with the most tows
#' @param ntows Number of tows in the fishing trip
#' @param start_clust Starting cluster to fish in
#' @param quotas Data frame of quota for target and weak stock species
#' @param scale Scale of movement; if scale == 'port', specify a d_port; if scale == 'scope',  specify scale
#' @param prob_type Probability type; type_prop_hauls -- frequency of encounter 
#' or type_clust_perc -- proportion of catch in each cluster

#' @export
#' @examples
#' #Specify arguments as a list
#' in_list <- list(nreps = 6, ncores = 6, ntows = 20, scope = 5, 
#'   scale = 'scope', the_port = "ASTORIA / WARRENTON", prob_type = "type_clust_perc",
#'   quotas = quotas)
#' run_fish_trip_simulation(in_list)

#---------------------------------------------------------------------------------

fish_trip_simulation <- function(in_list){
  #---------------------------------
  #Run simulation
  start_time <- Sys.time()
  the_runs <- mclapply(1:in_list$nreps, 
    FUN = function(seeds) fish_trip(ntows = in_list$ntows, scope = in_list$scope, 
      quotas = quotas, seed = seeds, scale = in_list$scale,
    the_port = in_list$the_port, catch_type = in_list$catch_type, prof_type = in_list$prof_type,
    objective = in_list$objective), 
    mc.cores = in_list$ncores)
  run_time <- Sys.time() - start_time

  #---------------------------------
  #Store the catch results in a list
  catches <- lapply(the_runs, FUN = function(x) x[[1]])
  catches <- list_to_df(catches, ind_name = "rep", 
    col_ind_name = 'rep_id')
  catches$rep_id <- factor(catches$rep_id, 
    levels = unique(catches$rep_id))
  catches$tow <- as.numeric(substr(catches$tow_num, 4, 
    nchar(catches$tow_num)))

  #---------------------------------
  #Store the quotas
  quotas <- lapply(the_runs, FUN = function(x) x[[2]])
  quotas <- list_to_df(quotas, ind_name = "rep", 
    col_ind_name = 'rep_id')
  quotas$rep_id <- factor(quotas$rep_id, 
    levels = unique(quotas$rep_id))

  #Add in skew of catch-quota balancing
  quotas <- quotas %>% group_by(rep_id) %>% mutate(skew = calc_skew(ratio)) %>% as.data.frame

  #---------------------------------
  #Add in Trip profits and skew to the trip information
  trip_profits <- catches %>% distinct(rep_id, tow_num, .keep_all = TRUE) %>% group_by(rep_id) %>%
    summarize(trip_profits = sum(profit_fuel_only)) %>% as.data.frame
  catches <- left_join(catches, trip_profits, by = "rep_id")

  #Add in skew to catches
  catches <- left_join(catches, quotas %>% select(rep_id, skew), by = 'rep_id')

  return(list(catches = catches, quotas = quotas, run_time = run_time))
}

# in_list1 <- list(nreps = 24, ncores = 6, ntows = 20, scope = 5, 
#   scale = 'port', the_port = "ASTORIA / WARRENTON", prob_type = "type_clust_perc",
#   quotas = quotas)t