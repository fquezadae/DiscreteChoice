#---------------------------------------------------------------------------------
#Plot histogram of profits from all replicates
#' Plot Profits

#' Function to plot profits associated with replicates. Currently configured to run with fish_trip
#' @param result_in Results of fish_trip_simulation
#' @export

plot_profits <- function(results_in, things){

  #Facet by scenario
  thing <- results_in[[1]] %>% group_by(rep_id, tow_num, scen) %>% mutate(prof = unique(profit_fuel_only)) %>%
    group_by(rep_id, scen) %>% mutate(prof = sum(prof), med_prof = median(prof)) %>%
    group_by(scen) %>% mutate(med_prof = median(prof)) %>% as.data.frame
  thing <- thing %>% distinct(scen, rep_id, .keep_all = T)
  
  pp <- thing %>% ggplot() + geom_histogram(aes(x = prof)) + 
  facet_grid( start_clust ~ scale) + 
    geom_vline(aes(xintercept = med_prof)) + geom_text(aes(x = med_prof + 100000, 
      label = round(med_prof, digits = 0), y = 200)) + xlab("Profits")
  return(pp)

}
