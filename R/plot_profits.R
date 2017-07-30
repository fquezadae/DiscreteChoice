#---------------------------------------------------------------------------------
#Plot histogram of profits from all replicates
#' Plot Profits

#' Function to plot profits associated with replicates. Currently configured to run with fish_trip
#' @param result_in Results of fish_trip_simulation
#' @export

plot_profits <- function(results_in){
  #
  results_in[[1]] %>% group_by(rep_id, tow_num) %>% summarize(prof = unique(profit_fuel_only)) %>%
    group_by(rep_id) %>% summarize(prof = sum(prof)) %>% ggplot() + geom_histogram(aes(x = prof))
}
