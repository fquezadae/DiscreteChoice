#' Plot fished clusters

#' Function to plot the paths of vessels. Currently configured to run with fish_trip_simulation
#' @param result_in Results of fish_trip_simulation
#' @param facet Facet the figure?
#' @param remove_legend Remove the legend if TRUE
#' @param add_map Add map to the figure?
#' @param xlims x axis limits for map
#' @param ylims y axis limits for map

#' @export

plot_paths <- function(results_in, facet = FALSE, remove_legend = TRUE, add_map = FALSE,
  ylims = c(43, 49), xlims = c(-126, -124)){
  #Convert tow numbers into factor
  results_in[[1]]$tow_num <- factor(results_in[[1]]$tow_num, levels = unique(results_in[[1]]$tow_num))
    
  pp <- results_in[[1]] %>% distinct(rep_id, tow_num, avg_long, avg_lat) %>% 
    ggplot(aes(x = avg_long, y = avg_lat)) + geom_point(aes(colour = rep_id)) + 
    geom_line(aes(group = rep_id, colour = rep_id), arrow = arrow(length = unit(.2, "cm"), 
      ends = 'first', type = 'closed'))

  if(facet == TRUE) pp <- pp + facet_wrap(~ rep_id)
  if(add_map == TRUE){pp <- pp + geom_map(data = states_map, map = states_map, 
            aes(x = long, y = lat, map_id = region)) + scale_x_continuous(limits = xlims) + 
            scale_y_continuous(limits = ylims)}
  if(remove_legend == TRUE) pp <- pp + theme(legend.position = 'none')

  print(pp)
  # results_in[[1]] %>% distinct(rep_id, tow_num, avg_long, avg_lat) %>% ggplot() + 
  #   geom_point(aes(x = avg_long, y = avg_lat)) + facet_wrap(~ rep_id)

  # results_in[[2]] %>% ggplot() + geom_histogram(aes(x = ratio)) + facet_wrap(~ rep_id)  
}

