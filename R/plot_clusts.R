#' Plot Clusters

#' Function to plot clusters

#' @param dat Input 
#' @param show_fig If TRUE, show figure in device
#' @param add_map If TRUE, add clusters to map
#' @param specify_lims If TRUE, specify x and y limits from the data

#' @export


plot_clusts <- function(dat, show_fig = TRUE, add_map = TRUE, specify_lims = TRUE){
  the_plot <- ggplot() + geom_segment(data = dat, aes(x = -set_long, xend = -up_long,
    y = set_lat, yend = up_lat, colour = unq_clust)) 

  if(add_map == TRUE){
    the_plot <- the_plot + 
                  geom_map(data = states_map, map = states_map, 
                  aes(x = long, y = lat, map_id = region))                 
  }
  
  if(specify_lims == TRUE){
    the_plot <- the_plot + 
                  scale_x_continuous(limits = -rev(range(c(dat$set_lon, dat$up_lon)))) + 
                  scale_y_continuous(limits =  range(c(dat$set_lat, dat$up_lat)))
  }

  if(show_fig == TRUE) print(the_plot) 
  
  return(the_plot)
}