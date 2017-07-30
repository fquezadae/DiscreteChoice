#' Function to plot pairs of values

#' Function to plot pairs of values

#' @param var_pp Variable of interest; Could by spp_clust_cathc
#' @param save_pp Save plot as png or not

#' @export

#Find way to summarize clusters 
plot_pairs <- function(var_pp = "spp_clust_catch", save_pp = TRUE){

  clust_info <- values_for_probs(poss_clusts = unique(filt_clusts$unq_clust), input_vfb = filt_clusts)
  clust_info <- clust_info %>% left_join(clust_summ, by = 'unq_clust')
  
  #Look at species proportions in each cluster
  clust_catch <- clust_info %>% select(species, unq_clust, spp_clust_catch, spp_clust_perc,
    spp_prop_hauls, ntows) %>% melt(id = c('unq_clust', 'ntows', 'species')) %>%
    filter(variable == var_pp) %>% 
    dcast(unq_clust + ntows ~ species) 
  names(clust_catch) <- gsub(" ", "_", names(clust_catch))

  filename <- paste0("figs/pairs_", var_pp, ".png")
  
  #if save_pp == FALSE, just pring
  if(save_pp == FALSE){
    print(ggpairs(clust_catch %>% select(-unq_clust, -ntows)))
  }

  if(save_pp == TRUE){
    png(width = 15.5, height = 15.5, file = filename, units = 'in', res = 200)
    print(ggpairs(clust_catch %>% select(-unq_clust, -ntows)))
    dev.off()  
  }
  
}
