#' Cut for merge

#' Cut cluster data for merge with original data frame. Output only has haul_id, clust,
#' and number of tows

#' @param input_data Original data input to function. Should be only the unique tows
#' @param clust_input Output from the clustering function. Clustering can take a while
#' @param cut_point Value to cut the data

#' @export
##' @examples
##' cut_for_merge(input_data = unq_tows, clust_input = xx, cut_point = calc_cut)

#Cut and add in to unique data

cut_for_merge <- function(input_data, clust_input, cut_point){
  #Cut based on cut point
  tree <- cutree(clust_input, h = cut_point)
  input_data$clust <- tree
  
  #Calculate number of tows per cluster
  input_data <- input_data %>% group_by(clust) %>% mutate(ntows = length(unique(haul_id))) %>%
    as.data.frame

  #Select specific columns
  input_data <- input_data %>% select(haul_id, clust, ntows)
  
  return(input_data)
}


