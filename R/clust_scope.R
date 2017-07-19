#' Define scope of potential new clusters

#' Function to define scope of potential new clusters

#' @param catch_input Catch input data, from catch_list
#' @param clust_scope Scope of possible new clusters
#' @param input_cs Input data used to deetermine next clusters

#' @export

#Function given a current cluster and scope, gives a string of possible clusters to move to

clust_scope <- function(catch_input, clust_scope, input_cs){
  xbin <- unique(catch_input$xbin)
  ybin <- unique(catch_input$ybin) 

  #Locate possible clusters within movement scope
  poss_clusts <- expand.grid((xbin - clust_scope):(xbin + clust_scope), 
    (ybin - clust_scope):(ybin + clust_scope))
  poss_clusts$unq <- paste(poss_clusts[, 1], poss_clusts[, 2])
  poss_movement <- input_cs %>% filter(unq %in% poss_clusts$unq)
  return(unique(poss_movement$unq_clust)) #return unique clusters
}
