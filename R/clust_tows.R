#' Cluster the data

#' Funtion that returns cluster output. The purpose is to allow clustering to occur only once
#' while exploring effects of different cut points

#' @param dat Input data
# ' @param port Port of interest. Function takes too long if done on all data

#' @export
#' @examples
#' clust_tows(dat = dat)

#
clust_tows <- function(dat){
  # temp <- subset(dat, dport_desc == port)
  
  # print(dim(temp))

  distances <- dist(dat[, c('up_lat', 'set_lat', 'trans_set_long', 'trans_up_long')],
    method = 'euclidean')
  
  clusts <- hclust(distances, method = 'average')
  return(clusts)

}