#' Cluster values by port

#' Function to cluster values by port. Can specify a cut_point or use a default calculation
#' from Trevor's fishing opportunity paper

#' @param port Name or index for port of interest
#' @param cut_point Default is calculation based on median tow distance

#' @export

#Write cluster function for mclapply call
clust_by_port <- function(port, cut_point = NA){
  temp <- wc_data %>% filter(dport_desc == port)

  #Filter out unique tows
  unq_tows <- temp %>% group_by(haul_id) %>% filter(row_number(haul_id) == 1) %>% as.data.frame
  xx <- clust_tows(dat = unq_tows)

  if(is.na(cut_point)) calc_cut <- sqrt(2) * (median(dat$dist_slc_km) * 360) / 40075
  if(is.na(cut_point) == FALSE) calc_cut <- cut_point

  unq_tows <- cut_for_merge(input_data = unq_tows, clust_input = xx, cut_point = calc_cut)

  temp <- left_join(dat, unq_tows, by = 'haul_id')
  return(temp)
}