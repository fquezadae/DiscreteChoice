#' Calculate Delta values for each vessel combination in each cluster

#' Function to calculate Delta values for each vessel combination in each cluster

#' @param ll is a row index in the data frame vess_combs, 
#' which has combinations of each vessel in each cluster

#' @export

#Function to calculate delta plot values for each vessel combination
calc_comb_deltas <- function(ll){
  comb_deltas <- top100_clusts %>% filter(drvid %in% c(ll$vess1, ll$vess2), unq_clust == ll$clust) %>% 
      group_by(drvid, species) %>% mutate(vess_clust_skew = calc_skew(log(hpounds))) %>%
      group_by(drvid) %>% mutate(nhauls = length(unique(haul_id))) %>%
      group_by(drvid, species) %>% mutate(nspphauls = length(unique(haul_id)),
        vess_clust_propzero = 1 - (nspphauls / nhauls)) %>% 
      select(drvid, unq_clust, species, nhauls, nspphauls, vess_clust_skew, vess_clust_propzero) %>% distinct(.keep_all = T)
    comb_deltas$unq <- paste(unique(comb_deltas$drvid), collapse = " ")
    
    #Calculate distance between points for each vessel
    comb_deltas <- comb_deltas %>% group_by(species) %>% 
      mutate(dist = sqrt((vess_clust_skew[1] - vess_clust_skew[2]) ^ 2 + 
          (vess_clust_propzero[1] - vess_clust_propzero[2]) ^ 2)) %>% 
      arrange(species, drvid)  %>% as.data.frame 

    return(comb_deltas)
}