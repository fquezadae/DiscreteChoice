#' port_rums function

#' Function to run and save RUM data for 8 top ports

#' @param m_y Minimum year
#' @param f_y Focus Year, also used as the maximum year
#' @param nhauls_sampled Number of hauls to sample
#' @param seed value to set seed
#' @param ncores Defaults to 6
#' @param r_c Risk Coefficient, defaults to 1
#' @param r_s Revenue scaling value, defaults to 10
#' @param dyz Days
#' @param h_d habit distance
#' @param n_c Net Cost Type; trev (total revenue), qcos (quota costs), cons (constraining species only)
#' @param quota_species Species to subtract from revenues; Options are "all" or a subset
#' of c("Canary Rockfish", "Bocaccio Rockfish", "Pacific Ocean Perch", "Yelloweye Rockfish", "Darkblotched Rockfish")

#' @export

port_rums <- function(m_y,
  f_y, nhauls_sampled = 75,
  seed, ncores, r_c = 1, r_s = 10, ports, dum_type = "no_bycatch", dyz, h_d, n_c,
  quota_species){

  nports <- length(ports)
  if(nports == 1) nports <- tolower(substr(paste0(ports[[1]], collapse = ""), 1, 3))

  #------------------------
  #Add feature to modify haul revenue values
  #Net out different species and with different risk coefficients 
  
  #Modify tgow_rev column for each haul
  # tc <- tows_clust %>% filter(haul_id == 85045) %>% 
  #   select(species, exval_pound, apounds, gross_rev, avg_quota_price)
  
  tows_clust$temp_qp <- 0
  
  #Update temp_qp for species in the quota_species vector
  #Get indices of rows with species in quota species
  qp_inds <- which(tows_clust$species %in% quota_species)
  tows_clust[qp_inds, 'temp_qp'] <- tows_clust[qp_inds, 'avg_quota_price']
  
  #Multiply the temp quota price by the risk coefficient
  tows_clust$temp_qp <- tows_clust$temp_qp * r_c
  
  #Expand to multiply 
  tows_clust$quota_cost <- tows_clust$temp_qp * tows_clust$apounds
  
  #Now update tgow, which is used in process_dummys 2 for average revenues
  #tgow stands for target, groundfish, other, and weak revenues
  tows_clust <- tows_clust %>% group_by(haul_id) %>%  
    mutate(tgow_rev2 = sum(gross_rev, na.rm = T) - sum(quota_cost)) %>%
    as.data.frame
  
  if("all" %in% quota_species == FALSE){
    print("use newly calculated revenues")

    tows_clust$tgow_rev <- tows_clust$tgow_rev2
  }
      
  #With a vector of species, zero out quota costs that are not in the vector
  #Give vector of species to subtract 
  #------------------------

  #Run the models
  runs <- lapply(1:length(ports), FUN = function(yy){
    st_time <- Sys.time()
    rum <- tryCatch(sampled_rums(data_in = tows_clust, the_port = ports[[yy]],
                            min_year = m_y, max_year = f_y,
                            risk_coefficient = r_c, ndays = dyz, focus_year = f_y, 
                            nhauls_sampled = nhauls_sampled,
                            seed = seed, ncores = ncores, rev_scale = r_s, model_type = dum_type,
                            habit_distance = h_d, net_cost = n_c ),
      error = function(e) NULL)
    
    r_time <- Sys.time() - st_time

    the_port <- ports[[yy]]
    the_port <- paste(the_port, collapse = "_")
    port_sv <- substr(the_port, 1, 3)

    #Species abbreviations
    spp_abv <- paste0(sapply(quota_species, FUN = function(xx) substr(xx, 1, 2)), collapse = "")
    filename <- paste0(port_sv, "_","runs", r_c, "_rev", r_s, "_minyr", m_y, '_focyr', f_y,  
      "_seed", seed, "_nday", dyz, '_hdist', h_d, "_netcost", n_c, 
      "_qspecies", spp_abv)

    mod <- rum[[2]]    
    if(Sys.info()[['sysname']] == "Darwin"){
      save(mod, file = paste0("/Volumes/udrive/", filename, '.Rdata'))
    }
    
    if(Sys.info()[['sysname']] != "Darwin"){
      save(mod, file = paste0("//udrive.uw.edu//udrive//", filename, '.Rdata'))
    }
    print(r_time)
    print(rum[[1]])
    return(rum)
  })

  ports_names <- lapply(ports, FUN = function(yy) paste(yy, collapse = "_"))
  ports_names <- ldply(ports_names)
  ports_names <- as.vector(ports_names$V1)
  
  coefs <- lapply(runs, FUN = function(xx) xx[[1]])
  names(coefs) <- ports_names
  
  spp_abv <- paste0(sapply(quota_species, FUN = function(xx) substr(xx, 1, 2)), collapse = "")
  filename <- paste0("coefs", r_c, "_rev", r_s, "_minyr", m_y, '_focyr', f_y, "_nports", nports,
    "_seed", seed, '_nday', dyz, "_hdist", h_d, "_netcost", n_c,
    "_qspecies", spp_abv)
  save(coefs, file = paste0("//udrive.uw.edu//udrive//", filename, ".Rdata"))
  # filename <- paste0("runs", r_c, "_rev", r_s, "_minyr", m_y, '_focyr', f_y, "_nports", nports,
  #   "_seed", seed)
  # filename <- paste0("runs", r_c, "_rev", r_s, '_focyr', f_y, "_nports", nports)
  # save(runs, file = paste0("//udrive.uw.edu//udrive//", filename, ".Rdata"))

  return(coefs)
}