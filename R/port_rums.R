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

#' @export

port_rums <- function(m_y,
  f_y, nhauls_sampled = 75,
  seed, ncores, r_c = 1, r_s = 10, ports, dum_type = "no_bycatch", dyz, h_d){
  
  nports <- length(ports)
  if(nports == 1) nports <- tolower(substr(paste0(ports[[1]], collapse = ""), 1, 3))

  #Run the models
  runs <- lapply(1:length(ports), FUN = function(yy){
    st_time <- Sys.time()
    rum <- tryCatch(sampled_rums(data_in = tows_clust, the_port = ports[[yy]],
                            min_year = m_y, max_year = f_y,
                            risk_coefficient = r_c, ndays = dyz, focus_year = f_y, 
                            nhauls_sampled = nhauls_sampled,
                            seed = seed, ncores = ncores, rev_scale = r_s, model_type = dum_type,
                            habit_distance = h_d),
      error = function(e) NULL)
    
    r_time <- Sys.time() - st_time

    the_port <- ports[[yy]]
    the_port <- paste(the_port, collapse = "_")
    port_sv <- substr(the_port, 1, 3)

    filename <- paste0(port_sv, "_","runs", r_c, "_rev", r_s, "_minyr", m_y, '_focyr', f_y,  
      "_seed", seed, "_nday", nday, '_hdist', h_d)

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

  filename <- paste0("coefs", r_c, "_rev", r_s, "_minyr", m_y, '_focyr', f_y, "_nports", nports,
    "_seed", seed)
  save(coefs, file = paste0("//udrive.uw.edu//udrive//", filename, ".Rdata"))
  # filename <- paste0("runs", r_c, "_rev", r_s, "_minyr", m_y, '_focyr', f_y, "_nports", nports,
  #   "_seed", seed)
  # filename <- paste0("runs", r_c, "_rev", r_s, '_focyr', f_y, "_nports", nports)
  # save(runs, file = paste0("//udrive.uw.edu//udrive//", filename, ".Rdata"))

  return(coefs)
}