#' port_rums function

#' Function to run and save RUM data for 8 top ports

#' @param m_y Minimum year
#' @param f_y Focus Year, also used as the maximum year
#' @param nhauls_sampled Number of hauls to sample
#' @param seed value to set seed
#' @param ncores Defaults to 6
#' @param r_c Risk Coefficient, defaults to 1
#' @param r_s Revenue scaling value, defaults to 10

#' @export

port_rums <- function(m_y,
  f_y, nhauls_sampled = 75,
  seed, ncores, r_c = 1, r_s = 10){

  runs1 <- lapply(1:length(ports), FUN = function(yy){
    st_time <- Sys.time()
    rum <- sampled_rums(data_in = tows_clust, the_port = ports[[yy]],
                        min_year = m_y, max_year = f_y,
                        risk_coefficient = r_c, ndays = 30, focus_year = f_y, 
                        nhauls_sampled = nhauls_sampled,
                        seed = seed, ncores = ncores, rev_scale = r_s)
    r_time <- Sys.time() - st_time
    print(r_time)
    print(rum[[1]])
    return(rum)
  })
  
  ports_names <- lapply(ports, FUN = function(yy) paste(yy, collapse = "_"))
  ports_names <- ldply(ports_names)
  ports_names <- as.vector(ports_names$V1)
  
  coefs1 <- lapply(runs1, FUN = function(xx) xx[[1]])
  names(coefs1) <- ports_names

  filename <- paste0("coefs", r_c, "_rev", r_s, '_focyr', f_y,)
  save(coefs1, file = paste0("//udrive.uw.edu//udrive//", filename, ".Rdata"))
  
}