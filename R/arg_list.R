#---------------------------------------------------------------------------------
#Function to generate list of arguments
#' Argument list
#' Function that creates arguments

#' @param m_y minimum year
#' @export

arg_list <- function(m_y = 2007, f_y = 2009, nhauls_sampled = 50,
                     ncores = 8, seed = the_seed, r_c = 50, r_s = 100, ports = the_ports, 
                     h_d = the_hd,
                     dyz = the_days, quota_species = c("Canary Rockfish"), 
                     n_c = "trev"){
  outs <- list(m_y = m_y, f_y = f_y, nhauls_sampled = nhauls_sampled,
                     ncores = ncores, seed = seed, r_c = r_c, r_s = r_s, 
                     ports = ports, h_d = h_d,
                     dyz = dyz, quota_species = quota_species, n_c = n_c)
  return(outs)
}
