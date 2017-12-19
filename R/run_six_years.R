#' Run Six Years Function
#' Run six years of model with different conditions

#' @param the_args List of arguments created by arg_list
#' @export


run_six_years <- function(the_args = a_l){
  rums_09 <- port_rums(m_y = the_args$m_y, f_y = 2009, 
                     nhauls_sampled = the_args$nhauls_sampled,
                     ncores = the_args$ncores, seed = the_args$seed, 
                     r_c = the_args$r_c, r_s = the_args$r_s, 
                     ports = the_args$ports, h_d = the_args$h_d,
                     dyz = the_args$dyz, quota_species = the_args$quota_species, 
                     n_c = "trev")
  rums_10 <- port_rums(m_y = the_args$m_y, f_y = 2010, 
                     nhauls_sampled = the_args$nhauls_sampled,
                     ncores = the_args$ncores, seed = the_args$seed, 
                     r_c = the_args$r_c, r_s = the_args$r_s, 
                     ports = the_args$ports, h_d = the_args$h_d,
                     dyz = the_args$dyz, quota_species = the_args$quota_species, 
                     n_c = "trev")
  rums_11 <- port_rums(m_y = the_args$m_y, f_y = 2011, 
                     nhauls_sampled = the_args$nhauls_sampled,
                     ncores = the_args$ncores, seed = the_args$seed, 
                     r_c = the_args$r_c, r_s = the_args$r_s, 
                     ports = the_args$ports, h_d = the_args$h_d,
                     dyz = the_args$dyz, quota_species = the_args$quota_species, 
                     n_c = the_args$n_c)
  rums_12 <- port_rums(m_y = the_args$m_y, f_y = 2012, 
                     nhauls_sampled = the_args$nhauls_sampled,
                     ncores = the_args$ncores, seed = the_args$seed, 
                     r_c = the_args$r_c, r_s = the_args$r_s, 
                     ports = the_args$ports, h_d = the_args$h_d,
                     dyz = the_args$dyz, quota_species = the_args$quota_species, 
                     n_c = the_args$n_c)
  rums_13 <- port_rums(m_y = the_args$m_y, f_y = 2013, 
                     nhauls_sampled = the_args$nhauls_sampled,
                     ncores = the_args$ncores, seed = the_args$seed, 
                     r_c = the_args$r_c, r_s = the_args$r_s, 
                     ports = the_args$ports, h_d = the_args$h_d,
                     dyz = the_args$dyz, quota_species = the_args$quota_species, 
                     n_c = the_args$n_c)
  rums_14 <- port_rums(m_y = the_args$m_y, f_y = 2014, 
                     nhauls_sampled = the_args$nhauls_sampled,
                     ncores = the_args$ncores, seed = the_args$seed, 
                     r_c = the_args$r_c, r_s = the_args$r_s, 
                     ports = the_args$ports, h_d = the_args$h_d,
                     dyz = the_args$dyz, quota_species = the_args$quota_species, 
                     n_c = the_args$n_c)
}



