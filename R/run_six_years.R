#' Run Six Years Function
#' Run six years of model with different conditions

#' @param the_args List of arguments created by arg_list
#' @param years Years to run
#' @export


run_six_years <- function(the_args = a_l, years = 2009:2014){

  #define Email parameters
  from_email <- "<pkuriyama@gmail.com>"
  to_email <- from_email
  subject <- "complete"
  body <- "complete"
  mailControl=list(smtpServer="ASPMX.L.GOOGLE.COM")

  if(2009 %in% years){
    rums_09 <- port_rums(m_y = the_args$m_y, f_y = 2009, 
                       nhauls_sampled = the_args$nhauls_sampled,
                       ncores = the_args$ncores, seed = the_args$seed, 
                       r_c = the_args$r_c, r_s = the_args$r_s, 
                       ports = the_args$ports, h_d = the_args$h_d,
                       dyz = the_args$dyz, quota_species = the_args$quota_species, 
                       n_c = "trev")  
    sendmail(from = from_email, to = to_email, subject = paste("2009", subject),
        msg = body, control = mailControl)
  }

  
  if(2010 %in% years){
      rums_10 <- port_rums(m_y = the_args$m_y, f_y = 2010, 
                         nhauls_sampled = the_args$nhauls_sampled,
                         ncores = the_args$ncores, seed = the_args$seed, 
                         r_c = the_args$r_c, r_s = the_args$r_s, 
                         ports = the_args$ports, h_d = the_args$h_d,
                         dyz = the_args$dyz, quota_species = the_args$quota_species, 
                         n_c = "trev")  
    sendmail(from = from_email, to = to_email, subject = paste("2010", subject),
        msg = body, control = mailControl)
  }
  

  if(2011 %in% years){
    rums_11 <- port_rums(m_y = the_args$m_y, f_y = 2011, 
                       nhauls_sampled = the_args$nhauls_sampled,
                       ncores = the_args$ncores, seed = the_args$seed, 
                       r_c = the_args$r_c, r_s = the_args$r_s, 
                       ports = the_args$ports, h_d = the_args$h_d,
                       dyz = the_args$dyz, quota_species = the_args$quota_species, 
                       n_c = the_args$n_c)  
    sendmail(from = from_email, to = to_email, subject = paste("2011", subject),
        msg = body, control = mailControl)
  }
  
  if(2012 %in% years){
    rums_12 <- port_rums(m_y = the_args$m_y, f_y = 2012, 
                       nhauls_sampled = the_args$nhauls_sampled,
                       ncores = the_args$ncores, seed = the_args$seed, 
                       r_c = the_args$r_c, r_s = the_args$r_s, 
                       ports = the_args$ports, h_d = the_args$h_d,
                       dyz = the_args$dyz, quota_species = the_args$quota_species, 
                       n_c = the_args$n_c)  
    sendmail(from = from_email, to = to_email, subject = paste("2012", subject),
        msg = body, control = mailControl)
  }
  
  if(2013 %in% years){
    rums_13 <- port_rums(m_y = the_args$m_y, f_y = 2013, 
                       nhauls_sampled = the_args$nhauls_sampled,
                       ncores = the_args$ncores, seed = the_args$seed, 
                       r_c = the_args$r_c, r_s = the_args$r_s, 
                       ports = the_args$ports, h_d = the_args$h_d,
                       dyz = the_args$dyz, quota_species = the_args$quota_species, 
                       n_c = the_args$n_c)  
     sendmail(from = from_email, to = to_email, subject = paste("2013", subject),
        msg = body, control = mailControl)
  }
  
  if(2014 %in% years){
    rums_14 <- port_rums(m_y = the_args$m_y, f_y = 2014, 
                       nhauls_sampled = the_args$nhauls_sampled,
                       ncores = the_args$ncores, seed = the_args$seed, 
                       r_c = the_args$r_c, r_s = the_args$r_s, 
                       ports = the_args$ports, h_d = the_args$h_d,
                       dyz = the_args$dyz, quota_species = the_args$quota_species, 
                       n_c = the_args$n_c)  
     sendmail(from = from_email, to = to_email, subject = paste("2014", subject),
        msg = body, control = mailControl)
  }
  
}



