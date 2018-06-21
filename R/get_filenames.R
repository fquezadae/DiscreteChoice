
#' Function to get filenames to read in model results
#' @param the_args List of the arguments for particular runs
#' @param years Vector of years; default to 2011:2014
#' @export

get_filenames <- function(the_args, years = 2011:2014, ports = NA){
  #Define things
  m_y = the_args$m_y; 
  nhauls_sampled = the_args$nhauls_sampled;
  ncores = the_args$ncores; seed = the_args$seed; 
  r_c = the_args$r_c; r_s = the_args$r_s; 
  h_d = the_args$h_d;
  dyz = the_args$dyz; quota_species = the_args$quota_species; 
  n_c = the_args$n_c  

  #Species abbreviations
  spp_abv <- paste0(sapply(quota_species, FUN = function(xx) substr(xx, 1, 2)), 
    collapse = "")
  
  #Loop through port and year combinations
  if(is.na(ports)) {
    ports <- the_args$ports
    ports <- ldply(ports)$V1
  }
  
  # if(is.na(ports) == FALSE)
  port_sv <- substr(ports, 1, 3)

  combs <- expand.grid(port_sv, years, KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = F)
  names(combs) <- c('port', 'year')

  # f_y <- 2011:2014
  files <- lapply(years, FUN = function(xx){
    filename <- paste0(port_sv, "_","runs", r_c, "_rev", r_s, "_minyr", m_y, 
    '_focyr', xx,  
      "_seed", seed, "_nday", dyz, '_hdist', h_d, "_netcost", n_c, 
      "_qspecies", spp_abv, "_nhauls", nhauls_sampled)      
  })
  
  return(unlist(files))  
}
