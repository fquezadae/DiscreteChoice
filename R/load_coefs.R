
#' Load coefficients function

#' Function to load coefficients from model runs

#' @param the_args List of arguments
#' @param res_dir Results directory
#' @export

load_coefs <- function(the_args = a_l, res_dir = "/Volumes/udrive/"){

  spp_abv <- paste0(sapply(a_l$quota_species, FUN = function(xx) substr(xx, 1, 2)), 
    collapse = "")

  #Make filename
  filename <- paste0("coefs", a_l$r_c, "_rev", a_l$r_s, "_minyr", a_l$m_y, 
    '_focyr', a_l$f_y, "_nports", length(a_l$ports),
        "_seed", a_l$seed, "_nday", a_l$dyz, '_hdist', a_l$h_d, "_netcost", a_l$n_c, 
        "_qspecies", spp_abv)
  
  flz <- list.files(res_dir)

  the_file <- flz[grep(filename, flz)]

  #load the thing out of a particular directory
  load(paste0(res_dir, the_file))
  return(coefs)
}



