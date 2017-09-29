#' Calculate values for delta plots

#' Delta plots come from Gillis et al. (2008) paper. Used to compare the proportion of zeroes and
#' skew of the catch distributions. Targeted species have skewed distributions. Function relies on
#' pull_catch and calc_skew functions. 

#' @param data Input data, defaults to wc_data
#' @param spp Species of interest
#' @param focus Column of interest, Options are hpounds, apounds

#' @export
#' @examples
#' calc_delta_plot(data = wc_data, spp = 'Dover Sole', focus = 'apound')

calc_delta_plot <- function(data, spp, focus = 'apound'){
browser()
  catch_data <- ch4::pull_catch(data = data, spp = spp, focus = focus)

  #Find the number of zeroes and remove them
  zeroes <- catch_data[catch_data[, 2] == 0, ]

  prop_zero <- nrow(zeroes) / nrow(catch_data)

  #Remove the zeroes
  nonzeroes <- catch_data[catch_data[, 2] != 0, ]

  #Log transform catch data
  nonzeroes[, 2] <- log(nonzeroes[, 2])
  nonzeroes <- as.data.frame(nonzeroes)

  skew <- calc_skew(nonzeroes[, 2])

  return(data.frame(prop_zero = prop_zero, skew = skew))

}