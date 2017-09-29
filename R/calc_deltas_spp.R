#' Calculate Delta Species

#' Function to Calculate delta plot values given a vector of species. This function is called by
#' delta_plot and applied across a vector of years

#' @param data Data that has catches per tow
#' @param spps Vector of species of interest
#' @param focus Column of focus, defaults to hpounds

#' @export
#' @examples
#' calc_deltas_spp(data = wc_data,)

calc_deltas_spp <- function(data, spps, focus = 'hpounds'){
  zeroes <- sapply(spps, FUN = function(x) calc_delta_plot(data = data, spp = x, focus = focus)[1])
  skews <- sapply(spps, FUN = function(x) calc_delta_plot(data = data, spp = x, focus = focus)[2])

  outs <- data.frame(species = spps, prop_zero = ldply(zeroes)$V1, skew = ldply(skews)$V1)
}