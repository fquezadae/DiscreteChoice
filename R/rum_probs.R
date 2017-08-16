#' rum_probs Function
#' Function to format rum data, fit models, then return the probabilities of fishing in each place

#' @param rc Risk Coefficient, used to scale costs of quota pounds
#' @param port Port of interest
#' @param years Vector of years, min_year and max_year
#' @param fc filt_clusts is default data in
#' @param ndays1 Number of days to look back, default to 60

#' @export

rum_probs <- function(rc = 1, port, years, fc = filt_clusts, ndays1 = 60){
  #Format data for first tow only
  first_tows <- format_rum_data(the_port = port,
    min_year = years[1], max_year = years[2], data_in = fc, ndays = ndays1,
    tow_num_range = c(1, 1))

  #Format data for second tows
  second_tows <- format_rum_data(the_port = port,
    min_year = years[1], max_year = years[2], data_in = fc, ndays = ndays1,
    tow_num_range = c(2, 30))

  #Fit the two models with dummy variables
  first_res <- mlogit(tow_clust ~ revs + dist | dummy - 1, first_tows)
  second_res <- mlogit(tow_clust ~ revs + dist | dummy - 1, second_tows)

  #Return the probabilities
  first_probs <- apply(fitted(first_res, outcome = FALSE), MAR = 2, FUN = mean)
  p1 <- data_frame(unq_clust = as.numeric(names(first_probs)), 
    probs = first_probs) %>% as.data.frame
  second_probs <- apply(fitted(second_res, outcome = FALSE), MAR = 2, FUN = mean)
  p2 <- data_frame(unq_clust = as.numeric(names(second_probs)), 
    probs = second_probs) %>% as.data.frame

  #Reformat to sample easily
  outs <- list(first_probs = p1, second_probs = p2, first_res = first_res,
    second_res = second_res)
  return(outs)
}