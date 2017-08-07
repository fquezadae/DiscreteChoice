#' Fit Random Utility model

#' Fit Random Utility Model to data. Uses multinomial logistic regression in package "nnet"

#' @param haul_dat Data to be input
#' @param max_iter Max number of iterations
#' @param max_weights Max. number of weights
#' @param print_trace If TRUE, print tracke

#' @export

fit_rum <- function(haul_dat, max_iter = 10000, max_weights = 3000, print_trace = FALSE){
  rum_res <- multinom(unq_clust_fact ~ revenue + d_port_clust_dist + Canary_Rockfish +
      Darkblotched_Rockfish + Pacific_Ocean_Perch + Yelloweye_Rockfish,
      data = haul_dat, 
      maxit = max_iter, MaxNWts = max_weights, trace = print_trace)  
  
  return(rum_res)
}




  # rum_res <- multinom(unq_clust_fact ~ revenue + d_port_clust_dist + Canary_Rockfish +
  #   Darkblotched_Rockfish + Pacific_Ocean_Perch + Yelloweye_Rockfish,
  #   data = haul_dat, 
  #   maxit = max_iter, MaxNWts = max_weights, trace = print_trace)  

  # return(rum_res)