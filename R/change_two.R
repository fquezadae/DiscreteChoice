#' Function to change two things at once
#' Function to change two things at once

#' @param in_list_start Starting list of arguments
#' @param to_change1 Thing one to change
#' @param to_change2 Thing two to change
#' @param values1 Values for to_change1
#' @param values2 Values for to_change2
#' @export

change_two <- function(in_list_start, to_change1, to_change2, values1, values2){
  #----------------------------------------------------------------
  #update in_list_start
  change_these <- expand.grid(values1, values2, stringsAsFactors = FALSE)
  
  the_lists <- lapply(1:nrow(change_these), FUN = function(xx){
    temp <- in_list_start
    temp[[to_change1]] <- change_these[xx, 1]
    temp[[to_change2]] <- change_these[xx, 2]
    return(temp)
  })

  #----------------------------------------------------------------
  #Run everything
  the_results <- lapply(1:length(the_lists), FUN = function(xx){
    temp <- the_lists[[xx]]
    temp_res <- fish_trip_simulation(temp)
    return(temp_res)
  })

  #----------------------------------------------------------------
  #Format the simulation results
  catches <- lapply(1:length(the_results), FUN = function(dd){
    tt <- the_results[[dd]][[1]]
    tt$t1 <- change_these[dd, 1]
    tt$t2 <- change_these[dd, 2]
    names(tt)[which(names(tt) == "t1")] <- to_change1
    names(tt)[which(names(tt) == "t2")] <- to_change2
    return(tt)
  })
  
  catches <- ldply(catches)
  
  quotas <- lapply(1:length(the_results), FUN = function(dd){
    tt <- the_results[[dd]][[2]]
    tt$t1 <- change_these[dd, 1]
    tt$t2 <- change_these[dd, 2]
    names(tt)[which(names(tt) == "t1")] <- to_change1
    names(tt)[which(names(tt) == "t2")] <- to_change2
    return(tt)
  })
  quotas <- ldply(quotas)


  run_time <- lapply(1:length(the_results), FUN = function(dd){
    tt <- the_results[[dd]][[3]]
  })
  run_time <- ldply(run_time)
  run_time <- cbind(run_time, change_these)
  names(run_time) <- c("run_time", to_change1, to_change2)

  return(list(catches = catches, quotas = quotas, run_time = run_time))
}