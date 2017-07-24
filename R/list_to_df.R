#' Function to convert a list to dataframe

#' ldply but gives option to name columns different things
#' @param the_thing Input
#' @param ind_name Index value names; like 'rep' or 'tow'
#' @param col_ind_name column name of index values

#' @export

list_to_df <- function(the_thing, ind_name, col_ind_name){
  names(the_thing) <- paste0(ind_name, 1:length(the_thing))
  the_thing <- ldply(the_thing)
  names(the_thing)[1] <- col_ind_name
  return(the_thing)
}



