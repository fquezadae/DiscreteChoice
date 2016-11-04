#'Sample Tows

#'Function to sample tows from logbook data

#'@param nreps Number of replicates
#'@param nsamps Number of samples in each replicate
#'@param tac Total allowable catch value

#'@export
#'@examples
#' sample_tows(5, 10, 1000)

sample_tows <- function(nreps = 5000, nsamps = 50, tac = 50000){
  #Things to save:
  #(1) Percentage of the TAC caught
  #(2) Summed Catch across all tows
  #(3) Row indices to see the amount of other things caught
  perc_samples <- vector('list', length = nreps)
  tc_samples <- vector('list', length = nreps)
  row_samples <- vector('list', length = nreps)

  for(ii in 1:length(perc_samples)){
    samp_rows <- sample(1:nrow(of_interest), nsamps, replace = FALSE)
    sampled <- of_interest[samp_rows, ] %>% arrange(desc(apounds))

    #Store outputs
    perc_samples[[ii]] <- sampled$apounds / tac
    tc_samples[[ii]] <- sum(sampled$apounds)
    row_samples[[ii]] <- samp_rows
  }

  return(list('perc_samples' = perc_samples, 'tot_catch' = unlist(tc_samples),
    'row_samples' = row_samples))
}


