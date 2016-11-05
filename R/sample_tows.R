#'Sample Tows

#'Function to sample tows from logbook data

#'@param nreps Number of replicates
#'@param nsamps Number of samples in each replicate
#'@param tac Total allowable catch value

#'@export
#'@examples
#' sample_tows(5, 10, 1000)

sample_tows <- function(nreps = 5000, nsamps = 50, tac = 50000, seed = 300){
  # print('in the apply function one')
  #Things to save:
  #(1) Percentage of the TAC caught
  #(2) Summed Catch across all tows
  #(3) Row indices to see the amount of other things caught
  perc_samples <- vector('list', length = nreps)
  tc_samples <- vector('list', length = nreps)
  row_samples <- vector('list', length = nreps)

  #Set Seed
  set.seed(seed)
  
  samp_rows <- lapply(row_samples, FUN = function(x) sample(1:nrow(of_interest), nsamps, replace = FALSE))
  sampled <- lapply(samp_rows, FUN = function(x) of_interest[x, ] %>% arrange(desc(apounds)))
  perc_samples <- sampled$apounds / tac
  tc_samples <- lapply(sampled, FUN = function(x) sum(x$apounds))

  return(list('perc_samples' = perc_samples, 'tot_catch' = unlist(tc_samples),
    'row_samples' = samp_rows))

}


