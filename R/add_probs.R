#' Function to predict prababilities for each sampled data set

#' Take a model and a predicted set of training data and calculate the probabilities of fishing 
#' in each site given a set choice set. 

#' @param train_dat Training data, sampled from training_data function
#' @param the_model Output from sampled_rums function
#' @param ncores Number of cores to run in parallel, defaults to 6

#' @export

add_probs <- function(train_dat, the_model, ncores = 6){
  #add probabilities to the first tows
  first_tows <- train_dat[[1]]

  first_tows <- mclapply(unique(first_tows$fished_haul), FUN = function(xx){
    temp <- first_tows %>% filter(fished_haul == xx)
    probs <- predict(the_model[[3]], temp)
    temp$probs <- probs
    return(temp)
  }, mc.cores = ncores)
  first_tows <- ldply(first_tows)

  #Add probabilities to the second tows
  second_tows <- train_dat[[2]]

  second_tows <- mclapply(unique(second_tows$fished_haul), FUN = function(xx){
    temp <- second_tows %>% filter(fished_haul == xx)
    probs <- predict(the_model[[4]], newdata = temp)
    temp$probs <- probs
    return(temp)
  }, mc.cores = ncores)
  second_tows <- ldply(second_tows)
  return(list(first_tows = first_tows, second_tows = second_tows))
}