#' Adjust Cluster Probabilities
#' Function to adjust cluster probabilities based on different things

#' @param rum1 RUM results
#' @param quotas1 Quota amounts
#' @param rc1 risk_coefficient

#'risk coefficients;
#' -1 -- blind fishing
#' 0-- only use original data values
#' 1--take catches into account; will eventually get towards 10
#' 10 -- avoid risk fishing

#' @export


adjust_probs <- function(rum1, quotas1, rc1){
  quotas1$prop <- round(quotas1$catch / quotas1$tac, digits = 4)
  quotas1$adj_prop <- quotas$prop * 10 #Maximum value

  #adjust back down depending on the risk coefficient
  if(rc1 == -1) quotas1$adj_prop <- 0 #Case where you don't care about catch expectations at all
  if(rc1 == 0) quotas1$adj_prop <- 1
  if(rc1 == 1){
    quotas1$adj_prop <- sapply(quotas1$adj_prop, FUN = function(xx) min(10, xx))
    quotas1[which(quotas1$adj_prop < 1), "adj_prop"] <- 1
  } 
  if(rc1 == 10) quotas1$adj_prop <- 10
  
  adj_probs <- quotas1 %>% filter(type == "weaks") %>% dcast(type ~ species, value.var = 'adj_prop')
  
  #Adjust the names
  names(adj_probs)[-1] <- gsub(" ", "_", names(adj_probs)[-1])
  X1 <- model.matrix(rum1)
  
  #Adjust catch expectations based on percentage of TAC caught
  for(aa in colnames(X1)[which(colnames(X1) %in% names(adj_probs))]){
    X1[, aa] <- X1[, aa] * adj_probs[, aa]    
  }

  clust_probs <- apply(predict(rum1, X1, type = 'probs'), 2, mean)
  # return probabilities, quotas, and data values
  return(list(probs = clust_probs, quotas = quotas1, Xvalues = X1))
}
