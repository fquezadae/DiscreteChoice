#' Calculate Probabilities 

#' Calculate probabilites based on type of profits and type of catch 

#' @param prof_type Type of profit; can be "avg_haul_profit" or "avg_profit_fuel_only"
#' @param catch_type Type of catch; can be "type_clust_catch", "type_clust_perc", or "type_prop_hauls"
#' @param objective Objective; to catch the "highest" of target species? "lowest" of weak stock species? 
#' Or area with the biggest "difference" between target and weaks?
#' @param poss_clusts1 Input to function; defaults to poss_clusts, called in the parent environment
#' @param in_cp_name Character string of input to calc_probs name

#' @export

#' @examples
#' f1 <- find_best_clusts(catch_type = "type_clust_perc", objective = "highest")
#' f2 <- find_best_clusts(catch_type = "type_prop_hauls", objective = "highest")

#' f3 <- find_best_clusts(catch_type = "type_clust_perc", objective = "lowest")
#' f4 <- find_best_clusts(catch_type = "type_prop_hauls", objective = "lowest")
#' #Results might be slightly different for different columns
#' cbind(f3$unq_clust, f4$unq_clust)

calc_probs <- function(prof_type = "avg_profit_fuel_only", catch_type, objective = "difference",
  in_cp_name = "poss_clusts1", poss_clusts1 = poss_clusts){

  #Three objectives: 
  #1. fish in places with the biggest difference between target and weaks
  #2. fish in places with the most targets
  #3. fish in places with the least weak stock species
# browser()  
  statement <- paste0("probs <- ", in_cp_name, " %>% distinct(type, unq_clust, type_clust_catch, type_clust_perc, 
    type_prop_hauls, avg_haul_profit, avg_profit_fuel_only) %>% dcast(unq_clust + ", 
    prof_type, " ~ type, value.var = ", "'", catch_type, "')")

  #-----------------------------------------------------------------------------------
  #Highest profits and biggest difference between target and weak species
  if(objective == "difference"){
    
    statement <- paste0(statement, "%>% mutate(targ_weak_diff = targets - weaks) %>%
      select(-targets, -weaks)")
    eval(parse(text = statement))
    probs <- probs %>% filter(is.na(targ_weak_diff) == FALSE)
    
    #Transform the values so that there are no negative numbers
    probs[, 2] <- probs[, 2] + abs(min(probs[, 2])) + 1
    probs[, 3] <- probs[, 3] + abs(min(probs[, 3])) + 1
      
    probs$probs <- (probs[, 2] / sum(probs[, 2]) + probs[, 3] / sum(probs[, 3])) / 2
  }
  
  #-----------------------------------------------------------------------------------
  #Highest profits and the highest target species amounts
  if(objective == "highest"){    
    eval(parse(text = statement))

    probs[, 2] <- probs[, 2] + abs(min(probs[, 2])) + 1
    probs$probs <- (probs[, 2] / sum(probs[, 2]) + probs$targets / sum(probs$targets)) / 2
  }
  
  #-----------------------------------------------------------------------------------
  #Highest profits and lowest amount of weak stock species
  if(objective ==  "lowest"){
    eval(parse(text = statement))
    probs <- probs %>% filter(is.na(weaks) == FALSE)
    probs[, 2] <- probs[, 2] + abs(min(probs[, 2])) + 1
    
    probs$probs <- (probs[, 2] / sum(probs[, 2]) + (1 - probs$weaks) / sum(1 - probs$weaks)) / 2    
  }

  probs <- probs %>% arrange(desc(probs))

  return(probs)
}
