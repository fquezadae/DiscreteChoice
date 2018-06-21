#' Prediction Metrics

#' Function that calculates metrics based on the tow with the highest modeled probability

#' @param choices Choice set
#' @param model Model results

#' @export

pred_metrics <- function(choices, model){

  rn <- row.names(mod$model)
  rn <- ldply(strsplit(rn, split = "\\."))
  rn <- unique(rn$V1)
  
  #Pull rows from choices
  preds <- choices[choices$fished_haul %in% rn, ]
  
  #Add probabilities into the predictions
  # res <- mlogit(mod$formula, mod$model)
  # pp <- predict(res, newdata = mod$model, outcome = FALSE)
  fits <- fitted(mod, outcome = FALSE)
  
  mfits <- melt(fits, byrow = T)
  names(mfits) <- c("rrow", "ccolumn", 'value')
  mfits <- mfits %>% arrange(rrow, ccolumn)
  
  preds$probs <- mfits$value
  
  ###Correct prediction using max probability value
  #Filter out the fished tows
  fished_tows <- preds %>% filter(fished == TRUE)
  pred_tows <- preds %>% group_by(fished_haul) %>% filter(probs == max(probs)) %>%
    as.data.frame
  
  correct_prediction <- sum(pred_tows$fished) / nrow(pred_tows) #score 1
  
  #Calculate distance between the two points
  pred_tows <- rbind(fished_tows, pred_tows) %>% arrange(fished_haul)
  
  tow_dists <- pred_tows %>% group_by(fished_haul) %>% 
    summarize(dist = gcd_slc(long1 = set_long[1], lat1 = set_lat[1], long2 = set_long[2], lat2 = set_lat[2])) %>%
    as.data.frame 
  tow_dists[which(is.na(tow_dists$dist)), 'dist'] <- 0
  tow_dists$correct <- 0
  tow_dists[tow_dists$dist <= 5, "correct"] <- 1
  
  #Final metrics
  correct_area <- sum(tow_dists$correct) / nrow(tow_dists) #score 2
  average_distance <- mean(tow_dists$dist) #score 4
  
  ##---------Second Metric
  #Correct prediction summed
  #Calculate the distance between the fished points and the others
  fished_locs <- preds %>% filter(fished == TRUE) %>% select(fished_haul, set_long, set_lat)
  fished_locs <- plyr::rename(fished_locs, c("set_long" = "fished_long", "set_lat" = "fished_lat"))
  preds <- preds %>% left_join(fished_locs, by = "fished_haul")
  preds$distance_from_fished <- gcd_slc(long1 = preds$set_long, lat1 = preds$set_lat,
    long2 = preds$fished_long, lat2 = preds$fished_lat)
  #Replace NAs with 0
  preds[is.na(preds$distance_from_fished), "distance_from_fished"] <- 0
  prob_in_area <- preds %>% filter(distance_from_fished <= 5) %>% group_by(fished_haul) %>%
    summarize(sum_prob = sum(probs)) 
  prob_mass <- mean(prob_in_area$sum_prob) #Score 3
  
  outs <- c(correct_prediction, correct_area, prob_mass, average_distance)
  return(outs)
}
