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
    tow_num_range = c(1, 1), risk_coefficient = rc)

  #Format data for second tows
  second_tows <- format_rum_data(the_port = port,
    min_year = years[1], max_year = years[2], data_in = fc, ndays = ndays1,
    tow_num_range = c(2, 30), risk_coefficient = rc)

  #Fit the two models with dummy variables  
  #So far only include missing value dummy variable, can add in last 30 years if desired
  first_res <- mlogit(tow_clust ~ revs + dist + dummy_missing - 1 , first_tows)  
# d1 <-  mlogit(tow_clust ~ revs + dist + dummy_prev30  - 1, first_tows) 
# d30 <- mlogit(tow_clust ~ revs + dist + dummy_prev30 + dummy_missing - 1, second_tows)
  second_res <- mlogit(tow_clust ~ revs + dist + dummy_missing - 1, second_tows)

# browser()
  # first_res <- mlogit(tow_clust ~ revs + dist + dummy_missing + dummy_prev30 - 1 , first_tows)  
  # second_res <- mlogit(tow_clust ~ revs + dist + dummy_missing + dummy_prev30 - 1, second_tows)

  #Return the probabilities
  first_probs <- apply(fitted(first_res, outcome = FALSE), MAR = 2, FUN = mean)
  p1 <- data_frame(unq_clust = as.numeric(names(first_probs)), 
    probs = first_probs) %>% as.data.frame
  second_probs <- apply(fitted(second_res, outcome = FALSE), MAR = 2, FUN = mean)
  p2 <- data_frame(unq_clust = as.numeric(names(second_probs)), 
    probs = second_probs) %>% as.data.frame

  #Reformat to sample easily
  outs <- list(first_probs = p1, second_probs = p2, first_res = first_res,
    second_res = second_res, first_tows = first_tows, second_tows = second_tows)
  return(outs)
}

# AIC(second_res)
# AIC(s_nodummy)
# f_nodummy <- mlogit(tow_clust ~ revs + dist  - 1 , first_tows)  
# s_nodummy <- mlogit(tow_clust ~ revs + dist  - 1 , second_tows)  




# #Try to evaluate the fits of the different things...
# fitted(second_res)

# dats <- second_tows %>% filter(tow_clust == TRUE) %>% select(alt) %>% table

# #Empirical values
# emp <- dats / sum(dats)
# preds <- apply(fitted(second_res, outcome = FALSE), MAR = 2, FUN = mean)

# #Compare empirical values to predictions
# emp <- round(emp, digits = 7)
# preds <- round(preds, digits = 7)
# second_res$freq / sum(second_res$freq)

# newdat <- second_tows %>% filter(haul_id_id == 3882) 
# newdat %>% filter(tow_clust == T)
# round(predict(second_res, newdata = newdat), digits = 0 )





  #Fit models without dummy variables
  # first_res_no_dummy <- try(mlogit(tow_clust ~ revs + dist - 1, first_tows))
  # second_res_no_dummy <- try(mlogit(tow_clust ~ revs + dist - 1, second_tows))

  #Use the modelw tih the lowest AIC
  # aic1 <- AIC(first_res)
  # aic11 <- AIC(first_res_no_dummy)

# #Pick a model
# the_models <- list(first_res, first_res_no_dummy, second_res, second_res_no_dummy)
# aic_res <- vector('list', length = length(the_models))

# for(ii in 1:length(the_models)){
#   temp_class <- class(the_models[[ii]])
  
#   if(temp_class == 'try-error'){
#     aic_value <- NA
#     next
#   } 
  
#   if(temp_class != 'try_error'){
#     aic_value <- AIC(the_models[[ii]])  
#   }
  
#   aic_res[[ii]] <- data_frame(temp_class, aic_value)
# }

# aic_res[[1]]$aic_value - aic_res[[2]]$aic_value

# aic_res[[1]]$aic_value - aic_res[[2]]$aic_value

# if(aic_res[[1]]$aic_value - aic_res[[2]]$aic_value > 0){
#   first_res <- first_res_no_dummy
# }
# aic_res[[1]]$aic_value, aic_res[[2]]$aic_value)

# aic_res[[3]]$aic_value > aic_res[[4]]$aic_value
# min(aic_res[[3]]$aic_value, aic_res[[4]]$aic_value)

# model_classes <- lapply(the_models, FUN = function(xx){
#   temp_class <- class(xx)
#   if(temp_class == 'try-error') 
#   aic_value <- AIC(xx)
#   return(data_frame(temp_class, aic_value))
# })
# data_frame("first_res", 'first_res_no_dummy')


# if(AIC(first_res) > AIC(first_res_no_dummy)){
#   first_res <- first_res_no_dummy
#   print('first no dummy is best fit')
# }

# apply()
# class(second_res) == 'try-error'
# class(second_res_no_dummy) == 'try-error'

# if(AIC(second_res) > AIC(second_res_no_dummy)){
#   second_res <- second_res_no_dummy
#   print('first no dummy is best fit')
# }

# AIC(first_res_no_dummy)

#   the_firsts <- c(NA, NA)
#   the_firsts[1] <- AIC(first_res)
#   the_firsts[2] <- AIC(first_res_no_dummy)  
#   the_firsts <- data_frame(mod = c("first_res", 'first_res_no_dummy'), 
#     AIC = the_firsts)

# predict(first_res)
# predict(first_res_no_dummy)

#   the_seconds <- c(NA, NA)
#   the_seconds[1] <- AIC(second_res)
#   the_seconds[2] <- AIC(second_res_no_dummy)
#   the_seconds <- data_frame(mod = c("second_res", 'second_res_no_dummy'), 
#     AIC = the_seconds)

#   #Pick the one that has the lowest AIC
#   apply(fitted(first_res, outcome = FALSE), MAR = 2, FUN = mean)
#   apply(fitted(first_res_no_dummy, outcome = FALSE), MAR = 2, FUN = mean)  
# # predict(first_res)