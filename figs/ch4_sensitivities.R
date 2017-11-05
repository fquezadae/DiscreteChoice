#-----------------------------------------------------------------------------------------------
#Get AIC, predictions, logLikes, and coefficients for each run
#
#-----------------------------------------------------------------------------------------------
#Compare files I have locally and on the udrive
local_files <- list.files("model_runs")
udrive_files <- list.files("/Volumes/udrive")
# local_files %in% udrive_files
udrive_files[which(udrive_files %in% local_files == FALSE)]
# the_files <- local_files
the_files <- udrive_files
#-----------------------------------------------------------------------------------------------
#For the ports


#Astoria
choices <- get_choice_set(data_in = tows_clust, the_port = "ASTORIA / WARRENTON",
  min_year = 2007, max_year = 2012, risk_coefficient = 1,
  ndays = 30, focus_year = 2012, nhauls_sampled = 50, seed = 10, ncores = 6, rev_scale = 100,
  model_type = 'no_bycatch', net_cost = "qcos", habit_distance = 5)
ast <- get_aic(the_port = "AST")

#Newport
choices <- get_choice_set(data_in = tows_clust, the_port = "NEWPORT",
  min_year = 2007, max_year = 2012, risk_coefficient = 1,
  ndays = 30, focus_year = 2012, nhauls_sampled = 50, seed = 10, ncores = 6, rev_scale = 100,
  model_type = 'no_bycatch', net_cost = "qcos", habit_distance = 5)
new <- get_aic(the_port = "NEW") #doesn't work for some reason

#Charleston
choices <- get_choice_set(data_in = tows_clust, the_port = "CHARLESTON (COOS BAY)",
  min_year = 2007, max_year = 2012, risk_coefficient = 1,
  ndays = 30, focus_year = 2012, nhauls_sampled = 50, seed = 10, ncores = 6, rev_scale = 100,
  model_type = 'no_bycatch', net_cost = "qcos", habit_distance = 5)
charleston <- get_aic(the_port = "CHA")

#Crescent City and Brookings
choices <- get_choice_set(data_in = tows_clust, the_port = c("CRESCENT CITY", 'BROOKINGS'),
  min_year = 2007, max_year = 2012, risk_coefficient = 1,
  ndays = 30, focus_year = 2012, nhauls_sampled = 50, seed = 10, ncores = 6, rev_scale = 100,
  model_type = 'no_bycatch', net_cost = "qcos", habit_distance = 5)
ccb <- get_aic(the_port = "CRE") #doesn't work

#Eureka
choices <- get_choice_set(data_in = tows_clust, the_port = "EUREKA",
  min_year = 2007, max_year = 2012, risk_coefficient = 1,
  ndays = 30, focus_year = 2012, nhauls_sampled = 50, seed = 10, ncores = 6, rev_scale = 100,
  model_type = 'no_bycatch', net_cost = "qcos", habit_distance = 5)
eur <- get_aic(the_port = "EUR")

choices <- get_choice_set(data_in = tows_clust, the_port = "FORT BRAGG",
  min_year = 2007, max_year = 2012, risk_coefficient = 1,
  ndays = 30, focus_year = 2012, nhauls_sampled = 50, seed = 10, ncores = 6, rev_scale = 100,
  model_type = 'no_bycatch', net_cost = "qcos", habit_distance = 5)
ftbragg <- get_aic(the_port = "FOR")

fits_2012 <- rbind(ast, new, charleston, ccb, eur, ftbragg)
fits_2012 <- fits_2012 %>% arrange(delta_aic)
fits_2012 <- fits_2012 %>% filter(radius != 16)

save(fits_2012, file = 'output/fits_2012.Rdata')

fits_2012 %>% filter(delta_aic == 0)
#-----------------------------------------------------------------------------------------------
load('output/fits_2012.Rdata')


#Significance any different
#Look at table of AIC values
fits_2012$unq <- paste(fits_2012$ndays, fits_2012$radius)
fits_2012 %>% dcast(fleet ~ unq, value.var = 'delta_aic')

#-----------------------------------------------------------------------------------------------
#look at differences in coefficient values
#Read in coefficients 
#------------------------------------------
#Load the coefficients
coef_files <- the_files[grep("coefs1_rev100_minyr2007_focyr2012_nports6_seed10", the_files)]
cfiles <- the_files[grep('coefs1', the_files)]
ccfiles <- cfiles[grep("seed10_nday30_hdist5.1", cfiles)]


#Load all the coefficients
the_coefs <- lapply(1:length(coef_files), FUN = function(xx){
  flz <- paste0('/Volumes/udrive/', coef_files[xx])
  load(flz)
  cc <- coefs
  return(cc)
})

#Add in the rownames as a column
the_coefs <- lapply(the_coefs, FUN = function(xx){
  for(yy in 1:length(xx)){
    xx[[yy]]$coef_names <- row.names(xx[[yy]])
  }
  return(xx)  
})

#Add in the port to each thing
the_coefs <- lapply(the_coefs, FUN = function(xx){
  temp <- ldply(xx)
  names(temp)[1] <- 'port'
  return(temp)
})
names(the_coefs) <- coef_files
the_coefs <- ldply(the_coefs)
names(the_coefs)[1] <- 'filename'
add_day_radius <- the_coefs %>% distinct(filename) %>% mutate(nday = c(30, 14, 14, 14, 30, 30),
	radius = c(3, 10, 3, 5, 10, 5))
the_coefs <- the_coefs %>% left_join(add_day_radius, by = 'filename')

the_coefs$model <- paste(the_coefs$nday, the_coefs$radius)
the_coefs$value <- paste0(the_coefs$coefs, the_coefs$significance)


pivoted <- the_coefs %>% dcast(port + coef_names ~ model, value.var = 'value')
write.csv(pivoted, file = 'output/coef_sensitivities.csv', row.names = F)











#-----------------------------------------------------------------------------------------------
grep('')
load("model_runs/AST_runs1_rev100_minyr2007_focyr2012_seed10_nday14_hdist5.1.Rdata")

str(mod$model)
fitted(mod) %>% head
predict(mod$model, mod$formula)
predict(mod$formula, newdata = mod$model)

rerun <- mlogit(mod$formula, mod$model)
probs <- predict(rerun, newdata = mod$model) 


rmultinom(n = 1, size = 1, prob = probs[1, ])



fitted(mod) %>% head

list.files("model_runs")

list.files('/Volumes/udrive')[list.files('/Volumes/udrive') %in% list.files("model_runs") == FALSE]

load(file = 'output/delts.Rdata')