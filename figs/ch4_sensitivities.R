#-----------------------------------------------------------------------------------------------
#Get AIC, predictions, logLikes, and coefficients for each run
#
#-----------------------------------------------------------------------------------------------
#Model selection to find best fit
#Tried with different 
local_files <- list.files("model_runs")
udrive_files <- list.files("/Volumes/udrive")

local_files %in% udrive_files

udrive_files[which(udrive_files %in% local_files == FALSE)]

#For the ports
ast <- get_aic(the_port = "AST")
new <- get_aic(the_port = "NEW") #doesn't work for some reason
charleston <- get_aic(the_port = "CHA")
ccb <- get_aic(the_port = "CRE") #doesn't work
eur <- get_aic(the_port = "EUR")
ftbragg <- get_aic(the_port = "FOR")

fits_2012 <- rbind(ast, new, charleston, ccb, eur, ftbragg)
fits_2012 <- fits_2012 %>% arrange(delta_aic)
fits_2012 <- fits_2012 %>% filter(radius != 16)

#-----------------------------------------------------------------------------------------------
#Significance any different
#Look at table of AIC values
fits_2012$unq <- paste(fits_2012$ndays, fits_2012$radius)
fits_2012 %>% dcast(fleet ~ unq, value.var = 'delta_aic')

#-----------------------------------------------------------------------------------------------
#look at differences in coefficient values
#Read in coefficients 


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