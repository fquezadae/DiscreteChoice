#---------------------------------------------------------------------------------
#Format all the coefficients by year
#See how behavior might change through time

#-------------------------------------------------------------------------------------
# Load the data

load("/Volumes/udrive/runs1_rev100_minyr2012_focyr2014_nports8_seed5.Rdata")

AIC(runs[[1]]$mod)
predict(runs[[1]]$mod$formula)

mf <- runs[[1]]$mod$formula
dd <- runs[[1]]$mod$model %>% head

fitted(runs[[1]]$mod, outcome = FALSE) %>% dim

#-------------------------------------------------------------------------------------
#Load the June 2018 run coefficients
dir_name <- '/Volumes/udrive/'
dir_name <- '//udrive.uw.edu//udrive//'

udrive_files <- list.files(dir_name)

udrive_files <- udrive_files[grep('coefs', udrive_files)]
udrive_files <- udrive_files[grep('seed', udrive_files)]
udrive_files <- udrive_files[grep('nports2|nports4', udrive_files)]

the_coefs <- lapply(udrive_files, FUN = function(xx){
# print(xx)
  filename <- paste0(dir_name, xx)
  load(filename)
  coefs <- lapply(coefs, FUN = function(yy){
    yy$coef_type <- row.names(yy)
    return(yy)
  })
  coefs <- ldply(coefs)
  if(dir_name == "//udrive.uw.edu//udrive//") to_parse <- 
    strsplit(filename, "\\//")[[1]][4]
  if(dir_name != "//udrive.uw.edu//udrive//") to_parse <- 
    strsplit(filename, "\\/")[[1]][4]
  
  to_parse <- strsplit(to_parse, "_")[[1]]
  
  coefs$year <- substr(to_parse[4], 6, 10)
  coefs$net_cost <- substr(to_parse[1], 6, 8)
  coefs$rev_scaling <- substr(to_parse[2], 4, 6)
  coefs$net_cost_type <- substr(to_parse[9], 8, 12)
  coefs$seed <- substr(to_parse[6], 5, 9)
  return(coefs)
})

the_coefs <- ldply(the_coefs)
names(the_coefs)[1] <- 'port'
the_coefs$value <- paste(formatC(the_coefs$coefs, digits = 5, format = 'f'), the_coefs$significance)
the_coefs <- the_coefs[which(duplicated(the_coefs) == F), ]





#Compare distance to revenue ratios
coefs_cast <- dcast(the_coefs, port + year + net_cost + net_cost_type ~ coef_type, value.var = "coefs")

the_coefs %>% filter(net_cost_type == "trev" & year == 2011)

coefs_cast %>% filter(net_cost_type == 'trev') %>% ggplot(aes(x = year,
  y = ))



coefs_cast %>% group_by(port, year, net_cost, net_cost_type) %>%
  summarize(dr = dist / rev, dr1 = dist1/rev1) %>% as.data.frame %>%
  melt %>% filter(net_cost_type == "qcos", variable == "dr" & net_cost %in% c(1, 5)) %>% ggplot(aes(x = year, y = value, colour = net_cost, 
    shape = net_cost_type)) + geom_point(size = 2) + 
  facet_wrap(~ port, scales = 'free')

#----------------------------------------------------
#Format just the total revenue model runs
trevs <- the_coefs %>% filter(net_cost_type == "trev") %>% 
  dcast(port + coef_type ~ year, value.var = "value")

coef_descs <- data.frame(coef_type = unique(trevs$coef_type), 
  coef_type_desc = c("Later Tow Distance", "First Tow Distance",
    "Fleet Habit", "Individual Habit", "Individual Habit Last Year", 
    "Later Tow Revenue", "First Tow Revenue"), stringsAsFactors = F) 

trevs <- trevs %>% left_join(coef_descs, by = 'coef_type')
write.csv(trevs, file = 'output//the_coefs_06_21_nday30_hdist5.1_for_plot.csv',
  row.names = F)



#-------------------------------------------------------------------------------------

predict(runs[[1]]$mod$formula, runs)

load("/Volumes/udrive/coefs1_rev100_minyr2009_focyr2011_nports8_seed5.Rdata")
coefs1 <- coefs

load("/Volumes/udrive/coefs1_rev100_minyr2010_focyr2012_nports8_seed5.Rdata")
coefs2 <- coefs

load("/Volumes/udrive/coefs1_rev100_minyr2011_focyr2013_nports8_seed5.Rdata")
coefs3 <- coefs

load("/Volumes/udrive/coefs1_rev100_minyr2012_focyr2014_nports8_seed5.Rdata")
coefs4 <- coefs


#-------------------------------------------------------------------------------------
ind <- 1
coefs_through_time <- lapply(c(1:6, 8), FUN = function(ind){
  outs <- data.frame(year1 = paste(coefs1[[ind]]$coefs, coefs1[[ind]]$significance), 
             year2 = paste(coefs2[[ind]]$coefs, coefs2[[ind]]$significance), 
             year3 = paste(coefs3[[ind]]$coefs, coefs3[[ind]]$significance),
             year4 = paste(coefs4[[ind]]$coefs, coefs4[[ind]]$significance))
  row.names(outs) <- row.names(coefs1[[1]])
  return(outs)

})
names(coefs_through_time) <- names(coefs1[c(1:6, 8)])



#-------------------------------------------------------------------------------------
#Combine coefficients from all the ports
format_coefs(coefs3)

#-------------------------------------------------------------------------------------
format_coefs <- function(coefs){
  coefs <- lapply(coefs, FUN = function(xx){
    xx$value = paste(formatC(xx$coefs, digits = 5, format = 'f'), xx$significance)
    return(xx)
  } )
  
  c1_table <- lapply(coefs, FUN = function(xx){
    return(xx$value)
  })
  
  
  #Try foramtting this shit in xtable
  c1_table <- lapply(coefs, FUN = function(xx){
    return(xx$value)
  })
  
  c1_table <- ldply(c1_table)
  names(c1_table) <- c('port', rownames(coefs[[1]]))
  c1_table <- t(c1_table)
  c1_table <- as.data.frame(c1_table)

  port_names <- c('Moss Landing / San Francisco', "Fort Bragg", 'Eureka',
  "Crescent City / Brookings", "Charleston", 'Newport', 'Ilwaco / Newport')
  
  names(c1_table) <- port_names
  c1_table <- c1_table[-1, ]
  names(c1_table) <- c("MOS/SF", "FTB", "EUR", "CC/B", "CHA",
    "NEW", "ILW/NEW")
  return(c1_table)
}

cormat
