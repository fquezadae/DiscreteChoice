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
