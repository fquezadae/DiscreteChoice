#-----------------------------------------------------------------
#Make table of RUM coefficients
library(xtable)


#-----------------------------------------------------------------
#Load data 
load("/Volumes/udrive/coefs1.Rdata")
load("/Volumes/udrive/coefs50.Rdata")

#Format Data
port_names <- c('Moss Landing / San Francisco', "Fort Bragg", 'Eureka',
  "Crescent City / Brookings", "Charleston", 'Newport', 'Astoria', 'Ilwaco / Newport')

names(coefs1) <- port_names
names(coefs50) <- port_names

#-----------------------------------------------------------------
#Paste significance values to 
#-------------------------------------------------------------------------------------

load("/Volumes/udrive/coefs1_rev10_minyr2009_focyr2011_nports7_seed305.Rdata")
coefs1 <- coefs

load("/Volumes/udrive/coefs1_rev10_minyr2010_focyr2012_nports7_seed305.Rdata")
coefs2 <- coefs

load("/Volumes/udrive/coefs1_rev10_minyr2011_focyr2013_nports7_seed305.Rdata")
coefs3 <- coefs

load("/Volumes/udrive/coefs1_rev10_minyr2011_focyr2014_nports7_seed305.Rdata")
coefs4 <- coefs


tbl1 <- format_coefs(coefs = coefs1)
tbl2 <- format_coefs(coefs = coefs2)
tbl3 <- format_coefs(coefs = coefs3)
tbl4 <- format_coefs(coefs = coefs4)


#Combine them to track over time
tbls <- vector('list', length = 7)

lapply(1:7, FUN = function(aa){
  t1 <- as.character(tbl1[, aa])
  t2 <- as.character(tbl2[, aa])
  t3 <- as.character(tbl3[, aa])
  t4 <- as.character(tbl4[, aa])
  temp <- cbind(t1, t2, t3, t4)
  temp <- as.data.frame(temp)
  row.names(temp) <- row.names(tbl1)
  names(temp) <- c("2011", '2012', '2013', '2014')
  names(tbl2) <- gsub("/", "_", names(tbl2))
  filename <- paste0('figs/', names(tbl2)[aa], '.csv')
  write.csv(temp, file = filename)
})

{
  tbl1
}

for(ii in 1:7){
  l
}



# format_coefs <- function(coefs1)


# coefs1 <- lapply(coefs1, FUN = function(xx){
#   xx$value = paste(formatC(xx$coefs, digits = 5, format = 'f'), xx$significance)
#   return(xx)
# } )

# c1_table <- lapply(coefs1, FUN = function(xx){
#   return(xx$value)
# })


# #Try foramtting this shit in xtable
# c1_table <- lapply(coefs1, FUN = function(xx){
#   return(xx$value)
# })

# c1_table <- ldply(c1_table)
# names(c1_table) <- c('port', rownames(coefs1[[1]]))
# c1_table <- t(c1_table)
# c1_table <- as.data.frame(c1_table)

# names(c1_table) <- port_names
# c1_table <- c1_table[-1, ]
# names(c1_table) <- c("MOS/SF", "FTB", "EUR", "CC/B", "CHA",
#   "NEW", "ILW/NEW")






coefs1



#Calculate difference between tables
# lapply(1:8, FUN = function(xx){
#   temp <- coefs50[[xx]]$coefs - coefs1[[xx]]$coefs
#   coefs1[[xx]]$change <- temp
#   return(coefs1[[xx]])
# } )


xtable(c1_table)
print(xtable(c1_table), file = 'figs/coefs1.tex')

#-----------------------------------------------------------------

coefs50 <- lapply(coefs50, FUN = function(xx){
  xx$value = paste(formatC(xx$coefs, digits = 5, format = 'f'), xx$significance)
  return(xx)
} )

#Calculate difference between tables
lapply(1:8, FUN = function(xx){
  temp <- coefs50[[xx]]$coefs - coefs50[[xx]]$coefs
  coefs50[[xx]]$change <- temp
  return(coefs50[[xx]])
} )

#Try foramtting this shit in xtable
c50_table <- lapply(coefs50, FUN = function(xx){
  return(xx$value)
})

c50_table <- ldply(c50_table)
names(c50_table) <- c('port', rownames(coefs50[[1]]))
c50_table <- t(c50_table)
c50_table <- as.data.frame(c50_table)

names(c50_table) <- port_names
c50_table <- c50_table[-1, ]
names(c50_table) <- c("MOS/SF", "FTB", "EUR", "CC/B", "CHA",
  "NEW", "AST", "ILW/NEW")

# xtable(c50_table)
print(xtable(c50_table), file = 'figs/coefs50.tex')



xtable(c1_table)
print(xtable(c1_table), file = 'figs/coefs50.tex')









as.vector(c1_table[1, ])

coefs1$c

coefs_table <- 

xtable()


coefs1[[1]]$coefs - coefs50[[1]]$coefs
coefs50[[1]]

obs_data <- obs_data %>% arrange(trip_id, haul_num)




