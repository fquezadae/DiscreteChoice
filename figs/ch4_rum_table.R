#-----------------------------------------------------------------
#Make table of RUM coefficients
library(xtable)

the_files <- list.files("/Volumes/udrive")
cfiles <- the_files[grep('coefs1', the_files)]
cfiles[grep('netcost', cfiles)]

#-----------------------------------------------------------------
#Paste significance values to 
#-------------------------------------------------------------------------------------
#2009 coefficient
# load("/Volumes/udrive/coefs1_rev100_minyr2007_focyr2009_nports6_seed7.Rdata")
# load("/Volumes/udrive/AST_coefs1_rev100_minyr2007_focyr_2009_seed7.Rdata")
# ast_coefs9 <- coefs
# coefs9[[6]] <- ast_coefs9
# names(coefs9)[6] <- "ASTORIA / WARRENTON"


load("/Volumes/udrive/coefs1_rev100_minyr2007_focyr2009_nports6_seed10_nday30_hdist5.1_netcosttrev.Rdata")
coefs9 <- coefs
rm(coefs)

#2010 coefficients
# load("/Volumes/udrive/coefs1_rev100_minyr2007_focyr2010_nports6_seed7.Rdata")
load("/Volumes/udrive/coefs1_rev100_minyr2007_focyr2010_nports6_seed10_nday30_hdist5.1_netcosttrev.Rdata")
coefs10 <- coefs
rm(coefs)
# coefs10[[6]] <- NULL

# load("/Volumes/udrive/AST_coefs1_rev100_minyr2007_focyr_2010_seed7.Rdata")
# coefs10[[6]] <- coefs
# names(coefs10)[6] <- "ASTORIA / WARRENTON" 
# rm(coefs)

#2011 coefficients
load("/Volumes/udrive/coefs1_rev100_minyr2007_focyr2011_nports6_seed10_nday30_hdist5.1_netcostqcos.Rdata")
coefs1 <- coefs

#2012 coefficients
# load("/Volumes/udrive/coefs1_rev100_minyr2007_focyr2012_nports6_seed7.Rdata")
load("/Volumes/udrive/coefs1_rev100_minyr2007_focyr2012_nports6_seed10_nday30_hdist5.1_netcostqcos.Rdata")
coefs2 <- coefs

#2013 coefficients
# load("/Volumes/udrive/coefs1_rev10_minyr2011_focyr2013_nports7_seed305.Rdata")
# load("/Volumes/udrive/coefs1_rev100_minyr2007_focyr2013_nports6_seed7.Rdata")
load("/Volumes/udrive/coefs1_rev100_minyr2007_focyr2013_nports6_seed10_nday30_hdist5.1_netcostqcos.Rdata")
coefs3 <- coefs

#2014 coefficients
load("/Volumes/udrive/coefs1_rev100_minyr2007_focyr2014_nports6_seed10_nday30_hdist5.1_netcostqcos.Rdata")
# load("/Volumes/udrive/coefs1_rev100_minyr2007_focyr2014_nports6_seed7.Rdata")
coefs4 <- coefs

#-------------------------------------------------------------------------------------
#coefs from 2009 and 2010
coefs9 <- lapply(coefs9, FUN = function(xx){
  xx$coef_type <- row.names(xx)
  xx <- plyr::rename(xx, c('coefs' = 'value'))
  xx$year <- 2009
  return(xx)
})
coefs9 <- ldply(coefs9)
coefs9 <- plyr::rename(coefs9, c(".id" = 'port'))

#2010
coefs10 <- lapply(coefs10, FUN = function(xx){
  xx$coef_type <- row.names(xx)
  xx <- plyr::rename(xx, c('coefs' = 'value'))
  xx$year <- 2010
  return(xx)
})
coefs10 <- ldply(coefs10)
coefs10 <- plyr::rename(coefs10, c(".id" = 'port'))

#Look at coefficients from 2011-2013

coefs1 <- lapply(coefs1, FUN = function(xx){
  xx$coef_type <- row.names(xx)
  xx <- plyr::rename(xx, c('coefs' = 'value'))
  xx$year <- 2011
  return(xx)
})
coefs1 <- ldply(coefs1)
coefs1 <- plyr::rename(coefs1, c(".id" = 'port'))

coefs2 <- lapply(coefs2, FUN = function(xx){
  xx$coef_type <- row.names(xx)
  xx <- plyr::rename(xx, c('coefs' = 'value'))
  xx$year <- 2012
  return(xx)
})

coefs2 <- ldply(coefs2)
coefs2 <- plyr::rename(coefs2, c(".id" = 'port'))

coefs3 <- lapply(coefs3, FUN = function(xx){
  xx$coef_type <- row.names(xx)
  xx <- plyr::rename(xx, c('coefs' = 'value'))
  xx$year <- 2013
  return(xx)
})

coefs3 <- ldply(coefs3)
coefs3 <- plyr::rename(coefs3, c(".id" = 'port'))

coefs4 <- lapply(coefs4, FUN = function(xx){
  xx$coef_type <- row.names(xx)
  xx <- plyr::rename(xx, c('coefs' = 'value'))
  xx$year <- 2014
  return(xx)
})

coefs4 <- ldply(coefs4)
coefs4 <- plyr::rename(coefs4, c(".id" = 'port'))

#-------------------------------------------------------------------------------------
#Combine them all into one data frame
all_coefs <- rbind(coefs9, coefs10, coefs1, coefs2, coefs3, coefs4)

all_coefs$value_sig <- paste(all_coefs$value, all_coefs$significance)
cc <- c('dist1', 'dist', 'rev1', 'rev', 'dmiss', 'dum30', 'dum30y')

portz <- unique(all_coefs$port)

#Format all the coefficients

the_coefs <- lapply(portz, FUN = function(xx){
  out <- all_coefs %>% filter(port == xx) %>% 
    dcast(port + coef_type ~ year, value.var = 'value_sig') %>%
    slice(match(cc, coef_type)) %>% as.data.frame
})

the_coefs <- ldply(the_coefs)

#add more descriptive names
coef_desc <- the_coefs %>% distinct(coef_type)
coef_desc$coef_type_desc <- c("First Tow Distance", "Later Tow Distance", 
  "First Tow Revenue", "Later Tow Revenue", "Fleet Habit", "Individual Habit",
  "Individual Habit Last Year")

the_coefs <- the_coefs %>% left_join(coef_desc, by = 'coef_type')

# write.csv(the_coefs, file = 'output/the_coefs_09_14.csv', row.names = F)
write.csv(the_coefs, file = 'output/the_coefs_09_14_nday30_hdist5.1_for_plot.csv', row.names = F)

#-------------------------------------------------------------------------------------
#For Table
all_coefs <- rbind(coefs9, coefs10, coefs1, coefs2, coefs3, coefs4)

all_coefs$value_sig <- paste0(all_coefs$value, all_coefs$significance)
cc <- c('dist1', 'dist', 'rev1', 'rev', 'dmiss', 'dum30', 'dum30y')

portz <- unique(all_coefs$port)

#Format all the coefficients

the_coefs <- lapply(portz, FUN = function(xx){
  out <- all_coefs %>% filter(port == xx) %>% 
    dcast(port + coef_type ~ year, value.var = 'value_sig') %>%
    slice(match(cc, coef_type)) %>% as.data.frame
})

the_coefs <- ldply(the_coefs)

#add more descriptive names
coef_desc <- the_coefs %>% distinct(coef_type)
coef_desc$coef_type_desc <- c("First Tow Distance", "Later Tow Distance", 
  "First Tow Revenue", "Later Tow Revenue", "Fleet Habit", "Individual Habit",
  "Individual Habit Last Year")

the_coefs <- the_coefs %>% left_join(coef_desc, by = 'coef_type')

# write.csv(the_coefs, file = 'output/the_coefs_09_14.csv', row.names = F)
write.csv(the_coefs, file = 'output/the_coefs_09_14_nday30_hdist8.05_for_table.csv', row.names = F)

#-------------------------------------------------------------------------------------
#Figure out how to format the coefs table...
portz <- unique(coefs$port)

for(pp in 1:length(unique(portz))){
  temp <- coefs %>% filter(port == portz[pp])
  temp1 <- temp %>% dcast(port + coef_type_desc ~ variable, value.var = 'value')
  # flname <- portz[pp]
  flname <- gsub(" ", "_",  tolower(portz[pp]))
  if(pp == 6) flname <- "astoria"
  flname <- paste0(flname, "_coefs.csv")
  flname <- paste0('figs/', flname)
  print(flname)
  write.csv(temp1, file = flname, row.names = FALSE)
}


#-------------------------------------------------------------------------------------
#look at some of the important coefficient results
coefs %>% dcast(port + coef_type + coef_type_desc ~ variable, value.var = 'sig')

coefs %>% filter(coef_type == 'dist') %>% filter(sig %in% c("", "."))

coefs %>% distinct(coef_type)

coefs[grep("-", coefs$value2, invert = T), 'value2'] <- paste0(" ", 
  coefs[grep("-", coefs$value2, invert = T), 'value2'])
coefs[which(coefs$sig == ""), 'value2'] <- paste(coefs[which(coefs$sig == ""), 'value2'], "   ")
coefs[which(coefs$sig == "."), 'value2'] <- paste(coefs[which(coefs$sig == "."), 'value2'], "  ")
coefs[which(coefs$sig == "*"), 'value2'] <- paste(coefs[which(coefs$sig == "*"), 'value2'], "  ")
coefs[which(coefs$sig == "**"), 'value2'] <- paste(coefs[which(coefs$sig == "**"), 'value2'], " ")

coefs$nchars <- nchar(coefs$value2)

coefs[grep("-", coefs$value2), ]

coefs %>% filter(sig %in% c("", ".")) %>% 
  filter(coef_type == "dmiss") %>% arrange(port)

#-------------------------------------------------------------------------------------
#process the coefficients in a different way















# ldply(coefs1)







# tbl1 <- format_coefs(coefs = coefs1)
# tbl2 <- format_coefs(coefs = coefs2)
# tbl3 <- format_coefs(coefs = coefs3)
# tbl4 <- format_coefs(coefs = coefs4)


# #Combine them to track over time
# tbls <- vector('list', length = 7)

# lapply(1:7, FUN = function(aa){
#   t1 <- as.character(tbl1[, aa])
#   t2 <- as.character(tbl2[, aa])
#   t3 <- as.character(tbl3[, aa])
#   t4 <- as.character(tbl4[, aa])
#   temp <- cbind(t1, t2, t3, t4)
#   temp <- as.data.frame(temp)
#   row.names(temp) <- row.names(tbl1)
#   names(temp) <- c("2011", '2012', '2013', '2014')
#   names(tbl2) <- gsub("/", "_", names(tbl2))
#   filename <- paste0('figs/', names(tbl2)[aa], '.csv')
#   write.csv(temp, file = filename)
# })



# # format_coefs <- function(coefs1)


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
  xx$value = paste(formatC(xx$coefs, digits = 4, format = 'f'), xx$significance)
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


#-----------------------------------------------------------------
#Load data 
# load("/Volumes/udrive/coefs1.Rdata")
# load("/Volumes/udrive/coefs50.Rdata")

# #Format Data
# port_names <- c('Moss Landing / San Francisco', "Fort Bragg", 'Eureka',
#   "Crescent City / Brookings", "Charleston", 'Newport', 'Astoria', 'Ilwaco / Newport')

# names(coefs1) <- port_names
# names(coefs50) <- port_names