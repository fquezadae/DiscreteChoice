#------------------------------------------------------------------
#Load the data from for 2009-2014
coefs <- read.csv('output/the_coefs_09_14.csv', stringsAsFactors = F)
coefs <- melt(coefs, id.vars = c('port', 'coef_type', 'coef_type_desc'))

coefs$variable <- as.character(coefs$variable)

cc <- sapply(strsplit(coefs$value, " "), FUN = function(xx) xx[1])
sig <- sapply(strsplit(coefs$value, " "), FUN = function(xx) xx[2])
sig[which(is.na(sig))] <- ""

coefs$cc <- cc
coefs$sig <- sig

coefs$pch <- 0
coefs$cc <- as.numeric(coefs$cc)

coefs$cc <- formatC(coefs$cc, digits = 4, format = 'f')
coefs$value2 <- paste(coefs$cc, coefs$sig)
coefs$variable <- substr(coefs$variable, 2, 5)

#------------------------------------------------------------------
#Add characters to make sure the number of characters is the same

coefs[grep("-", coefs$value2, invert = T), 'value2'] <- paste0(" ", 
  coefs[grep("-", coefs$value2, invert = T), 'value2'])
coefs[which(coefs$sig == ""), 'value2'] <- paste0(coefs[which(coefs$sig == ""), 'value2'], "   ")
coefs[which(coefs$sig == "."), 'value2'] <- paste0(coefs[which(coefs$sig == "."), 'value2'], "  ")
coefs[which(coefs$sig == "*"), 'value2'] <- paste0(coefs[which(coefs$sig == "*"), 'value2'], "  ")
coefs[which(coefs$sig == "**"), 'value2'] <- paste0(coefs[which(coefs$sig == "**"), 'value2'], " ")
coefs$nchars <- nchar(coefs$value2)

#------------------------------------------------------------------
#Save the newly formatted table that should have alignment at the decimal
coefs_tbl <- coefs %>% dcast(port + coef_type + coef_type_desc ~ variable, value.var = 'value2')

cc <- c('dist1', 'dist', 'rev1', 'rev', 'dmiss', 'dum30', 'dum30y')
coefs_tbl <- coefs_tbl %>% group_by(port) %>%  slice(match(cc, coef_type)) %>% as.data.frame
write.csv(coefs_tbl, file = 'figs/coefs_tbl.csv', row.names = F)

# # coefs[which(coefs$nchars != 11 & grep("-", coefs$value2)), 'value2']

# # coefs[which(coefs$nchars != 11) %in% grep("-", coefs$value2), 'value2']


# coefs %>% dcast(port + coef_type + coef_type_desc ~ variable, value.var = 'value2')



# coefs[which(coefs$sig %in% c('', ".")), 'pch'] <- 19
# coefs$variable <- substr(coefs$variable, 2, 5)

# #Add coefficients as a data frame to port_plot
# cc <- c('dist1', 'dist', 'rev1', 'rev', 'dmiss', 'dum30', 'dum30y')

# require(RColorBrewer)
# coef_colors <- brewer.pal(7,"Dark2")

# # coef_colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f")
# # coef_colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628")
# coef_colors <- coefs %>% distinct(coef_type) %>% mutate(coef_colors = coef_colors)

# coefs <- coefs %>% left_join(coef_colors, by = 'coef_type')

# #Adjust colors
# coef_sigs <- coefs %>% distinct(sig) 
# coef_sigs$alpha <- c(1, 0, 1, 1 , 0)
# coefs <- coefs %>% left_join(coef_sigs, by = 'sig')
# coefs$sig_color <- apply(coefs, MAR = 1, FUN = function(xx) adjustcolor(xx['coef_colors'], xx['alpha']))
