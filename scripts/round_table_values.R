# fleet_name1 <- "ASTORIA / WARRENTON"

#Change the model coefficients

#------------------------------------------------------------
parse_cols <- function(the_column){
  
  temp <- strsplit(the_column, split = " ") 
  nwords <- sapply(temp, FUN = function(ll){
    length(ll)
  })
  #Extract the values
  temp[which(nwords == 0)]
  
  temp[which(nwords == 1)]
  v1 <- temp[which(nwords == 1)]
  v1 <- ldply(v1) 
  v1 <- format(round(as.numeric(v1$V1), digits = 3), nsmall = 3)

  v2 <- ldply(temp[which(nwords == 2)])
  v2$V1 <- as.numeric(v2$V1)
  v2$V1 <- format(round(v2$V1, digits = 3), nsmall = 3)
  v2$V2 <- "*"
  v2 <- paste0(v2$V1, " ", v2$V2)
  
  #Format the output
  out_column <- vector(length = length(temp))
  out_column[which(nwords == 0)] <- ""
  out_column[which(nwords == 1)] <- v1
  out_column[which(nwords == 2)] <- v2
  return(out_column)
}

par

#------------------------------------------------------------
#Astoria
ast <- read.csv("figs/astoria_model_sens.csv", stringsAsFactors = F)
ast_new <- ast
ast_new$X2011 <- parse_cols(ast$X2011)
ast_new$X2012 <- parse_cols(ast$X2012)
ast_new$X2013 <- parse_cols(ast$X2013)
ast_new$X2014 <- parse_cols(ast$X2014)
write.csv(ast_new, file = "figs/astoria_rounded.csv", row.names = F)

#Newport
ast <- read.csv("figs/new_model_sensitivities.csv", stringsAsFactors = F)
ast_new <- ast
ast_new$X2011 <- parse_cols(ast$X2011)
ast_new$X2012 <- parse_cols(ast$X2012)
ast_new$X2013 <- parse_cols(ast$X2013)
ast_new$X2014 <- parse_cols(ast$X2014)
write.csv(ast_new, file = "figs/newport_rounded.csv", row.names = F)

#Eureka
ast <- read.csv("figs/eur_model_sensitivities.csv", stringsAsFactors = F)
ast_new <- ast
ast_new$X2011 <- parse_cols(ast$X2011)
ast_new$X2012 <- parse_cols(ast$X2012)
ast_new$X2013 <- parse_cols(ast$X2013)
ast_new$X2014 <- parse_cols(ast$X2014)
write.csv(ast_new, file = "figs/eureka_rounded.csv", row.names = F)

#Eureka
ast <- read.csv("figs/cha_model_sensitivities.csv", stringsAsFactors = F)
ast_new <- ast
ast_new$X2011 <- parse_cols(ast$X2011)
ast_new$X2012 <- parse_cols(ast$X2012)
ast_new$X2013 <- parse_cols(ast$X2013)
ast_new$X2014 <- parse_cols(ast$X2014)
write.csv(ast_new, file = "figs/cha_rounded.csv", row.names = F)





