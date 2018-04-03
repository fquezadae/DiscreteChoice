#----------------------------------------------------------------
#Process coefficients
#risk_coefficient can be 5, 20, 50, 100,
#rev_type can be "qcos", "trev"

udrive_files[grep(75, udrive_files)]

compare_coefficients <- function(risk_coefficient, net_cost,
  seed, nhauls, years = 2009:2014, nports = 4){

  #----------------------------------------------------------
  #Determine directory based on system type
  if(Sys.info()['sysname'] == 'Darwin'){
    the_directory <- "/Volumes/udrive/"
    udrive_files <- list.files(the_directory)  
  }
  
  if(Sys.info()['sysname'] != 'Darwin'){  
    the_directory <- "//udrive.uw.edu//udrive//"
    udrive_files <- list.files(the_directory)  
  }
  if(length(udrive_files) == 0) stop('check udrive connection')

  #----------------------------------------------------------
  #Now read in the coefficients
  #By risk coefficient
  coefs_names <- udrive_files[grep(paste0('coefs', risk_coefficient, "_"),
    udrive_files)]
  
  #Then filter by net_cost
  coefs_names <- coefs_names[grep(net_cost, coefs_names)]

  #Filter by seed 
  coefs_names <- coefs_names[grep(as.character(seed), coefs_names)]  

  #Add in number of hauls
  coefs_names <- coefs_names[grep(as.character(nhauls), coefs_names)]  

  #Filter by years
  yrz <- paste(years, collapse = "|")
  coefs_names <- coefs_names[which(grepl(yrz, coefs_names))  ]

  #number of ports, defaults to 4
  coefs_names <- coefs_names[grep(as.character(nports), coefs_names)]  

  #----------------------------------------------------------
  process_coefficients(filename = coefs_names[2],
    dir = the_directory)

  #Process the coefficients
  coefs <- lapply(coefs_names, FUN = function(xx)
    process_coefficients(filename = xx, dir = the_directory))
  coefs <- ldply(coefs)
  coefs$net_cost <- net_cost
  print(coefs_names)
  return(coefs)
}

#----------------------------------------------------------------
##Compare results with quotas species
# trev1 <- compare_coefficients(risk_coefficient = 1, net_cost = 'trev',
#   seed = 1002, nhauls = 50, years = 2011:2014)
# trev1$seed <- 1002
# trev1$net_cost <- "Revenues only"

qcos <- compare_coefficients(risk_coefficient = 100,
  net_cost = 'qcos', seed = 1002, nhauls = 50, years = 2011:2014)
qcos$seed <- 1002
qcos$net_cost <- "100x quota species"

qcos1 <- compare_coefficients(risk_coefficient = 1,
  net_cost = 'qcos', seed = 1002, nhauls = 50, years = 2011:2014)
qcos1$seed <- 1002
qcos1$net_cost <- "1x quota species"

qcos50 <- compare_coefficients(risk_coefficient = 50,
  net_cost = 'qcos', seed = 1002, nhauls = 50, years = 2011:2014)
qcos50$seed <- 1002
qcos50$net_cost <- "50x quota species"

coefs <- rbind(qcos, qcos50, qcos1)
coefs$net_cost <- factor(coefs$net_cost, 
  levels = rev(c("100x quota species", "50x quota species", "1x quota species")))

#Add column describing significance
coefs$sig <- "significant"
coefs[which(coefs$significance %in% c(" ", ".")), "sig"] <- "not significant"

#specify fill colors
coefs$unq <- paste(coefs$sig, coefs$net_cost, sep = "_")
coefs$unq <- factor(coefs$unq, levels = c(
  "not significant_1x quota species",
  "not significant_50x quota species", 'not significant_100x quota species',
  "significant_1x quota species",
  "significant_50x quota species", 'significant_100x quota species'))

#--------------------------
#Load likelihood values
qcosll <- compare_aic(risk_coefficient = 100,
  net_cost = 'qcos', seed = 1002, nhauls = 50, years = 2011:2014, ncores = 6)
# qcos$seed <- 1002
# qcos$net_cost <- "100x quota species"

qcos1ll <- compare_aic(risk_coefficient = 1,
  net_cost = 'qcos', seed = 1002, nhauls = 50, years = 2011:2014)
# qcos1$seed <- 1002
# qcos1$net_cost <- "1x quota species"

qcos50ll <- compare_aic(risk_coefficient = 50,
  net_cost = 'qcos', seed = 1002, nhauls = 50, years = 2011:2014)
# qcos50$seed <- 1002
# qcos50$net_cost <- "50x quota species"

#--------------------------

#Extract default ggplot colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = 3
cols = gg_color_hue(n)

#Look how coefficient estimates change over time
ports <- c("EUREKA", 'NEWPORT', 'ASTORIA / WARRENTON', "CHARLESTON (COOS BAY)")

for(ii in 1:length(ports)){
  the_port <- ports[ii]
  port_label <- tolower(the_port)
  port_label <- gsub(" \\/ ", " ", port_label)
  file_name <- paste0("figs/coefs_by_port/", port_label, "_coefficients.png")
  
  port_label <- tools::toTitleCase(port_label)
  
  coefs %>% filter(port == the_port,
    net_cost != "Revenues only") %>% ggplot(aes(x = year, y = value)) + 
    geom_line(aes(colour = net_cost, group = net_cost)) +
    geom_point(aes(shape = net_cost, fill = unq, colour = net_cost), 
      alpha = .7, size = 2) + scale_shape_manual(values = c(21, 22, 23)) +
    scale_fill_manual(values = c('white', 'white', 'white', 
      cols)) +
    scale_colour_manual(values = cols) +
    facet_wrap(~ coef, scales = 'free') + guides(fill = 'none') +
    ggtitle(paste0(port_label, " Coefficients")) + ggsave(width = 11.5, height = 9,
      file = file_name)
}
    
#--------------------------
#Process distance/revenue values
coefs_c <- coefs %>% dcast(year + port + net_cost ~ coef, value.var = 'value')   
coefs_dr <- coefs_c %>% group_by(year, port, net_cost) %>%
              summarize(dr = dist / rev, dr1 = dist1 / rev1) %>% as.data.frame
coefs_dr <- melt(coefs_dr, id = c('year', 'port', 'net_cost'))
coefs_dr$unq <- paste(coefs_dr$variable, coefs_dr$seed, sep = "_")


#First Tow: distance to revenue ratio of later tows 
coefs_dr %>% filter(variable == 'dr1') %>% 
  ggplot(aes(x = year, y = value)) + geom_point(aes(colour = net_cost,
    shape = net_cost), size = 2) + facet_wrap(~ port, scales = 'free') +
  geom_line(aes(group = net_cost, colour = net_cost)) + 
  ylab("First tows; distance to revenue ratio") + ggsave(width = 7.8, height = 7,
    file = 'figs/coefs_by_port/dr1.png')
  
#Later Tow: distance to revenue ratio of later tows 
coefs_dr %>% filter(variable == 'dr') %>% 
  ggplot(aes(x = year, y = value)) + geom_point(aes(colour = net_cost,
    shape = net_cost), size = 2) + facet_wrap(~ port, scales = 'free') +
  geom_line(aes(group = net_cost, colour = net_cost)) + 
  ylab("Later tows; distance to revenue ratio") + 
  ggsave(width = 7.8, height = 7,
    file = 'figs/coefs_by_port/dr.png')

#----------------------------------------------------------------
#----------------------------------------------------------------
##Compare similarities with higher seeds
trev1 <- compare_coefficients(risk_coefficient = 1, net_cost = 'trev',
  seed = 1002, nhauls = 75, years = 2009:2014)
trev1$seed <- 1002

trev2 <- compare_coefficients(risk_coefficient = 1, net_cost = 'trev',
  seed = 1012, nhauls = 75, years = 2009:2014)
trev2$seed <- 1012

coefs <- rbind(trev1, trev2)
coefs$seed <- as.character(coefs$seed)

#Add column describing significance
coefs$sig <- 1
coefs[which(coefs$significance %in% c(" ", ".")), "sig"] <- 0

coefs_c <- coefs %>% dcast(year + port + seed ~ coef, value.var = 'value')   
coefs_dr <- coefs_c %>% group_by(year, port, seed) %>%
            summarize(dr = dist / rev, dr1 = dist1 / rev1) %>% as.data.frame
coefs_dr <- melt(coefs_dr, id = c('year', 'port', 'seed'))
coefs_dr$unq <- paste(coefs_dr$variable, coefs_dr$seed, sep = "_")

#Add column describing significance
coefs$sig <- "significant"
coefs[which(coefs$significance %in% c(" ", ".")), "sig"] <- "not significant"

#specify fill colors
coefs$unq <- paste(coefs$sig, coefs$seed, sep = "_")
coefs$unq <- factor(coefs$unq, levels = c("not significant_1002",
  "not significant_1012", 'significant_1002', "significant_1012"))

# coefs_dr %>% ggplot(aes(x = year, y = value)) + geom_point(aes(colour = variable,
#   shape = seed)) + facet_wrap(~ port, scales = 'free') +
# geom_line(aes(colour = variable, lty = seed, group = unq))

#--------------------------
#Look how coefficient estimates change over time
#Comparing different seeds with higher samples
ports <- c("EUREKA", 'NEWPORT', 'ASTORIA / WARRENTON', "CHARLESTON (COOS BAY)")

for(ii in 1:length(ports)){
  the_port <- ports[ii]
  port_label <- tolower(the_port)
  port_label <- gsub(" \\/ ", " ", port_label)
  file_name <- paste0("figs/coefs_by_port/", port_label, "_coefficients_75samples.png")
  
  port_label <- tools::toTitleCase(port_label)
  
  pp <- coefs %>% filter(port == the_port) %>% ggplot(aes(x = year, y = value)) + 
    geom_point(aes(shape = seed, fill = unq, colour = seed), size = 2)
  
  coefs %>% filter(port == the_port) %>% ggplot(aes(x = year, y = value)) + 
    geom_point(aes(shape = seed, fill = unq, colour = seed), size = 2) +
    geom_line(aes(colour = seed, group = seed)) +
    scale_shape_manual(values = c(21, 22)) + scale_fill_manual(values = c('white', 
      'white', '#F8766D', '#00BFC4')) + 
    scale_colour_manual(values = c('#F8766D', '#00BFC4')) +
    facet_wrap(~ coef, scales = 'free') + guides(fill = 'none') +
    ggtitle(paste0(port_label, " Coefficients")) + ggsave(width = 7.9, height = 7,
      file = file_name)  
}
    

#--------------------------
#Process distance/revenue values
coefs_c <- coefs %>% dcast(year + port + seed ~ coef, value.var = 'value')   
coefs_dr <- coefs_c %>% group_by(year, port, seed) %>%
              summarize(dr = dist / rev, dr1 = dist1 / rev1) %>% as.data.frame
coefs_dr <- melt(coefs_dr, id = c('year', 'port', 'seed'))
coefs_dr$unq <- paste(coefs_dr$variable, coefs_dr$seed, sep = "_")


#First Tow: distance to revenue ratio of later tows 
coefs_dr %>% filter(variable == 'dr1') %>% 
  ggplot(aes(x = year, y = value)) + geom_point(aes(colour = seed,
    shape = seed), size = 2) + facet_wrap(~ port, scales = 'free') +
  geom_line(aes(group = seed, colour = seed)) + 
  ylab("First tows; distance to revenue ratio") + ggsave(width = 7.8, height = 7,
    file = 'figs/coefs_by_port/dr1_75hauls.png')
  
#Later Tow: distance to revenue ratio of later tows 
coefs_dr %>% filter(variable == 'dr') %>% 
  ggplot(aes(x = year, y = value)) + geom_point(aes(colour = seed,
    shape = seed), size = 2) + facet_wrap(~ port, scales = 'free') +
  geom_line(aes(group = seed, colour = seed)) + 
  ylab("Later tows; distance to revenue ratio") + 
  ggsave(width = 7.8, height = 7,
    file = 'figs/coefs_by_port/dr_75hauls.png')
#----------------------------------------------------------------
#See how distance/revenue tradeoffs change over time

#Distance to revenues
ggplot(coefs_dr, aes(x = year, y = dr)) + geom_point(aes(colour = net_cost),
  size = 1.5) + 
  geom_line(aes(colour = net_cost, group = net_cost)) +
  facet_wrap(~ port, scales = 'free')

#Distance to revenues first tow           
ggplot(coefs_dr, aes(x = year, y = dr1)) + geom_point(aes(colour = net_cost),
  size = 1.5) + 
  geom_line(aes(colour = net_cost, group = net_cost)) +
  facet_wrap(~ port, scales = 'free')



#Plot them against
coefs %>% dcast(year + port + net_cost ~ coef, value.var = 'value') %>%
  group_by(year, port, net_cost) %>% 
  summarize(dr = dist / rev, dr1 = dist1 / rev1) %>% 
  ggplot(aes(x = dr, y = dr1)) + geom_point(aes(colour = year,
    shape = net_cost), size = 3) + 
  facet_wrap(~ port, scales = 'free')

#----------------------------------------------------------------
#Compare results from different seeds
trev1 <- compare_coefficients(risk_coefficient = 1, net_cost = 'trev',
  seed = 1002, nhauls = 50)
trev1$seed <- 1002

trev2 <- compare_coefficients(risk_coefficient = 1, net_cost = 'trev',
  seed = 1012, nhauls = 50)
trev2$seed <- 1012

coefs <- rbind(trev1, trev2)

#Look at the raw change in coefficient values over time
coefs %>% ggplot(aes(x = year, y = value,
  group = net_cost)) + geom_point(aes(colour = port),
  size = 2) + geom_line(aes(x = year, y = value, group = net_cost,
    colour = net_cost)) + 
  facet_wrap(~ coef, scales = "free") 



coefs %>% dcast(year + port + seed ~ coef, value.var = 'value') %>%
  group_by(year, port, seed) %>% 
  summarize(dr = dist / rev, dr1 = dist1 / rev1) %>% 
  ggplot(aes(x = dr, y = dr1)) + geom_point(aes(colour = year), size = 3) + 
  facet_wrap(~ port, scales = 'free_x')


#----------------------------------------------------------------
qcos <- compare_aic(risk_coefficient = 100,
  net_cost = 'qcos', seed = 1002, nhauls = 50, years = 2011:2014)


