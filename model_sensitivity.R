#----------------------------------------------------------------
#Process coefficients
#risk_coefficient can be 5, 20, 50, 100,
#rev_type can be "qcos", "trev"

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
#Compare results with quotas species
trev1 <- compare_coefficients(risk_coefficient = 1, net_cost = 'trev',
  seed = 1002, nhauls = 50, years = 2011:2014)
trev1$seed <- 1002
trev1$net_cost <- "Revenues only"


qcos <- compare_coefficients(risk_coefficient = 100,
  net_cost = 'qcos', seed = 1002, nhauls = 50, years = 2011:2014)
qcos$seed <- 1002
qcos$net_cost <- "100x quota species"

coefs <- rbind(trev1, qcos)
coefs$net_cost <- factor(coefs$net_cost, 
  levels = c("100x quota species", "Revenues only"))

#Add column describing significance
coefs$sig <- 1
coefs[which(coefs$significance %in% c(" ", ".")), "sig"] <- 0

#Look how coefficient estimates change over time

#Eureka
coefs %>% filter(port == 'EUREKA') %>% ggplot(aes(x = year, y = value)) + 
  geom_line(aes(group = net_cost, linetype = net_cost), alpha = .5) +
  geom_point(aes(shape = net_cost, colour = sig), size = 3, alpha = .7) + 
  scale_colour_gradient(low = 'gray') +
  facet_wrap(~ coef, scales = 'free') + ggtitle("Eureka Coefficients")

#Newport
coefs %>% filter(port == 'NEWPORT') %>% ggplot(aes(x = year, y = value)) + 
  geom_line(aes(group = net_cost, linetype = net_cost), alpha = .5) +
  geom_point(aes(shape = net_cost, colour = sig), size = 3, alpha = .7) + 
  scale_colour_gradient(low = 'gray') +
  facet_wrap(~ coef, scales = 'free') + ggtitle("Newport Coefficients")

#Astoria
coefs %>% filter(port == 'ASTORIA / WARRENTON') %>% ggplot(aes(x = year, y = value)) + 
  geom_line(aes(group = net_cost, linetype = net_cost), alpha = .5) +
  geom_point(aes(shape = net_cost, colour = sig), size = 3, alpha = .7) + 
  scale_colour_gradient(low = 'gray') +
  facet_wrap(~ coef, scales = 'free') + ggtitle("Astoria Coefficients")

#Charleston
coefs %>% filter(port == 'CHARLESTON (COOS BAY)') %>% ggplot(aes(x = year, y = value)) + 
  geom_line(aes(group = net_cost, linetype = net_cost), alpha = .5) +
  geom_point(aes(shape = net_cost, colour = sig), size = 3, alpha = .7) + 
  scale_colour_gradient(low = 'gray') +
  facet_wrap(~ coef, scales = 'free') + ggtitle("Charleston Coefficients")


coefs <- coefs %>% dcast(year + port + net_cost ~ coef, value.var = 'value') 
#See how distance/revenue tradeoffs change over time
coefs %>% group_by(year, port, net_cost) %>%
           summarize(dr = dist / rev, dr1 = dist1 / rev1) %>% as.data.frame
ggplot(coefs, aes(x = year, y = dr))
           

coefs %>% dcast(year + port + net_cost ~ coef, value.var = 'value') %>%
  group_by(year, port, net_)


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




coefs100 %>% dcast(year + port ~ coef, value.var = 'value') %>% 
  group_by(year, port) %>% summarize(dr = dist / rev, dr1 = dist1 / rev1) %>%
  as.data.frame %>% melt(id.vars = c('year', 'port')) %>%
  ggplot(aes(x = year, y = value)) + geom_point(aes(colour = variable)) +
  geom_line(aes(colour = variable, group = variable)) +
  facet_wrap(~ port)



#----------------------------------------------------------------
#-----------------------------------------
#Testing things
the_ports <- "EUREKA"

the_args <- arg_list(ncores = 1, seed = the_seed, r_c = 100, r_s = 100, ports = the_ports,
                     h_d = the_hd, dyz = the_days, quota_species = quota_species, 
                     n_c = 'trev',
                nhauls_sampled = 50)

test_qcos <- port_rums(m_y = the_args$m_y, f_y = 2011, 
                       nhauls_sampled = the_args$nhauls_sampled,
                       ncores = 8, seed = the_args$seed, 
                       r_c = the_args$r_c, r_s = the_args$r_s, 
                       ports = the_args$ports, h_d = the_args$h_d,
                       dyz = the_args$dyz, quota_species = the_args$quota_species, 
                       n_c = "qcos")  

test_trev <- port_rums(m_y = the_args$m_y, f_y = 2011, 
                       nhauls_sampled = the_args$nhauls_sampled,
                       ncores = 8, seed = the_args$seed, 
                       r_c = the_args$r_c, r_s = the_args$r_s, 
                       ports = the_args$ports, h_d = the_args$h_d,
                       dyz = the_args$dyz, quota_species = the_args$quota_species, 
                       n_c = "trev")  

#-----------------------------------------
#Load the two models because the prices should be pretty different

#Look at the model files to verify that the quota costs are subtracted correctly
the_directory <- "/Volumes/udrive/"
udrive_files <- list.files(the_directory)  

udrive_files <- udrive_files[grep("runs100_", udrive_files)]
udrive_files <- udrive_files[grep("focyr2011", udrive_files)]
udrive_files <- udrive_files[grep("EUR", udrive_files)]
udrive_files <- udrive_files[grep("1001", udrive_files)]

#Load the first one
load(paste0(the_directory, udrive_files[1]))
mod1 <- mod
rm(mod)

#Then the second one
load(paste0(the_directory, udrive_files[2]))
mod2 <- mod

mod1$model[which(mod1$model$dummy_prev_days == 1)[1], ]
unique(mod1$model$dummy_prev_days)
mod1$model %>% head


hist(mod1$model$miss_rev_adj)
hist(mod2$model$miss_rev_adj)

head(mod1$model)

tows_clust %>% filter(haul_id == "88714")

range(mod1$model$miss_rev_adj)
range(mod2$model$miss_rev_adj)
head(mod1$model$miss_rev_adj, n = 100) == head(mod2$model$miss_rev_adj, n = 100)

#----------------------------------------------------------------
#Coefficient of 1
coefs1_names <- udrive_files[grep('coefs1_rev', udrive_files)]
coefs1_names <- coefs1_names[grep("hdist8.05_netcost", coefs1_names)]

#----------------------------------
#Coefficient of 5
coefs5_names <- udrive_files[grep('coefs5_', udrive_files)]

#Only 3:5 models converged
coefs5_names <- coefs5_names[3:5]

coefs5 <- lapply(coefs5_names, 
  FUN = function(xx) process_coefficients(filename = xx, 
    dir = the_directory))
coefs5 <- ldply(coefs5)

xx <- process_coefficients(filename = coefs5_names[2], dir = the_directory)

#----------------------------------
#Coefficient of 10
coefs10_names <- udrive_files[grep('coefs10_', udrive_files)]
coefs10 <- lapply(coefs10_names, FUN = function(xx) process_coefficients(filename = xx))
coefs10 <- ldply(coefs10)
#All these worked

#----------------------------------
#None of these worked
#Coefficient of 50
coefs50_names <- udrive_files[grep('coefs50_rev', udrive_files)]
coefs50_names <- coefs50_names[grep("1000", coefs50_names)]
coefs50 <- lapply(coefs50_names, FUN = function(xx) process_coefficients(filename = xx))
coefs50 <- ldply(coefs50)

xx <- process_coefficients(filename = coefs50_names[6], dir = the_directory)

#----------------------------------
#Coefficients of 100

coefs100_names <- udrive_files[grep('coefs100_rev', udrive_files)]

coefs100_names <- udrive_files[grep('coefs100_rev', udrive_files)]
coefs100_names <- coefs100_names[grep('trev', coefs100_names)]

coefs100 <- lapply(coefs100_names, FUN = function(xx) process_coefficients(filename = xx,
  dir = the_directory))
coefs100 <- ldply(coefs100)

#Visualize the change in distance to revenue ratios over time
coefs100 %>% dcast(year + port ~ coef, value.var = 'value') %>% 
  group_by(year, port) %>% summarize(dr = dist / rev, dr1 = dist1 / rev1) %>%
  as.data.frame %>% melt(id.vars = c('year', 'port')) %>%
  ggplot(aes(x = year, y = value)) + geom_point(aes(colour = variable)) +
  geom_line(aes(colour = variable, group = variable)) +
  facet_wrap(~ port)


#----------------------------------------------------------------
#Compare coefficients
coefs <- rbind(coefs5, coefs10, coefs50)

cc <- dcast(coefs, port + price_multiplier + year + hdist + spp ~ coef, value.var = 'value')
cc <- cc %>% group_by(port, year, price_multiplier) %>% summarize(rd = rev/dist, rd1 = rev1 / dist1) %>%
  as.data.frame

#rd
cc$year <- as.numeric(cc$year)
cc %>% ggplot() + geom_point(aes(x = year, y = rd, colour = price_multiplier)) + facet_grid(~ port) + 
  geom_vline(xintercept = 2010.5, lty = 2)

cc %>% ggplot() + geom_point(aes(x = year, y = rd1, colour = price_multiplier)) + facet_wrap(~ port)




