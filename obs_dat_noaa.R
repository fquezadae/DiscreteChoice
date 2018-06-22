#Specify Computer
setwd("C://Users//peter.kuriyama//ch4//")

#---------------------------------------------------------------------------------
#Start of obs_dat, work off this script
#Load Packages
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(devtools)
library(maps)
library(doParallel)
library(lubridate)
library(GGally)
library(nnet)
library(tidyr)
library(mlogit)
library(parallel)
library(sendmailR)

#Install ch4 package
devtools::install_github("peterkuriyama/ch4", auth_token = "83f947b716e40172803f0ff798c46f5ff9ca3cd1")
library("ch4")

#Install delta plot functions
devtools::install_github("peterkuriyama/ch2vms/ch2vms")
library(ch2vms)

#States Map
states_map <- map_data("state")

#---------------------------------------------------------------------------------
#Load and format data
#More formatting in ch4_movement
# load("//udrive.uw.edu//udrive//tows_clust_1010.Rdata")
# load("//udrive.uw.edu//udrive//tows_clust_0620.Rdata")
load('output//tows_clust_0621.Rdata')
tows_clust %>% filter(set_year == 2011) %>% distinct(haul_id, .keep_all = T) %>%
    group_by(trip_id) %>% summarize(avg_tgow = mean(tgow_rev))
tows_clust %>% filter(tgow_rev == max(tgow_rev)) %>% head



# load_all()

#------------------------------------------------------------------------------
#Sensitivity Runs with different quota species
#Canary only
the_ports <- list("EUREKA", "CHARLESTON (COOS BAY)", 
  "NEWPORT", "ASTORIA / WARRENTON")

# the_ports <- "EUREKA"
the_seed <- 1002
the_days <- 30
the_hd <- 5.1 #habit distance

quota_species = c("Canary Rockfish", "Darkblotched Rockfish",
  "Pacific Ocean Perch", "Yelloweye Rockfish", "Bocaccio Rockfish")

#-----------------------------------------

##1: CSDE Cluster: Run with Coefficient of 1 and total revenues for all years 
a_l <- arg_list(ncores = 10, seed = the_seed, r_c = 1, r_s = 100, ports = the_ports,
                     h_d = the_hd, dyz = the_days, quota_species = quota_species, 
                     n_c = 'trev',
                nhauls_sampled = 50)
run_six_years(the_args = a_l, years = 2009:2014)

##2: Blackfish Run: with Coefficient of 100 and quota costs for 2011:2014 for all 
#constraining species
a_l <- arg_list(ncores = 8, seed = the_seed, r_c = 100, r_s = 100, ports = the_ports,
                     h_d = the_hd, dyz = the_days, quota_species = quota_species, 
                     n_c = 'qcos',
                nhauls_sampled = 50)
run_six_years(the_args = a_l, years = 2011:2014)
#Because there's no difference in the model looking at Canary only


##3: Some computer: Run with a different seed to see if results change
a_l <- arg_list(ncores = 10, seed = the_seed + 10, r_c = 1, r_s = 100, 
      ports = the_ports,
                     h_d = the_hd, dyz = the_days, quota_species = quota_species, 
                     n_c = 'trev',
                nhauls_sampled = 50)
run_six_years(the_args = a_l, years = 2009:2014)

#4: Run with 75 hauls - CSDE
a_l <- arg_list(ncores = 10, seed = the_seed + 10, r_c = 1, r_s = 100, 
                ports = the_ports,
                h_d = the_hd, dyz = the_days, quota_species = quota_species, 
                n_c = 'trev',
                nhauls_sampled = 75)
run_six_years(the_args = a_l, years = 2009:2014)
