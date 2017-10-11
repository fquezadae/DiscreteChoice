#Whitefish Runs
#---------------------------------------------------------------------------------
#Specify Computer
setwd('/Users/peterkuriyama/School/Research/ch4')
# setwd('c://Users//Peter//ch4')

# list.files("//udrive.uw.edu//udrive//file_clusts_dist.Rdata")
setwd('c://Users//Peter//Desktop//ch4')


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
library(tidyr)
library(mlogit)
library(parallel)

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
# load("//udrive.uw.edu//udrive//obs_data1.Rdata")
# load("//udrive.uw.edu//udrive//file_clusts_dist.Rdata")
# load("//udrive.uw.edu//udrive//filt_clusts_w_monthly_prices.Rdata")
# load("//udrive.uw.edu//udrive//tows_clust_921.Rdata")
# load("//udrive.uw.edu//udrive//tows_clust_925_depth_bin.Rdata")
load("//udrive.uw.edu//udrive//tows_clust_1010.Rdata")

#--------------------------------------------------------------------------------- 
#Example Run to see if things make sense
start_time <- Sys.time()
cc <- sampled_rums(data_in = tows_clust, the_port = "ILWACO/CHINOOK",
                   min_year = 2007, max_year = 2011,
                   risk_coefficient = 1, ndays = 30, focus_year = 2011,
                   nhauls_sampled = 50, seed = 300, ncores = 20, rev_scale = 100)
run_time <- Sys.time() - start_time; run_time

#--------------------------------------------------------------------------------- 
#Define the ports

ports <- list(c("MOSS LANDING", "SAN FRANCISCO"),
     "FORT BRAGG",
     "EUREKA",
     c("CRESCENT CITY", "BROOKINGS"),
     "CHARLESTON (COOS BAY)",
     "NEWPORT", 
     "ASTORIA / WARRENTON", 
     c("ILWACO/CHINOOK", "WESTPORT"))

#------------------------------------------------------------------------------

rum <- sampled_rums(data_in = tows_clust, the_port = ports[[yy]],
                    min_year = 2010, max_year = 2012,
                    risk_coefficient = 1, ndays = 30, focus_year = 2012, nhauls_sampled = 50,
                    seed = 305, ncores = 10, rev_scale = 100)

rum13 <- sampled_rums(data_in = tows_clust, the_port = ports[[yy]],
                    min_year = 2011, max_year = 2013,
                    risk_coefficient = 1, ndays = 30, focus_year = 2012, nhauls_sampled = 50,
                    seed = 305, ncores = 10, rev_scale = 100)

#------------------------------------------------------------------------------
#Run test runs for the relatively quick ones ports
# runs1 <- lapply(c(1, 2, 3), FUN = function(yy){
#   st_time <- Sys.time()
#   rum <- sampled_rums(data_in = tows_clust, the_port = ports[[yy]],
#                       min_year = 2011, max_year = 2013,
#                       risk_coefficient = 1, ndays = 30, focus_year = 2013, nhauls_sampled = 50,
#                       seed = 305, ncores = 10)
#   r_time <- Sys.time() - st_time
#   print(r_time)
#   print(rum[[1]])
#   return(rum)
# })
# 
# rums_iw <- port_rums(m_y = 2010, f_y = 2012, nhauls_sampled = 70,
#                      ncores = 10, seed = 305, r_c = 1, r_s = 10, 
#                      ports =  list(c("ILWACO/CHINOOK", "WESTPORT")))
# 
# newp_12 <- sampled_rums(data = tows_clust, the_port = "NEWPORT", min_year = 2010, max_year = 2012, focus_year = 2012, 
#              nhauls_sampled = 50, seed = 301, ncores = 10, rev_scale = 100)
# 
# newp_12_303 <- sampled_rums(data = tows_clust, the_port = "NEWPORT", min_year = 2010, max_year = 2012, focus_year = 2012, 
#                         nhauls_sampled = 50, seed = 303, ncores = 10, rev_scale = 100)
# 
# newp_12_303 <- sampled_rums(data = tows_clust, the_port = "NEWPORT", min_year = 2010, max_year = 2012, focus_year = 2012, 
#                             nhauls_sampled = 70, seed = 303, ncores = 10, rev_scale = 100)
# beep("mario")

#------------------------------------------------------------------------------
#Run for years in succession
#--------------------------Runs with risk coefficient of 1
#Run Without Astoria because it takes so long
nine_ports <-list("ILWACO/CHINOOK", "NEWPORT", 
                 "BROOKINGS", 'CHARLESTON (COOS BAY)', "EUREKA",
                 "FORT BRAGG", "ASTORIA / WARRENTON")

start_time <- Sys.time()
cc <- sampled_rums(data_in = tows_clust, the_port = "ILWACO/CHINOOK",
  min_year = 2007, max_year = 2013,
  risk_coefficient = 1, ndays = 30, focus_year = 2013,
  nhauls_sampled = 50, seed = 300, ncores = 10, rev_scale = 100)
run_time <- Sys.time() - start_time; run_time




#2011
rums_11 <- port_rums(m_y = 2009, f_y = 2011, nhauls_sampled = 50,
             ncores = 10, seed = 7, r_c = 1, r_s = 100, ports = nine_ports)

#2012
rums_12 <- port_rums(m_y = 2010, f_y = 2012, nhauls_sampled = 50,
          ncores = 10, seed = 7, r_c = 1, r_s = 100, ports = nine_ports)
#2013
rums_13 <- port_rums(m_y = 2011, f_y = 2013, nhauls_sampled = 50,
          ncores = 10, seed = 7, r_c = 1, r_s = 100, ports = nine_ports)

#2014
rums_14 <- port_rums(m_y = 2012, f_y = 2014, nhauls_sampled = 50,
          ncores = 10, seed = 7, r_c = 1, r_s = 100, ports = nine_ports)



