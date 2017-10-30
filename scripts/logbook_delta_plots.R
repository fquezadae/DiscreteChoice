#--------------------------------------------------------------------
#Run permutation delta plots with logbook data

#--------------------------------------------------------------------
#load and format logbook data
load('data/LBKDATA_Barnett_Logbook_Data_2002_2015_2016-06-07.Rda')

lbk_dat <- LBK
names(lbk_dat) <- tolower(names(lbk_dat))

#add targets and constraining species IDs
lbk_dat$type <- NULL

#Define species categories
targ_spp <- c('Dover Sole', 'Lingcod', "Longspine Thornyhead",
  "Petrale Sole", "Sablefish", "Shortspine Thornyhead")
weak_spp <- c("Bocaccio Rockfish", 'Canary Rockfish', "Cowcod Rockfish",
  "Darkblotched Rockfish", 'Pacific Ocean Perch', "Yelloweye Rockfish")
gfish_spp <- c("Arrowtooth Flounder", "Bank Rockfish", "Black Rockfish", 
  "Chilipepper Rockfish", "English Sole", "Greenspotted Rockfish", "Greenstriped Rockfish",
  "Longnose Skate", "Vermilion Rockfish", "Widow Rockfish", "Yellowtail Rockfish")

lbk_type <- data_frame(species = c(targ_spp, weak_spp, gfish_spp), type = 
  c(rep('targets', 6), rep('weaks', 6), rep('groundfish', 
    length(gfish_spp)))) %>% as.data.frame

lbk_dat <- lbk_dat %>% left_join(lbk_type, by = 'species')

#Parse towdate column
lbk_dat$set_date <- lbk_dat$towdate
lbk_dat$set_date <- dmy(lbk_dat$set_date)
lbk_dat %>% select(towdate, set_date) %>% head
lbk_dat$set_year <- year(lbk_dat$set_date)
lbk_dat$set_month <- month(lbk_dat$set_date)
lbk_dat$set_day <- day(lbk_dat$set_date)

lbk_dat <- lbk_dat %>% filter(set_year >= 2007, set_year <= 2014)
lbk_dat$avg_depth <- lbk_dat$depth1

#Add in missing columns for avg_depth, set_day and set_month
#Make sure all the names are there

# which(c("haul_id", "drvid", "set_lat", "set_long", "up_lat", "up_long", 
#   "avg_depth", "hpounds", "apounds", "set_day", "set_month", "set_year") %in% names(lbk_dat) == FALSE)
# c("haul_id", "drvid", "set_lat", "set_long", "up_lat", "up_long", 
#   "avg_depth", "hpounds", "apounds", "set_day", "set_month", "set_year")[c(7, 10, 11)]

lbk_dat$when <- 'before'
lbk_dat[which(lbk_dat$set_year >= 2011), 'when'] <- 'after'


#add before after indicators
spp_when <- lbk_dat %>% distinct(species, when, type)
spp_when <- spp_when %>% filter(when != 'baseline')
spp_when <- spp_when %>% arrange(species, when)

spp_when <- spp_when %>% group_by(species) %>% mutate(bef_aft = length(unique(when)))
spp_when <- spp_when %>% filter(bef_aft == 2)
spp_when <- spp_when %>% as.data.frame
spp_when <- spp_when[which(is.na(spp_when$type) == FALSE), ]

#--------------------------------------------------------------------
#Calculate delta plots for logbook data

spp_when$set_year <- 2007
spp_when[which(spp_when$when == 'after'), 'set_year'] <- 2011

#Test that this works
mm <- 2
outs <- when_spp_delta(period = spp_when[mm, "when"], 
  spp = spp_when[mm, 'species'])

#Now run across
start_time <- Sys.time()
delts_befaft <- mclapply(1:nrow(spp_when), FUN = function(mm) {
  outs <- when_spp_delta(data_type = lbk_dat, period = spp_when[mm, "when"], 
    spp = spp_when[mm, 'species'])
  return(outs)
}, mc.cores = 6)
run_time <- Sys.time() - start_time; run_time
delts_befaft <- ldply(delts_befaft)

save(delts_befaft, file = 'output/lbk_delts_befaft.Rdata')



