#------------------------------------------------------------------------------------------------------------
#Permutation test with the species information to see if changes in prop_zero and skew sere
#significant before/after catch shares

load(file = 'output/delts_befaft.Rdata')

#For each haul, resample the years; calculate the skews for specific species
#For now keep only targets, weaks, and groundfish. more tractable

#Focus species
managed <- tows_clust %>% filter(type != 'other')

#sample haul years...?

#Assign a different year to the hauuls
#calculate the deltas and prop_zero for each species
managed <- managed %>% select(set_year, apounds, species, type, haul_id)

tt <- managed %>% distinct(haul_id, .keep_all = TRUE) %>% select(haul_id, set_year)


set.seed(300)

#--------------------------------------------------------------------
#Resample for some number of iterations
start_time <- Sys.time()
samp_delts <- mclapply(1:1000,  FUN = function(xx){
  set.seed(xx)
  rowz <- sample(1:nrow(tt), replace = F)
  
  #Format the resampled data
  tt1 <- tt
  tt1$samp_year <- tt1[rowz, 'set_year']
  managed1 <- managed %>% left_join(tt1 %>% select(haul_id, samp_year), by = 'haul_id')
  managed1$when <- "before"
  managed1[which(managed1$samp_year >= 2011), 'when'] <- 'after'
  
  #Calculate the proportion zeroes and skew for all species in the data frame
  outs <- lapply(unique(managed1$species), FUN = function(ss){
    spp_delts(ss, managed2 = managed1)
  })
  outs <- ldply(outs)

  return(outs)
}, mc.cores = 6)
run_time <- Sys.time() - start_time; run_time

names(samp_delts) <- 1:length(samp_delts)
samp_delts1 <- ldply(samp_delts)

#Pull skew values and cast
skews <- samp_delts1 %>% dcast(species + .id + type ~ when, 
  value.var = c("skew"))
skews$diffs <- skews$after - skews$before

#Pull proportions and cast
props <- samp_delts1 %>% dcast(species + .id + type ~ when, value.var = "prop_zero")
props$diffs <- props$after - props$before

save(skews, file = 'output/perm_skews.Rdata')
save(props, file = 'output/perm_props.Rdata')


#--------------------------------------------------------------------
#Look at significant changes in prop zero and skew
head(delts_befaft)
load('output/perm_skews.Rdata')
skews$species <- as.character(skews$species)

#Process the skews
emp_skew <- delts_befaft %>% filter(type != 'other') %>% 
 dcast(species + type + plot_type ~ when, value.var = 'skew') %>% 
 mutate(diffs = after - before)
emp_skew$species <- as.character(emp_skew$species)

#Look at the p values
sppz <- unique(emp_skew$species)
emp_skew$pval <- 999

for(ss in 1:length(sppz)){
  emp <- subset(emp_skew, species == sppz[ss] )
  samp <- subset(skews, species == sppz[ss])
  
  p_val <- sum(emp$diffs >= samp$diffs)
  emp_skew[ss, 'pval'] <- p_val / 1000
  
}

#Process the proportion of zeroes
load('output/perm_props.Rdata')
emp_props <- delts_befaft %>% filter(type != 'other') %>% 
 dcast(species + type + plot_type ~ when, value.var = 'prop_zero') %>% 
 mutate(diffs = after - before)
emp_props$species <- as.character(emp_props$species)

#Look at the p values
sppz <- unique(emp_props$species)
emp_props$pval <- 999

for(ss in 1:length(sppz)){
  emp <- subset(emp_props, species == sppz[ss] )
  samp <- subset(props, species == sppz[ss])
  
  p_val <- sum(emp$diffs >= samp$diffs)
  emp_props[ss, 'pval'] <- p_val / 1000
}

#Add significance columns to the two data frames
emp_props$prop_sig <- "yes" #all significant decreases in proportion zeroes
emp_skew$skew_sig <- "yes"
emp_skew[which(emp_skew$pval != 1 & emp_skew$pval != 0), 'skew_sig'] <- 'no'


sigs <- emp_skew %>% select(species, skew_sig) %>% 
  left_join(emp_props %>% select(species, prop_sig), by = 'species')
delta_sigs <- sigs
save(delta_sigs, file =  'output/delta_sigs.Rdata')

#
#--------------------------------------------------------------------

samp_delts1 %>% filter(species == "Arrowtooth Flounder") %>% ggplot()

names(samp_delts)

#Write function for managed1 for one species
# sppz <- "Arrowtooth Flounder"
# spp_delts("Arrowtooth Flounder")

spp_delts <- function(sppz, managed2){
  #Take unique hauls in each period
  bef_hauls <- managed2 %>% filter(when == 'before') %>% select(haul_id)
  aft_hauls <- managed2 %>% filter(when == 'after') %>% select(haul_id)
  spp_hauls <- managed2 %>% filter(species == sppz) %>% select(species, haul_id, apounds, type)
  
  bef_hauls <- bef_hauls %>% left_join(spp_hauls, fill = 0, by = 'haul_id')
  aft_hauls <- aft_hauls %>% left_join(spp_hauls, fill = 0, by = 'haul_id')
  
  bef_propzero <- sum(is.na(bef_hauls$apounds)) / nrow(bef_hauls)
  aft_propzero <- sum(is.na(aft_hauls$apounds)) / nrow(aft_hauls)
  
  bef_hauls <- bef_hauls %>% filter(is.numeric(apounds), apounds != 0) %>% mutate(log_apounds = log(apounds))
  bef_skew <- calc_skew(bef_hauls$log_apounds)
  
  aft_hauls <- aft_hauls %>% filter(is.numeric(apounds), apounds != 0) %>% mutate(log_apounds = log(apounds))
  aft_skew <- calc_skew(aft_hauls$log_apounds)
  
  spp_delts <- data.frame(prop_zero = c(bef_propzero, aft_propzero), skew = c(bef_skew, aft_skew), 
    when = c('before', 'after'), species = sppz, type = unique(spp_hauls$type))
  return(spp_delts)
}










# bef_hauls <- bef_hauls %>% filter(is.numeric(apounds)) %>% mutate(log_apounds = log(apounds))

bef_apounds <- bef_hauls$log_apounds
bef_apounds <- bef_apounds[which(is.na(bef_apounds) == FALSE)]

bef_apounds[order(bef_apounds)] %>% head



bef_apounds %>% filter(i)
calc_skew(bef_apounds$log_apounds)


unique(bef_hauls$log_apounds)


calc_skew(log(bef_hauls[which(is.na(bef_hauls$apounds) == FALSE), 'apounds']))

prop_zero <- 


rowz <- sample(1:nrow(tt), replace = F)



temp <- managed
temp$set_year



# spp_when$set_year <- 2007
# spp_when[which(spp_when$when == 'after'), 'set_year'] <- 2011
# mm <- 2
# outs <- when_spp_delta(period = spp_when[mm, "when"], 
#   spp = spp_when[mm, 'species'])

# start_time <- Sys.time()
# delts_befaft <- mclapply(1:nrow(spp_when), FUN = function(mm) {
#   outs <- when_spp_delta(period = spp_when[mm, "when"], 
#     spp = spp_when[mm, 'species'])
#   return(outs)
# }, mc.cores = 6)
# run_time <- Sys.time() - start_time; run_time




