#-------------------------------------------------------------------------------------
#Additional analyses based on reveiwer comments
#-------------------------------------------------------------------------------------

#Figure 2, look at changes by depth and latitude
tc_unq_hauls <- tows_clust %>% distinct(haul_id, .keep_all = T)

tows_clust_bin_depth <- bin_data(tc_unq_hauls, x_col = 'avg_depth', y_col = 'avg_lat', 
  group = 'set_year', 
  grid_size = c(50, .5),
  group_vec = 2007:2014)
tows_clust_bin_depth$when <- 'before'
tows_clust_bin_depth[which(tows_clust_bin_depth$year >= 2011), 'when'] <- 'after'

clust_dims <- tows_clust_bin_depth %>% distinct(x, y, xmin, xmax, ymin, ymax, 
  xbin, ybin, unq) 
tc_unq_hauls$lat_bin <- 999
tc_unq_hauls$depth_bin <- 999
tc_unq_hauls$unq <- 999

#Need to filter for boats that fished before and after catch shares
#Species diversity in each bin 
for(ii in 1:nrow(clust_dims)){
  temp <- clust_dims[ii, ]
  
  the_inds <- which(tc_unq_hauls$avg_set_depth < temp$xmax &  
    tc_unq_hauls$avg_set_depth > temp$xmin & 
    tc_unq_hauls$avg_lat > temp$ymin &tc_unq_hauls$avg_lat < temp$ymax)
  
  tc_unq_hauls[the_inds, 'lat_bin'] <- temp$y
  tc_unq_hauls[the_inds, 'depth_bin'] <- temp$x  
  tc_unq_hauls[the_inds, 'unq'] <- temp$unq 
}
tc_unq_hauls$lat_bin <- round(tc_unq_hauls$lat_bin, digits = 2)
tc_unq_hauls$depth_bin <- round(tc_unq_hauls$depth_bin, digits = 2)

#-------------------------------------------------------------------------------------
#Check that all the values line up
tc_unq_hauls %>% group_by(lat_bin, depth_bin, set_year) %>% 
  summarize(nvals = length(avg_set_depth)) %>% arrange(desc(nvals)) %>% head

tows_clust_bin_depth %>% arrange(desc(count)) %>% head

#Looks good

#-------------------------------------------------------------------------------------
#Look at before and after diversity indices
#Compare the differences to those from a null distribution
tc_unq_hauls$when <- 'before'
tc_unq_hauls[which(tc_unq_hauls$set_year >= 2011), 'when'] <- 'after'


shan_bc <- tc_unq_hauls %>% group_by(lat_bin, depth_bin, when, drvid) %>%
  summarize(ntows = length(haul_id)) %>% group_by(lat_bin, depth_bin, when) %>%
  mutate(tot_tows = sum(ntows), ptows = ntows / tot_tows,
    plog = log(ptows), plpi = ptows * plog, shan = -sum(plpi)) %>%
  distinct(lat_bin, depth_bin, when, shan) %>% dcast(lat_bin + depth_bin ~ when,
  value.var = 'shan') %>% mutate(delta_shan = after - before)

#Shuffle years and calculate the shannon indices
set.seed(300)

nreps <- 1000
start_time <- Sys.time()
null_dists <- mclapply(1:nreps, FUN = function(nn){
      temp <- tc_unq_hauls
      temp$samp_years <- base::sample(temp$set_year, replace = F)
      temp$when <- 'before'
      temp[which(temp$samp_years >= 2011), 'when'] <- 'after'
    
      resamps <- temp %>% group_by(lat_bin, depth_bin, when, drvid) %>%
        summarize(ntows = length(haul_id)) %>% group_by(lat_bin, depth_bin, when) %>%
        mutate(tot_tows = sum(ntows), ptows = ntows / tot_tows,
          plog = log(ptows), plpi = ptows * plog, shan = -sum(plpi)) %>%
        distinct(lat_bin, depth_bin, when, shan) %>% dcast(lat_bin + depth_bin ~ when,
        value.var = 'shan') %>% mutate(delta_shan = after - before)
      return(resamps)
    }, mc.cores = 6)

names(null_dists) <- 1:nreps
null_dists <- ldply(null_dists)

run_time <- Sys.time() - start_time; run_time #600 runs in 39 seconds; 1000 runs in 58 seconds


hist(null_dists$delta_shan)


#Compare the shannon_bc values to the null distribution, find the number of values less than 
shan_bc$greater_than <- 999

for(ii in 1:nrow(shan_bc)){
  value <- shan_bc[ii, 'delta_shan']
  one_row <- null_dists %>% filter(lat_bin == shan_bc$lat_bin[ii], depth_bin == shan_bc$depth_bin[ii])
  shan_bc$greater_than[ii] <- sum(value >= one_row$delta_shan) / 1000
}

sigs <- shan_bc %>% filter(is.na(greater_than) == F & greater_than >= .95 | greater_than <= .05)

sigs %>% filter(delta_shan > 0) %>% nrow / nrow(shan_bc) #4% of locations had increases
sigs %>% filter(delta_shan < 0) %>% nrow / nrow(shan_bc) #50% of locations had decreases

shan_bc$sig <- FALSE
shan_bc[which(shan_bc$greater_than >= .95 | shan_bc$greater_than <= .05), 'sig'] <- TRUE
#add transparency
shan_bc$ffill <- 0
shan_bc[which(shan_bc$sig), 'ffill'] <- 1
#Add colors
shan_bc$inc_dec <- "decrease"
shan_bc[which(shan_bc$delta_shan >= 0), 'inc_dec'] <- 'increase'


shan_bc %>% ggplot(aes(x = -depth_bin, y = lat_bin)) + 
  geom_point(aes(alpha = ffill, fill = inc_dec), pch = 21) + facet_wrap(~ )


#-------------------------------------------------------------------------------------
#Calculate moran's I for the years before and after catch shares
# https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
library(ape)
tows_ll <- bin_data(tc_unq_hauls, x_col = 'avg_long', y_col = 'avg_lat', 
                                 group = 'set_year', 
                                 grid_size = c(.5, .5),
                                 group_vec = 2007:2014)
tows_ll$when <- 'before'
tows_ll[which(tows_ll$year >= 2011), 'when'] <- 'after'


bef_aft <- tows_ll %>% group_by(when, x, y, unq, xbin, ybin) %>%
  summarize(count = sum(count))

#Sum by periods
befs <- bef_aft %>% filter(when == 'before')

grid_dists <- as.matrix(dist(cbind(befs$x, befs$y)))
grid_dist_inv <- 1 / grid_dists
diag(grid_dist_inv) <- 0

emp_bef <- Moran.I(befs$count, grid_dist_inv)

#Do the same with values after
afts <- bef_aft %>% filter(when == 'after')
grid_dists <- as.matrix(dist(cbind(afts$x, afts$y)))
grid_dist_inv <- 1 / grid_dists
diag(grid_dist_inv) <- 0

emp_aft <- Moran.I(afts$count, grid_dist_inv)
emp_aft$observed - emp_bef$observed


#-------------------------------------------------------------------------------------
#Run Moran's I with sampled years
set.seed(300)
surr_years <- make_surrogate_data(tc_unq_hauls$set_year, n = 1000)
start_time <- Sys.time()
surr_years_moran <- lapply(1:1000, FUN = function(ii){
  temp <- tc_unq_hauls
  temp$set_year <- surr_years[, ii]
  tows_ll <- bin_data(temp, x_col = 'avg_long', y_col = 'avg_lat', 
                      group = 'set_year', 
                      grid_size = c(.5, .5),
                      group_vec = 2007:2014)
  tows_ll$when <- 'before'
  tows_ll[which(tows_ll$year >= 2011), 'when'] <- 'after'

  #Calculate moran's I things
  bef_aft <- tows_ll %>% group_by(when, x, y, unq, xbin, ybin) %>%
    summarize(count = sum(count))
  
  #Sum by periods
  befs <- bef_aft %>% filter(when == 'before')
  
  
  bef_grid_dists <- as.matrix(dist(cbind(befs$x, befs$y)))
  bef_grid_dist_inv <- 1 / bef_grid_dists
  diag(bef_grid_dist_inv) <- 0
  
  bef_m <- Moran.I(befs$count, bef_grid_dist_inv)
  
  #Do the same with values after
  afts <- bef_aft %>% filter(when == 'after')
  aft_grid_dists <- as.matrix(dist(cbind(afts$x, afts$y)))
  aft_grid_dist_inv <- 1 / aft_grid_dists
  diag(aft_grid_dist_inv) <- 0
  
  aft_m <- Moran.I(afts$count, aft_grid_dist_inv)
  diff_m <- aft_m$observed - bef_m$observed
  return(diff_m)
})
run_time <- Sys.time() - start_time; run_time

surr_years_moran <- (ldply(surr_years_moran))
save(surr_years_moran, file = 'output/moran_surrogates.Rdata')


load("output/moran_surrogates.Rdata")

emp_moran <- emp_aft$observed - emp_bef$observed
length(which(emp_moran >= surr_years_moran)) / 1000
#Statistically significant decrease in spatial autocorrelation, 
#per Moran's I, confounded potentially with the overall decrease in effort


#-------------------------------------------------------------------------------------
#For the vessels that remained, was there statistically significant change in spatial
#autocorrelation
vess <- tc_unq_hauls %>% group_by(drvid) %>% summarize(tot_years = length(unique(set_year)),
                                               aft_years = length(unique(set_year >= 2011))) %>%
  filter(aft_years >= 1 & tot_years > 4) %>% pull(drvid)

tc_unq_hauls1 <- tc_unq_hauls %>% filter(drvid %in% vess)                                                 
tows_ll_vess <- bin_data(tc_unq_hauls1, x_col = 'avg_long', y_col = 'avg_lat', 
                    group = 'set_year', 
                    grid_size = c(.5, .5),
                    group_vec = 2007:2014)
tows_ll_vess$when <- 'before'
tows_ll_vess[which(tows_ll_vess$year >= 2011), 'when'] <- 'after'


bef_aft_vess <- tows_ll_vess %>% group_by(when, x, y, unq, xbin, ybin) %>%
  summarize(count = sum(count))

#Sum by periods
befs_vess <- bef_aft_vess %>% filter(when == 'before')

grid_dists_vess <- as.matrix(dist(cbind(befs_vess$x, befs_vess$y)))
grid_dist_inv_vess <- 1 / grid_dists_vess
diag(grid_dist_inv_vess) <- 0

emp_bef_vess <- Moran.I(befs_vess$count, grid_dist_inv_vess)

#Do the same with values after
afts_vess <- bef_aft_vess %>% filter(when == 'after')
grid_dists_vess <- as.matrix(dist(cbind(afts_vess$x, afts_vess$y)))
grid_dist_inv_vess <- 1 / grid_dists_vess
diag(grid_dist_inv_vess) <- 0

emp_aft_vess <- Moran.I(afts_vess$count, grid_dist_inv_vess)
vess_emp <- emp_aft_vess$observed - emp_bef_vess$observed

                                                 
set.seed(300)
surr_years <- make_surrogate_data(tc_unq_hauls1$set_year, n = 1000)

start_time <- Sys.time()
surr_years_moran <- lapply(1:1000, FUN = function(ii){
  temp <- tc_unq_hauls1
  temp$set_year <- surr_years[, ii]
  tows_ll <- bin_data(temp, x_col = 'avg_long', y_col = 'avg_lat', 
                      group = 'set_year', 
                      grid_size = c(.5, .5),
                      group_vec = 2007:2014)
  tows_ll$when <- 'before'
  tows_ll[which(tows_ll$year >= 2011), 'when'] <- 'after'
  
  #Calculate moran's I things
  bef_aft <- tows_ll %>% group_by(when, x, y, unq, xbin, ybin) %>%
    summarize(count = sum(count))
  
  #Sum by periods
  befs <- bef_aft %>% filter(when == 'before')
  
  
  bef_grid_dists <- as.matrix(dist(cbind(befs$x, befs$y)))
  bef_grid_dist_inv <- 1 / bef_grid_dists
  diag(bef_grid_dist_inv) <- 0
  
  bef_m <- Moran.I(befs$count, bef_grid_dist_inv)
  
  #Do the same with values after
  afts <- bef_aft %>% filter(when == 'after')
  aft_grid_dists <- as.matrix(dist(cbind(afts$x, afts$y)))
  aft_grid_dist_inv <- 1 / aft_grid_dists
  diag(aft_grid_dist_inv) <- 0
  
  aft_m <- Moran.I(afts$count, aft_grid_dist_inv)
  diff_m <- aft_m$observed - bef_m$observed
  return(diff_m)
})
run_time <- Sys.time() - start_time; run_time #runs in about 3 minutes

surr_years_moran_vess <- (ldply(surr_years_moran))
save(surr_years_moran_vess, file = 'output/moran_surrogates_vess.Rdata')

load('output/moran_surrogates_vess.Rdata')
emp_moran_vess <- emp_aft_vess$observed - emp_bef_vess$observed
length(which(emp_moran_vess >= surr_years_moran_vess)) / 1000
#Insignificant increase in Moran's I


#-------------------------------------------------------------------------------------
#Run the same null distributions only with the vessels that stayed the whole time



#-------------------------------------------------------------------------------------
#Look at the diversity in each bin in each year
lat_bin == 46.3
depth_bin == 75

#Use shannon index instead
shan <- tc_unq_hauls %>% group_by(lat_bin, depth_bin, set_year, drvid) %>%
  summarize(ntows = length(haul_id)) %>% group_by(lat_bin, depth_bin, set_year) %>%
  mutate(tot_tows = sum(ntows), ptows = ntows / tot_tows,
    plog = log(ptows), plpi = ptows * plog, shan = -sum(plpi)) %>%
  distinct(lat_bin, depth_bin, set_year, shan)

ggplot(shan, aes(x = depth_bin, y = lat_bin)) + geom_point(aes(size = shan), alpha = .5) + 
  facet_wrap(~ set_year)

#Look at vessels that remained before and after
remain_vess <- tc_unq_hauls %>% group_by(drvid) %>% summarize(nyears = length(unique(set_year))) %>%
  filter(nyears >= 5) %>% pull(drvid)
shan_remain <- tc_unq_hauls %>% filter(drvid %in% remain_vess) %>% group_by(lat_bin, depth_bin, set_year, drvid) %>%
  summarize(ntows = length(haul_id)) %>% group_by(lat_bin, depth_bin, set_year) %>%
  mutate(tot_tows = sum(ntows), ptows = ntows / tot_tows,
    plog = log(ptows), plpi = ptows * plog, shan = -sum(plpi)) %>%
  distinct(lat_bin, depth_bin, set_year, shan)
ggplot(shan_remain, aes(x = depth_bin, y = lat_bin)) + geom_point(aes(size = shan), alpha = .5) + 
  facet_wrap(~ set_year)




  group_by(lat_bin, depth_bin, set_year) %>% summarize(plpi = plog * )



shan %>% filter()

tc_unq_hauls %>% filter(lat_bin == 45.75 & depth_bin == 75 & set_year == 2012) %>%
  group_by(drvid) %>% summarize(ntows = length(haul_id)) %>% mutate(N = sum(ntows),
    n_n1 = ntows * (ntows - 1), simpsons = sum(n_n1) / (N * (N - 1)))

s1 <- tc_unq_hauls %>% group_by(lat_bin, depth_bin, set_year, drvid) %>%
  summarize(ntows = length(haul_id)) %>% group_by(lat_bin, depth_bin, set_year, 
    drvid) %>% ungroup() %>% group_by(lat_bin, depth_bin, set_year) %>%
  summarize(N = sum(ntows))
dim(s1)
length(which(s1$N == 1)  )
hist(s1$N)

  %>%
  summarize(N = sum(ntows), n_n1 = ntows * (ntows - 1)) 
s1 %>% group_by(lat_bin, depth_bin, set_year) %>% 
  summarize(simpsons = sum(n_n1))



  %>%
  group_by(lat_bin, depth_bin, set_year) %>% summarize(simpsons = sum(n_n1) / 
    (N * (N - 1)))

    , simpsons = sum(n_n1) / (N * (N - 1))) %>% 
  distinct(lat_bin, depth_bin, set_year, simpsons)

  %>% group_by(lat_bin, depth_bin, set_year) %>%
   mutate(N = sum(ntows),
    n_n1 = ntows * (ntows - 1), simpsons = sum(n_n1) / (N * (N - 1))) %>% 
  distinct(lat_bin, depth_bin, set_year, simpsons)





tc_unq_hauls[which(tc_unq_hauls$avg_set_depth < temp$xmax &  
  tc_unq_hauls$avg_set_depth > temp$xmin & 
  tc_unq_hauls$avg_lat > temp$ymin &tc_unq_hauls$avg_lat < temp$ymax), ]
tc_unq_hauls 


#Add columns indicating the number of years and if vessel fished before and after catch shares
tows_clust_bin_depth <- tows_clust_bin_depth %>% group_by(unq) %>% 
  mutate(nyears = length(unique(year)), 
    bef_aft = if_else(length(unique(when)) == 2, TRUE, FALSE))  %>% as.data.frame

tows_clust_bin_depth <- tows_clust_bin_depth %>% arrange(year)

#Convert year to index
year_index <- data_frame(year = 2007:2014, xx = 1:8)
tows_clust_bin_depth <- tows_clust_bin_depth %>% left_join(year_index, by = 'year')



# #-------------------------------------------------------------------------------------
#Test Moran's I with a different metric
# standardize <- function(input){
#   output <- (input - mean(input, na.rm = T)) / sd(input, na.rm = T)
#   return(output)
# }

# tows_clust_bin_depth$xstd <- standardize(tows_clust_bin_depth$x)
# tows_clust_bin_depth$ystd <- standardize(tows_clust_bin_depth$y)


# bef_aft <- tows_clust_bin_depth %>% group_by(when, xstd, ystd, unq, xbin, ybin) %>%
#   summarize(count = sum(count))
# names(bef_aft)[2:3] <- c("x", "y")

# #Sum by periods
# befs <- bef_aft %>% ungroup %>% filter(when == 'before')

# grid_dists <- as.matrix(dist(cbind(befs$x, befs$y)))
# grid_dist_inv <- 1 / grid_dists
# diag(grid_dist_inv) <- 0

# emp_bef <- Moran.I(befs$count, grid_dist_inv)

# #Do the same with values after
# afts <- bef_aft %>% ungroup %>%filter(when == 'after')
# grid_dists <- as.matrix(dist(cbind(afts$x, afts$y)))
# grid_dist_inv <- 1 / grid_dists
# diag(grid_dist_inv) <- 0

# emp_aft <- Moran.I(afts$count, grid_dist_inv)
# emp_aft$observed - emp_bef$observed
