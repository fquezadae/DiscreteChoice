
#----------------------------------------------------------------------
state_effort <- obs_data %>% group_by(set_year, d_state) %>% summarize(ntows = length(unique(haul_id))) %>%
  filter(set_year != 2001)

state_effort %>% group_by(set_year) %>% summarize(ntows = sum(ntows))

state_effort %>% ggplot(aes(x = set_year, y = ntows)) + geom_line(aes(group = d_state, colour = d_state)) +
  geom_point(aes(group = d_state, colour = d_state)) + 
  geom_vline(xintercept = 2010.5, lty = 2) + ggsave(width = 10, height = 6.5, 
                                                    file = 'figs/state_tows_effort.png')

obs_data %>% group_by(d_state, set_year, d_port) %>% 
  summarize(ntows = length(unique(haul_id))) %>% 
  ggplot(aes(x = set_year, ntows)) + geom_line(aes(x = set_year, y = ntows, group = d_port,
                                                   colour = d_port)) + 
  geom_vline(xintercept = 2010.5, lty = 2) + geom_point(aes(x = set_year, y = ntows, group = d_port,
                                                            colour = d_port)) +
  facet_wrap(~ d_state) + theme(legend.position = 'none') + ggsave(file = 'figs/state_tows.png', 
                                                                   width = 9.7, height = 6.5)


#----------------------------------------------------------------------
#Overall changes
#Number of vessels decreased
agg_effort %>% filter(set_year != 2001) %>% ggplot(aes(x = set_year, y = nvess)) + geom_line() + 
  geom_point() + geom_vline(xintercept = 2010.5, lty = 2) + ylim(limits = c(0, 200)) 

#Number of tows decreased  
agg_effort %>% filter(set_year != 2001) %>% ggplot(aes(x = set_year, y = ntows)) + geom_line() + 
  geom_point() + geom_vline(xintercept = 2010.5, lty = 2) + ylim(limits = c(0, 20000)) 

#Was the decrease signficiant
agg_effort1 <- agg_effort %>% filter(set_year >= 2007) %>% as.data.frame

###############################
#Plot of aggregate changes
par(oma = c(.5, .5, 0, 2))
plot(agg_effort1$set_year, agg_effort1$nvess, ylim = c(0, 120), pch = 19, type = 'b',
     xlab = "Year", ylab = "Number of vessels")
abline(v = 2010.5, lty = 2)
par(new = TRUE)
plot(agg_effort1$set_year, agg_effort1$ntows, ylim = c(0, 18000), pch = 19, type = 'b',
     col = 'gray', ann = F, axes = F)
axis(side = 4)
mtext(side = 4, "Number of tows", line = 2)
legend("bottomleft", c('Vessel', 'Tows'), pch = 19, col = c('black', 'gray'), bty = 'n')

###############################
#p_value check
#Vessels     
agg_effort1$when <- c(rep('before', 4), rep('after', 4))
agg_vess <- simple_permute(agg_effort1, perm_column = 'nvess', index = c(2007, 2010))

bef <- mean(agg_vess[[1]][1:4, 'nvess'])
aft <- mean(agg_vess[[1]][5:8, 'nvess'])
dd <- aft - bef
length(which(agg_vess[[2]] > dd))

hist(agg_vess[[2]], breaks = 30)
abline(v = aft-bef, col = 'red', lwd = 2)

#Tows
agg_tows <- simple_permute(agg_effort1, perm_column = 'ntows', index = c(2007, 2010))
bef <- mean(agg_tows[[1]][1:4, 'ntows'])
aft <- mean(agg_tows[[1]][5:8, 'ntows'])
dd <- aft- bef
length(which(agg_tows[[2]] > dd))

hist(agg_tows[[2]], breaks = 30)
abline(v = aft-bef, col = 'red', lwd = 2)

#----------------------------------------------------------------------
#Number of tows per trip
tows_per_trip <- obs_data %>% group_by(set_year, trip_id) %>% summarize(nhauls = length(unique(haul_id))) %>%
  group_by(set_year) %>% summarize(avg_nhauls = mean(nhauls)) %>% as.data.frame

tows_per_trip %>% filter(set_year >= 2007) %>% ggplot(aes(x = set_year, y = avg_nhauls)) + 
  geom_point() + geom_line() + ylim(limits = c(0, 9)) + geom_vline(xintercept = 2010.5, lty = 2)

tt <- simple_permute(tows_per_trip, perm_column = 'avg_nhauls', index = c(2007, 2010))

#Trip Duration
trip_time <- obs_data %>% group_by(set_year) %>% summarize(avg_duration = mean(trip_duration)) %>% 
  as.data.frame

d_date <- paste(obs_data$dday, obs_data$dmonth, obs_data$dyear, sep = "-") 
d_date <- dmy(d_date)

r_date <- paste(obs_data$rday, obs_data$rmonth, obs_data$ryear, sep = "-") 
r_date <- dmy(r_date)

dates <- data.frame(d_date = d_date, r_date = r_date)
dates$int <- dates$r_date - dates$d_date
dates$trip_duration <- as.numeric(dates$int)

obs_data$t_duration <- dates$trip_duration

obs_data %>% distinct(trip_id, .keep_all = T) %>% group_by(set_year) %>% 
  summarize(trip_duration = mean(t_duration)) %>% filter(set_year >= 2007)


#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------

obs_data <- arrange_tows(obs_data)
obs_data$trans_avg_long <-(obs_data$trans_set_long + obs_data$trans_up_long) / 2


#--------------------------------------------------------------------------------------------------------

#Bin based on lat and long
ll_binned <- bin_data(data = obs_data %>% distinct(haul_id, .keep_all = T), 
                      x_col = "avg_long", y_col = "avg_lat", 
                      grid_size = c(.0909, .11), group = "set_year")
ll_binned <- ll_binned %>% filter(year >= 2007)


#Binning works as it should
obs_data %>% filter(set_year >= 2007) %>% group_by(set_year) %>% summarize(ntows = length(unique(haul_id)))


ggplot() + geom_tile(data = ll_binned, aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + labs(fill = "# tows") + facet_wrap(~ year, ncol = 4) +
  geom_map(data = states_map, map = states_map, 
         aes(x = long, y = lat, map_id = region)) + scale_x_continuous(limits = c(-126, -118)) + 
  scale_y_continuous(limits = c(30, 49))

#Calculate before and
ll_binned$when <- "after"
ll_binned[which(ll_binned$year < 2011), 'when'] <- 'before'

ll_binned <- ll_binned %>% group_by(unq, when) %>% mutate(avg_count = mean(count)) %>% as.data.frame

ll_binned_diffs <- ll_binned %>% distinct(x, y, unq, when, avg_count, .keep_all = TRUE) %>% 
  dcast(x + y + unq ~ when, value.var = 'avg_count') 
ll_binned_diffs$diff <- ll_binned_diffs$after - ll_binned_diffs$before

ggplot() + geom_tile(data = ll_binned_diffs, aes(x = x, y = y, fill = diff)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + labs(fill = "# tows")  +
  geom_map(data = states_map, map = states_map, 
           aes(x = long, y = lat, map_id = region)) + scale_x_continuous(limits = c(-126, -118)) + 
  scale_y_continuous(limits = c(30, 49))

ll_binned %>% filter(unq == c("11 85")) %>% simple_permute(input = ., perm_column = 'count',
                                                          nperms = 1000, column = 'year',

                                                          
                                                                                                                    index = c(2007, 2010)) -> thing
unqs <- unique(ll_binned$unq)

ll_binned %>% filter(unq == unqs[10])

ll_binned_perms <- vector('list', length = 1039)

#Write this in parallel
nncores <- 6

clusters <- parallel::makeCluster(nncores)
doParallel::registerDoParallel(clusters)

#Only run this with sites that have values before and after
unqs <- ll_binned %>% group_by(unq) %>% summarize(bef_aft = length(unique(when))) %>% filter(bef_aft == 2) %>%
          select(unq)
unqs <- unqs$unq

to_run <- 1:length(unqs)

start_time <- Sys.time()

#Run the thing in parallel
ll_binned_perms <- foreach(ii = to_run,
                           .packages = c('plyr', 'dplyr', 'reshape2', 'ch4'),
                           .export = c('ll_binned', 'unqs')) %dopar% {
                          par_simple_permute(ind = ii, perm_column = 'count',
                                             nperms = 1000, column = 'year', index = c(2007, 2010))}

stopCluster(clusters)

#Record run time
run_time <- Sys.time() - start_time

ll_binned_perms1 <- lapply(ll_binned_perms, FUN = function(x) x[[1]])
ll_binned_perms1 <- ldply(ll_binned_perms1)

save(ll_binned_perms, file = 'output/ll_binned_perms.Rdata')

ll_binned_perms1 %>% distinct(unq, .keep_all = TRUE) %>% group_by(sig_count) %>%
  summarize(nn = n())

ll_binned_perms1 %>% filter(sig_count == 'significant increase') 
ll_binned %>% filter(unq == '14 123')

##Combine with ll_binned
ll_binned$sig_count <- NULL
ll_binned$p_val_count <- NULL


ll_binned_comb <- ll_binned %>% 
  left_join(ll_binned_perms1 %>% select(unq, year, p_val_count, sig_count), by = c('unq', 'year'))

ll_binned_comb[which(is.na(ll_binned_comb$sig_count)), 'sig_count'] <- "not enough years"

#Give values that were not significant the same names
ll_binned_comb[grep('not significant', ll_binned_comb$sig_count), 'sig_count'] <- 'not significant'
ll_binned_comb[which(ll_binned_comb$sig_count == 'significant'), 'sig_count'] <- 'not significant'
    
unique(ll_binned_comb$sig_count)
ll_binned <- ll_binned_comb

#Function to permute speicific sites    
par_simple_permute <- function(ind, perm_column = 'count', nperms = 1000,
                               column = 'year', index = c(2007, 2010)){

  temp <- ll_binned %>% filter(unq == unqs[ind])
    
  outs <- simple_permute(input = temp, perm_column = perm_column, nperms = nperms,
                             column = column, index = index)
}

#----------------------------------------------------------------------
#Map of all the changes
ll_binned <- ll_binned %>% group_by(when, unq) %>% mutate(when_avg_count = mean(count)) %>% as.data.frame

ll_binned %>% distinct(unq, when, .keep_all = TRUE) %>% dcast(unq ~ when, value.var = "when_avg_count") %>%
  mutate(diff_avg_count = after - before) %>% select(unq, diff_avg_count) %>% 
  right_join(ll_binned, by = 'unq') -> ll_binned
  

 
ggplot() + geom_tile(data = ll_binned, aes(x = x, y = y, fill = diff_avg_count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + labs(fill = "# tows")  +
  geom_map(data = states_map, map = states_map, 
           aes(x = long, y = lat, map_id = region)) + scale_x_continuous(limits = c(-126, -120)) + 
  scale_y_continuous(limits = c(33, 49)) + facet_wrap(~ sig_count) + theme_bw() + 
  ggsave(file = 'figs/sig_map.png', width = 6.5, height = 10)

#Table of significnace
sig_table <- ll_binned %>% distinct(unq, .keep_all = TRUE) %>% group_by(sig_count) %>% summarize(nn = n())
sig_table$prop <- round((sig_table$nn / sum(sig_table$nn)) * 100, digits = 0)


#--------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------
#Bin by latitude and depth

hist(obs_data$avg_depth)
quantile(obs_data$avg_depth)
obs_data %>% group_by(set_year) %>% summarize(max_depth = max(avg_depth))

ld_binned <- bin_data(data = obs_data, x_col = "avg_depth", y_col = "avg_lat", grid_size = c(25, .11), 
                      group = "set_year")

ld_binned <- ld_binned %>% filter(year >= 2007)

#Binning works as it should
obs_data %>% filter(set_year >= 2007) %>% group_by(set_year) %>% summarize(ntows = length(unique(haul_id)))

ggplot() + geom_tile(data = ld_binned, aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + labs(fill = "# tows") + facet_wrap(~ year, ncol = 4) +
  scale_x_reverse()


#Calculate before and
ld_binned$when <- "after"
ld_binned[which(ld_binned$year < 2011), 'when'] <- 'before'

ld_binned <- ld_binned %>% group_by(unq, when) %>% mutate(avg_count = mean(count)) %>% as.data.frame

ld_binned_diffs <- ld_binned %>% distinct(x, y, unq, when, avg_count, .keep_all = TRUE) %>% 
  dcast(x + y + unq ~ when, value.var = 'avg_count') 
ld_binned_diffs$diff <- ld_binned_diffs$after - ld_binned_diffs$before

ggplot() + geom_tile(data = ld_binned_diffs, aes(x = x, y = y, fill = diff)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + labs(fill = "# tows")  
# geom_map(data = states_map, map = states_map, 
#          aes(x = long, y = lat, map_id = region)) + scale_x_continuous(limits = c(-126, -118)) + 
# scale_y_continuous(limits = c(30, 49))


unqs <- unique(ld_binned$unq)
ld_binned_perms <- vector('list', length = length(unqs))

#Write this in parallel
nncores <- 6

clusters <- parallel::makeCluster(nncores)
doParallel::registerDoParallel(clusters)

#Only run this with sites that have values before and after
unqs <- ld_binned %>% group_by(unq) %>% summarize(bef_aft = length(unique(when))) %>% filter(bef_aft == 2) %>%
  select(unq)
unqs <- unqs$unq

to_run <- 1:length(unqs)

start_time <- Sys.time()

#Run the thing in parallel
ld_binned_perms <- foreach(ii = to_run,
                           .packages = c('plyr', 'dplyr', 'reshape2', 'ch4'),
                           .export = c('ld_binned', 'unqs')) %dopar% {
                             par_simple_permute(ind = ii, perm_column = 'count',
                                                nperms = 1000, column = 'year', index = c(2007, 2010))}

stopCluster(clusters)

#Record run time
run_time <- Sys.time() - start_time

ld_binned_perms1 <- lapply(ld_binned_perms, FUN = function(x) x[[1]])
ld_binned_perms1 <- ldply(ld_binned_perms1)

# save(ld_binned_perms, file = 'output/ld_binned_perms.Rdata')

ld_binned_perms1 %>% distinct(unq, .keep_all = TRUE) %>% group_by(sig_count) %>%
  summarize(nn = n())

ld_binned_perms1 %>% filter(sig_count == 'significant increase') 
ld_binned %>% filter(unq == '14 123')

##Combine with ld_binned
ld_binned$sig_count <- NULL
ld_binned$p_val_count <- NULL


ld_binned_comb <- ld_binned %>% 
  left_join(ld_binned_perms1 %>% select(unq, year, p_val_count, sig_count), by = c('unq', 'year'))

ld_binned_comb[which(is.na(ld_binned_comb$sig_count)), 'sig_count'] <- "not enough years"

#Give values that were not significant the same names
ld_binned_comb[grep('not significant', ld_binned_comb$sig_count), 'sig_count'] <- 'not significant'
ld_binned_comb[which(ld_binned_comb$sig_count == 'significant'), 'sig_count'] <- 'not significant'

unique(ld_binned_comb$sig_count)
ld_binned <- ld_binned_comb

#Function to permute speicific sites    
par_simple_permute <- function(ind, perm_column = 'count', nperms = 1000,
                               column = 'year', index = c(2007, 2010)){
  
  temp <- ld_binned %>% filter(unq == unqs[ind])
  
  outs <- simple_permute(input = temp, perm_column = perm_column, nperms = nperms,
                         column = column, index = index)
}

#----------------------------------------------------------------------
#Map of all the changes
ld_binned <- ld_binned %>% group_by(when, unq) %>% mutate(when_avg_count = mean(count)) %>% as.data.frame

ld_binned %>% distinct(unq, when, .keep_all = TRUE) %>% dcast(unq ~ when, value.var = "when_avg_count") %>%
  mutate(diff_avg_count = after - before) %>% select(unq, diff_avg_count) %>% 
  right_join(ld_binned, by = 'unq') -> ld_binned



ggplot() + geom_tile(data = ld_binned, aes(x = x, y = y, fill = diff_avg_count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + labs(fill = "# tows")  +
  geom_map(data = states_map, map = states_map, 
           aes(x = long, y = lat, map_id = region)) + scale_x_continuous(limits = c(-126, -120)) + 
  scale_y_continuous(limits = c(33, 49)) + facet_wrap(~ sig_count) + theme_bw() + 
  ggsave(file = 'figs/sig_map.png', width = 6.5, height = 10)

#Table of significnace
sig_table <- ld_binned %>% distinct(unq, .keep_all = TRUE) %>% group_by(sig_count) %>% summarize(nn = n())
sig_table$prop <- round((sig_table$nn / sum(sig_table$nn)) * 100, digits = 0)


# #Time per tow
# time_tow <- obs_data %>% group_by(set_year) %>% summarize(haul_time = mean(haul_duration )) %>% as.data.frame
# time_tow %>% filter(set_year >= 2007) %>% ggplot(aes(x = set_year, y = haul_time)) + 
#   geom_point() + geom_line() + ylim(limits = c(0, 5)) + geom_vline(xintercept = 2010.5, lty = 2)
#  
# tt_perm <- simple_permute(time_tow, perm_column = 'haul_time', index = c(2007, 2010))






 
# #Tow duration
# tows 
#  
#  
#  obs_data %>% distinct(haul_id, .keep_all = T) %>% group_by(set_year) %>%  
#   summarize(duration = sum(haul_duration), ntows = length(unique(haul_id)), duration_per_tow = duration / ntows) %>%
#   ggplot() + geom_point(aes(x = set_year, y = duration_per_tow)) + geom_line(aes(x = set_year, y = duration_per_tow)) +
#   ylim(limits = c(0, 5)) + geom_vline(xintercept = 2010.5, lty = 2)
# 
# #Look at effort by port groupsamps
# obs_data %>% group_by(r_port_group_desc) %>% summarize(ntows = length(unique(haul_id))) %>% arrange(desc(ntows))
# 
# obs_data %>% group_by(r_port_group_desc, set_year, r_state)%>% filter(set_year >= 2007) %>% 
#   summarize(ntows = length(unique(haul_id))) %>% 
#   ggplot(aes(x = set_year, y = ntows)) + facet_wrap(~ r_state) + geom_line(aes(colour = r_port_group_desc)) +
#   geom_vline(xintercept = 2010.5, lty = 2)
