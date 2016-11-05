#Working script for Chapter 4
#Evaluating catch-quota balancing under different TAC configurations in a catch
#share fishery

setwd('/Users/peterkuriyama/School/Research/ch4')

library(devtools)
devtools::install_github('peterkuriyama/ch4')

#Make sure that packages are loaded
library(ch4)

library(plyr)
library(ggplot2)
library(dplyr)
library(reshape2)
#---------------------------------------------------------------------------------
#Start of Analysis
#Load the Tow Data
wc_data <- load_tow_data() #Data should have spatial ids with years and without years

#Data Processing initially
#Try sampling of data to stay within the TAC of multiple species
wc_data <- wc_data[-grep("\\.", wc_data$row_name), ]

#Keep only certain columns
tows <- wc_data %>% select(trip_id, haul_id, hpounds, apounds, species, rport_desc,
  agid, id_by_year, id_no_year)

#Set TACs for a particular species
tows$species <- tolower(tows$species)

#Classify Species
targets <- c('dover sole', 'sablefish', 'petrale sole',
  'longspine thornyhead', 'lingcod', 'shortspine thornyhead')
constraining <- c('yelloweye rockfish', 'cowcod rockfish',
  'bocaccio rockfish', 'canary rockfish', 'pacific ocean perch',
  'darkblotched rockfish')
tows$category <- 'other'
tows[tows$species %in% targets, 'category'] <- 'targets'
tows[tows$species %in% constraining, 'category'] <- 'constraining'

#Find all the tows that have a particular species 
#and look at their compositions
of_interest <- tows %>% filter(species == 'sablefish') %>% 
  arrange(desc(apounds))

########################################################################################
#Try it for only sablefish in WA
of_interest <- tows %>% filter(species == 'sablefish' & agid == 'O')

#look at the 
out <- calc_bycatch(nreps = 10, nsamps = 100, tac = 50000, of_interest = of_interest)
pairs <- spp_corrs(tows = tows, output = out, two_species = c('dover sole', 'longspine thornyhead'))
hist(pairs$corrs$r2, breaks = 50)

########################################################################################
#Look at locations with the most tows

#Sites with the most tows
wc_data %>% group_by(id_no_year) %>% summarize(ntows = length(unique(haul_id))) %>%
  arrange(desc(ntows))

of_interest <- tows %>% filter(id_no_year == 845 & species == 'sablefish') 
out <- calc_bycatch(nreps = 1, nsamps = nrow(of_interest), tac = 50000, of_interest = of_interest)
pairs <- spp_corrs(tows, output = out, two_species = c('dover sole', 'sablefish'))

plot(pairs$pairs$spp1, pairs$pairs$spp2, pch = 19)



########################################################################################
#To Do:
#Parallelize and see how high bycatch rates are in certain places



pairs_orig <- pairs
pairs1 <- pairs %>% filter(replicate == 1)

res <- lm(pairs1[, 4] ~ pairs1[, 3])


pairs1$fits <- fitted(res)

pairs1 <- pairs1[order(pairs1$fits), ]
plot(pairs1[, 3], pairs1[, 4], pch = 19)
lines(pairs1[, 3], pairs1$fits)

summary(res)$r.squared


fitted(res)[order(fitted(res))]

lines(res$fitted.values)
lines()

coef(res)
plot(res)
#Calcluate bycatch correlations?






#Plot the pair catch amounts 


plot(pairs[, 3], pairs[, 4], pch = 19, xlab = names(pairs)[3], ylab = names(pairs)[4],
  xaxs = 'i', yaxs = 'i', xpd = TRUE)



plot(pairs[, 3], pairs[, 4])
ggplot(pairs)




plot_pairs(tows = tows, output = out)


#Look at Interactions between species







#Plot histograms of bycatch
bycatch <- melt(out[[1]])
names(bycatch)[3] <- 'tot_apounds'
names(bycatch)[4] <- 'iteration'

#Find Medians


bycatch$category <- '999'
bycatch[bycatch$species %in% targets, 'category'] <- 'targets'
bycatch[bycatch$species %in% constraining, 'category'] <- 'constraining'

#Violin Plot
ggplot(bycatch, aes(factor(species), value)) + geom_violin() + facet_wrap(~ category, scales = 'free')

###_--


which(out[[3]][1] %in% tows$haul_id)
which(tows$haul_id %in% out[[3]][1])

tows[out[[3]][1], 'haul_id']

#Histograms for target species
targs <- 

#Look at pairs of target and constraining species, i.e. which tows caught dover and sablefish?
out$hauls

sab_dov <- bycatch %>% filter(species %in% c('sablefish', 'dover sole'))



ggplot(bycatch, aes(value)) + geom_histogram() + facet_wrap(~ species + category, scales = 'free') + 
  theme_bw()

 
#Plot as violin plots
ggplot(bycatch, aes(factor(species), value)) + geom_boxplot()
ggplot(bycatch, aes(factor(species), value)) + geom_violin() + facet_wrap(~ species)

p <- ggplot(mtcars, aes(factor(cyl), mpg))
p + geom_violin()


#Plot the Distribution of fishing locations


ggplot(out[[2]], )

bh <- ggplot(data, aes(x = longitude, y = latitude, group = year)) +
    stat_bin2d(binwidth = bw)
bw = c(0.0909, 0.11)

#Plot histograms of bycatch from sampled tows
bycatch <- melt(out[[1]])

mm <- bycatch %>% filter(species == 'sablefish')
hist(mm$tot_apounds, breaks = 50)

hist(bycatch %>% filter(species == 'sablefish') )

ggplot(bycatch) + geom_histogram(aes(x = tot_apounds), binwidth = 50) + 
  facet_wrap(~ species) + theme_bw()




#
res <- sample_tows(10, nsamps = 50)
median(res[[2]])
hist(res[[2]], breaks = 100)

lapply(res[[3]], FUN = function(x){
  hauls <- of_interest[x, 'haul_id']
  tows$haul_id
  return(hauls)
})

res[[3]]

#Look at all the other tows
for(rr in 1:length(res[[3]])){

}



of_interest[res[[3]], 'haul_id']

llply(res[[3]], FUN = function(xx){
  hauls <- of_interest[xx, 'haul_id']
  return(hauls)
})





#Loop over nsamples to evaluate the medians associated with each
samp_vec <- seq(500, 10000, by = 500)
outs <- vector('list', length = length(samp_vec))

for(jj in 1:length(samp_vec)){
  outs[[jj]] <- sample_tows(5000, nsamp = samp_vec[jj])$tot_catch
  print(jj)
}



plot(samp_vec, unlist(lapply(outs, FUN = median)), pch = 19)
for_gg <- melt(outs)

ggplot() + geom_histogram(data = for_gg, aes(x = value), binwidth = 1000) + facet_grid(L1 ~ .)




sample_tows(5000, nsamps)


#How to set the TACs?













perc_samples <- vector('list', length = 5000)
tc_samples <- vector('list', length = 5000)

for(ii in 1:length(perc_samples)){
  samp_rows <- sample(1:nrow(of_interest), 50, replace = FALSE)  
  sampled <- of_interest[samp_rows, ] %>% arrange(desc(apounds))

  perc_samples[[ii]] <- sampled$apounds / tac 
  tc_samples[[ii]] <- sum(sampled$apounds)
}

hist(unlist(tc_samples), breaks = 100)
ggp

#Get this thing to move a little bit?

#Draw 50
samp_rows <- sample(1:nrow(of_interest), 50, replace = FALSE)
sampled <- of_interest[samp_rows, ] %>% arrange(desc(apounds))

#How far from TAC?

sampled$apounds / tac






of_interest



#Did 



tows_of_interest <- tows[unique(of_interest$haul_id) %in% 
  tows$haul_id, ]



tows$haul_id %in% of_in
of_interest$haul_id

tows_of_interest <- tows %>% filter(haul_id,)

sables <- tows %>% filter()

wc_data








