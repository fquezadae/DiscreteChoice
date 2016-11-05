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
wc_data <- wc_data[-grep("\\.", row.names(wc_data)), ]

# tows <- wc_data[-grep("\\.", row.names(wc_data)), ]

#Keep only certain columns
tows <- wc_data %>% select(trip_id, haul_id, hpounds, apounds, species, rport_desc,
  agid)

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
#Looking at sablefish, 50 tows on average will catch
sample_tows <- function(nreps = 5000, nsamps = 50, tac = 50000){
  #Things to save:
  #(1) Percentage of the TAC caught
  #(2) Summed Catch across all tows
  #(3) Row indices to see the amount of other things caught
  perc_samples <- vector('list', length = nreps)
  tc_samples <- vector('list', length = nreps)
  row_samples <- vector('list', length = nreps)

  for(ii in 1:length(perc_samples)){
    samp_rows <- sample(1:nrow(of_interest), nsamps, replace = FALSE)
    sampled <- of_interest[samp_rows, ] %>% arrange(desc(apounds))

    #Store outputs
    perc_samples[[ii]] <- sampled$apounds / tac
    tc_samples[[ii]] <- sum(sampled$apounds)
    row_samples[[ii]] <- samp_rows
  }

  return(list('perc_samples' = perc_samples, 'tot_catch' = unlist(tc_samples),
    'row_samples' = row_samples))
}

#Try writing a sample tows function that uses apply to run faster
nreps <- 1000
nsamps <- 100
tac <- 50000

sample_tows <- function(nreps = 5000, nsamps = 50, tac = 50000, seed = 300){
  print('in the apply function one')
  #Things to save:
  #(1) Percentage of the TAC caught
  #(2) Summed Catch across all tows
  #(3) Row indices to see the amount of other things caught
  perc_samples <- vector('list', length = nreps)
  tc_samples <- vector('list', length = nreps)
  row_samples <- vector('list', length = nreps)

  #Set Seed
  set.seed(seed)
  
  samp_rows <- lapply(row_samples, FUN = function(x) sample(1:nrow(of_interest), nsamps, replace = FALSE))
  sampled <- lapply(samp_rows, FUN = function(x) of_interest[x, ] %>% arrange(desc(apounds)))
  perc_samples <- sampled$apounds / tac
  tc_samples <- lapply(sampled, FUN = function(x) sum(x$apounds))

  return(list('perc_samples' = perc_samples, 'tot_catch' = unlist(tc_samples),
    'row_samples' = samp_rows))

  # for(ii in 1:length(perc_samples)){
  #   samp_rows <- sample(1:nrow(of_interest), nsamps, replace = FALSE)
  #   sampled <- of_interest[samp_rows, ] %>% arrange(desc(apounds))

  #   #Store outputs
  #   perc_samples[[ii]] <- sampled$apounds / tac
  #   tc_samples[[ii]] <- sum(sampled$apounds)
  #   row_samples[[ii]] <- samp_rows
  # }

  # return(list('perc_samples' = perc_samples, 'tot_catch' = unlist(tc_samples),
  #   'row_samples' = row_samples))
}

########################################################################################
#Function to Calculate Bycatch
#of_interest is the data frame that is filtered to be only one species, maybe in a specific
#region.

calc_bycatch <- function(nreps = 5000, nsamps = 50, tac = 50000,
  of_interest){
# browser() 
  # of_interest <- tows %>% filter(species == target_species) %>% arrange(desc(apounds))

  samps <- sample_tows(nreps = nreps, nsamps = nsamps, tac = tac)

  #Pull out sampled rows and look at bycatch
  ind_rows <- lapply(samps[[3]], FUN = function(x){
    hauls <- of_interest[x, 'haul_id']
  })

  ########---------------------------------------########
  #Do two things with the sampled rows, right now only focusing on apounds
  #1. Save the bycatch associated with each sample
    #Only save the target and constraining species
  #2. Save the coordinates of each tow
  ########---------------------------------------########
  
  #1 Aggregated Bycatch
  agg_byc <- lapply(ind_rows, FUN = function(x){
               byc <- tows[tows$haul_id %in% x,]
               agg_byc_t <- byc %>% filter(category == 'targets' | category == 'constraining') %>% 
                 group_by(species) %>% summarize(tot_apounds = sum(apounds, na.rm = TRUE)) %>% 
                 arrange(desc(tot_apounds)) %>% as.data.frame
               return(agg_byc_t)
               }
             )

  #2 Haul Locations From West Coast data
  locs <- lapply(ind_rows, FUN = function(x){
    ll <- wc_data[wc_data$haul_id %in% x, c('lat', 'long')]
  })

  return(list('agg_byc' = agg_byc , 'locs' = locs, 'hauls' = ind_rows))
}

########################################################################################
#Function to plot interaction bycatch
#Pass the output of calc_bycatch, and pass the tows dataframe

#Calculate species correlations

spp_corrs <- function(tows = tows, output, two_species = c('dover sole', 'sablefish')){
  
  #Pull out the sampled tows from each 
  the_tows <- lapply(out$hauls, FUN = function(x) tows %>% filter(haul_id %in% x))
 
  filtered_tows <- lapply(the_tows, function(x) x %>% filter(species %in% two_species) %>% 
                     select(haul_id, apounds, species))

  #cast the tows to plot them
  casted <- lapply(filtered_tows, FUN = function(x) dcast(x, haul_id ~ species, value.var = 'apounds'))
  names(casted) <- 1:length(casted)

  #Replace the NAs with 0
  casted <- ldply(casted)
  names(casted)[1] <- 'replicate'
  casted[is.na(casted[, 3]), 3] <- 0
  casted[is.na(casted[, 4]), 4] <- 0
  
  #Calculate correlations between pairs
  names(casted)[3] <- 'spp1'
  names(casted)[4] <- 'spp2'

  pairs %>% group_by(replicate) %>% do({
    mod <- lm(spp2 ~ spp1, data = .)
    r2 <- summary(mod)$r.squared
    data.frame(., r2)
  }) %>% as.data.frame -> corrs

  unq_r2 <- corrs %>% distinct(replicate, r2)

  return(list("pairs" = casted, 'corrs' = unq_r2))
}


########################################################################################
#Try it for only sablefish in WA
of_interest <- tows %>% filter(species == 'sablefish' & agid == 'O' )

out <- calc_bycatch(nreps = 100, nsamps = 100, tac = 50000, of_interest = of_interest)
pairs <- spp_corrs(tows = tows, output = out, two_species = c('dover sole', 'longspine thornyhead'))
hist(pairs$corrs$r2, breaks = 50)




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








