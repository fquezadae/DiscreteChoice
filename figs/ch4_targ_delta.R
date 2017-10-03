#Delta plots

# Something with shortspines
# thornys <- tows_clust[grep("Thornyhead", tows_clust$species), ]
# thornys %>% group_by(set_year, species) %>% summarize(apounds = sum(apounds)) %>% arrange(species) %>%
#   as.data.frame


#Do this with tows clust
# spp_years <- tows_clust %>% filter(type != 'other') %>% distinct(set_year, species)

# delts <- mclapply(1:nrow(spp_years), FUN = function(mm) {
#   outs <- year_spp_delta(year = spp_years[mm, "set_year"], 
#     spp = spp_years[mm, 'species'])
#   return(outs)
# }, mc.cores = 6)

# delts <- ldply(delts)
# delts <-  delts %>% arrange(species, year)
# delts$when <- 'before'
# delts[which(delts$year >= 2011), 'when'] <- 'after'

# save(delts, file = 'output/delts.Rdata')

#------------------------------------------------------------------------------------------------------------
load(file = 'output/delts.Rdata')

# ggplot(delts) + geom_point(aes(x = prop_zero, y = skew, colour = when)) + facet_wrap(~ species)

#Plot targets
# types <- tows_clust %>% filter(type != 'other') %>% distinct(type, species)
# delts <- delts %>% left_join(types, by = "species")

targs <- filter(delts, type == 'targets')
# ggplot(targs) + geom_point(aes(x = prop_zero, y = skew, colour = when)) + facet_wrap(~ species)
tspp <- unique(targs$species) #target species


#------------------------------------------------------------------------------------------------------------
#The figure
png(width = 5.9, height = 5.3, file = 'figs/ch4_targ_delta.png', res = 200, units = 'in')

par(mfrow = c(2, 3), mar = c(0, 0, 0, 0), oma = c(3, 4, 1, .5))
for(ii in 1:6){
  bef <- targs %>% filter(species == tspp[ii], when == 'before')
  aft <- targs %>% filter(species == tspp[ii], when == 'after')
  
  plot(temp$prop_zero, temp$skew, xlim = c(0, 1), ylim = c(-2, .5), ann = F, axes = F,
    type = 'n')
  # points(bef$prop_zero, bef$skew, pch = 19, col = adjustcolor( "black", alpha.f = 0.5), cex = 1.2)
  points(aft$prop_zero, aft$skew, pch = 19, col = adjustcolor( "black", alpha.f = 0.5), cex = 1.7)
  # points(aft$prop_zero, aft$skew, pch = 21, col = rgb(0, 0, 0, .85), cex = 1.2)
  points(bef$prop_zero, bef$skew, pch = 21, col = rgb(0, 0, 0, .85), cex = 1.7)
  abline(h = 0, lty = 2)
  box()
  #Add axes
  if(ii %in% c(1, 4)) axis(side = 2, mgp = c(0, .5, 0), at = pretty(targs$skew),
    las = 2)
  if(ii > 3) axis(side = 1, mgp = c(0, .5, 0), at = c(0, .2, .4, .6, .8))
  if(ii == 3) legend('bottomright', bty = 'n', legend = c("Before", "After"), pch = c(21, 19),
    col = c(adjustcolor('black', alpha.f = 0.5), 
      adjustcolor('black', alpha.f = 0.5)), pt.bg = c('white', 'gray80'), pt.cex = 1.7)
  mtext(side = 3, text = paste0(letters[ii], ") ", tspp[ii]), cex = .7, adj = .04,
   line = -1.25)
}

mtext(side = 1, outer = T, "Proportion Zero Values", line = 1.7)
mtext(side = 2, outer = T, "Skew", line = 2.2)

dev.off()
