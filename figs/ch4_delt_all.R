
#------------------------------------------------------------------------------------------------------------
#Plot all species, combined by before or after catch shares
spp_when <- tows_clust %>% distinct(species, when, type)
spp_when <- spp_when %>% filter(when != 'baseline')
spp_when <- spp_when %>% arrange(species, when)

spp_when <- spp_when %>% group_by(species) %>% mutate(bef_aft = length(unique(when)))
spp_when <- spp_when %>% filter(bef_aft == 2)
spp_when <- spp_when %>% as.data.frame

tows_clust %>% filter(type == 'other') %>% group_by(species) %>% summarize(pounds = sum(apounds, na.rm = T)) %>%
  arrange(desc(pounds))

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

# delts_befaft <- ldply(delts_befaft)

# save(delts_befaft, file = 'output/delts_befaft.Rdata')

#------------------------------------------------------------------------------------------------------------
load(file = 'output/delts_befaft.Rdata')
load(file = 'output/delta_sigs.Rdata')
#------------------------------------------------------------------------------------------------------------
#Figure with before and after only for all species

whenz <- c('before', 'after')
delts_befaft <- delts_befaft %>% filter(is.na(skew) == FALSE)

# delts_befaft %>% filter(prop_zero <= .6, type == 'other')
# delts_befaft <- delts_bef_aft_all
# delts_befaft_all <- delts_befaft #Includes all species

# delts_befaft <- delts_befaft %>% filter(type != 'other')
# delts_befaft %>% filter(prop_zero <= .6, type != 'targets ')

delts_befaft$plot_type <- delts_befaft$type

targs <- which(delts_befaft$species %in% c("Petrale Sole", "Sablefish", "Lingcod", 'Dover Sole', 
      'Shortspine Thornyhead', "Longspine Thornyhead"))

delts_befaft$plot_type <- as.character(delts_befaft$plot_type)

dba <- delts_befaft %>% distinct(when, plot_type) %>% arrange(plot_type)
dba$when <- rep(c('before', 'after'), 4)
dba$plot_type <- c("other", 'other', 'targets', 'targets', 'weaks', 'weaks', 'groundfish',
  'groundfish')
dba$plot_val <- c('Other', "", "Targets", "", "Constraining", "", "Groundfish", "")

#------------------------------------------------------------------------------------------------------------
#Plot only the targets and rebuilding species
#Add in significance also
dba2 <- dba[3:8, ]

#Add in species abbreviations
delts_befaft <- subset(delts_befaft, plot_type != 'other')

delts_befaft$spp_abb <- substr(delts_befaft$species, 1, 3)

#Manually adjust
delts_befaft[which(delts_befaft$species == "Greenstriped Rockfish"), 'spp_abb'] <- "Gst"
delts_befaft[which(delts_befaft$species == "Greenspotted Rockfish"), 'spp_abb'] <- "Gsp"
delts_befaft[which(delts_befaft$species == "Longnose Skate"), 'spp_abb'] <- "Lsk"

#Also which species are significant?

#------------------------------------------------------------------------------------------------------------
delts_befaft <- delts_befaft %>% left_join(delta_sigs, by = 'species')


props <- delts_befaft %>% dcast(species ~ when, value.var = "prop_zero")
props$diffs <- props$after - props$before

skews <- delts_befaft %>% dcast(species ~ when, value.var = "skew")
skews$diffs <- skews$after - skews$before

#------------------------------------------------------------------------------------------------------------
#Which species had increases in targeting; decrease in prop_zero and decrease in skew that was significant

delts_befaft %>% distinct(species, skew_sig, prop_sig)

skews %>% filter(diffs > 0)
length(which(skews$diffs > 0))

delts_befaft %>% filter(plot_type == 'targets')
delts_befaft %>% filter(plot_type == 'weaks') %>% distinct(species) 


length(skews[which(skews$diffs > 0), 'species'])


props_dec_spp <- props[which(props$diffs < 0), 'species']
inc_targ_spp <- props_dec_spp[props_dec_spp %in% skews[which(skews$diffs < 0), 'species'] ]

delts_befaft %>% filter(species %in% inc_targ_spp) %>% distinct(species)

delts_befaft$species[delts_befaft$species %in% inc_targ_spp]
unique(delts_befaft$species[delts_befaft$species %in% inc_targ_spp] )

skews %>% filter(diffs)



delts_befaft %>% group_by(species)


#------------------------------------------------------------------------------------------------------------


png(width = 7, height = 7, file = 'figs/ch4_delt_all.png', units = 'in', res = 200)

par(mfrow = c(3, 2), oma = c(3.5, 3.5, 2, 11), mar = c(0, 0, .5, 0))
for(ii in 1:6){
  
  temp <- subset(delts_befaft, when == dba2[ii, 'when'] & type == dba2[ii, 'plot_type']) 
  both_sig <- temp %>% filter(skew_sig == 'yes', prop_sig == 'yes')
  one_sig <- temp %>% filter(skew_sig != 'yes' | prop_sig != 'yes')

  plot(both_sig$prop_zero, both_sig$skew, xlim = c(0, 1.1), ylim = c(-2, 1.5), pch = 19, 
    col = adjustcolor( "black", alpha.f = 0.2), ann = F, axes = F, xaxs = 'i',
    yaxs = 'i', cex = 2.5)
  points(one_sig$prop_zero, one_sig$skew, pch = 21, cex = 2.5)
  # if(ii %in% c(2, 4, 6)) text(temp$prop_zero, temp$skew, temp$spp_abb, adj = 1.2)
  if(ii == 1){
    one <- temp[5, ]
    two <- temp[-5, ]
    text(two$prop_zero, two$skew, two$spp_abb, adj = 1.2)
    text(one$prop_zero, one$skew, one$spp_abb, adj = -.3)
    #Split out dover and sablefish
  }
  
  if(ii == 2){
    #Add legend
    legend('topright', bty = 'n', pt.cex = 2, c("Significant", "Non-significant"),
      pch = c(19, 21), col = c(adjustcolor( "black", alpha.f = 0.2), "black"))
  }

  if(ii == 3){
    boca <- temp[1, ]
    two <- temp[-1, ]
    text(two$prop_zero, two$skew, two$spp_abb, adj = 1.5)
    text(boca$prop_zero, boca$skew, boca$spp_abb, adj = c(.5, -1.1))
  }

  if(ii %in% c(2, 4, 6)){
    #Add axis on side 4 explaining
    yvals <- seq(1.5, -2, by = -.35)
    caps <- paste(temp$spp_abb, tolower(temp$species), sep = " - ")
    yvals <- yvals[1:length(caps)]
    axis(side = 4, at = yvals, labels = caps, las = 2, lwd.tick = 0, adj = 1,
      mgp = c(0, .3, 0))
  }
  if(ii == 5){
    par(xpd = T)
    rights <- temp[which(temp$species %in% c("Black Rockfish", "Widow Rockfish", 
          "Greenspotted Rockfish")), ]
    lefts <- temp[which(temp$species %in% c("Black Rockfish", "Widow Rockfish", 
          "Greenspotted Rockfish") == FALSE), ]
    
    text(lefts$prop_zero, lefts$skew, lefts$spp_abb, adj = 1.5)
    text(rights$prop_zero, rights$skew, rights$spp_abb, adj = -.2)
  }
  if(ii %in% c(2, 4, 6)) text(temp$prop_zero, temp$skew, temp$spp_abb, adj = 1.4)
  
  if(ii == 1)mtext(side = 3, "Before", outer = T, line = .1, cex = 1.1, adj = .25)
  if(ii == 2)mtext(side = 3, "After", outer = T, line = .1, cex = 1.1, adj = .75)

  # points(temp_targs$prop_zero, temp_targs$skew, pch = 0, col = 'black', cex = 1.2)
  abline(h = 0, lty = 2)
  box()
  if(ii >= 5) {axis(side = 1, mgp = c(0, .5, 0), labels = c('0', .2, .4, .6, .8, "1"),
      at = c(0, .2, .4, .6, .8, 1), cex.axis = 1.2)}
  if(ii %in% c(1, 3, 5, 7)) axis(side = 2, las = 2, mgp = c(0, .5, 0),
    at = c(-2, -1, 0, 1), cex.axis = 1.2)
  
  mtext(side = 3, line = -1.3, paste0(letters[ii], ") ", dba2[ii, 'plot_val']), adj = .02, cex = .9)
  
  # mtext(side = 3, line = -1.2, dba[ii, 2], adj = .15, cex = .7)
}

# mtext(side = 3, "Before", outer = T, line = .1, cex = 1.1, adj = .25)

mtext(side = 1, "Proportion zero", outer = T, line = 1.8, cex = 1.2)
mtext(side = 2, "Skew", outer = T, line = 1.7, cex = 1.2)

dev.off()


#------------------------------------------------------------------------------------------------------------
png(width = 3.6, height = 6.5, file = 'figs/ch4_delt_all_old.png', units = 'in', res = 200)
par(mfrow = c(4, 2), oma = c(3, 3, 2, 0), mar = c(0, 0, 0, .5))
for(ii in 1:8){
  temp <- subset(delts_befaft, when == dba[ii, 'when'] & type == dba[ii, 'plot_type']) 
  # targs <- which(temp$species %in% c("Petrale Sole", "Sablefish", "Lingcod", 'Dover Sole', 
  #     'Shortspine Thornyhead', "Longspine Thornyhead"))
  # temp_targs <- temp[targs, ]
  # temp <- temp[-targs, ]

  plot(temp$prop_zero, temp$skew, xlim = c(0, 1.05), ylim = c(-2.7, 3.1), pch = 19, 
    col = adjustcolor( "black", alpha.f = 0.2), ann = F, axes = F, xaxs = 'i',
    yaxs = 'i', cex = 1.3)
  # points(temp_targs$prop_zero, temp_targs$skew, pch = 0, col = 'black', cex = 1.2)
  abline(h = 0, lty = 2)
  box()
  if(ii >= 7) {axis(side = 1, mgp = c(0, .5, 0), labels = c('0', .2, .4, .6, .8, "1"),
      at = c(0, .2, .4, .6, .8, 1))}
  if(ii %in% c(1, 3, 5, 7)) axis(side = 2, las = 2, mgp = c(0, .5, 0))
  
  mtext(side = 3, line = -1.2, paste0(letters[ii], ") ", dba[ii, 'plot_val']), adj = .02, cex = .7)
  
  # mtext(side = 3, line = -1.2, dba[ii, 2], adj = .15, cex = .7)
}

mtext(side = 3, "Before", outer = T, line = .5, cex = .8, adj = 0)
mtext(side = 3, "After", outer = T, line = .5, cex = .8, adj = .55)
mtext(side = 1, "Proportion Zero", outer = T, line = 1.7, cex = .9)
mtext(side = 2, "Skew", outer = T, line = 1.7, cex = .9)

dev.off()

#------------------------------------------------------------------------------------------------------------
#Find species with proportion zero less than 0.5

delts_befaft %>% filter(prop_zero <= .5, type == 'other') 

#THe other species with proportions zero less than 0.5 are
delts_befaft %>% filter(prop_zero <= .5, type == 'groundfish') %>% distinct(species)







