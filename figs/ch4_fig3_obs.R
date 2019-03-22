#------------------------------------------------------------------------------------------------------------
load(file = '//udrive.uw.edu//udrive//obs_delta_plots//obs_only_delts_befaft.Rdata')
load(file = '//udrive.uw.edu//udrive//obs_delta_plots//obs_delta_sigs.Rdata')

if(Sys.info()['sysname'] == "Darwin"){
  load(file = '//Volumes//udrive//obs_delta_plots//obs_only_delts_befaft.Rdata')
  load(file = '//Volumes//udrive//obs_delta_plots//obs_delta_sigs.Rdata')
}

#Use observer data
# load(file = 'output/obs_only_delts_befaft.Rdata')
# load(file = 'output/obs_delta_sigs.Rdata')
#------------------------------------------------------------------------------------------------------------
#Figure with before and after only for all species

whenz <- c('before', 'after')
delts_befaft <- delts_befaft %>% filter(is.na(skew) == FALSE)

delts_befaft$plot_type <- delts_befaft$type

targs <- which(delts_befaft$species %in% c("Petrale Sole", "Sablefish", "Lingcod", 'Dover Sole', 
                                           'Shortspine Thornyhead', "Longspine Thornyhead"))

delts_befaft$plot_type <- as.character(delts_befaft$plot_type)

dba <- delts_befaft %>% distinct(when, plot_type) %>% arrange(plot_type)
dba$plot_val <- c("Groundfish", "", "Targets", "", "Constraining", "")

#------------------------------------------------------------------------------------------------------------
#Plot only the targets and rebuilding species
#Add in species abbreviations
delts_befaft <- subset(delts_befaft, plot_type != 'other')

delts_befaft$spp_abb <- substr(delts_befaft$species, 1, 3)

#Manually adjust
delts_befaft[which(delts_befaft$species == "Greenstriped Rockfish"), 'spp_abb'] <- "Gst"
delts_befaft[which(delts_befaft$species == "Greenspotted Rockfish"), 'spp_abb'] <- "Gsp"
delts_befaft[which(delts_befaft$species == "Longnose Skate"), 'spp_abb'] <- "Lsk"
delts_befaft[which(delts_befaft$species == "Yellowtail Rockfish"), 'spp_abb'] <- "Ylt"
#Also which species are significant?

#------------------------------------------------------------------------------------------------------------
delts_befaft <- delts_befaft %>% left_join(delta_sigs, by = 'species')

props <- delts_befaft %>% dcast(species + prop_sig ~ when, value.var = "prop_zero")
props$diffs <- props$after - props$before

skews <- delts_befaft %>% dcast(species + skew_sig ~ when, value.var = "skew")
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

#Load logbook significance

#------------------------------------------------------------------------------------------------------------
#Last formatting

skews <- delts_befaft %>% dcast(species ~ when, value.var = "skew")
skews$skew_diffs <- skews$after - skews$before

props <- delts_befaft %>% dcast(species ~ when, value.var = "prop_zero")
props$prop_diffs <- props$after - props$before

skews <- skews %>% left_join(props %>% select(species, prop_diffs), by = "species")
skews <- skews %>% select(species, skew_diffs, prop_diffs)

delts_befaft <- delts_befaft %>% left_join(skews, by = 'species')

# delts_befaft <- delts_befaft %>% group_by(species) %>% mutate(skew_diffs = skew[2] - skew[1],
#                                                               prop_diffs = prop_zero[2] - prop_zero[1]) %>% as.data.frame

#Plot the logbook instead of the combined data set
dba$plot_type <- factor(dba$plot_type, levels = c("targets", "weaks", "groundfish"))
dba$when <- factor(dba$when, levels = c("before", "after"))
dba <- dba %>% arrange(plot_type, when)
dba$plot_type <- as.character(dba$plot_type)
dba$when <- as.character(dba$when)
dba$plot_val <- c("Targets", "", "Constraining", "", "Groundfish", "")

#Change cases of species in delts_befaft
delts_befaft$species_lower <- tolower(delts_befaft$species)
delts_befaft[c(grep("english", delts_befaft$species_lower),
  grep("pacific ocean", delts_befaft$species_lower)), "species_lower"] <-
  c("English sole", "English sole", "Pacific ocean perch", "Pacific ocean perch")

# delts_befaft %>% filter(type == 'targets') %>% ggplot(aes(x = prop_zero, y = skew, colour = when)) +
#   geom_point() + facet_wrap(~ species)

# tiff(width = 170, height = 170, file = 'figs/ch4_fig3_obs.tiff', units = 'mm', res = 300)

png(width = 7, height = 7, file = 'figs/ch4_fig3_obs.png', units = 'in', res = 200)
ylimz <- c(-1.2, 1)

par(mfrow = c(3, 2), oma = c(3.5, 3.5, 2, 11), mar = c(0, 0, .5, 0))
for(ii in 1:6){
  
  #Subset the data
# par(mfrow = c(1, 2))
  temp <- subset(delts_befaft, when == dba[ii, 'when'] & type == dba[ii, 'plot_type']) 
  
  # sig_skew_dec <- temp %>% filter(skew_sig == "yes", skew_diffs < 0)
  both_sig <- temp %>% filter(skew_sig == 'yes', skew_diffs < 0)
  
  # both_sig <- temp %>% filter(skew_sig == 'yes', prop_sig == 'yes', skew_diffs < 0,
  #   prop_diffs < 0)
  # # one_sig <- temp %>% filter(skew_sig != 'yes' | prop_sig != 'yes')
  one_sig <- temp %>% filter(species %in% both_sig$species == F)
  
  plot(both_sig$prop_zero, both_sig$skew, xlim = c(0, 1.1), ylim = ylimz, pch = 19, 
       col = adjustcolor( "black", alpha.f = 0.2), ann = F, axes = F, xaxs = 'i',
       yaxs = 'i', cex = 2.5)
  points(one_sig$prop_zero, one_sig$skew, pch = 21, cex = 2.5)
  
  # if(ii %in% c(2, 4, 6)) text(temp$prop_zero, temp$skew, temp$spp_abb, adj = 1.2)
  if(ii == 1){
    one <- temp[c(1, 5, 6), ]
    two <- temp[c(-1, -5, -6), ]
    text(two$prop_zero, two$skew, two$spp_abb, adj = 1.4)
    text(one$prop_zero, one$skew, one$spp_abb, adj = -.4)
    #Split out dover and sablefish
  }
  
  if(ii == 2){
    
    lefts <- temp[c(1, 5), ]
    rights <- temp[c(2, 4, 3, 6), ]
    text(rights$prop_zero, rights$skew, rights$spp_abb, adj = -.4)
    text(lefts$prop_zero, lefts$skew, lefts$spp_abb, adj = 1, pos = 3)
    
    # text(temp$prop_zero, temp$skew, temp$spp_abb, adj = 1.5)
    #Add legend
    legend('topright', bty = 'n', pt.cex = 2, 
      c("Significant decline in skew", "Non-significant"),
           pch = c(19, 21), col = c(adjustcolor( "black", alpha.f = 0.2), "black"))
  }
  
  if(ii == 3){
    boca <- temp[1, ]
    yel <- temp[6, ]
    two <- temp[c(-1, -6), ]
    text(two$prop_zero, two$skew, two$spp_abb, adj = 1.5)
    text(yel$prop_zero, yel$skew, yel$spp_abb, adj = 1.5, pos = 1)
    text(boca$prop_zero, boca$skew, boca$spp_abb, adj = c(.5, -1.1))
  }
  
  if(ii %in% c(2, 4, 6)){
    #Add axis on side 4 explaining
    yvals <- seq(ylimz[2], ylimz[1], length.out = 11)
    caps <- paste(temp$spp_abb, temp$species_lower, sep = " - ")
    yvals <- yvals[1:length(caps)]
    axis(side = 4, at = yvals, labels = caps, las = 2, lwd.tick = 0, adj = 1,
         mgp = c(0, .3, 0))
  }
  
  if(ii == 4){
    low <- temp[1, ]
    high <- temp[-1, ]
    text(low$prop_zero, low$skew, low$spp_abb, pos = 1, adj = .3)
    text(high$prop_zero, high$skew, high$spp_abb, pos = 3, adj = .3)
  }
  
  if(ii == 5){
    par(xpd = T)
# plot(1:10, xlim = c(0, 1), ylim = ylimz)    
    up <- subset(temp, species == "Widow Rockfish")
    temp <- subset(temp, species != "Widow Rockfish")

    down <- subset(temp, species == "Greenstriped Rockfish")
    temp <- subset(temp, species != "Greenstriped Rockfish")

    lefts <- temp[which(temp$species %in% c("Vermilion Rockfish", "Black Rockfish", 
                                            "Greenspotted Rockfish") == FALSE), ]
    rights <- temp[which(temp$species %in% c("Vermilion Rockfish", "Black Rockfish",
                                             "Greenspotted Rockfish")), ]
    

    text(lefts$prop_zero, lefts$skew, lefts$spp_abb, adj = 1.5)
    text(rights$prop_zero, rights$skew, rights$spp_abb, adj = -.3)
    text(up$prop_zero, up$skew, up$spp_abb, adj = -.3, pos = 3)
    text(down$prop_zero, down$skew, down$spp_abb, pos = 1)
  }
  
  if(ii == 6){
    ups <- subset(temp, species == "Longnose Skate")
    temp <- subset(temp, species != "Longnose Skate")
    rights <- subset(temp, species %in% c('Vermilion Rockfish',
      "Longnose Skate", "English Sole"))
    lefts <- subset(temp, species %in% c('Vermilion Rockfish',
      "Longnose Skate", "English Sole") == F)

    text(lefts$prop_zero, lefts$skew, lefts$spp_abb, adj = 1.5)
    text(ups$prop_zero, ups$skew, ups$spp_abb, pos = 3)
    text(rights$prop_zero, rights$skew, rights$spp_abb, adj = -.3)
  }
  
  if(ii == 1)mtext(side = 3, "Before", outer = T, line = .1, cex = 1.1, adj = .25)
  if(ii == 2)mtext(side = 3, "After", outer = T, line = .1, cex = 1.1, adj = .75)
  
  # points(temp_targs$prop_zero, temp_targs$skew, pch = 0, col = 'black', cex = 1.2)
  abline(h = 0, lty = 2)
  box()
  if(ii >= 5) {axis(side = 1, mgp = c(0, .5, 0), labels = c('0', .2, .4, .6, .8, "1"),
                    at = c(0, .2, .4, .6, .8, 1), cex.axis = 1.2)}
  if(ii %in% c(1, 3, 5, 7)) axis(side = 2, las = 2, mgp = c(0, .5, 0),
                                 at = c(-2, -1, 0, 1), cex.axis = 1.2)
  
  mtext(side = 3, line = -1.3, paste0(letters[ii], ") ", dba[ii, 'plot_val']), adj = .02, cex = .9)
  
  # mtext(side = 3, line = -1.2, dba[ii, 2], adj = .15, cex = .7)
}

mtext(side = 1, "Proportion zero", outer = T, line = 1.8, cex = 1.2)
mtext(side = 2, "Skew", outer = T, line = 1.7, cex = 1.2)

dev.off()
