
#Run the models minimizing vessel movement
#Add in option to filter by

#Bin the data by each port and 
#------------------------------------------------------------------------------------------------------
#Bin the data in 
the_ports <- list("FORT BRAGG", "EUREKA", 'ccb',
                  "CHARLESTON (COOS BAY)", "NEWPORT", "ASTORIA / WARRENTON")

#------------------------------------------------------------------------------------------------------
#Bin the data and 
#Combine certain ports

tows_clust$fleet_name_comb <- tows_clust$fleet_name
tows_clust[which(tows_clust$fleet_name_comb %in% c("CRESCENT CITY", "BROOKINGS")), 'fleet_name_comb'] <- "ccb"

port_bins <- lapply(the_ports, FUN = function(xx){
  temp_dat <- tows_clust %>% filter(fleet_name_comb %in% xx) %>% distinct(haul_id, .keep_all = T)
  temp_bin <- bin_data(data = temp_dat, x_col = 'avg_long', y_col = 'avg_lat',
    group = "set_year", grid_size = c(.0909, .11), group_vec = 2007:2014)

  #Count number of vessels in each grid
  unq_bins <- temp_bin %>% distinct(unq, .keep_all = T)

  temp_dat$unq <- 999
  temp_dat$bin_x <- 999
  temp_dat$bin_y <- 999

  for(ii in 1:nrow(unq_bins)){
    tb <- unq_bins[ii, ]
    the_inds <- which(temp_dat$avg_long > tb$xmin & temp_dat$avg_long < tb$xmax &
        temp_dat$avg_lat > tb$ymin & temp_dat$avg_lat < tb$ymax)
    temp_dat[the_inds, 'unq'] <- tb$unq
    temp_dat[the_inds, 'bin_x'] <- tb$x
    temp_dat[the_inds, 'bin_y'] <- tb$y
  }

  #Count number of vessels in each unique location
  unq_vess <- temp_dat %>% group_by(unq, set_year) %>% summarize(nvess = length(unique(drvid)))
  unq_vess <- plyr::rename(unq_vess, c("set_year" = 'year'))

  #Add this in to the binned data
  temp_bin <- temp_bin %>% left_join(unq_vess, by = c('unq', 'year'))
  temp_bin$fleet_name_comb <- xx
  return(temp_bin)
})

#------------------------------------------------------------------------------------------------------
port_bins <- ldply(port_bins)
# port_bins %>% filter(fleet_name == 'NEWPORT', nvess >= 3, year >= 2011) %>% 
#   ggplot() + geom_tile(aes(x = x, y = y,
#   fill = count)) + facet_wrap(~ year, nrow = 1) 
port_bins <- port_bins %>% filter(nvess >= 3)

#Maybe use 100 as a cutoff for plotting?

scaled_value <- 100

# max_value <- 15
port_bins$plot_value <- port_bins$count
port_bins[which(port_bins$plot_value >= scaled_value), 'plot_value'] <- scaled_value

port_bins$plot_value <- round(port_bins$plot_value / max(port_bins$plot_value) * 100,
  digits = 0)

port_bins$greys <- paste0('grey', 100 - port_bins$plot_value)
port_bins$greys <- rgb(t(col2rgb(port_bins$greys)), maxColorValue = 255)

gg <- port_bins %>% 
  distinct(plot_value, .keep_all = T) %>% 
  arrange(plot_value) %>% select(greys)

#------------------------------------------------------------------------------------------------------
#Construct a data frame for each of the analyzed ports
#and include the port locations, xlimits and ylimits
the_ports <- list("FORT BRAGG", "EUREKA", "CRESCENT CITY", 'BROOKINGS',
                  "CHARLESTON (COOS BAY)", "NEWPORT", "ASTORIA / WARRENTON")

port_locs <- tows_clust %>% distinct(dport_desc, d_port_long, d_port_lat)

port_plot <- lapply(the_ports, FUN = function(xx){
  temp <- port_locs %>% filter(dport_desc %in% xx)
  temp_clusts <- tows_clust %>% filter(fleet_name == xx) 
  xlims <- range(pretty(range(temp_clusts$set_long)))
  ylims <- range(pretty(range(temp_clusts$set_lat)))

  xx$port_loc <- c(temp$d_port_long, temp$d_port_lat)  
  xx$xlims <- xlims
  xx$ylims <- ylims
  names(xx)[1] <- 'port_name'
  return(xx)
})

#Manually combine 3 and 4
cc_port <- c(-124.1841, 41.7490)
br_port <- c(-124.2697, 42.0470)

#Remove Brookings from this
port_plot <- port_plot[-4]
#------------------------------------------------------------------------------------------------------
#add significance values also
coefs <- read.csv('output/the_coefs_09_14.csv', stringsAsFactors = F)
# coefs <- read.csv('output/the_coefs_09_14_nday30_hdist8.05_for_plot.csv', stringsAsFactors = F)

coefs <- melt(coefs, id.vars = c('port', 'coef_type', 'coef_type_desc'))

coefs$variable <- as.character(coefs$variable)

coefs$cc <- sapply(strsplit(coefs$value, " "), FUN = function(xx) xx[1])
coefs$sig <- sapply(strsplit(coefs$value, " "), FUN = function(xx) xx[2])
coefs$pch <- 0

# coefs <- cbind(coefs, ldply(strsplit((coefs$value), " ")))
# coefs <- plyr::rename(coefs, c("V1" = "cc", "V2" = "sig"))
coefs[is.na(coefs$sig), 'sig'] <- ""

coefs[which(coefs$sig %in% c('', ".")), 'pch'] <- 19
coefs$variable <- substr(coefs$variable, 2, 5)

#Add coefficients as a data frame to port_plot
cc <- c('dist1', 'dist', 'rev1', 'rev', 'dmiss', 'dum30', 'dum30y')

require(RColorBrewer)
coef_colors <- brewer.pal(7,"Dark2")

# coef_colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f")
# coef_colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628")
coef_colors <- coefs %>% distinct(coef_type) %>% mutate(coef_colors = coef_colors)

coefs <- coefs %>% left_join(coef_colors, by = 'coef_type')

#Adjust colors
coef_sigs <- coefs %>% distinct(sig) 
coef_sigs$alpha <- .4
coef_sigs[which(coef_sigs$sig %in% c(".", "")), 'alpha'] <- 0
coef_sigs[which(coef_sigs$sig == "***"), 'alpha'] <- 1


# coef_sigs$alpha <- c(1, 1, 0, 0 , 1)
coefs <- coefs %>% left_join(coef_sigs, by = 'sig')
coefs$sig_color <- apply(coefs, MAR = 1, FUN = function(xx) adjustcolor(xx['coef_colors'], xx['alpha']))

#figure out 

#Colors
coefs_colors <- coefs %>% dcast(port + coef_type ~ variable, value.var = 'sig_color')
cc <- rev(c('dist1', 'dist', 'rev1', 'rev', 'dmiss', 'dum30', 'dum30y'))
coefs_colors <- coefs_colors %>% group_by(port) %>%  slice(match(cc, coef_type)) %>% as.data.frame

#Which ones to add points to?
coefs$add_point <- "no"
coefs[which(coefs$sig == "***"), 'add_point'] <- 'yes'
coefs_point <- coefs %>% dcast(port + coef_type ~ variable, value.var = 'add_point')
coefs_point <- coefs_point %>% group_by(port) %>% slice(match(cc, coef_type)) %>% as.data.frame

coefs[which(coefs$cc > 0), 'pch'] <- 19
coefs[which(coefs$cc < 0), 'pch'] <- 15
coefs_pch <- coefs %>% dcast(port + coef_type ~ variable, value.var = 'pch')
coefs_pch <- coefs_pch %>% group_by(port) %>% slice(match(cc, coef_type)) %>% as.data.frame

base_colors <- coefs %>% dcast(port + coef_type ~ variable, value.var = 'coef_colors')
base_colors <- base_colors %>% group_by(port) %>% slice(match(cc, coef_type)) %>% as.data.frame
#------------------------------------------------------------------------------------------------------
#Modify the 
port_plot[[1]]$xlims <- c(-126, -123)
port_plot[[1]]$ylims <- c(38, 40)
port_plot[[1]]$ylabs <- c(38, 39, 40)
port_plot[[6]]$letts <- letters[1:6]
port_plot[[1]]$coefs_colors <- coefs_colors %>% filter(port == "FORT BRAGG")
port_plot[[1]]$base_colors <- base_colors %>% filter(port == "FORT BRAGG")
port_plot[[1]]$coefs_point <- coefs_point %>% filter(port == "FORT BRAGG")
port_plot[[1]]$coefs_pch <- coefs_pch %>% filter(port == "FORT BRAGG")
# port_plot[[1]]$coefs <- port_plot[[1]]$coefs[7:1, ]


port_plot[[2]]$xlims <- c(-126, -123)
port_plot[[2]]$ylims <- c(40, 42)
port_plot[[2]]$ylabs <- c(40, 41, 42)
port_plot[[5]]$letts <- letters[7:12]
port_plot[[2]]$coefs_colors <- coefs_colors %>% filter(port == "EUREKA")
port_plot[[2]]$base_colors <- base_colors %>% filter(port == "EUREKA")
port_plot[[2]]$coefs_point <- coefs_point %>% filter(port == "EUREKA")
port_plot[[2]]$coefs_pch <- coefs_pch %>% filter(port == "EUREKA")
# port_plot[[2]]$coefs <- port_plot[[2]]$coefs[7:1, ]


port_plot[[3]]$port_name <- "ccb"
port_plot[[3]]$xlims <- c(-126, -123)
port_plot[[3]]$ylims <- c(41, 43)
port_plot[[3]]$ylabs <- c(41, 42, 43)
# port_plot[[4]]$letts <- c('i', 'j', 'k', 'l')
port_plot[[4]]$letts <- letters[13:18]
port_plot[[3]]$coefs_colors <- coefs_colors %>% filter(port == "CRESCENT CITY_BROOKINGS")
port_plot[[3]]$base_colors <- base_colors %>% filter(port == "CRESCENT CITY_BROOKINGS")
port_plot[[3]]$coefs_point <- coefs_point %>% filter(port == "CRESCENT CITY_BROOKINGS")
port_plot[[3]]$coefs_pch <- coefs_pch %>% filter(port == "CRESCENT CITY_BROOKINGS")


# port_plot[[3]]$coefs <- port_plot[[3]]$coefs[7:1, ]
# port_plot[[4]]$xlims <- c(-126, -123)
# port_plot[[4]]$ylims <- c(41, 43)

port_plot[[4]]$xlims <- c(-126, -123)
port_plot[[4]]$ylims <- c(42, 44)
port_plot[[4]]$ylabs <- c(42, 43, 44)
port_plot[[3]]$letts <- letters[19:24]
port_plot[[4]]$coefs_colors <- coefs_colors %>% filter(port == "CHARLESTON (COOS BAY)")
port_plot[[4]]$base_colors <- base_colors %>% filter(port == "CHARLESTON (COOS BAY)")
port_plot[[4]]$coefs_point <- coefs_point %>% filter(port == "CHARLESTON (COOS BAY)")
port_plot[[4]]$coefs_pch <- coefs_pch %>% filter(port == "CHARLESTON (COOS BAY)")
# port_plot[[4]]$coefs <- port_plot[[4]]$coefs[7:1, ]

port_plot[[5]]$xlims <- c(-126, -123)
port_plot[[5]]$ylims <- c(43.5, 46)
port_plot[[5]]$ylabs <- c(44, 45, 46)
port_plot[[2]]$letts <- c(letters[25:26], 'aa', 'ab', 'ac', 'ad')
port_plot[[5]]$coefs_colors <- coefs_colors %>% filter(port == "NEWPORT")
port_plot[[5]]$base_colors <- base_colors %>% filter(port == "NEWPORT")
port_plot[[5]]$coefs_point <- coefs_point %>% filter(port == "NEWPORT")
port_plot[[5]]$coefs_pch <- coefs_pch %>% filter(port == "NEWPORT")
# port_plot[[5]]$coefs <- port_plot[[5]]$coefs[7:1, ]

port_plot[[6]]$xlims <- c(-126, -123)
port_plot[[6]]$ylims <- c(45, 48.5)
port_plot[[6]]$ylabs <- c(45, 46, 47, 48)
# port_plot[[1]]$letts <- c('u', 'v', 'w', 'x')
port_plot[[1]]$letts <- c('ae', 'af', 'ag', 'ah', 'ai', 'aj')
port_plot[[6]]$coefs_colors <- coefs_colors %>% filter(port == "ASTORIA / WARRENTON")
port_plot[[6]]$base_colors <- base_colors %>% filter(port == "ASTORIA / WARRENTON")
port_plot[[6]]$coefs_point <- coefs_point %>% filter(port == "ASTORIA / WARRENTON")
port_plot[[6]]$coefs_pch <- coefs_pch %>% filter(port == "ASTORIA / WARRENTON")
# port_plot[[6]]$coefs <- port_plot[[6]]$coefs[7:1, ]

#Add colors in for significance


#------------------------------------------------------------------------------------------------------
#Manually change the limits
#Arrange the plots by latitude
some_port_locs <- port_locs %>% filter(dport_desc %in% ldply(the_ports)$V1,
  dport_desc != "BROOKINGS") %>% arrange(desc(d_port_lat))

some_port_locs$name <- c("Astoria", "Newport", 'Charleston', "Brookings & Crescent City",
  'Eureka', 'Fort Bragg')

some_port_locs <- lapply(1:6, FUN = function(xx){
  some_port_locs[xx, ]
})

#Add in certain points, and reverse the list
some_port_locs[[4]] <- rbind(some_port_locs[[4]], some_port_locs[[4]])
some_port_locs[[4]][2, 'd_port_long'] <- -124.2497
some_port_locs[[4]][2, 'd_port_lat'] <- 42.0470

some_port_locs <- some_port_locs[6:1]

yrz <- 2009:2014








#------------------------------------------------------------------------------------------------------
#Come up with rule to manipulate the layout to stretch y axis enough so that x axis is consistent
#for all plots
# # png()
# png(width = 15, height = 20, units = 'in', res = 200, file = 'figs/test_plot.png')
# matlay <- matrix(c(1, 2, 3, 4,
#                    5, 6, 7, 8, 
#                    9, 10, 11, 12,
#                    13, 14, 15, 16,
#                    17, 18, 19, 20,
#                    21, 22, 23, 24,
#                    25, 26, 27, 28), ncol = 4, byrow = T)
# # layout(matlay)
# layout(matlay, heights = rep(1, 7), widths = rep(.7, 7))
# # layout(matlay, heights = c(1.3, rep(.95, 6)), widths = rep(.7, 7))
# # layout.show(28)
# #------------------------------------------------------------------------------------------------------

# # par(mar = c(0, 0, 0, 0), oma = c(1, 1, 1, 0.25))
# for(pp in 7:1){
#   plot_dat <- port_plot[[pp]]
#   temp_bin <- port_bins %>% filter(fleet_name %in% plot_dat$port_name)

#   for(yy in 1:length(yrz)){
#     yr_temp <- temp_bin %>% filter(year == yrz[yy])
#     map('state', fill = FALSE, col = 'white', xlim = plot_dat$xlims,
#       ylim = plot_dat$ylims, mar = c(0, 0, 0, 0), asp = 1.3 )
#     points(yr_temp$x, yr_temp$y, pch = 15, cex = 1, col = yr_temp$greys)
#     map('state', fill = TRUE, col = 'gray95', xlim = plot_dat$xlims,
#       ylim = plot_dat$ylims, mar = c(0, 0, 0, 0), add = T, asp = 1.3 )
#     points(some_port_locs$d_port_long, some_port_locs$d_port_lat, pch = 19, col = 'red')
#     box()
#   }
# }

# dev.off()






# #------------------------------------------------------------------------------------------------------
# astoria <- port_bins %>% filter(fleet_name == 'ASTORIA / WARRENTON')
# astoria <- port_bins %>% filter(fleet_name == 'NEWPORT')

# map('state', fill = FALSE, col = 'gray95', xlim = c(-126, -123), ylim = c(38, 48.5), 
#   mar = c(0, 0, 0, 0))
# points(astoria$x, astoria$y, pch = 15, cex = .6, col = port_bins$greys)
# # points(port_bins$x, port_bins$y, pch = 15, cex = .6, col = port_bins$greys)
# map('state', fill = TRUE, col = 'gray95', xlim = c(-126, -123), ylim = c(38, 48.5), add = T)
# points(some_port_locs$d_port_long, some_port_locs$d_port_lat, pch = 19, col = 'red')


# for(yy in 1:length(yrz)){
#   yr_temp <- eur %>% filter(year == yrz[yy])
  
#   map('state', fill = FALSE, col = 'white', xlim = xlims, asp = 1.1, ylim = ylims,
#     mar = c(0, 0, 0, 0) )
#   points(yr_temp$x, yr_temp$y, pch = 15, cex = 2, col = yr_temp$greys)
  
#   map('state', fill = TRUE, col = 'gray95', xlim = xlims, asp = 1.1, ylim = ylims,
#     mar = c(0, 0, 0, 0), add = T)
#   points(x = -124.1660, y = 40.8070, pch = 19, cex = 2)
#   box()
# }



# for(ii in 1:8){
#   tt <- tows_clust_bin %>% filter(year == yrz[ii])
#   map('state', fill = FALSE, col = 'white', xlim = c(-126, -120.5), asp = 1.3, ylim = c(34, 49),
#     mar = c(0, 0, 0, 0))
#   points(tt$x, tt$y, pch = 15, cex = 1, xlim = c(-126, -120.5), ylim = c(34, 49), col = tt$greys)
#   map('state', fill = TRUE, col = 'gray95', xlim = c(-126, -120.5), asp = 1.3, ylim = c(34, 49),
#     mar = c(0, 0, 0, 0), add = T)
#   box()
  
#   mtext(paste0(letters[ii], ".", ") ", yrz[ii]), adj = .05, cex = .6, side = 1,
#     line = -1.2, bg = 'white')

#   #Add axes
#   if(ii %in% c(1, 5)) axis(side = 2, las = 2, mgp = c(0, .5, 0))
#   if(ii >= 5) axis(side = 1, mgp = c(0, .5, 0), at = c(-125, -123, -121), labels = c('125', '123', '121'))
# }

# #Add Color Bar
# rect(xleft = -123.9, ybottom = 42.75, xright = -120.55, ytop = 48, col = 'white', border = 'black')






