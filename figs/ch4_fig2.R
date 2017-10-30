#------------------------------------------------------------------------
#Figure 2
#------------------------------------------------------------------------
#Format the Data

#Find unique tows
tows_clust_unq <- tows_clust %>% distinct(haul_id, .keep_all = T)

tows_clust_bin <- bin_data(tows_clust_unq, x_col = 'avg_long', y_col = 'avg_lat', 
  group = 'set_year', grid_size = c(.0909, .11),
  group_vec = 2007:2014)

#Specify maximum value
scaled_value <- 200
tows_clust_bin$plot_value <- tows_clust_bin$count
tows_clust_bin[which(tows_clust_bin$plot_value >= scaled_value), 'plot_value'] <- scaled_value

tows_clust_bin$plot_value <- round(tows_clust_bin$plot_value / max(tows_clust_bin$plot_value) * 100,
  digits = 0)

# gray.colors(10, start = 0, end = 1)

#Specify gray colors to use, make minimum a darker shade of grey
greys <- gray.colors(100, start = 0, end = .9)
greys <- data_frame(greys = greys, plot_value = rev(1:100)) %>% as.data.frame

tows_clust_bin <- tows_clust_bin %>% left_join(greys, by = 'plot_value')
tows_clust_bin[is.na(tows_clust_bin$greys), 'greys'] <- "#FFFFFF"

# tows_clust_bin$greys <- paste0('grey', 100 - tows_clust_bin$plot_value)
# tows_clust_bin$greys <- rgb(t(col2rgb(tows_clust_bin$greys)), maxColorValue = 255)

gg <- tows_clust_bin %>% 
  distinct(plot_value, .keep_all = T) %>% 
  arrange(plot_value) %>% select(greys)

#------------------------------------------------------------------------
#Count number of vessels in each grid cell

# tows_clust_bin <- bin_data(tows_clust, x_col = 'avg_long', y_col = 'avg_lat', group = 'set_year', grid_size = c(.0909, .11),
#   group_vec = 2007:2014)

#Back assign the clustered values
unq_bins <- tows_clust_bin %>% distinct(unq, .keep_all = T)

#Loop through all the unq_bins rows
tows_clust$unq <- "999"
tows_clust$bin_x <- "999"
tows_clust$bin_y <- "999"

for(ii in 1:nrow(unq_bins)){
  tb <- unq_bins[ii, ]
  the_inds <- which(tows_clust$avg_long > tb$xmin & tows_clust$avg_long < tb$xmax &
    tows_clust$avg_lat > tb$ymin & tows_clust$avg_lat < tb$ymax)
  tows_clust[the_inds, 'unq'] <- tb$unq
  tows_clust[the_inds, 'bin_x'] <- tb$x
  tows_clust[the_inds, 'bin_y'] <- tb$y
}

tows_clust$bin_x <- round(as.numeric(tows_clust$bin_x), digits = 4)
tows_clust$bin_y <- round(as.numeric(tows_clust$bin_y), digits = 4)

#unq columns and set year that I can keep
keepers <- tows_clust %>% group_by(unq, set_year) %>% summarize(nvess = length(unique(drvid))) 
keepers <- plyr::rename(keepers, c('set_year' = "year"))

tows_clust_bin <- tows_clust_bin %>% left_join(keepers, by = c('unq', "year")) 
tows_clust_bin <- tows_clust_bin %>% filter(nvess >= 3)

#------------------------------------------------------------------------
#Figure
# dev.new(width = 4.68, height = 7)

# pdf(width = 3.8, height = 6.6, file = 'figs/ch4_fig2.pdf')

xlabels <- c(expression("125"~degree ~ W),
             expression("123"~degree ~ W),
             expression("121"~degree ~ W))

ylabels <- c(expression("34"~degree ~ N),
             expression("36"~degree ~ N),
             expression("38"~degree ~ N),
             expression("40"~degree ~ N),
             expression("42"~degree ~ N),
             expression("44"~degree ~ N),
             expression("46"~degree ~ N),
             expression("48"~degree ~ N))
                    
png(width = 3.866, height = 6.6, res = 200, units = 'in',
  file = 'figs/ch4_fig2.png')

par(mar = c(0, 0, 0, 0), oma = c(3.5, 3, 0, 1.5), mfrow = c(2, 4))
yrz <- 2007:2014

for(ii in 1:8){
  tt <- tows_clust_bin %>% filter(year == yrz[ii])
  map('state', fill = FALSE, col = 'white', xlim = c(-126, -120.5), asp = 1.3, ylim = c(34, 49),
    mar = c(0, 0, 0, 0))
  points(tt$x, tt$y, pch = 15, cex = .3, xlim = c(-126, -120.5), ylim = c(34, 49), col = tt$greys)
  map('state', fill = TRUE, col = 'gray95', xlim = c(-126, -120.5), asp = 1.3, ylim = c(34, 49),
    mar = c(0, 0, 0, 0), add = T)
  box()
  
  mtext(yrz[ii], adj = .05, cex = .6, side = 1,
    line = -1.2, bg = 'white')
  #Add letters
  # mtext(paste0(letters[ii], ") ", yrz[ii]), adj = .05, cex = .6, side = 1,
  #   line = -1.2, bg = 'white')

  #Add axes
  if(ii %in% c(1, 5)) axis(side = 2, las = 2, mgp = c(0, .5, 0), at = c(34, 36, 38, 40,
    42, 44, 46, 48), labels = ylabels)
  if(ii >= 5) axis(side = 1, mgp = c(0, .5, 0), at = c(-125, -123, -121), labels = xlabels,
    las = 2)
  if(ii == 4) mtext(side = 4, "Before", line = .5)
  if(ii == 8) mtext(side = 4, "After", line = .5)

}

#Add Color Bar
rect(xleft = -123.3, ybottom = 42.75, xright = -120.55, ytop = 48.2, col = 'white', border = 'black')

par(mar = c(0, 0, 0, 0), fig = c(.97, 0.99, 0.3, .45), new = T)  
color_bar(lut = gg$greys, Cex = .3, nticks = 5, min = 0, max = 1, tick_labs = c("0", "50", 
  "100", "150", ">200"))

dev.off()

#-----------------------------------------------------------------------
#Function for Color Bar on Plot
color_bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), 
  tick_labs, title='', Cex = .5) {
    # browser()
    scale = (length(lut)-1)/(max-min)
    par(mgp = c(0, .5, 0))
    # dev.new(width=1.75, height=5)
    plot(c(0,10), c(min,max), type='n', xaxt='n', xlab='', yaxt='n', ylab=title, main='', 
      cex.lab=1.5, mgp = c(0, .05, 0), bg = 'white', bty = 'n')
    axis(2, at = ticks, labels = tick_labs, las=1, cex = Cex)
    for (i in 1:(length(lut)-1)) {
     y = (i-1)/scale + min
     rect(0,y,10,y+1/scale, col=lut[i], border=NA)
    }
}









#-----------------------------------------------------------------------
#Old fig2 Code
# #Get distinct grey values
# gg <- tows_clust_bin %>% 
#   distinct(scaled_value, .keep_all = T) %>% 
#   arrange(scaled_value) %>% select(greys)

# #-----------------------------------------------------------------------
# #Bin by latitude and depth
# tows_clust_bin_depth <- bin_data(tc_unq_hauls, x_col = 'avg_depth', y_col = 'avg_lat', group = 'set_year', 
#   grid_size = c(50, .5),
#   group_vec = 2007:2014)

# # unq <- c("5 8")

# # tt <- tows_clust_bin_depth %>% filter(unq == c('5 8'))
# # mod <- lm(count ~ year, data = tt)


# slopies <- tows_clust_bin_depth %>% group_by(unq) %>% mutate(nyears = length(unique(year))) %>%
#   filter(nyears > 1) %>% arrange(year) %>%
#   group_by(unq) %>%
#   do({
#     mod <- lm(count ~ year, data = .)
#     slope <- mod$coefficients[2]
#     names(slope) <- NULL
#     data.frame(., slope)
#   }) %>% as.data.frame

# # test %>% filter(nyears > 1) %>% distinct %>%
# #   group_by(unq) %>%
# #   do({
# #   mod <- lm(count ~ group, data = .)
# #   slope <- mod$coefficients[2]
# #   names(slope) <- NULL
# #   data.frame(., slope)
# # }) %>% as.data.frame -> test

# #Add colors to the slopies based on the magnitude of the slope
# slopies$abs_slope <- abs(slopies$slope)

# # slopies %>% distinct(unq, .keep_all = T) %>% 
# #   filter(abs_slope > 10) %>% nrow
# # nrow(slopies)
# slopies1 <- slopies

# # max_value <- 15
# # slopies1$plot_value <- slopies1$abs_slope

# # slopies1[which(slopies1$abs_slope >= max_value), 'plot_value'] <- max_value

# # slopies1$scaled_value <- round(slopies1$plot_value / max(slopies1$plot_value) * 100, 
# #   digits = 0)
# # greys <- paste0('grey', 100 - slopies1$scaled_value)
# # slopies1$greys <- rgb(t(col2rgb(greys)), maxColorValue = 255)

# # slopies1 <- slopies1 %>% distinct(unq, .keep_all = T)

# spos <- slopies1 %>% filter(slope >= 0)
# sneg <- slopies1 %>% filter(slope < 0)

# #-----------------------------------------------------------------------
# #Changes in effort by latitude and depth
# # dev.new(width = 3.75, height = 5.85)

# #-----------------------------------------------------------------------
# #Run a linear model in each area

# #Expand the map figure to show more of the data






# #-----------------------------------------------------------------------

# #-----------------------------------------------------------------------
# #Try histograms of latitude and longitude by year

# #Expand the values for histograms
# tows_clust_bin$row_index <- 1:nrow(tows_clust_bin)
# exp_tcb <- tows_clust_bin[rep(tows_clust_bin$row_index, tows_clust_bin$count), ]

# exp_tcb %>% ggplot() + geom_histogram(aes(x = x)) + facet_wrap(~ year)
# exp_tcb %>% ggplot() + geom_histogram(aes(x = y)) + facet_wrap(~ year)
# tows_clust_bin[]


# longs <- rep(tows_clust_bin$x, tows_clust_bin$count)



# #-----------------------------------------------------------------------
# #Histograms of latitudes and longitudes
# tows_clust_bin$row_value <- 1:nrow(tows_clust_bin)
# tt_exp <- tows_clust_bin[rep(tows_clust_bin$row_value, tows_clust_bin$count), ]

# long_hist <- hist(tt$x, plot = F, breaks = 20)




# yearz <- 2007:2014

# par(mfrow = c(2, 4), mar = c(0, 0, 0, 0), oma = c(3, 3, 0, 0),
#   mgp = c(0, .5, 0))

# for(ii in 1:8){
#   tt_temp <- tt_exp %>% filter(year == yearz[ii])
#   aa <- hist(tt_temp$x, plot = F, breaks = seq(-126.5, -120, by = .25))
#   barplot(aa$counts, space = 0, ylim = c(0, 6200), axes = F, ann = F)
#   box()

#   #Add median line
#   med_val <- median(tt_temp$x)


#   print(med_val)
#   print(mean(tt_temp$x))

  
#   (median(tt_temp$x) + 124)
  
#   abline(v = abs(median(tt_temp$x) + 124) * 24, las = 2)
#   # abline()
#   # 126 - 124

#   if(ii %in% c(1, 5)) axis(side = 2, las = 2, at = c(0, 1000, 2000, 3000, 4000, 5000))
#   if(ii >= 5) axis(side = 1, at = c(0, 12, 24), 
#     labels = c(126, 124, 121))  
# }

# mtext(side = 1, outer = T, expression("Longitude" ~degree ~ W), line = 2)



# for(ii in 1)
# # par(mar = c(0, 0, 0, 0))
# hist(tt$x, plot = T, breaks = 20, ann = F, axes = F, col = 'gray')


# hist()





# #-----------------------------------------------------------------------


# #Add these

# # mat <- matrix(1:8, 2, 4, byrow = TRUE)
# # layout(mat, widths = rep(1, 8), heights = rep(3, 8))
# mat <- rbind(c(2, 2, 0), c(1, 1, 3), c(1, 1, 3) )
# layout(mat)

# tt <- tows_clust_bin %>% filter(year == 2007)

# #Plot 1
# map('state', fill = FALSE, col = 'white', xlim = c(-126, -120.5), asp = 1.3, ylim = c(34, 49),
#   mar = c(0, 0, 0, 0))
# points(tt$x, tt$y, pch = 15, cex = .65, xlim = c(-126, -120.5), ylim = c(34, 49), col = tt$greys)
# map('state', fill = TRUE, col = 'gray95', xlim = c(-126, -120.5), asp = 1.3, ylim = c(34, 49),
#   mar = c(0, 0, 0, 0), add = T)

# #Plot 2



# # par(mar = c(0, 0, 0, 0))
# barplot(long_hist$mids, long_hist$counts)



# #Plot 3


# box()

#   mtext(paste0(letters[ii], ".", ") ", yrz[ii]), adj = .05, cex = .6, side = 1,
#     line = -1.2, bg = 'white')


# hist(longs)

#  %>% ggplot() + geom_histogram(aes(x = x))

















# # axis(side = 1)
# # axis(side = 2)

# # map('state', fill = TRUE, col = 'white', xlim = c(-128, -120), asp = 1.3, ylim = c(34, 49),
# #     mar = c(0, 0, .5, 0))
# # points(x = -127, y = 41.5, pch = 19)

# #------------------------------
# # #Play with rotating map
# # map('state', fill = TRUE, col = 'white', xlim = c(-128, -120), asp = 1.3, ylim = c(34, 49),
# #     mar = c(0, 0, .5, 0), orientation = c(41.5, -127, 20))
# # box()

# # #------------------------------------------


# # tt <- tows_clust_bin %>% filter(year == 2011)
# # map('state', fill = TRUE, border = 0, col = 'white', xlim = c(-126, -120), asp = 1.3, ylim = c(34, 49),
# #     mar = c(0, 0, .5, 0))
# # points(tt$x, tt$y, pch = 15, cex = .65, xlim = c(-126, -120), ylim = c(34, 49), col = tt$greys,
# #   border = 'white')
# # map('state', fill = TRUE, col = 'gray', xlim = c(-126, -120), asp = 1.3, ylim = c(34, 49),
# #     mar = c(0, 0, .5, 0), add = T)

# # add()



# # plot(tt$x, tt$y, pch = 15, cex = .5)




# # ggplot() + geom_tile(data = ll_binned, aes(x = x, y = y, fill = count)) + 
# #   scale_fill_gradient2(low = 'blue', high = "black") + labs(fill = "# tows") + facet_wrap(~ year, ncol = 4) +
# #   geom_map(data = usa, map = usa, 
# #            aes(x = long, y = lat, map_id = region), fill = 'gray', 
# #            colour = 'gray70') + scale_x_continuous(limits = c(-126, -120)) + 
# #   scale_y_continuous(limits = c(34, 48.2)) + coord_fixed(1.3) + theme_bw() + 
# #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
# #   xlab("Longitude") + ylab("Latitude") + theme(axis.text.x = element_text(angle = 45))


# # ll_binned <- bin_data(data = obs_data %>% distinct(haul_id, .keep_all = T), 
# #                       x_col = "avg_long", y_col = "avg_lat", 
# #                       grid_size = c(.0909, .11), group = "set_year")
# # ll_binned <- ll_binned %>% filter(year >= 2007)
# # ll_binned %>% group_by(year) %>% summarize(ntows = sum(count))

# #Binning works as it should
# # obs_data %>% filter(set_year >= 2007) %>% group_by(set_year) %>% summarize(ntows = length(unique(haul_id)))