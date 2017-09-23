#------------------------------------------------------------------------
#Figure 2
load("/Volumes/udrive/tows_clust_921.Rdata")

tc_unq_hauls <- tows_clust %>% distinct(haul_id, .keep_all = T)

tows_clust_bin <- bin_data(tc_unq_hauls, x_col = 'avg_long', y_col = 'avg_lat', group = 'set_year', grid_size = c(.0909, .11),
  group_vec = 2007:2014)


# load("/Volumes/udrive/comb_data_bin.Rdata")
# tows_clust_bin <- comb_data_bin

usa <- map_data('state')

#Add Color scale based on counts

#------------------------------------------------------------------------
#Scale colors to provide more contrast rather than white and black

#How to pick values
length(which(tows_clust_bin$count >= 2000)) / nrow(tows_clust_bin)

quantile(tows_clust_bin$count)

tows_clust_bin$plot_value <- tows_clust_bin$value

max_value <- 250
tows_clust_bin[which(tows_clust_bin$plot_value >= max_value), 'plot_value'] <- max_value
tows_clust_bin$scaled_value <- round(tows_clust_bin$plot_value / max(tows_clust_bin$plot_value) * 100, 
  digits = 0)
greys <- paste0('grey', 100 - tows_clust_bin$scaled_value)
tows_clust_bin$greys <- rgb(t(col2rgb(greys)), maxColorValue = 255)


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


#Get distinct grey values
gg <- tows_clust_bin %>% 
  distinct(scaled_value, .keep_all = T) %>% 
  arrange(scaled_value) %>% select(greys)
#-----------------------------------------------------------------------

# dev.new(width = 4.68, height = 7)
png(width = 3.8, height = 6.6, res = 200, units = 'in',
  file = 'figs/ch4_fig2.png')

# mat <- matrix(1:8, 2, 4, byrow = TRUE)
# layout(mat, widths = rep(1, 8), heights = rep(3, 8))

par(mar = c(0, 0, 0, 0), oma = c(3, 3, 0, 0), mfrow = c(2, 4))
yrz <- 2007:2014

for(ii in 1:8){
  tt <- tows_clust_bin %>% filter(year == yrz[ii])
  map('state', fill = FALSE, col = 'white', xlim = c(-126, -120.5), asp = 1.3, ylim = c(34, 49),
    mar = c(0, 0, 0, 0))
  points(tt$x, tt$y, pch = 15, cex = .65, xlim = c(-126, -120.5), ylim = c(34, 49), col = tt$greys)
  map('state', fill = TRUE, col = 'gray95', xlim = c(-126, -120.5), asp = 1.3, ylim = c(34, 49),
    mar = c(0, 0, 0, 0), add = T)
  box()
  
  # legend('topleft', paste0(letters[ii], ".", ") ", yrz[ii]), 
  #   box.col = "white", bg = "white", adj = 0.05, cex = .5, cex.text = .75)
  
  # mtext(paste0(letters[ii], ".", ") ", yrz[ii]), adj = .05, cex = .65, side = 3,
  #   line = -1.2, bg = 'white')
  mtext(paste0(letters[ii], ".", ") ", yrz[ii]), adj = .05, cex = .75, side = 1,
    line = -1.2, bg = 'white')
}

#Add Color Bar
rect(xleft = -122.9, ybottom = 42.75, xright = -120.55, ytop = 47.8, col = 'white', border = 'black')

par(mar = c(0, 0, 0, 0), fig = c(.97, 0.99, 0.3, .45), new = T)  
color_bar(lut = gg$greys, Cex = .5, nticks = 6 , min = 0, max = 1, tick_labs = c("0", "50", 
  "100", "150", "200", '250'))
      
dev.off()


# axis(side = 1)
# axis(side = 2)

# map('state', fill = TRUE, col = 'white', xlim = c(-128, -120), asp = 1.3, ylim = c(34, 49),
#     mar = c(0, 0, .5, 0))
# points(x = -127, y = 41.5, pch = 19)

#------------------------------
# #Play with rotating map
# map('state', fill = TRUE, col = 'white', xlim = c(-128, -120), asp = 1.3, ylim = c(34, 49),
#     mar = c(0, 0, .5, 0), orientation = c(41.5, -127, 20))
# box()

# #------------------------------------------


# tt <- tows_clust_bin %>% filter(year == 2011)
# map('state', fill = TRUE, border = 0, col = 'white', xlim = c(-126, -120), asp = 1.3, ylim = c(34, 49),
#     mar = c(0, 0, .5, 0))
# points(tt$x, tt$y, pch = 15, cex = .65, xlim = c(-126, -120), ylim = c(34, 49), col = tt$greys,
#   border = 'white')
# map('state', fill = TRUE, col = 'gray', xlim = c(-126, -120), asp = 1.3, ylim = c(34, 49),
#     mar = c(0, 0, .5, 0), add = T)

# add()



# plot(tt$x, tt$y, pch = 15, cex = .5)




# ggplot() + geom_tile(data = ll_binned, aes(x = x, y = y, fill = count)) + 
#   scale_fill_gradient2(low = 'blue', high = "black") + labs(fill = "# tows") + facet_wrap(~ year, ncol = 4) +
#   geom_map(data = usa, map = usa, 
#            aes(x = long, y = lat, map_id = region), fill = 'gray', 
#            colour = 'gray70') + scale_x_continuous(limits = c(-126, -120)) + 
#   scale_y_continuous(limits = c(34, 48.2)) + coord_fixed(1.3) + theme_bw() + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#   xlab("Longitude") + ylab("Latitude") + theme(axis.text.x = element_text(angle = 45))


# ll_binned <- bin_data(data = obs_data %>% distinct(haul_id, .keep_all = T), 
#                       x_col = "avg_long", y_col = "avg_lat", 
#                       grid_size = c(.0909, .11), group = "set_year")
# ll_binned <- ll_binned %>% filter(year >= 2007)
# ll_binned %>% group_by(year) %>% summarize(ntows = sum(count))

#Binning works as it should
# obs_data %>% filter(set_year >= 2007) %>% group_by(set_year) %>% summarize(ntows = length(unique(haul_id)))