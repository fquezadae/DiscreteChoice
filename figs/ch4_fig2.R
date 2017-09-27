#------------------------------------------------------------------------
#Figure 2
#------------------------------------------------------------------------
#Format the Data
load("/Volumes/udrive/tows_clust_921.Rdata")

tc_unq_hauls <- tows_clust %>% distinct(haul_id, .keep_all = T)



tows_clust_bin_depth <- bin_data(tc_unq_hauls, x_col = 'avg_depth', y_col = 'avg_lat', group = 'set_year', 
  grid_size = c(25, .25),
  group_vec = 2007:2014)


slopies <- tows_clust_bin_depth %>% group_by(unq) %>% mutate(nyears = length(unique(year))) %>%
  filter(nyears > 1) %>% arrange(year) %>%
  group_by(unq) %>%
  do({
    mod <- lm(count ~ year, data = .)
    slope <- mod$coefficients[2]
    names(slope) <- NULL
    data.frame(., slope)
  }) %>% as.data.frame

slopies$abs_slope <- abs(slopies$slope)
slopies1 <- slopies
spos <- slopies1 %>% filter(slope >= 0)
sneg <- slopies1 %>% filter(slope < 0)

# max_value <- 15
slopies1$plot_value <- slopies1$abs_slope

slopies1[which(slopies1$abs_slope >= max_value), 'plot_value'] <- max_value

slopies1$scaled_value <- round(slopies1$plot_value / max(slopies1$plot_value) * 100, 
  digits = 0)
greys <- paste0('grey', 100 - slopies1$scaled_value)
slopies1$greys <- rgb(t(col2rgb(greys)), maxColorValue = 255)

slopies1 <- slopies1 %>% distinct(unq, .keep_all = T)
slopies1$xmin <- round(slopies1$xmin)

spos <- slopies1 %>% filter(slope >= 0)
sneg <- slopies1 %>% filter(slope < 0)
spos$xmin <- round(spos$xmin)


png(width = 7, height = 7, res = 200, units = 'in', file = 'figs/ch4_fig2.png')
ch4_fig2(mv = 20, lev = 20)
dev.off()

#------------------------------------------------------------------------
#All the functions used
ch4_fig2 <- function(mv, lev){
  par(mfcol = c(1, 3), mar = c(0, 0, 0, 0), oma = c(3.5, 3.5, 1, 0), mgp = c(0, .5, 0))
  
  #-------------------------------------------------------------------------------------
  #Positive Slopes
  format_fc_plot(spos, max_value = mv, the_levels = lev, xlims = c(0, 700),
    ylims = c(34, 49))
  box()
  mtext(paste0(letters[1], ")", " Positive Slopes"), side = 3, line = -1.5, adj = .03, cex = .8)
  mtext(paste0("n = ", nrow(spos) ), side = 3, line = -2.75, adj = .03, cex = .8)
  axis(side = 1, at = c(100, 300, 500, 700), labels = c(600, 400, 200, 0), cex.axis = 1.2)
  axis(side = 2, las = 2, cex.axis = 1.2)
  mtext(side = 2,  expression("Latitude" ~degree ~ N), outer = T, line = 1.7, cex = 1)
  
  #-------------------------------------------------------------------------------------
  #Negative slopes
  format_fc_plot(sneg, max_value = mv, the_levels = lev, xlims = c(0, 700), ylims = c(34, 49))
  box()
  axis(side = 1, at = c(100, 300, 500, 700), labels = c(600, 400, 200, 0), cex.axis = 1.2)
  mtext(paste0(letters[2], ")", " Negative Slopes"), side = 3, line = -1.5, adj = .03, cex = .8)
  mtext(paste0("n = ", nrow(sneg) ), side = 3, line = -2.75, adj = .03, cex = .8)
  
  #-------------------------------------------------------------------------------------
  #Map
  map('state', fill = TRUE, col = 'gray95', xlim = c(-126, -120.5), asp = 1.3, ylim = c(34, 49),
      mar = c(0, 0, 0, 0))
  box()
  mtext(paste0(letters[3], ")"), side = 3, line = -1.5, adj = .03, cex = .8)
  mtext(side = 1, outer = T, "Depth (fathoms)", adj = .3, line = 2)
}





# unique(spos$xmin)
# unique(spos$ymin)

#Need to flip the x axis

format_fc_plot <- function(input, max_value = 10, the_levels = 10, 
  xlims = c(0, 700), ylims = c(34, 49), flip_x_axis = TRUE, xint = 25, yint = .25){  
  
  #Flip x axis values
  input$x <- input$xmin
  input$y <- input$ymin

  if(flip_x_axis == T){
    input$x <- xlims[2] - input$x
  }

  #Scale the slope values to the plotted value
  input$plot_value <- input$abs_slope
  input[which(input$abs_slope >= max_value), 'plot_value'] <- max_value
  
  #Format matrices for filled_contour plot
  input$x <- round(input$x)
  input$y <- round(input$y, digits = 3)
  
  xx <- seq(xlims[1], xlims[2], by = xint)
  yy <- seq(ylims[1], ylims[2], by = yint)
  
  pos <- expand.grid(xx, yy)
  pos <- data.frame(x = pos[, 1], y = pos[, 2])
  pos$x <- round(pos$x, digits = 0)
  pos$y <- round(pos$y, digits = 3)
    
  pos1 <- pos %>% left_join(input %>% select(x, y, abs_slope, plot_value, greys), by = c('x', 'y'),
    fill = 0)
  
  na_inds <- is.na(pos1$abs_slope)
  pos1[na_inds, 'abs_slope'] <- 0
  pos1[na_inds, 'plot_value'] <- 0
  pos1[na_inds, 'greys'] <- 'white'

  zz <- matrix(pos1$plot_value, nrow = length(xx), ncol = length(yy))
  
  filled.contour2(xx, yy, zz, nlevels = the_levels, 
    col = grey.colors(n = the_levels , start = 1, end = 0), ann = F,
    axes = F, ylim = ylims, xlim = xlims )
  
}



filled.contour2 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...) 
{
  # modification by Ian Taylor of the filled.contour function
  # to remove the key and facilitate overplotting with contour()
  # further modified by Carey McGilliard and Bridget Ferris
  # to allow multiple plots on one page

  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
 # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
 # on.exit(par(par.orig))
 # w <- (3 + mar.orig[2]) * par("csi") * 2.54
 # par(las = las)
 # mar <- mar.orig

 plot.new()
 # par(mar=mar)
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
    stop("no proper 'z' matrix specified")
  if (!is.double(z)) 
    storage.mode(z) <- "double"

  .filled.contour(x, y , z, levels, col)
  #22222222
  # .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
  #                         col = col))
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) 
    box()
  if (missing(plot.title)) 
    title(...)
  else plot.title
  invisible()
}



# str(spos$y)
# str(pos$y)


# pos %>% filter(x == 25, y == 40.755)

# pos$zz <- pos %>% 

# spos
# head(spos)

# expand.grid(range(spos$x))
# range(spos$x)

# range(spos$y)


# max_value <- 5
# slopies1$plot_value <- slopies1$abs_slope

# slopies1[which(slopies1$abs_slope >= max_value), 'plot_value'] <- max_value

# slopies1$scaled_value <- round(slopies1$plot_value / max(slopies1$plot_value) * 100, 
#   digits = 0)
# greys <- paste0('grey', 100 - slopies1$scaled_value)
# slopies1$greys <- rgb(t(col2rgb(greys)), maxColorValue = 255)

# slopies1 <- slopies1 %>% distinct(unq, .keep_all = T)

# plot(-slopies1$x, slopies1$y, 
#   col = slopies1$greys, pch = 15, cex = 1)

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

# # dev.new(width = 4.68, height = 7)
# png(width = 3.8, height = 6.6, res = 200, units = 'in',
#   file = 'figs/ch4_fig2.png')

# # mat <- matrix(1:8, 2, 4, byrow = TRUE)
# # layout(mat, widths = rep(1, 8), heights = rep(3, 8))

# par(mar = c(0, 0, 0, 0), oma = c(3, 3, 0, 0.25), mfrow = c(2, 4))
# yrz <- 2007:2014

# for(ii in 1:8){
#   tt <- tows_clust_bin %>% filter(year == yrz[ii])
#   map('state', fill = FALSE, col = 'white', xlim = c(-126, -120.5), asp = 1.3, ylim = c(34, 49),
#     mar = c(0, 0, 0, 0))
#   points(tt$x, tt$y, pch = 15, cex = .65, xlim = c(-126, -120.5), ylim = c(34, 49), col = tt$greys)
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
# rect(xleft = -122.9, ybottom = 42.75, xright = -120.55, ytop = 48, col = 'white', border = 'black')

# par(mar = c(0, 0, 0, 0), fig = c(.97, 0.99, 0.3, .45), new = T)  
# color_bar(lut = gg$greys, Cex = .3, nticks = 6 , min = 0, max = 1, tick_labs = c("0", "50", 
#   "100", "150", "200", '250'))
# mtext(side = 1,  expression("Longitude" ~degree ~ W), outer = T, line = 2, cex = .9)
# mtext(side = 2,  expression("Latitude" ~degree ~ N), outer = T, line = 1.5, cex = .9)

# dev.off()

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