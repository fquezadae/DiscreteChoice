#-------------------------------------------------------------------------------------
#Figure 3, look at changes by depth and latitude
tc_unq_hauls <- tows_clust %>% distinct(haul_id, .keep_all = T)

tows_clust_bin_depth <- bin_data(tc_unq_hauls, x_col = 'avg_depth', y_col = 'avg_lat', group = 'set_year', 
  grid_size = c(50, .5),
  group_vec = 2007:2014)
tows_clust_bin_depth$when <- 'before'
tows_clust_bin_depth[which(tows_clust_bin_depth$year >= 2011), 'when'] <- 'after'


#Add columns indicating the number of years and if vessel fished before and after catch shares
tows_clust_bin_depth <- tows_clust_bin_depth %>% group_by(unq) %>% 
  mutate(nyears = length(unique(year)), 
    bef_aft = if_else(length(unique(when)) == 2, TRUE, FALSE))  %>% as.data.frame

tows_clust_bin_depth <- tows_clust_bin_depth %>% arrange(year)

#Only use clusters that had values before and after catch shares
slopies <- tows_clust_bin_depth %>% filter(nyears > 2, bef_aft == TRUE) %>%
  # arrange(year) %>%
  group_by(unq) %>%
  do({
    mod <- lm(count ~ year, data = .)
    slope <- mod$coefficients[2]
    p_val <- summary(mod)[[4]][, 4][2]
    names(slope) <- NULL
    names(p_val) <- NULL
# data.frame(., slope, p_val)    
    data.frame(., slope, p_val)
  }) %>% as.data.frame

#Find the proportion of slopes that had significant changes
slopies$sig <- "no"
slopies[which(slopies$p_val <= 0.05), 'sig'] <- 'yes'
slopies$abs_slope <- abs(slopies$slope)
slopies1 <- slopies

slopies1 <- slopies1 %>% distinct(unq, .keep_all = T)
slopies1$xmin <- round(slopies1$xmin)
slopies1$ymin <- round(slopies1$ymin, digits = 2)

slopies1$x <- slopies1$xmax
slopies1$y <- slopies1$ymax

spos <- slopies1 %>% filter(slope >= 0)
sneg <- slopies1 %>% filter(slope < 0)

#-------------------------------------------------------------------------------------

# spos <- spos %>% arrange(x, y)


#-------------------------------------------------------------------------------------
###Table Values
#Number

#Total number of locations
nlocs <- tows_clust_bin_depth %>% distinct(unq, .keep_all = T) %>% select(unq) %>% nrow

#Locations with not enough years or not fished before/after catch shares
not_enough <- tows_clust_bin_depth %>% distinct(unq, .keep_all = T) %>%
  filter(bef_aft == FALSE, nyears < 3) %>% select(unq) %>% nrow
not_enough / nlocs; nlocs

#------Increases
#Percentage of increases
perc_increase <- slopies %>% filter(slope > 0) %>% distinct(unq) %>% nrow / nlocs
perc_increase <- round(perc_increase, digits = 2)

#Significant increases
slopies %>% filter(sig == 'yes', slope > 0) %>% distinct(unq) %>% nrow / nlocs
perc_increase_sig <- round(slopies %>% filter(sig == 'yes', slope > 0) %>% 
    distinct(unq) %>% nrow / nlocs, digits = 2)

#------Decreases
#Percentage of increases
perc_decrease <- round(slopies %>% filter(slope < 0) %>% distinct(unq) %>% nrow / nlocs, digits = 2)

#Significant increases
perc_decrease_sig <- round(slopies %>% 
  filter(sig == 'yes', slope < 0) %>% distinct(unq) %>% nrow / nlocs, digits = 2)

ss <- slopies %>% distinct(abs_slope) 
quantile(ss$abs_slope, .95)
length(which(ss >= 20)) / nrow(ss)
  
quantile(ss$abs_slope, 20)

#-------------------------------------------------------------------------------------
mv <- 50
lev <- 10
#The actual plot
png(width = 7, height = 7, res = 200, units = 'in', file = 'figs/ch4_fig3.png')
ch4_fig3(mv = 50, lev = 20)
dev.off()


#Scratch stuff

length(seq(0, 700, by = 50)) #The x limits
length(seq(34, 49, .5)) #the y limits for first two panels
pretty(c(34, 49))

#-------------------------------------------------------------------------------------
# ch4_fig3(mv = 10, lev = 20)
format_fc_plot(sneg, max_value = 50, the_levels = 10, xlims = c(1, 15),
    ylims = c(1, 31), xint = 1, yint = 1, flip_x_axis = TRUE)

x1 <- data.frame(depth = seq(0, 700, 50), xbin = 1:length(seq(0, 700, 50)) )
x1$xbin1 <- x1$xbin - 1
x1$xbin_rev <- rev(x1$xbin)

y1 <- data.frame(lat = seq(34, 49, .5), ybin = 1:length(seq(34, 49, .5)))
y1$ybin1 <- y1$ybin - 1

length(seq(0, 700, 50))
length(seq(34, 49, by = .5))
xx <- 1:15
yy <- 1:31

expand.grid(xx, yy)
zz <- matrix(1:50, nrow = length(xx), ncol = length(yy))
image(xx, yy, zz)

#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
format_fc_plot <- function(input, max_value = 10, the_levels = 10, 
  xlims = c(1, 15), ylims = c(1, 31), flip_x_axis = TRUE, xint = 25, yint = .25){  
  input$x <- input$xbin 
  input$y <- input$ybin

  if(flip_x_axis == T){
    input$x <- xlims[2] - input$x
  }

  #Scale the slope values to the plotted value
  input$plot_value <- input$abs_slope
  input[which(input$abs_slope >= max_value), 'plot_value'] <- max_value
  
  xx <- seq(xlims[1], xlims[2], by = 1)
  yy <- seq(ylims[1], ylims[2], by = 1)

  pos <- expand.grid(xx, yy)
  pos <- data.frame(x = pos[, 1], y = pos[, 2])

  pos1 <- pos %>% left_join(input %>% select(x, y, abs_slope, plot_value), by = c('x', 'y'),
    fill = 0)
  
  na_inds <- is.na(pos1$abs_slope)
  pos1[na_inds, 'abs_slope'] <- 0
  pos1[na_inds, 'plot_value'] <- 0
  
  zz <- matrix(pos1$plot_value, nrow = length(xx), ncol = length(yy))
# browser()  
  image(xx + .5, yy, zz, col = grey.colors(n = the_levels, start = 1, end = 0), ann = F,
    axes = F, xlim = xlims, ylim = ylims, bg = 'black')
  
}


# ch4_fig3(mv = 20, lev = 20)


color_bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), 
  tick_labs, title='', Cex = .5) {
    # browser()
    scale = (length(lut)-1)/(max-min)
# scale = (length(lut)1)/(max-min)
    par(mgp = c(0, .5, 0))

    plot(c(0,10), c(min,max), type='n', xaxt='n', xlab='', yaxt='n', ylab="", main="", 
      cex.lab=1.5, mgp = c(0, .05, 0), bg = 'white', bty = 'n')
    axis(2, at = ticks, labels = tick_labs, las=1, cex = Cex)
    mtext(side = 3, cex = Cex * 2.5, text = title, adj = .75, line = .75)
    for (i in 1:(length(lut)-1)) {
     y = (i-1)/scale + min
     rect(0,y,10,y+1/scale, col=lut[i], border=NA)
    }
}

# ch4_fig3(mv = 20, lev = 20)
#Need to add color bar

#-------------------------------------------------------------------------------------
# Functions Used


axis(side = 2, at = c(1, 5, 9, 13, 17, 21, 25, 29), seq(34, 48, by = 2))
axis(side = 1, at = c(15, 11, 7, 3), labels = (c(0, 200, 400, 600)))

ch4_fig3 <- function(mv, lev){

  par(mfcol = c(1, 3), mar = c(0, 0, 0, 0), oma = c(3.5, 3.5, 1, 0), mgp = c(0, .5, 0))

  #-------------------------------------------------------------------------------------
  #Positive Slopes
  # format_fc_plot(spos, max_value = mv, the_levels = lev, xlims = c(0, 700),
  #   ylims = c(34, 49), xint = 25)
  format_fc_plot(spos, max_value = mv, the_levels = lev, xlims = c(1, 15),
    ylims = c(1, 31), xint = 1, yint = 1)
  box()
  mtext(paste0(letters[1], ") ", perc_increase * 100, "% positive"), side = 3, line = -1.5, adj = .03, cex = .8)
  mtext(paste0("      ", 100 * perc_increase_sig, "% significant"), side = 3, line = -2.75, adj = .03, cex = .8)
  mtext(paste0("      n = ", nlocs), side = 3, line = -4, adj = .03, cex = .8)

# axis1 <- cbind(seq(0, 700, 50), 0:14, 14:0)  
# axis2 <- cbind(seq(34, 49, .5), 0:(length(seq(34, 49, .5)) - 1))  
  axis(side = 1, at = c(15, 11, 7, 3), labels = (c(0, 200, 400, 600)), cex.axis = 1.2)
  # axis(side = 1, at = c(2, 6, 10, 15), labels = c(600, 400, 200, 0), cex.axis = 1.2)
  # axis(side = 1, at = c(100, 300, 500, 700), labels = c(600, 400, 200, 0), cex.axis = 1.2)
  # axis(side = 2, las = 2, cex.axis = 1.2)
  axis(side = 2, at = c(1, 5, 9, 13, 17, 21, 25, 29), seq(34, 48, by = 2), las = 2, cex.axis = 1.2, mgp = c(0, .5, 0))
  mtext(side = 2,  expression("Latitude" ~degree ~ N), outer = T, line = 1.7, cex = 1)
  
  #-------------------------------------------------------------------------------------
  #Negative slopes
  # format_fc_plot(sneg, max_value = mv, the_levels = lev, xlims = c(0, 700), ylims = c(34, 49))
  format_fc_plot(sneg, max_value = mv, the_levels = lev, xlims = c(1, 15),
    ylims = c(0, 31), xint = 1, yint = 1)
  box()
  axis(side = 1, at = c(15, 11, 7, 3), labels = (c(0, 200, 400, 600)), cex.axis = 1.2)
  # axis(side = 1, at = c(100, 300, 500, 700), labels = c(600, 400, 200, 0), cex.axis = 1.2)
  mtext(paste0(letters[2], ") ", perc_decrease * 100, "% negative"), side = 3, line = -1.5, adj = .03, cex = .8)
  mtext(paste0("    ", 100 * perc_decrease_sig, "% significant"), side = 3, line = -2.75, adj = .03, cex = .8)
  mtext(paste0("      n = ", nlocs), side = 3, line = -4, adj = .03, cex = .8)
  # mtext(paste0(letters[2], ")", " Negative Slopes"), side = 3, line = -1.5, adj = .03, cex = .8)
  # mtext(paste0("n = ", nrow(sneg) ), side = 3, line = -2.75, adj = .03, cex = .8)
  
  #-------------------------------------------------------------------------------------
  #Map
  map('state', fill = TRUE, col = 'gray95', xlim = c(-126, -120.5), asp = 1.3, ylim = c(34, 49),
      mar = c(0, 0, 0, 0))
  box()
  mtext(paste0(letters[3], ")"), side = 3, line = -1.5, adj = .03, cex = .8)
  mtext(side = 1, outer = T, "Depth (fathoms)", adj = .3, line = 2)
# browser()
  par(mar = c(0, .5, 0, 0), fig = c(.75, 0.78, 0.02, .17), new = T)  
  color_bar(lut = grey.colors(n = lev, start = 1, end = 0), 
    Cex = .3, nticks = 6, min = 0, max = 1, tick_labs = c(0, 2, 4,
      6, 8,">=10"), title = "Magnitude")
  
}




# filled.contour2 <-
#   function (x = seq(0, 1, length.out = nrow(z)),
#             y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
#             ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
#             levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
#             col = color.palette(length(levels) - 1), plot.title, plot.axes, 
#             key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
#             axes = TRUE, frame.plot = axes,mar, ...) 
# {
#   # modification by Ian Taylor of the filled.contour function
#   # to remove the key and facilitate overplotting with contour()
#   # further modified by Carey McGilliard and Bridget Ferris
#   # to allow multiple plots on one page

#   if (missing(z)) {
#     if (!missing(x)) {
#       if (is.list(x)) {
#         z <- x$z
#         y <- x$y
#         x <- x$x
#       }
#       else {
#         z <- x
#         x <- seq.int(0, 1, length.out = nrow(z))
#       }
#     }
#     else stop("no 'z' matrix specified")
#   }
#   else if (is.list(x)) {
#     y <- x$y
#     x <- x$x
#   }
#   if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
#     stop("increasing 'x' and 'y' values expected")
#  # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
#  # on.exit(par(par.orig))
#  # w <- (3 + mar.orig[2]) * par("csi") * 2.54
#  # par(las = las)
#  # mar <- mar.orig

#  plot.new()
#  # par(mar=mar)
#   plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
#   if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
#     stop("no proper 'z' matrix specified")
#   if (!is.double(z)) 
#     storage.mode(z) <- "double"

#   .filled.contour(x, y , z, levels, col)
#   #22222222
#   # .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
#   #                         col = col))
#   if (missing(plot.axes)) {
#     if (axes) {
#       title(main = "", xlab = "", ylab = "")
#       Axis(x, side = 1)
#       Axis(y, side = 2)
#     }
#   }
#   else plot.axes
#   if (frame.plot) 
#     box()
#   if (missing(plot.title)) 
#     title(...)
#   else plot.title
#   invisible()
# }