#-------------------------------------------------------------------------------------
#Figure 2, look at changes by depth and latitude
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

#Convert year to index
year_index <- data_frame(year = 2007:2014, xx = 1:8)
tows_clust_bin_depth <- tows_clust_bin_depth %>% left_join(year_index, by = 'year')

#Only use clusters that had values before and after catch shares
slopies <- tows_clust_bin_depth %>% filter(nyears > 2, bef_aft == TRUE) %>%
  # arrange(year) %>%
  group_by(unq) %>%
  do({
    mod <- lm(count ~ xx , data = .)
    slope <- mod$coefficients[2]
    p_val <- summary(mod)[[4]][, 4][2]
    names(slope) <- NULL
    names(p_val) <- NULL
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

#Assign colors to slopies1
length(unique(slopies1$x)[order(unique(slopies1$x))])
length(unique(slopies1$y)[order(unique(slopies1$y))])

#-------------------------------------------------------------------------------------
#compare the slopies
# round(slopies1$slope, digits = 4) == round(slopies_year$slope, digits = 4)

#-------------------------------------------------------------------------------------
###Table Values
#Number
14 * 29

#Total number of locations
nlocs <- tows_clust_bin_depth %>% distinct(unq, .keep_all = T) %>% select(unq) %>% nrow
length(unique(tows_clust_bin_depth$x))
length(unique(tows_clust_bin_depth$y))

nlocs <- 406
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

# perc_increase
# perc_decrease 
# quantile(ss$abs_slope, .95)
# length(which(ss >= 20)) / nrow(ss)
  
# quantile(ss$abs_slope, 20)
 
ss <- slopies %>% distinct(abs_slope) 

#-------------------------------------------------------------------------------------
#The actual plot
png(width = 7, height = 7, res = 200, units = 'in', file = 'figs/ch4_fig2.png')
ch4_fig2(mv = 50, lev = 20)
dev.off()

#Scratch stuff
length(seq(0, 700, by = 50)) #The x limits
length(seq(34, 49, .5)) #the y limits for first two panels
pretty(c(34, 49))

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
  pos1[na_inds, 'plot_value'] <- -5
  
  zz <- matrix(pos1$plot_value, nrow = length(xx), ncol = length(yy))
  shadez <- seq(0, 1, length.out = the_levels)
  
  if(mean(input$slope) >= 0){colz <- sapply(shadez, 
    FUN = function(xx) adjustcolor('red', alpha.f = xx))}
  
  if(mean(input$slope) <= 0){colz <- sapply(shadez, 
    FUN = function(xx) adjustcolor('blue', alpha.f = xx))}

  return(list(xx = xx, yy = yy, zz = zz, colz = colz, pos1 = pos1))
}


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

#Need to add color bar
#-------------------------------------------------------------------------------------
# Functions Used

ch4_fig2 <- function(mv, lev){
  par(mfcol = c(1, 2), mar = c(0, 0, 0, 0), oma = c(3.5, 3.5, 1, 0), mgp = c(0, .5, 0))

  #-------------------------------------------------------------------------------------
  #Positive Slopes
  ppos <- format_fc_plot(spos, max_value = mv, the_levels = lev, xlims = c(1, 15),
    ylims = c(1, 31), xint = 1, yint = 1)
  image(ppos$xx + .5, ppos$yy, ppos$zz, col = ppos$colz, ann = F, axes = F, 
    xlim = c(1, 15), ylim = c(1, 31) )
  
  #Negative Slopes
  nneg <- format_fc_plot(sneg, max_value = mv, the_levels = lev, xlims = c(1, 15),
    ylims = c(1, 31), xint = 1, yint = 1)
  image(nneg$xx + .5, nneg$yy, nneg$zz, col = nneg$colz, ann = F, axes = F, 
    xlim = c(1, 15), ylim = c(1, 31), add = T)

  #Add points
  names(ppos$pos1)[4] <- 'pos_slope'
  names(nneg$pos1)[4] <- 'neg_slope'
  ptz <- ppos$pos1 %>% left_join(nneg$pos1, by = c('x', 'y')) 
  # ptz <- ppos$pos1 
  ptz$miss <- 'no'
  ptz[which(ptz$pos_slope == -5 & ptz$neg_slope == -5), 'miss'] <- 'yes'
  # ptz[which(ptz$pos_slope == -5), 'miss'] <- 'yes'
  ptz <- ptz %>% filter(miss == 'yes')
  points(ptz$x + .5, ptz$y, pch = '.')

  box()

  #In fathoms
  # axis(side = 1, at = c(15, 11, 7, 3), labels = (c(0, 200, 400, 600)), cex.axis = 1)
  
  #In meters
  axis(side = 1, at = c(15, 11, 7, 3), labels = (c(0, 366, 732, 1098)), cex.axis = 1)

  ylabels <- c(expression("34"*degree*N),
               expression("36"*degree*N),
               expression("38"*degree*N),
               expression("40"*degree*N),
               expression("42"*degree*N),
               expression("44"*degree*N),
               expression("46"*degree*N),
               expression("48"*degree*N))
  
  axis(side = 2, at = c(1, 5, 9, 13, 17, 21, 25, 29), seq(34, 48, by = 2), las = 2, cex.axis = 1, mgp = c(0, .5, 0), 
    labels = ylabels)
  # mtext(side = 1, outer = T, "Depth (fathoms)", adj = .3, line = 2, cex = 1.3)
  mtext(side = 1, outer = T, "Depth (meters)", adj = .3, line = 2, cex = 1.3)

  #-------------------------------------------------------------------------------------
  #Add Map
  map('state', fill = TRUE, col = 'gray95', xlim = c(-126, -120.5), asp = 1.3, ylim = c(34, 49),
      mar = c(0, 0, 0, 0))
  box()

  par(mar = c(0, .5, 0, 0), fig = c(.66, 0.71, 0.02, .32), new = T)  
  blues <- rev(sapply(seq(0, 1, length.out = lev), FUN = function(xx) adjustcolor('blue', xx)))
  reds <- sapply(seq(0, 1, length.out = lev), FUN = function(xx) adjustcolor('red', xx))

  color_bar(lut = c(blues, reds), 
    Cex = .3, nticks = 11, min = 0, max = 1, 
    # tick_labs = c(0, 10, 20,
    #   30, 40, 50, 60, 70, 80, 90, 100), 
    tick_labs = c(-50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50),
    title = "")
}
