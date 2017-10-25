#------------------------------------------------------------------------------------------------------
# dev.new(width = 8.95, height = 8.43)
# 
png(width = 8.95, height = 8.43, units = 'in', file = 'figs/ch4_sig_fig.png', res = 250)
# tiff(width = 8.75, height = 8.34, units = 'in', filename = 'figs/ch4_sig_fig.tiff', res = 200)
# pdf(width = 8.75, height = 8.34, file = 'figs/ch4_sig_fig.pdf')
# dev.new(width = 8.75, height = 8.34)

matlay <- matrix(c( 1,  2, 0,  3,  4,  5,  6, 
                    7,  8, 0,   9, 10, 11, 12,
                   13, 14, 0, 15, 16, 17, 18,
                   19, 20, 0, 21, 22, 23, 24, 
                   25, 26, 0, 27, 28, 29, 30,
                   31, 32, 0, 33, 34, 35, 36), ncol = 7, byrow = T)

layout(matlay, widths = c(lcm(2.5), lcm(2.5), lcm(.25), rep(lcm(2.5), 4)), 
  heights = lcm(2.75 * c(1.75, 1.25, 1, 1, 1, 1)))

#no color for no significance, 
#color for significance at .05
#point on top of color for very significant coefficients

#------------------------------------------------------------------------------------------------------
par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0)) #small numbers

for(pp in 6:1){  
  plot_dat <- port_plot[[pp]]
  temp_bin <- port_bins %>% filter(fleet_name_comb %in% plot_dat$port_name)
  port_locz <- some_port_locs[[pp]]
  yrects <- seq(plot_dat$ylims[1], plot_dat$ylims[1] + 2, length.out = 8)
  ylabels <- parse(text = paste(plot_dat$ylabs, "~degree~N", sep = " "))

# axis(side = 2, at = plot_dat$ylabs, labels = ylabels, las = 2)

  for(yy in 1:length(yrz)){  
    par(mar = c(0, 0, 0, 0))
    yr_temp <- temp_bin %>% filter(year == yrz[yy])
    
    map('state', fill = FALSE, col = 'white', xlim = plot_dat$xlims,
      ylim = plot_dat$ylims, mar = c(0, 0, 1, 0), asp = 1.3)
    points(yr_temp$x, yr_temp$y, pch = 15, cex = .6, col = yr_temp$greys)
    map('state', fill = TRUE, col = 'gray95', xlim = plot_dat$xlims,
      ylim = plot_dat$ylims, mar = c(0, 0, 1, 0), add = T, asp = 1.3 )
    points(port_locz$d_port_long, port_locz$d_port_lat, pch = 19, col = 'red')
    box()
    mtext(side = 3, text = paste0(plot_dat$letts[yy], ")"), adj = 0.02, line = -1,
      cex = .8)
    
    #-----Add coefficient significance
    rect(xleft = -123.32, xright = -123, ybottom = plot_dat$ylims[1], ytop = plot_dat$ylims[1] + 2, 
      col = 'white', border = FALSE)
    
    #Add rectangles
    for(fc in 1:7){
      points(x = -123.16, y = mean(c(yrects[fc], yrects[fc + 1])), 
        pch = plot_dat$coefs_pch[fc, yy + 2], 
        col = plot_dat$coefs_colors[fc, yy + 2], cex = 2)  
    }
    
    # for(fc in 1:7){
    #   rect(xleft = -123.32, xright = -123, ybottom = yrects[fc], 
    #     ytop = yrects[fc + 1], col = plot_dat$coefs_colors[fc, yy + 2], border = FALSE)  
    # }

    #Add points to significant and nonsignificant points
    ypoints <- yrects[1:7] + diff(yrects)/2
    # points(x = rep(-123.16, 7), y = ypoints, pch = plot_dat$coefs_point)
    # points(x = rep(-123.16, 7), y = ypoints, pch = (plot_dat$coefs_pch[, yy + 2]), 
    #   col = plot_dat$base_colors[, yy + 2])

    #Add black points if very significant
    ptz <- plot_dat$coefs_point[, c(1, 2, (yy + 2))]
    ptz$pch <- NA
    ptz[which(ptz[, 3] == 'yes'), 4] <- 19
    
    points(x = rep(-123.16, 7), y = ypoints, pch = ptz[, 4], cex = .6, col = 'white')

    #-----Add Axes
    if(yy == 1 & pp != 6){
      axis(side = 2, las = 2, mgp = c(0, .5, 0), at = plot_dat$ylabs, labels = 
        ylabels)
      mtext(side = 3, unique(port_locz$name), line = 0, adj = 0)
    } 
    
    if(pp == 6 & yy == 1){
      # paste0("'", substr(yrz[yy], 3, 4))
      # mtext(side = 3, paste0("'", substr(yrz[yy], 3, 4)), adj = 0, outer = F)
      axis(side = 2, las = 2, mgp = c(0, .5, 0), at = plot_dat$ylabs, labels = ylabels)
      mtext(side = 3, yrz[yy], adj = 1, outer = F)
      mtext(side = 3, unique(port_locz$name), line = 0, adj = 0)
      
    }
    if(pp == 6 & yy == 3) mtext(side = 3, line = 1.2, "After", adj = 0, cex = 1.2)
    if(pp == 6 & yy == 2){
      mtext(side = 3, yrz[yy], adj = 1, outer = F)
      mtext(side = 3, line = 1.2, "Before", adj = 1, cex = 1.2)
    }

    if(pp == 6 & yy >= 3){
      mtext(side = 3, yrz[yy], adj = 0, outer = F)
    }

    
    
xlabels <- c(expression("126"~degree ~ W),
             expression("125"~degree ~ W),
             expression("124"~degree ~ W))
  
    if(pp == 1){
      axis(side = 1, mgp = c(0, .5, 0), at = c(-126, -125, -124), 
        labels = xlabels, las = 3)
    } 
    if(yy == 6){
      axis(side = 4, las = 2, mgp = c(0, .5, 0), 
        at = seq(plot_dat$ylims[1] + .1, plot_dat$ylims[1] + 1.9, length.out = 7),
        labels = c('individual habit year prior', 
          'individual habit', 'fleet habit', 'later tow revenue', 
          'first tow revenue', 'later tow distance', "first tow distance"))
    }
  }
}

#----------------------------------------------
#Add legend
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
    mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

#Add first legend
add_legend(x = .72, y = .97, legend = c('   positive', '   negative', 
  ''), pch = c(15, 19, 15), 
  xpd = TRUE, pt.cex = c(1.5, 1.5), bty = 'n')  
add_legend(x = .72, y = .97, legend = c('', '', 
  ''), pch = c(NA, NA, 19), col = 'white',
  xpd = TRUE, pt.cex = c(NA, NA, .5), bty = 'n')  

#Add second point
add_legend(x = .74, y = .97, legend = c("", "", "highly significant"),
  pch = c(NA, NA, 19), pt.cex = 1.5, bty = 'n')
add_legend(x = .74, y = .97, legend = c('', '', 
  ''), pch = c(NA, NA, 19), col = 'white',
  xpd = TRUE, pt.cex = c(NA, NA, .5), bty = 'n')  

# add_legend(x = .72, y = .97, legend = c('', '', 
#   ''), pch = c(NA, NA, 19), col = 'white',
#   xpd = TRUE, pt.cex = c(NA, NA, .5), bty = 'n')  


dev.off()
