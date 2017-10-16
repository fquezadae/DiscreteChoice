


#------------------------------------------------------------------------------------------------------
# png(width = 6.7, height = 8.13, units = 'in', res = 200, file = 'figs/test_sig.png')
dev.new(width = 7.29, height = 8.39)

# png(width = 11, height = 9, units = 'in', res = 200, file = 'figs/ch4_sig_fig.png')
# png(width = 5.621951, height = 8.36, units = 'in', file = 'figs/ch4_sig_fig.png', res = 200)

matlay <- matrix(c( 1,  2, 0,  3,  4,  5,  6, 
                    7,  8, 0,   9, 10, 11, 12,
                   13, 14, 0, 15, 16, 17, 18,
                   19, 20, 0, 21, 22, 23, 24, 
                   25, 26, 0, 27, 28, 29, 30,
                   31, 32, 0, 33, 34, 35, 36), ncol = 7, byrow = T)
                   # 5, 6, 7, 8, 
                   # 9, 10, 11, 12,
                   # 13, 14, 15, 16,
                   # 17, 18, 19, 20,
                   # 21, 22, 23, 24), ncol = 4, byrow = T)
# layout(matlay)
# lcm(2.5 * c(1.75, 1.25, 1, 1, 1, 1))

layout(matlay, widths = c(lcm(2.5), lcm(2.5), lcm(.25), rep(lcm(2.5), 4)), 
  heights = lcm(2.75 * c(1.75, 1.25, 1, 1, 1, 1)))
  # heights = c(lcm(2.5 * 1.75 * 1.3), 
  # lcm(2.5 * 1.55 * 1.3), rep(lcm(2.5 * 1.3), 4)),
  # widths = rep(lcm(2.5), 6))

# layout.show(24)
# layout(matlay, heights = c(1.3, 1, rep(1, 5)), widths = rep(.7, 7), respect = T)

#------------------------------------------------------------------------------------------------------
# par(mar = c(0, 0, 0, 0), oma = c(1, 1, 1, 0.25))
# names(port_plot) <- sapply(port_plot, FUN = function(xx) xx[[1]])
# sapply(port_plot, FUN = function(xx) xx[[1]])

# par(mar = c(0, 0, 0, 0), oma = c(.5, .75, 0, .3)) #small numbers

par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0)) #small numbers

for(pp in 6:1){  
  plot_dat <- port_plot[[pp]]
  temp_bin <- port_bins %>% filter(fleet_name_comb %in% plot_dat$port_name)
  port_locz <- some_port_locs[[pp]]
  
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
      col = 'white', border = NULL)
    points(rep(-123.16, 7), seq(plot_dat$ylims[1] + .1, plot_dat$ylims[1] + 1.9, length.out = 7), 
      pch = plot_dat$coefs[, yy + 2], bg = 'gray70',
      xpd = T, cex = 1.7, col = 'gray70')
# seq(plot_dat$ylims[1], plot_dat$ylims[1] + 2, length.out = 7)
# (((plot_dat$ylims[1] + 2) - plot_dat$ylims[1]) / 7)

    #-----Add Axes
    if(yy == 1 & pp != 6){
      axis(side = 2, las = 2, mgp = c(0, .5, 0), at = plot_dat$ylabs)
      mtext(side = 3, unique(port_locz$name), line = 0, adj = 0)
    } 
    
    if(pp == 6 & yy == 1){
      # paste0("'", substr(yrz[yy], 3, 4))
      # mtext(side = 3, paste0("'", substr(yrz[yy], 3, 4)), adj = 0, outer = F)
      mtext(side = 3, yrz[yy], adj = 1, outer = F)
      mtext(side = 3, unique(port_locz$name), line = 0, adj = 0)
    }
    if(pp == 6 & yy != 1){
      mtext(side = 3, yrz[yy], adj = 1, outer = F)
      # mtext(side = 3, paste0("'", substr(yrz[yy], 3, 4)), adj = 1, outer = F)
      # mtext(side =3, unique(port_locz$name), line = 1.2, adj = 0)
    }

    if(pp == 1){
      axis(side = 1, mgp = c(0, .5, 0), at = c(-125.5, -124.5, -123.5), 
        labels = c(125.5, 124.5, 123.5))
    } 
    if(yy == 6){
      axis(side = 4, las = 2, mgp = c(0, .5, 0), 
        at = seq(plot_dat$ylims[1] + .1, plot_dat$ylims[1] + 1.9, length.out = 7),
        labels = c('individual habit year prior', 
          'individual habit', 'fleet habit', 'later tow revenue', 
          'first tow revenue', 'later tow distance', "first tow distance"))
        # labels = c('dum30y', 'dum30', 'dmiss', 'rev', 'rev1', 'dist', "dist1"))
    }
  }
}
mtext(side = 1,  expression("Longitude" ~degree ~ W), outer = T, line = -1.5, cex = 1.2)
mtext(side = 2,  expression("Latitude" ~degree ~ N), outer = T, line = -2, cex = 1.2)


# dev.off()