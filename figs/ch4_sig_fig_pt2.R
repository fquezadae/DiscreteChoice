
#------------------------------------------------------------------------------------------------------
# dev.new(width = 5.5, height = 8.28)

matlay <- matrix(c(1, 2, 3, 4,
                   5, 6, 7, 8, 
                   9, 10, 11, 12,
                   13, 14, 15, 16,
                   17, 18, 19, 20,
                   21, 22, 23, 24), ncol = 4, byrow = T)
# layout(matlay)
layout(matlay, heights = c(lcm(4), lcm(4), rep(lcm(3), 4)),
  widths = rep(lcm(3), 4))

# layout.show(24)
# layout(matlay, heights = c(1.3, 1, rep(1, 5)), widths = rep(.7, 7), respect = T)

#------------------------------------------------------------------------------------------------------
# par(mar = c(0, 0, 0, 0), oma = c(1, 1, 1, 0.25))
# names(port_plot) <- sapply(port_plot, FUN = function(xx) xx[[1]])
# sapply(port_plot, FUN = function(xx) xx[[1]])

par(mar = c(0, 0, 0, 0), oma = c(2, 2, 1, 0))

for(pp in 6:1){  
  plot_dat <- port_plot[[pp]]
  temp_bin <- port_bins %>% filter(fleet_name_comb %in% plot_dat$port_name)
  port_locz <- some_port_locs[[pp]]
  
  for(yy in 1:length(yrz)){  
    yr_temp <- temp_bin %>% filter(year == yrz[yy])
    map('state', fill = FALSE, col = 'white', xlim = plot_dat$xlims,
      ylim = plot_dat$ylims, mar = c(0, 0, 0, 0), asp = 1.3 )
    points(yr_temp$x, yr_temp$y, pch = 15, cex = .6, col = yr_temp$greys)
    map('state', fill = TRUE, col = 'gray95', xlim = plot_dat$xlims,
      ylim = plot_dat$ylims, mar = c(0, 0, 0, 0), add = T, asp = 1.3 )
    points(port_locz$d_port_long, port_locz$d_port_lat, pch = 19, col = 'red')
    box()
    mtext(side = 3, text = paste0(plot_dat$letts[yy], ")"), adj = 0.02, line = -1,
      cex = .8)
    if(yy == 1){
      axis(side = 2, las = 2, mgp = c(0, .5, 0), at = plot_dat$ylabs)
      mtext(side = 3, unique(port_locz$name), line = 0, adj = 0)
    } 
    if(pp == 1) axis(side = 1, mgp = c(0, .5, 0), at = c(-125, -124, -123, -122), 
      labels = c(125, 125, 123, 122))
  }
}



# dev.off()