

png(width = 5.9, height = 5.3, file = 'figs/ch4_weak_delta.png', res = 200, units = 'in')

par(mfrow = c(2, 3), mar = c(0, 0, 0, 0), oma = c(3, 4, 1, .5))
for(ii in 1:6){
  bef <- weaks %>% filter(species == wspp[ii], when == 'before')
  aft <- weaks %>% filter(species == wspp[ii], when == 'after')
  
  plot(temp$prop_zero, temp$skew, xlim = c(0, 1), ylim = c(-2, 2), ann = F, axes = F,
    type = 'n')
  # points(bef$prop_zero, bef$skew, pch = 19, col = adjustcolor( "black", alpha.f = 0.5), cex = 1.2)
  points(aft$prop_zero, aft$skew, pch = 19, col = adjustcolor( "black", alpha.f = 0.5), cex = 1.2)
  # points(aft$prop_zero, aft$skew, pch = 21, col = rgb(0, 0, 0, .85), cex = 1.2)
  points(bef$prop_zero, bef$skew, pch = 21, col = rgb(0, 0, 0, .85), cex = 1.2)
  abline(h = 0, lty = 2)
  box()
  #Add axes
  if(ii %in% c(1, 4)) axis(side = 2, mgp = c(0, .5, 0), at = pretty(weaks$skew),
    las = 2)
  if(ii > 3) axis(side = 1, mgp = c(0, .5, 0), at = c(0, .2, .4, .6, .8))
  if(ii == 3) legend('bottomright', bty = 'n', legend = c("Before", "After"), pch = c(21, 19),
    col = c('black', adjustcolor( "black", alpha.f = 0.5)), pt.bg = c('white', 'gray80'), pt.cex = 1.2)
  mtext(side = 3, text = paste0(letters[ii], ") ", wspp[ii]), cex = .7, adj = .04,
   line = -1.25)
}

mtext(side = 1, outer = T, "Proportion Zero Values", line = 1.7)
mtext(side = 2, outer = T, "Skew", line = 2.2)

dev.off()
