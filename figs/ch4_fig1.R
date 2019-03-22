#--------------------------------------------------------
#Choice set schematic figure

# rgb(red, green, blue, alpha, names = NULL, maxColorValue = 1)
tblack <- adjustcolor("gray", alpha.f = .5)

#To Do:
# Add jittered points indicating that the vessel or fleet recently visited

# sample some points within radius
jitters <- function(x = 8, y = 3, radius = 1, n){
  xvals <- runif(min = x - radius, max = x + radius, n = n)
  yvals <- runif(min = y - radius, max = y + radius, n = n)
  return(data.frame(x = xvals, y = yvals))
  # browser()
}

#--------------------------------------------------------
#specify point type, color, and alpha
# setwd("/Users/peterkuriyama/Dropbox/phd/research/ch4")


#--------------------------------------------------------
#Full figure
# pdf(width = 7, height = 7, file = 'figs/ch4_choice_set_ex.pdf')
png(width = 7, height = 7, file = 'figs/ch4_fig1.png',
  res = 150, units = 'in')

par(mar = c(0, 0, 0, 0))
plot(1:10, type = 'n', ylim = c(0, 12), 
  ann = F, axes = F, xlim = c(-3, 12))

#Add distance segments
segments(x0 = 10, x1 = 8, y0 = 3, y1 = 8)
segments(x0 = 8, x1 = 3, y0 = 8, y1 = 5)

#Add radius points
symbols(8, 8, circles = 1, inches = FALSE, fg = 'black', lty = 1, add = T)
symbols(3, 5, circles = 1, inches = FALSE, fg = 'black', lty = 1, add = T)

#empirical tows
points(8, 8, pch = 19, col = 'black', cex = 1.2)
set.seed(301)
jj1 <- jitters(x = 8.5, y = 8.5, radius = .5, n = 3)
jj1[1, 2] <- 8.55

points(jj1, pch = 22,
  bg = tblack, col = 'black')
points(8.5, 7.75, pch = 21, bg = tblack, col = 'black', cex = 1.2)

text(8, 8, '1', pos = 4)

points(3, 5, pch = 19, col = 'black', cex = 1.2)
set.seed(501)
jj <- jitters(x = 3, y = 5, radius = 1, n = 4)
jj[4, 2] <- 5.5
jj[2, 2] <- 4.7

points(jj, pch = 22,
  bg = tblack, col = 'black')
text(3, 5, '2', pos = 2)

#---------------------
#Sampled tows 1
segments(x0 = 10, x1 = 8, y0 = 3, y1 = 3, lty = 2)
segments(x0 = 10, x1 = 9, y0 = 3, y1 = 11, lty = 2)

points(8, 3, pch = 21, bg = 'white', cex = 1.2)
text(8, 3, '1b', pos = 2)
jj3 <- jitters(x = 8, y = 3, radius = .75, n = 1)
jj3$y <- 3.3
points(jj3, pch = 21,
  bg = tblack, col = 'black', cex = 1.2)

points(9, 11, pch = 21, bg = 'white', cex = 1.2)
text(9, 11, '1a', pos = 4)

symbols(8, 3, circles = 1, inches = FALSE, fg = 'black', lty = 2, add = T)
symbols(9, 11, circles = 1, inches = FALSE, fg = 'black', lty = 2, add = T)

#---------------------
#sampled tows 2
segments(x0 = 8, x1 = 2, y0 = 8, y1 = 10, lty = 2)
segments(x0 = 8, x1 = 1, y0 = 8, y1 = 1, lty = 2)

points(2, 10, pch = 21, bg = 'white', cex = 1.2)
text(2, 10, "2a", pos = 2)
jj2 <- jitters(x = 2, y = 10, radius = .75, n = 2)
jj2[1, ] <- c(2.5, 10.4)
points(jj2, pch = 21,
  bg = tblack, col = 'black', cex = 1.2)

points(1, 1, pch = 21, bg = 'white', cex = 1.2)
text(1, 1, "2b", pos = 2)
points(1.25, .75, pch = 22, col = 'black', bg = tblack, cex = 1.2)

symbols(2, 10, circles = 1, inches = FALSE, fg = 'black', lty = 2, add = T)
symbols(1, 1, circles = 1, inches = FALSE, fg = 'black', lty = 2, add = T)

#port point
points(10, 3, pch = 19, col = 'red', cex = 3)

legend("topleft", legend = c("Port", "Fished location", "Sampled location", 
# legend(x = -3.5, y = 5, legend = c("Port", "Fished location", "Sampled location", 
  "Vessel recent tow", "Fleet recent tow"),
  pch = c(19, 19, 21, 21, 22), bty = 'n', 
  col = c('red', 'black', 'black', 'black', 'black'),
  pt.bg = c('white', 'white', 'white', tblack, tblack),
  cex = 1)
dev.off()

#--------------------------------------------------------

tiff(width = 7, height = 7, file = 'figs/ch4_fig1.tiff',
  res = 300, units = 'in')

par(mar = c(0, 0, 0, 0))
plot(1:10, type = 'n', ylim = c(0, 12), 
  ann = F, axes = F, xlim = c(-3, 12))

#Add distance segments
segments(x0 = 10, x1 = 8, y0 = 3, y1 = 8)
segments(x0 = 8, x1 = 3, y0 = 8, y1 = 5)

#Add radius points
symbols(8, 8, circles = 1, inches = FALSE, fg = 'black', lty = 1, add = T)
symbols(3, 5, circles = 1, inches = FALSE, fg = 'black', lty = 1, add = T)

#empirical tows
points(8, 8, pch = 19, col = 'black', cex = 1.2)
set.seed(301)
jj1 <- jitters(x = 8.5, y = 8.5, radius = .5, n = 3)
jj1[1, 2] <- 8.55

points(jj1, pch = 22,
  bg = tblack, col = 'black')
points(8.5, 7.75, pch = 21, bg = tblack, col = 'black', cex = 1.2)

text(8, 8, '1', pos = 4)

points(3, 5, pch = 19, col = 'black', cex = 1.2)
set.seed(501)
jj <- jitters(x = 3, y = 5, radius = 1, n = 4)
jj[4, 2] <- 5.5
jj[2, 2] <- 4.7

points(jj, pch = 22,
  bg = tblack, col = 'black')
text(3, 5, '2', pos = 2)

#---------------------
#Sampled tows 1
segments(x0 = 10, x1 = 8, y0 = 3, y1 = 3, lty = 2)
segments(x0 = 10, x1 = 9, y0 = 3, y1 = 11, lty = 2)

points(8, 3, pch = 21, bg = 'white', cex = 1.2)
text(8, 3, '1b', pos = 2)
jj3 <- jitters(x = 8, y = 3, radius = .75, n = 1)
jj3$y <- 3.3
points(jj3, pch = 21,
  bg = tblack, col = 'black', cex = 1.2)

points(9, 11, pch = 21, bg = 'white', cex = 1.2)
text(9, 11, '1a', pos = 4)

symbols(8, 3, circles = 1, inches = FALSE, fg = 'black', lty = 2, add = T)
symbols(9, 11, circles = 1, inches = FALSE, fg = 'black', lty = 2, add = T)

#---------------------
#sampled tows 2
segments(x0 = 8, x1 = 2, y0 = 8, y1 = 10, lty = 2)
segments(x0 = 8, x1 = 1, y0 = 8, y1 = 1, lty = 2)

points(2, 10, pch = 21, bg = 'white', cex = 1.2)
text(2, 10, "2a", pos = 2)
jj2 <- jitters(x = 2, y = 10, radius = .75, n = 2)
jj2[1, ] <- c(2.5, 10.4)
points(jj2, pch = 21,
  bg = tblack, col = 'black', cex = 1.2)

points(1, 1, pch = 21, bg = 'white', cex = 1.2)
text(1, 1, "2b", pos = 2)
points(1.25, .75, pch = 22, col = 'black', bg = tblack, cex = 1.2)

symbols(2, 10, circles = 1, inches = FALSE, fg = 'black', lty = 2, add = T)
symbols(1, 1, circles = 1, inches = FALSE, fg = 'black', lty = 2, add = T)

#port point
points(10, 3, pch = 19, col = 'red', cex = 3)

legend("topleft", legend = c("Port", "Fished location", "Sampled location", 
# legend(x = -3.5, y = 5, legend = c("Port", "Fished location", "Sampled location", 
  "Vessel recent tow", "Fleet recent tow"),
  pch = c(19, 19, 21, 21, 22), bty = 'n', 
  col = c('red', 'black', 'black', 'black', 'black'),
  pt.bg = c('white', 'white', 'white', tblack, tblack),
  cex = 1)
dev.off()

#--------------------------------------------------------
#Tow 1 figure
png(width = 7, height = 7, file = 'figs/ch4_choice_set_ex_tow1.png',
  res = 150, units = 'in')
plot(1:10, type = 'n', ylim = c(0, 12), 
  ann = F, axes = F, xlim = c(0, 11))
box()
#Add distance segments
segments(x0 = 10, x1 = 8, y0 = 3, y1 = 8)
# segments(x0 = 8, x1 = 3, y0 = 8, y1 = 5)
points(10, 3, pch = 19, col = 'red', cex = 3)
#Add radius points
symbols(8, 8, circles = 1, inches = FALSE, fg = 'black', lty = 1, add = T)
#empirical tows
points(8, 8, pch = 19, col = 'black', cex = 1.2)
text(8, 8, '1', pos = 4)
dev.off()

#--------------------------
png(width = 7, height = 7, file = 'figs/ch4_choice_set_ex_tow1plus.png',
  res = 150, units = 'in')
plot(1:10, type = 'n', ylim = c(0, 12), 
  ann = F, axes = F, xlim = c(0, 11))
box()
#Add distance segments
segments(x0 = 10, x1 = 8, y0 = 3, y1 = 8)
# segments(x0 = 8, x1 = 3, y0 = 8, y1 = 5)
points(10, 3, pch = 19, col = 'red', cex = 3)
#Add radius points
symbols(8, 8, circles = 1, inches = FALSE, fg = 'black', lty = 1, add = T)
#empirical tows
points(8, 8, pch = 19, col = 'black', cex = 1.2)
text(8, 8, '1', pos = 4)
set.seed(301)
points(jitters(x = 8.5, y = 8.5, radius = .5, n = 3), pch = 15,
  col = tblack)
points(8.5, 7.75, pch = 21, bg = tblack, col = 'black', cex = 1.2)
dev.off()

#--------------------------
png(width = 7, height = 7, file = 'figs/ch4_choice_set_ex_tow1plus_samp.png',
  res = 150, units = 'in')
plot(1:10, type = 'n', ylim = c(0, 12), 
  ann = F, axes = F, xlim = c(0, 11))
box()
#Add distance segments
segments(x0 = 10, x1 = 8, y0 = 3, y1 = 8)
# segments(x0 = 8, x1 = 3, y0 = 8, y1 = 5)
#Add radius points
symbols(8, 8, circles = 1, inches = FALSE, fg = 'black', lty = 1, add = T)
#empirical tows
points(8, 8, pch = 19, col = 'black', cex = 1.2)
text(8, 8, '1', pos = 4)
set.seed(301)
points(jitters(x = 8.5, y = 8.5, radius = .5, n = 3), pch = 15,
  col = tblack)
points(8.5, 7.75, pch = 21, bg = tblack, col = 'black', cex = 1.2)
segments(x0 = 10, x1 = 8, y0 = 3, y1 = 3, lty = 2)
segments(x0 = 10, x1 = 9, y0 = 3, y1 = 11, lty = 2)

points(8, 3, pch = 21, bg = 'white', cex = 1.2)
text(8, 3, '1b', pos = 2)
points(jitters(x = 8, y = 3, radius = .75, n = 1), pch = 21,
  bg = tblack, col = 'black', cex = 1.2)

points(9, 11, pch = 21, bg = 'white', cex = 1.2)
text(9, 11, '1a', pos = 4)

symbols(8, 3, circles = 1, inches = FALSE, fg = 'black', lty = 2, add = T)
symbols(9, 11, circles = 1, inches = FALSE, fg = 'black', lty = 2, add = T)
points(10, 3, pch = 19, col = 'red', cex = 3)
dev.off()






