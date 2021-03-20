rm(list = ls())

library(imager)

setwd("C:/Work")
print(getwd())

im <- load.image('party.jpg')plot(im)im.blurry <- isoblur(im, 10) #Blurry parrots!
plot(im.blurry)

im.xedges <- deriche(im, 2, order = 2, axis = "x") #Edge detector along x-axis
plot(im.xedges)

df <- as.data.frame(im)

#Sample functions and turn them into separate R,G,B channels
R <- as.cimg(function(x, y) sin(cos(3 * x * y)), 100, 100)
G <- as.cimg(function(x, y) sin(cos(3 * x * y + pi / 2)), 100, 100)
B <- as.cimg(function(x, y) exp(-.03 * x), 100, 100)
trippy <- imappend(list(R, G, B), "c") #Bind the three channels into one image
plot(trippy)
