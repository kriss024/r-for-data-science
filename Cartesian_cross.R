rm(list=ls())

A <- factor(c(1,2,3))
B <- c('x','y')
C <- c(0.1,0.5,0.7)

d <- expand.grid(A = A, B = B, C = C)

library(data.table)
d2 <- CJ(A = A, B = B, C = C)  # Cross join