rm(list = ls())

library(e1071)

x <- matrix(1:10, ncol = 2)
x[c(1, 3, 7)] <- NA
print(x)
y <- impute(x, what = c("mean"))
print(y)
x[is.na(x)] <- 0
z <- x
print(z)