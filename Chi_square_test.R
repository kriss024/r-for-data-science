rm(list = ls())
dane <- read.csv("https://goo.gl/j6lRXD")

library(MASS)
tbl = table(dane$treatment, dane$improvement)
print(tbl)

test.chi <- chisq.test(tbl)
print(test.chi)
print(test.chi$statistic)
print(test.chi$p.value)

library("gplots")
balloonplot(t(tbl), main = "Chi-Square Test of Independence", xlab = "", ylab = "",
            label = FALSE, show.margins = FALSE)
