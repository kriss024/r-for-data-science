rm(list = ls())

data("mtcars")

fit <- lm(mpg ~ cyl + wt, data = mtcars)
print(fit)

plot(fit, pch = 18, col = "red", which = c(4))

cooks <- cooks.distance(fit)
cooks <- round(cooks, 5)
cooks <- rev(sort(cooks))

df <- data.frame(name = names(cooks), cooks)
rownames(df) <- NULL
print(df)