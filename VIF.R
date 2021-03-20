rm(list = ls())

library(car)

data(mtcars)

# checking multicolinearity for independent variables.
fit <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
summary(fit)

vif <- vif(fit)
print(vif)

fit.r2 <- summary(fit)$adj.r.squared


fit2 <- lm(mpg ~ hp + wt + drat, data = mtcars)
summary(fit2)

vif2 <- vif(fit2)
print(vif2)

fit2.r2 <- summary(fit2)$adj.r.squared

sprintf("%.2f%%", fit.r2 * 100)
sprintf("%.2f%%", fit2.r2 * 100)