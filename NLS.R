rm(list = ls())
set.seed(955)

x <- seq(0, 6, 0.0001)

moja.funkcja <- function(x) {
    y <- tanh(x)+sin(x)
    return(y)
}
n <- length(x)
y <- moja.funkcja(x) + rnorm(n)

dat <- data.frame(x, y)

index <- sample(n, floor(n / 100), replace = FALSE)

dat.sample <- dat[index,]

library(ggplot2)
library(cowplot)
p1 <- ggplot(dat, aes(x = x, y = y)) + geom_point(alpha = 1 / 20)
p2 <- ggplot(dat.sample, aes(x = x, y = y)) + geom_point()
plot_grid(p1, p2, ncol = 2, nrow = 1)

moja.funkcja.fit <- function(x,a,b,c,d) {
    y <- a * x ^ 3 + b * x ^ 2 + c * x + d
    return(y)
}

fit.model = nls(y ~ moja.funkcja.fit(x, a, b, c, d), data = dat.sample, start = list(a = 0, b = 0, c = 0, d = 0))
summary(fit.model)

wektor.x <- dat.sample$x
y <- predict(fit.model, list(x = wektor.x))

dat.new <- data.frame(x=wektor.x, y)
p1 <- ggplot(dat, aes(x = x, y = y)) + geom_point(alpha = 1 / 20)
p2 <- ggplot(dat.sample, aes(x = x, y = y)) + geom_point()
p3 <- ggplot(dat.new, aes(x = x, y = y)) + geom_line() + geom_point()
plot_grid(p1, p2, p3, ncol = 3, nrow = 1)
