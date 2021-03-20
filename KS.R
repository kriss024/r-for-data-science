data(mtcars)
head(mtcars)

x <- data.frame(x = rnorm(50))
y <- data.frame(x = runif(50) + 2)


x$rozklad <- 'rozklad 1'
y$rozklad <- 'rozklad 2'

dane <- rbind(x, y)

library(ggplot2)
ggplot(dane, aes(x, fill = rozklad)) + geom_density(alpha = 0.2)

# Do x and y come from the same distribution?
ks <- ks.test(x$x, y$x)
print(ks$statistic)
print(ks$p.value)
