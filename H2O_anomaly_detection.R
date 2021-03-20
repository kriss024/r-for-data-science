rm(list = ls())

x <- seq(0, 99)
x <- x / pi
y <- sin(x)
y[20] <- 1.5
y[50] <- 2
y[80] <- 2.5

library(h2o)
h2o.init(ip = "localhost", port = 54321, nthreads = -1, max_mem_size = "8g")

y.hex <- as.h2o(y)
model <- h2o.deeplearning(x = 1, training_frame = y.hex, autoencoder = TRUE,
                               hidden = c(5, 5), epochs = 10)

y.anon.per.feature = h2o.anomaly(model, y.hex, per_feature = TRUE)
head(y.anon.per.feature)

export <- as.data.frame(y.anon.per.feature)

h2o.shutdown(prompt = FALSE)

y2 <- export$reconstr_x.SE

library(ggplot2)
y.data <- data.frame(x, y, type = 'original data')
y2.data <- data.frame(x, y = y2, type = 'reconstruction MSE')

charts.data <- rbind(y.data, y2.data)

ggplot() + geom_line(aes(y = y, x = x, colour = type), size = 1.5,
                           data = charts.data, stat = 'identity')