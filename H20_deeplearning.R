rm(list = ls())
url = "http://freakonometrics.free.fr/german_credit.csv"
credit <- read.csv(url, header = TRUE, sep = ",")

library(h2o)

h2o.init(ip = "localhost", port = 54321, nthreads = -1, max_mem_size = "8g")

credit_h2o <- as.h2o(credit, "credit")

h2o.describe(credit_h2o)

credit_h2o[, 1] <- as.factor(credit_h2o[, 1])

credit_h2o.split = h2o.splitFrame(credit_h2o, ratios = 0.75)

training <- credit_h2o.split[[1]]
validation <- credit_h2o.split[[2]]

model <- h2o.deeplearning(x = 2:21, y = 1, hidden = c(10, 20, 50), epochs = 60, training_frame = training, validation_frame = validation)

summary(model)
h2o.saveModel(object = model, path = "c:/work", force = TRUE)

perf <- h2o.performance(model, credit_h2o)

h2o.giniCoef(perf)
print(h2o.auc(model, valid = TRUE))
h2o.gainsLift(perf)

response <- h2o.predict(model, credit_h2o)

pred <- response$predict

table <- h2o.table(pred, credit_h2o[, 1])

print(table)

export <- as.data.frame(response)