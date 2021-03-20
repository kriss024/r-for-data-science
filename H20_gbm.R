library(h2o)
h2o.init(ip = "localhost", port = 54321, nthreads = -1, max_mem_size = "8g")

bank.h2o <- h2o.importFile("https://s3.amazonaws.com/h2o-public-test-data/smalldata/demos/bank-additional-full.csv")

h2o.names(bank.h2o)
tab <- h2o.tabulate(data = bank.h2o, x = "age", y = "y",
             weights_column = NULL, nbins_x = 10, nbins_y = 2)
plot(tab)

bank.h2o.split = h2o.splitFrame(bank.h2o, ratios = 0.5)

training <- bank.h2o.split[[1]]
validation <- bank.h2o.split[[2]]

bank.h2o.model <- h2o.gbm(x = 1:20, y = "y", training_frame = training, validation_frame = validation,
        ntrees = 100, learn_rate = 0.01, score_each_iteration = TRUE)

plot(bank.h2o.model)

impo <- h2o.varimp(bank.h2o.model)

library(ggplot2)
impo$variable <- factor(impo$variable, levels = impo$variable[order(impo$scaled_importance)])
ggplot(impo, aes(variable, scaled_importance)) + geom_bar(stat = "identity") + coord_flip() + theme_bw()

perf <- h2o.performance(bank.h2o.model, bank.h2o)
h2o.giniCoef(perf)
print(h2o.auc(bank.h2o.model, valid = TRUE))

h2o.confusionMatrix(bank.h2o.model, bank.h2o)

predictions <- h2o.predict(bank.h2o.model, bank.h2o)

predicted.y <- predictions$predict

prostate.cbind <- h2o.cbind(bank.h2o, predicted.y)

h2o.exportFile(prostate.cbind, path = "c:/work/pred.csv", force = TRUE)