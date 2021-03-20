rm(list = ls())

library(caret)
data(GermanCredit)
str(GermanCredit)

library(RWeka)
list_Weka_interfaces()

TrainData <- GermanCredit[, c("Duration", "Amount", "Age", "Housing.Own")]
TrainData$Housing.Own <- as.factor(TrainData$Housing.Own)
TrainClasses <- GermanCredit[, c("Class")]

jripFit <- train(TrainData, TrainClasses, method = "JRip")
jripFit <- train(TrainData, TrainClasses, method = "J48")
jripFit <- train(TrainData, TrainClasses, method = "OneR")
jripFit <- train(TrainData, TrainClasses, method = "PART")

summary(jripFit)
print(jripFit$finalModel)

score <- predict(jripFit, newdata = TrainData)

table <- table(Predicted = score, Original = TrainClasses)

print(table)
accuracy <- sum(diag(table)) / sum(table)
accuracy <- floor(accuracy  * 100)
cat(paste("Accuracy: ", accuracy, "%"))
missing<-100-accuracy
cat(paste("Misclassification rate: ", missing, "%"))