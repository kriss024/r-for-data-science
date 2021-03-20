rm(list = ls())

library(neuralnet)
library(datasets)
library(data.table)

data(iris)
summary(iris)

#Train the neural network
#Going to have 10 hidden layers
net = neuralnet(Species ~ ., data = iris, hidden = 10, act.fct = "logistic", linear.output = FALSE)
print(net$call)
print(net$result.matrix)

#Plot the neural network
plot(net)

#Test the neural network on some training data
results <- compute(net, iris)
output <- results$net.result

target <- iris[, 5]

names <- levels(target)
target.val <- names[max.col(output)]
output <- data.frame(output)
colnames(output) <- names
data.iris <- data.frame(output, Prediction = target.val, Original = target)
tabs <- xtabs(~Prediction + Original, data = data.iris)
accuracy <- sum(diag(tabs)) / sum(tabs)
error <- 1 - accuracy
message("Accuracy: ", accuracy)
message("Misclassification rate: ", error)