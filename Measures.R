library(measures)
rm(list=ls())

# Mean of squared errors, Root mean squared error, Mean of absolute errors
n = 200
set.seed(123)
truth = rnorm(n)
response = rnorm(n)
MSE(truth, response)
RMSE(truth, response)
MAE(truth, response)

# Area under the curve

truth = as.factor(sample(c(1,0), n, replace = TRUE))
probabilities = runif(n)
response = as.factor(as.numeric(probabilities > 0.5))
positive = 1
negative = 0
AUC(probabilities, truth, negative, positive)

# multiclass AUC

truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
probabilities = matrix(runif(60), n, 3)
probabilities = probabilities/rowSums(probabilities)
colnames(probabilities) = c(1,2,3)
multiclass.AU1U(probabilities, truth)

truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
response = as.factor(sample(c(1,2,3), n, replace = TRUE))
ACC(truth, response)