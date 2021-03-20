rm(list = ls())

dataDirectory <- "C:\\Temp\\"
data <- read.csv(paste(dataDirectory, 'regression.csv', sep=""), header = TRUE)

# Create a linear regression model
model <- lm(Y ~ X, data)

plot(data, pch=16)

predictedY <- predict(model, data)

points(data$X, predictedY, col = "blue", pch=4)

library(caret)

predictionRMSE <- RMSE(predictedY,data$Y)
print(predictionRMSE) # 5.703778

library(e1071)
model <- svm(Y ~ X , data)
predictedY <- predict(model, data)
points(data$X, predictedY, col = "red", pch=4)

##Calculate parameters of the SVR model

#Find value of W
W = t(model$coefs) %*% model$SV

#Find value of b
b = model$rho

svrPredictionRMSE <- RMSE(predictedY,data$Y)  
print(svrPredictionRMSE) # 3.157061

# perform a grid search
tuneResult <- tune(svm, Y~X, data=data,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))

#Print optimum value of parameters
print(tuneResult)

#Plot the perfrormance of SVM Regression model
plot(tuneResult)

#Find out the best model
tunedModel <- tuneResult$best.model

#Predict Y using best model
tunedModelY <- predict(tunedModel, data) 

tunedModelRMSE <- RMSE(tunedModelY,data$Y)  
print(tunedModelRMSE) # 1.975572

## Plotting SVR Model and Tuned Model in same plot
plot(data, pch=16, main="")
points(data$X, predictedY, col = "blue", pch=3)
points(data$X, tunedModelY , col = "red", pch=4)
points(data$X, predictedY, col = "blue", pch=3, type="l")
points(data$X, tunedModelY , col = "red", pch=4, type="l")