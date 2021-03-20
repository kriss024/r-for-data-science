rm(list = ls())

library(xgboost)
url <- "http://freakonometrics.free.fr/german_credit.csv"
credit <- read.csv(url, header = TRUE, sep = ",")

first.columns <- 2
last.columns <- ncol(credit)
columns <- seq(first.columns, last.columns)

for (i in columns) {
    elem <- length(unique(credit[, i]))
    if (elem <= 20) credit[, i] <- as.factor(credit[, i])
}

library(Hmisc)
k <- contents(credit)
print(k, sort='names', prlevels=FALSE)

## ** ** ** ** ** ** ** ** ** ** ** ** **

X <- model.matrix( ~ . - Creditability, data = credit)

inTraining <- sample(1:nrow(credit), size = 700)
dtrain <- xgb.DMatrix(X[inTraining,], label = credit$Creditability[inTraining])
dtest <- xgb.DMatrix(X[ - inTraining,], label = credit$Creditability[ - inTraining])

model <- xgboost(data = dtrain, max_depth = 2, nrounds = 30, objective = "binary:logistic")

err <- mean(round(predict(model, dtest)) != getinfo(dtest, 'label'))
print(paste("test-error=", err))

importance.matrix <- xgb.importance(model = model, feature_names = colnames(X))
xgb.plot.importance(importance.matrix)

model.watchlist <- xgb.train(data = dtrain, max_depth = 2, nrounds = 100, objective = "binary:logistic", watchlist = list(train = dtrain, test = dtest))

model.auc <- xgb.train(data = dtrain, max_depth = 2, nrounds = 100, objective = "binary:logistic",
                       watchlist = list(train = dtrain, test = dtest), eval_metric = 'auc', eval_metric = 'logloss')

## ** ** ** ** ** ** ** ** ** ** ** ** **

setwd("C:\\Work")
print(getwd())

xgb.DMatrix.save(dtrain, 'xgb.DMatrix.data')
dtrain <- xgb.DMatrix('xgb.DMatrix.data')

xgb.save(model.auc, 'xgb.model')
model.load <- xgb.load('xgb.model')
pred <- round(predict(model.load , dtest))

original <- credit[-inTraining,1]
tabela <- table(Przewidywane = pred, Rzeczywiste = original)
print(tabela)
poprawne <- sum(diag(tabela)) / sum(tabela)
cat(paste("Poprawne: ", floor(poprawne * 100), "%"))
