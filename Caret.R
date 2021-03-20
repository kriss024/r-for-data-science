rm(list = ls())

library(caret)

data(GermanCredit)

age <- GermanCredit$Age

library(e1071)
bc <- BoxCoxTrans(age)
print(bc)

age_bc <- predict(bc, age)

par(mfrow = c(1, 2))
hist(age)
hist(age_bc)