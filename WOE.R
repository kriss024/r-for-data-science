rm(list = ls())

library(InformationValue)

data('ActualsAndScores')
plotROC(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)

misClassError(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores, threshold=0.5)

somersD(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)

ks_stat(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)

ks_plot(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)

optimalCutoff(actuals = ActualsAndScores$Actuals, predictedScores = ActualsAndScores$PredictedScores) 

require(caret)
data(GermanCredit)

str(GermanCredit)

credit <- data.frame(Age.Group=cut(GermanCredit$Age, 6), Age=GermanCredit$Age,
                    InstallmentRatePercentage=as.factor(GermanCredit$InstallmentRatePercentage),
                    default=ifelse(GermanCredit$Class=="Bad", 1, 0))

options(scipen = 999, digits = 2)
woe<-WOETable(X=credit$Age.Group, Y=credit$default)
print(woe)

options(scipen = 999, digits = 6)
iv<-IV(X=credit$Age.Group, Y=credit$default)
print(iv)

#--------------------------------

options(scipen = 999, digits = 2)
woe<-WOETable(X=credit$InstallmentRatePercentage, Y=credit$default)
print(woe)

options(scipen = 999, digits = 6)
iv<-IV(X=credit$InstallmentRatePercentage, Y=credit$default)
print(iv)



