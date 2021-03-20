rm(list = ls())

url = "http://freakonometrics.free.fr/german_credit.csv"
credit <- read.csv(url, header = TRUE, sep = ",")

library(ggplot2)

Duration.of.Credit.month <- credit$Duration.of.Credit..month.
Creditability <- as.character(credit$Creditability)

Creditability[Creditability == "1"] <- "Good"
Creditability[Creditability == "0"] <- "Bad"

df <- data.frame(Duration.of.Credit.month, Creditability)

ggplot(df, aes(x = Duration.of.Credit.month, fill = Creditability)) + geom_density(alpha = .3)

library(pROC)
AUC <- auc(Creditability, Duration.of.Credit.month)
print(AUC)
Gini <- 2 * AUC - 1
print(Gini)