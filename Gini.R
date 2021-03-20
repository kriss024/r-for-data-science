rm(list = ls())

save(iris, file = "iris.Rda")

load("iris.Rda")

dane <- subset(iris, iris$Species %in% c("versicolor", "virginica"))

library(pROC)

names <- colnames(dane)
n <- length(names) - 1

wynik <- data.frame(name = character(), Gini = double(), stringsAsFactors = FALSE)

for (i in 1:n) {
    name <- names[i]
    dane2 <- subset(dane, select = c(name, "Species"))

    x <- dane2[, 1]
    y <- as.character(dane2[, 2])

    AUC <- auc(y, x)

    Gini <- 2 * AUC - 1
    wynik[i, 1] <- name
    wynik[i, 2] <- Gini
}

print(wynik)