rm(list = ls())
n1 <- 100
n2 <- 200
n3 <- 300

normal_distribution <- function(n) {
    y <- rnorm(n)
    y.min <- min(y)
    y.max <- max(y)
    y.mean <- mean(y)
    y.range <- y.max - y.min
    p <- y - y.min
    d <- p / y.range
    return(d)
}

chisquared_distribution <- function(n, df) {
    y <- rchisq(n, df)
    y.min <- min(y)
    y.max <- max(y)
    y.mean <- mean(y)
    y.range <- y.max - y.min
    p <- y - y.min
    d <- p / y.range
    return(d)
}

dane <- data.frame()

s <- 'Analityk Kredytowy'
d <- chisquared_distribution(n1, 3)
w <- as.integer(d * 20)
d.good <- data.frame(Stanowisko = s, Staz = w, Default = 1)

s <- 'Analityk Kredytowy'
d <- chisquared_distribution(n3, 20)
w <- as.integer(d * 20)
d.bed <- data.frame(Stanowisko = s, Staz = w, Default = 0)

dane <- rbind(dane, d.good, d.bed)

s <- 'Student'
d <- chisquared_distribution(n1, 2)
w <- as.integer(d * 10)
d.good <- data.frame(Stanowisko = s, Staz = w, Default = 1)

s <- 'Student'
d <- chisquared_distribution(n2, 10)
w <- as.integer(d * 10)
d.bed <- data.frame(Stanowisko = s, Staz = w, Default = 0)

dane <- rbind(dane, d.good, d.bed)

s <- 'Analityk Systemowy'
d <- chisquared_distribution(n1, 5)
w <- as.integer(d * 20)
d.good <- data.frame(Stanowisko = s, Staz = w, Default = 1)

s <- 'Analityk Systemowy'
d <- chisquared_distribution(n3, 20)
w <- as.integer(d * 20)
d.bed <- data.frame(Stanowisko = s, Staz = w, Default = 0)

dane <- rbind(dane, d.good, d.bed)

s <- 'Adwokat'
d <- chisquared_distribution(n1, 4)
w <- as.integer(d * 20)
d.good <- data.frame(Stanowisko = s, Staz = w, Default = 1)

s <- 'Adwokat'
d <- chisquared_distribution(n3, 25)
w <- as.integer(d * 20)
d.bed <- data.frame(Stanowisko = s, Staz = w, Default = 0)

dane <- rbind(dane, d.good, d.bed)

s <- 'Emeryt'
d <- chisquared_distribution(n2, 3)
w <- as.integer(d * 20)
d.good <- data.frame(Stanowisko = s, Staz = w, Default = 0)

s <- 'Emeryt'
d <- chisquared_distribution(n3, 20)
w <- as.integer(d * 20)
d.bed <- data.frame(Stanowisko = s, Staz = w, Default = 1)

dane <- rbind(dane, d.good, d.bed)

dane$Default <- as.factor(dane$Default)

dane.analiza <- dane[sample(nrow(dane), nrow(dane), replace = FALSE),]

rownames(dane.analiza) <- NULL

save(dane.analiza, file = "dane_analiza.RData")

rm(list = ls())

load("dane_analiza.RData")

library(ggplot2)

ggplot(dane.analiza, aes(x = Staz, fill = Default, color = Default)) +
        geom_histogram(binwidth = 1, alpha = .5, position = "identity")

ggplot(dane.analiza, aes(x = Staz, fill = Stanowisko, color = Stanowisko)) +
        geom_histogram(binwidth = 1, alpha = .5, position = "identity")

library(rpart)
cart.tree <- rpart(Default ~ ., data = dane.analiza, method = "class")

print(cart.tree)
print(cart.tree$variable.importance)

library(rpart.plot)
rpart.plot(cart.tree, cex = .7)

scoring <- predict(cart.tree, newdata = dane.analiza, type = "class")
tabela <- table(Przewidywane = scoring, Rzeczywiste = dane.analiza$Default)
print(tabela)

poprawne <- sum(diag(tabela)) / sum(tabela)
cat(paste("Poprawnoœæ: ", floor(poprawne * 100), "%"))
cat(paste(" Odsetek b³êdnych klasyfikacji: ", 100 - floor(poprawne * 100), "%"))

library(pROC)
oceny <- predict(cart.tree, newdata = dane.analiza, type = "prob")
roc <- roc(dane.analiza$Default, oceny[, 2], direction = "<")

roc.rpart.df <- data.frame(x = 1 - roc$specificities, y = roc$sensitivities, m = "Drzewo decyzyjne")
auc <- auc(roc)
gini <- 2 * auc - 1
title <- paste(" Wykres ROC,", "AUC:", round(auc, digits = 4), "Gini:", round(gini, digits = 4))

ggplot(roc.rpart.df, aes(x, y, color = m)) + geom_line(size = 2, alpha = 0.8) +
    labs(title = title,
       x = "1-Specyficznoœæ",
       y = "Czu³oœæ",
       color = "Model")

library(InformationValue)

dane.analiza$Scoring <- as.factor(scoring)
woe <- WOETable(X = dane.analiza$Scoring, Y = dane.analiza$Default)
print(woe)
woe.tabl <- data.frame(CAT = woe$CAT, WOE_Scoring = woe$WOE)

dane.analiza.woe <- merge(dane.analiza, woe.tabl, by.x = "Scoring", by.y = "CAT", all.x = TRUE)

model <- glm(Default ~ WOE_Scoring, family = binomial(link = 'logit'), data = dane.analiza.woe)
summary(model)

scoring <- predict(model, newdata = dane.analiza.woe, type = "response")
oceny <- ifelse(scoring < 0.5, 0, 1)

tabela <- table(Przewidywane = oceny, Rzeczywiste = dane.analiza.woe$Default)
print(tabela)

poprawne <- sum(diag(tabela)) / sum(tabela)
cat(paste("Poprawnoœæ: ", floor(poprawne * 100), "%"))
cat(paste(" Odsetek b³êdnych klasyfikacji: ", 100 - floor(poprawne * 100), "%"))