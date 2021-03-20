rm(list = ls(all = TRUE))

SAdata = read.table("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data", sep = ",", head = T, row.names = 1)

saveRDS(SAdata, file = "SAdata.Rda")

SAdata <- readRDS(file = "SAdata.Rda")

k10.cross.validation <- function(equation, df) {

    n <- nrow(df)
    c <- ncol(df)
    c.dummy <- (c + 1)
    missing.array <- 0

    k.index <- sample(1:10, n, replace = TRUE)
    df.dummy <- data.frame(df, k.index.dummy_ = k.index)

    for (i in 1:10) {
        k = i;
        df.dummy.test <- df.dummy[which(df.dummy[, 'k.index.dummy_'] == k), - c.dummy]
        df.dummy.train <- df.dummy[which(df.dummy[, 'k.index.dummy_'] != k), - c.dummy]

        fit.glm <- glm(equation, data = df.dummy.train, family = binomial())
        y.pred <- predict(fit.glm, newdata = df.dummy.test, type = "response")
        y.pred <- ifelse(y.pred > 0.5, 1, 0)

        target <- all.vars(equation)[1]
        a <- df.dummy.test[, target]
        b <- y.pred
        cross.table <- table(a, b)
        good <- sum(diag(cross.table))
        all <- sum(cross.table)
        accuracy <- good / all
        missing <- 1 - accuracy
        missing.array[i] <- missing
    }

    misclassification.rate <- mean(missing.array)

    return(misclassification.rate)
}

missing.rate <- k10.cross.validation(chd ~ age, SAdata)
print(missing.rate)

missing.rate <- k10.cross.validation(chd ~ age + famhist + typea + tobacco + ldl, SAdata)
print(missing.rate)