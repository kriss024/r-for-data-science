rm(list = ls())
set.seed(123456)

p <- rnorm(2000000)
p1.min <- min(p)
p1.max <- max(p)
p1.range <- p1.max - p1.min
p1.half <- p1.min + p1.range / 2

Age <- 20 + p * 5

df1 <- data.frame(Age, Default = as.factor(1), p)

p <- rnorm(200000)
p1.min <- min(p)
p1.max <- max(p)
p1.range <- p1.max - p1.min
p1.half <- p1.min + p1.range / 2

Age <- 20 + p * 5

df1.a <- data.frame(Age, Default = as.factor(0), p)

p <- rnorm(3000000)
p1.min <- min(p)
p1.max <- max(p)
p1.range <- p1.max - p1.min
p1.half <- p1.min + p1.range / 2

Age <- 40 + p * 5

df2 <- data.frame(Age, Default = as.factor(0), p)

p <- rnorm(300000)
p1.min <- min(p)
p1.max <- max(p)
p1.range <- p1.max - p1.min
p1.half <- p1.min + p1.range / 2

Age <- 40 + p * 5

df2.a <- data.frame(Age, Default = as.factor(1), p)

p <- rnorm(1000000)
p1.min <- min(p)
p1.max <- max(p)
p1.range <- p1.max - p1.min
p1.half <- p1.min + p1.range / 2

Age <- 60 + p * 5

df3 <- data.frame(Age, Default = as.factor(1), p)

p <- rnorm(100000)
p1.min <- min(p)
p1.max <- max(p)
p1.range <- p1.max - p1.min
p1.half <- p1.min + p1.range / 2

Age <- 60 + p * 5

df3.a <- data.frame(Age, Default = as.factor(0), p)


dataset <- rbind(df1, df1.a, df2, df2.a, df3, df3.a)

sort <- order(dataset$p)

dataset.sort <- dataset[sort,]

index <- sample(1:nrow(dataset.sort), 1000, replace = FALSE)

dataset.sample <- dataset.sort[index,]

dataset.sample$p <- NULL

library(ggplot2)
ggplot(dataset.sample, aes(x = Age, fill = Default)) + geom_density(alpha = .3)

#------------------------------------------------

library(rpart)
fit <- rpart(Default ~ Age, method = "class", data = dataset.sample)
print(fit)
plot(fit, uniform = TRUE, main = "Classification Tree")
text(fit, use.n = TRUE, all = TRUE, cex = .8)

library("partykit")
pred.node <- predict(as.party(fit), type = "node")
dataset.node <- data.frame(dataset.sample, Node = as.factor(pred.node))


library(party)
fit <- ctree(Default ~ Age, data = dataset.sample)
plot(fit, main = "Conditional Inference Tree")
pred.node <- predict(fit, dataset.sample, type = "node")
dataset.node <- data.frame(dataset.sample, Node = as.factor(pred.node))

#------------------------------------------------

library(dplyr)

summarize <- dataset.node %>%
  select(Age, Node) %>%
  group_by(Node) %>%
  summarise(Age.min = min(Age), Age.max = max(Age))

#------------------------------------------------


library(InformationValue)
options(scipen = 999, digits = 4)
woe <- WOETable(X = dataset.node$Node, Y = dataset.node$Default)
print(woe)

woe.cat <- data.frame(Node = as.factor(woe$CAT), WOE = woe$WOE)
dataset.node.woe <- merge(dataset.node, woe.cat, by = "Node")

library(MASS)
dataset.node.woe.glm <- glm(Default ~ WOE, data = dataset.node.woe, family = binomial(logit))
summary(dataset.node.woe.glm)

pred.response <- predict(dataset.node.woe.glm, newdata = dataset.node.woe, type = "response")
Default.predicted <- ifelse(pred.response < 0.5, 0, 1)

dataset.Default.predicted <- data.frame(dataset.node.woe, Default.predicted)

cross.table <- table(Predicted = dataset.Default.predicted$Default.predicted, Original = dataset.Default.predicted$Default)
print(cross.table)

misclassified <- 1 - sum(diag(cross.table)) / sum(cross.table)

library(scales)
paste <- paste('Misclassification error', percent(misclassified))
noquote(paste)

#------------------------------------------------

library(woeBinning)
binning <- woe.binning(dataset.node, 'Default', 'Node', event.class = '1')
print(binning)

tabulate.binning <- woe.binning.table(binning)
print(tabulate.binning)

dataset.node.woe <- woe.binning.deploy(dataset.node, binning, add.woe.or.dum.var = 'woe')

library(MASS)
dataset.node.woe.glm <- glm(Default ~ woe.Node.binned, data = dataset.node.woe, family = binomial(logit))
summary(dataset.node.woe.glm)

pred.response <- predict(dataset.node.woe.glm, newdata = dataset.node.woe, type = "response")
Default.predicted <- ifelse(pred.response < 0.5, 0, 1)

dataset.Default.predicted <- data.frame(dataset.node.woe, Default.predicted)

cross.table <- table(Predicted = dataset.Default.predicted$Default.predicted, Original = dataset.Default.predicted$Default)
print(cross.table)

misclassified <- 1 - sum(diag(cross.table)) / sum(cross.table)

library(scales)
paste <- paste('Misclassification error', percent(misclassified))
noquote(paste)