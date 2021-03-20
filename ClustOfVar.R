library(mlbench)
library(Hmisc)

data(PimaIndiansDiabetes)
pima <- PimaIndiansDiabetes[, 1:8]
m <- data.matrix(pima)
colnames(m) <- colnames(pima)
clust <- varclus(m, similarity = "spear")
plot(clust)

#----------------------------
library(ClustOfVar)
tree <- hclustvar(m)
plot(tree)

stab <- stability(tree, B = 40)
plot(stab, main = "Stability of the partitions")
part <- cutreevar(tree, 3)


library(corrplot)
corr <- cor(part$scores)
corrplot(corr, type = "upper", order = "hclust", method = "color")

df <- data.frame(part$scores, pima)
corr <- cor(df)
corrplot(corr, type = "upper", order = "hclust", method = "color")