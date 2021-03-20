rm(list = ls())

data(iris)

iris.numeric <- iris[, 1:4]

library(FactoMineR)

pca = PCA(iris.numeric, scale.unit = TRUE,  graph = FALSE)

print(pca$var$coord)

library(ggplot2)

scores = data.frame(pca$ind$coord, Species = iris$Species, Species.num = as.numeric(as.factor(iris$Species)))

ggplot(data = scores, aes(x = Dim.1, y = Dim.2, color = Species, label = Species)) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(alpha = 0.8, size = 4) +
  ggtitle("PCA plot of Iris datset")