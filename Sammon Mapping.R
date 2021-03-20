rm(list=ls())
data("iris")
org_dane<-iris
num_dane<-org_dane[,1:4]
lab<-org_dane[,5]

summary(num_dane)
for(y in 1:150) 
{
  for(x in 1:4) 
  {
    num_dane[y,x]<-num_dane[y,x]+rnorm(1)/(10^6)
    
  }
}

summary(num_dane)
#Nieliniowe skalowanie wielowymiarowe (Sammon Mapping)

odleglosci <- dist(num_dane)

library(MASS)
noweSammon <- sammon(odleglosci, k=2, trace=FALSE)

x<-noweSammon$points[,1]
y<-noweSammon$points[,2]

plot(x,y, pch=18, col="blue", panel.first = grid(), xlab="Dimension 1", ylab="Dimension 2")
title(main = "Nieliniowe skalowanie wielowymiarowe (Sammon Mapping)")
text(x, y, lab, cex=0.6, pos=4, col="red")a4

dens <- kde2d(x,y)
filled.contour(dens, color.palette = heat.colors)
title(main = "Kernel Density Estimation")


#Skalowanie wielowymiarowe Kruskalla (MDS, ang. Multidimensional Scaling)
library(cluster)
niepodobienstwa <- daisy(num_dane)
skalowanie <- isoMDS(niepodobienstwa, k=2)

x<-noweSammon$points[,1]
y<-noweSammon$points[,2]

plot(x,y, pch=18, col="blue", panel.first = grid(), xlab="Dimension 1", ylab="Dimension 2")
title(main = "Skalowanie wielowymiarowe Kruskalla (MDS, ang. Multidimensional Scaling)")
text(x, y, lab, cex=0.6, pos=4, col="red")

# Ward Hierarchical Clustering
odleglosci <- dist(num_dane, method = "euclidean")
fit <- hclust(odleglosci, method="ward.D") 
plot(fit)

# Load the kohonen package 
require(kohonen)

# the SOM training process. 
data(wines)
set.seed(7)

training <- sample(nrow(wines), 120)
Xtraining <- scale(wines[training, ])
Xtest <- scale(wines[-training, ],
               center = attr(Xtraining, "scaled:center"),
               scale = attr(Xtraining, "scaled:scale"))

som.wines <- som(Xtraining, grid = somgrid(5, 5, "hexagonal"))

som.prediction <- predict(som.wines, newdata = Xtest,
                          trainX = Xtraining,
                          trainY = factor(wine.classes[training]))
table(wine.classes[-training], som.prediction$prediction)


# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(num_dane, method.hclust="ward.D", method.dist="euclidean")
plot(fit)
pvrect(fit, alpha=.95)

