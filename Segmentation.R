dane<-iris[,1:4]
lbl<-iris[,5]

fac <- as.factor(lbl)
num<-as.numeric(fac)

summary(dane)

dane2<-scale(dane, center = FALSE, scale = TRUE)
summary(dane2)

# Classical MDS
d <- dist(dane2)
fit <- cmdscale(d,eig=TRUE, k=2)

x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Multidimensional Scaling",	type="n")
text(x, y, labels = lbl, cex=.7, col=num*2)

# K-Means Cluster Analysis
km.fit <- kmeans(dane2, 3)
summary(km.fit)

x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="K-Means Cluster Analysis",	type="n")
num<-km.fit$cluster
text(x, y, labels = lbl, cex=.7, col=num*2)

