data(iris)
dane<-data.matrix(iris[,-5])
grupy<-iris[,5]

svd_deko<-svd(dane)

library(Matrix)
u<-svd_deko$u
v<-svd_deko$v
s<-svd_deko$d
l<-length(s)
diag_matrix<-Diagonal(l, s)

library(lattice)
xyplot(s~1:l, grid = TRUE, type="l")

a<-u %*% diag_matrix %*% t(v)

u_new<- dane %*% v %*% solve(diag_matrix)

x<-u[,1]
y<-u[,2]


xyplot(y~x, grid = TRUE, groups=grupy, auto.key = TRUE,cex=1.5)

pca <- prcomp(dane, center = TRUE, scale = TRUE) 

pc<-predict(pca, newdata=dane)

x<-pc[,1]
y<-pc[,2]

xyplot(y~x, grid = TRUE, groups=grupy, auto.key = TRUE,cex=1.5)

