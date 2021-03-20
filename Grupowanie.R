rm(list = ls())
library(ggplot2)
library(ggrepel)
library(xlsx)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

ggplot(iris, aes(x=Petal.Length, y=Petal.Width, shape=Species, color=Species)) +
    geom_point(alpha = 0.5) +
    geom_text_repel(aes(label = Species,  color = Species), size = 3)+
    scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

mydata<-iris[,c('Petal.Length', 'Petal.Width', 'Species')]
row.names(mydata) <- paste(mydata$Species,row.names(mydata), sep = ".")
mydata$Species<-NULL

# Prepare Data
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables
head(mydata)

# Kendall correlation distance
distance <- get_dist(mydata)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#K-Means Clustering 
clusters <- 3
fit <- kmeans(mydata, clusters)
print(fit$centers)

# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)

# append cluster assignment
mydata.cluster <- data.frame(mydata, Id.cluster = factor(fit$cluster))
write.xlsx(mydata.cluster, "mydata_cluster.xlsx", row.names = FALSE)

table(fit$cluster, iris$Species)

fviz_cluster(fit, data = mydata)

ggplot(mydata.cluster, aes(x=Petal.Length, y=Petal.Width, shape=Id.cluster, color=Id.cluster)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(aes(label = Id.cluster,  color = Id.cluster), size = 3)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))


#The interpretation of the silhouette width is the following:
#  Si > 0 means that the observation is well clustered. The closest it is to 1, the best it is clustered.
#  Si < 0 means that the observation was placed in the wrong cluster.
#  Si = 0 means that the observation is between two clusters.
# The silhouette plot below gives us evidence that our clustering using four groups is good because there’s no negative silhouette width and most of the values are bigger than 0.5.

clusters <- 3
fit <- kmeans(mydata, clusters)
sil <- silhouette(fit$cluster, dist(mydata))
fviz_silhouette(sil)

# Total within sum of square
fviz_nbclust(mydata, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 2)
# We can implement this in R with the following code. 
# The results suggest that 3 is the optimal number of clusters as it appears to be the bend in the knee (or elbow).

# Average silhouette for kmeans
fviz_nbclust(mydata, kmeans, method = "silhouette")

#Gap Statistic Method
gap_stat <- clusGap(mydata, FUN = kmeans, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# Gap statistic for hierarchical clustering
gap_stat <- clusGap(mydata, FUN = hcut, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

#C-Means Clustering
library(e1071)
clusters <- 3
fit <- cmeans(mydata, clusters)
print(fit$centers)

mydata.cluster <- data.frame(mydata, Id.cluster = factor(fit$cluster))
write.xlsx(mydata.cluster, "mydata_cluster.xlsx", row.names = FALSE)

table(fit$cluster, iris$Species)

ggplot(mydata.cluster, aes(x=Petal.Length, y=Petal.Width, shape=Id.cluster, color=Id.cluster)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(aes(label = Id.cluster,  color = Id.cluster), size = 3)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))


# Ward Hierarchical Clustering
# method "average"  "ward.D2", "single", "complete", "average", "median", "centroid"
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
print(fit)

plot(fit) # display dendogram

clusters <- 3
groups <- cutree(fit, clusters) # cut tree into 3 clusters

# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=clusters, border="red")

mydata.cluster <- data.frame(mydata, Id.cluster = factor(groups))
write.xlsx(mydata.cluster, "mydata_cluster.xlsx", row.names = FALSE)

table(groups, iris$Species)

ggplot(mydata.cluster, aes(x=Petal.Length, y=Petal.Width, shape=Id.cluster, color=Id.cluster)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(aes(label = Id.cluster,  color = Id.cluster), size = 3)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))