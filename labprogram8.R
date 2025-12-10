library(rattle)
library(ggplot2)
library(cluster)
library(factoextra)

normalize <- function(data){
  return((data-min(data))/(max(data)-min(data)))
}

wine <- wine
wine_data <- wine[,-1]
wine_norm <- as.data.frame(lapply(wine_data,normalize))
wine_pca <- prcomp(wine_norm,scale=TRUE)
summary(wine_pca)

wine_pca_data <- as.data.frame(wine_pca$x[,1:2])
elbow_wine <- fviz_nbclust(wine_pca_data,kmeans,method="wss")
print(elbow_wine)
silhouette_wine <- fviz_nbclust(wine_pca_data,kmeans,method="silhouette")
print(silhouette_wine)

set.seed(123)
wine_kmeans <- kmeans(wine_pca_data,centers=3,nstart=25)
wine_pca_data$cluster <- as.factor(wine_kmeans$cluster)

p1 <- ggplot(wine_pca_data,aes(x=PC1,y=PC2,color=cluster))+
  geom_point(size=3)+
  labs(title="K-Means Clustering on Wine Dataset")
print(p1)

cat(wine_kmeans$size)