
#Name : Zullinira Dwi Utami

library(tidyverse)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)

Dataset: Iris
Flow :
  - melakukan PCA sehingga mereduksi data Iris menjadi 2 kolom saja
  - Melakukan K-Means Clustering dari data Iris yang telah berhasil direduksi. 


# Import data iris
data(iris)

# Hilangkan kolom 'label' bunga sehingga data 'terkesan' seperti unlabeled
df <- cbind(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width)

# Buatlah PCA dari df
pca_iris = prcomp(df, center = TRUE, scale = TRUE)

# Lihat summary model PCA untuk menentukan eigenvalue 1 dan 2
summary(pca_iris)

# Berdasarkan yang didapat, nilai eigen value 1 dan 2 berturut2 adalah 1.7084 dan 0.9560

#Berdasarkan data yang didapat informasi yang dapat dijelaskan oleh oleh PCA1 dan PCA2 adalah 
#sebesar 0.7296 + 0.2285 =0.9581. Berarti bisa menjelaskan 95% data dan kehilangan kurang lebih 5% data.


# Reduksilah data iris menjadi 2 kolom saja
iris_transform = as.data.frame(-pca_iris$x[,1:2])

# Kemudian, lakukan evaluasi k-means terhadap iris_transform
fviz_nbclust(iris_transform, kmeans, method = 'wss')
fviz_nbclust(iris_transform, kmeans, method = 'silhouette')
fviz_nbclust(iris_transform, kmeans, method = 'gap_stat')

# Pilihlah k paling optimal
k = 3

# Visualisasikan k-means clustering pada data Iris yang telah tereduksi
kmeans_iris = kmeans(iris_transform, centers = k, nstart = 50)
fviz_cluster(kmeans_iris, data = iris_transform)


