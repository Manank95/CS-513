## Manank Valand
## 10429101
## Assignment 8

## reference from lecture code provided for hclustering on canvas.
rm(list=ls())

# load and remove NA
dsn<-read.csv('C://Users/manan/Desktop/R/breast-cancer-wisconsin.data.csv', na.strings = '?')
dsn1<-na.omit(dsn)
h_dist <-dist(dsn1[,2:10])

# Hirarchical clustering with average linkage method
h_results<-hclust(h_dist, method="average")
plot(h_results)

h_clust <-cutree(h_results, k=2)
plot(h_clust)
#h_clust


# K-means clustering method
k_means<-kmeans(dsn1[,2:10], centers=2)
k_means$size
plot(k_means$cluster)
str(k_means)