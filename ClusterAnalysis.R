expect <- read.csv("/Users/poojadesai/Downloads/MVA/lfe.csv")
View(expect)

install.packages("cluster", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(cluster)
library(readr)
head(expect)

sapply(expect, function(x) sum(is.na(x)))
expect <- expect[complete.cases(expect),]  ## to remove which has null values
sapply(expect, function(x) sum(is.na(x)))
expect_x <- subset.data.frame(expect, Year == "2000")
expect_y <- expect_x[4:13,5:14]
View(expect_y)
colnames(expect_y) <- rownames(expect_y)
dist.expect <- as.dist(expect_y)
dist.expect

#Single 
expect_single <- hclust(dist.expect,method = "single")
plot(expect_single,hang=-1,xlab="Object",ylab="Distance",
     main="Dendogram.Nearest neighbour linkage")

#Default - Complete
expect_complete <- hclust(dist.expect)
plot(expect_complete,hang=-1,xlab="Object",ylab="Distance",
     main="Dendogram.Farthest neighbour linkage")

#Average 
expect_average <- hclust(dist.expect,method = "average")
plot(expect_average,hang=-1,xlab="Object",ylab="Distance",
     main="Dendogram.Group average linkage")
#Hierarchial Clustering
#Creating Euclidean distance matrix of the standardized data.
expect.can <- scale(expect_y)
expect.euclidean <- dist(expect.can,method = "euclidean")

#Invoking hclust command (cluster analysis by single linkage method)
expect_hclust <- hclust(expect.euclidean,method="single")

#Plotting vertical dendogram
plot(as.dendrogram(expect_hclust),ylab="Distance Calculation",
     ylim= c(0,8),main="Vertical Dendogram")

#Plotting Horizontal Dendogram
plot(as.dendrogram(expect_hclust),ylab="Distance Calculation",
     ylim= c(0,8),main="Horizontal Dendogram")

plot(as.dendrogram(expect_hclust),xlab="Distance Calculation",
     xlim= c(8,0),horiz = TRUE,main="Horizontal Dendogram")

# We will use agnes function as it allows us to select option for data standardization, 
#the distance measure and clustering algorithm in one single function
?agnes
(agn.expect <- agnes(expect_y, metric="euclidean", stand=TRUE, method = "single"))
View(agn.expect)

#Description of cluster merging
agn.expect$merge

#Dendogram
plot(as.dendrogram(agn.expect), xlab= "Distance between Countries",xlim=c(10,0),
     horiz = TRUE)

#Interactive Plots
plot(agn.expect,ask=TRUE)
plot(agn.expect, which.plots=2)

#K-Means Clustering

attach(expect_y)
# Standardizing the data with scale()
matstd.employ <- scale(expect_y)
# K-means, k=2, 3, 4, 5, 6
# Centers (k's) are numbers thus, 10 random sets are chosen
(kmeans2.expect <- kmeans(expect_y,2,nstart = 1))
# Computing the percentage of variation accounted for. Two clusters
perc.var.2 <- round(100*(1 - kmeans2.expect$betweenss/kmeans2.expect$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2

# Computing the percentage of variation accounted for. Three clusters
(kmeans3.expect <- kmeans(expect_y,3,nstart = 1))
perc.var.3 <- round(100*(1 - kmeans3.expect$betweenss/kmeans3.expect$totss),1)
names(perc.var.3) <- "Perc. 3 clus"
perc.var.3

#Saving three k-means clusters in a list
clus1 <- matrix(names(kmeans3.expect$cluster[kmeans3.expect$cluster == 1]), 
                ncol=1, nrow=length(kmeans3.expect$cluster[kmeans3.expect$cluster == 1]))
colnames(clus1) <- "Cluster 1"
clus2 <- matrix(names(kmeans3.expect$cluster[kmeans3.expect$cluster == 2]), 
                ncol=1, nrow=length(kmeans3.expect$c3luster[kmeans3.expect$cluster == 2]))
colnames(clus2) <- "Cluster 2"
clus3 <- matrix(names(kmeans3.expect$cluster[kmeans3.expect$cluster == 3]), 
                ncol=1, nrow=length(kmeans3.expect$cluster[kmeans3.expect$cluster == 3]))
colnames(clus3) <- "Cluster 3"
list(clus1,clus2,clus3,clus4)

detach(expect)