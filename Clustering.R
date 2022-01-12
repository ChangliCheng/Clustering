if (!require("psych")) {
  install.packages("psych")
  library("psych")
}

RNGkind(sample.kind = "Rejection")
set.seed(50)

#read in data
setwd("C:/Users/ccheng2/Desktop/QTM3/Data")
cereals <- read.csv("cereals.csv")
#Check if there any missing value
apply(cereals,2,anyNA)
#replace missing values with their mean
cereals$carbo[is.na(cereals$carbo)] <- 
  mean(cereals$carbo[!is.na(cereals$carbo)])
anyNA(cereals$carbo)

cereals$sugars[is.na(cereals$sugars)] <- 
  mean(cereals$sugars[!is.na(cereals$sugars)])
anyNA(cereals$sugars)

cereals$potass[is.na(cereals$potass)] <- 
  mean(cereals$potass[!is.na(cereals$potass)])
anyNA(cereals$potass) 
#store in a data frame that will be normalized (scaled)
myData <- cereals[c(3:ncol(cereals))]
#name all rows with the Company label
row.names(myData) <- cereals$name
#scale numerical data
myDataScaled <- scale(myData)
#compute normalized distance based on scaled Sales and Fuel Cost
distNorm <- dist(myDataScaled, method="euclidean")

#create hierarchical clusters based on specific linkage method
hcAverage <- hclust(distNorm, method="average")


#plot the clusters (dendrogram)
plot(hcAverage, hang = -1, ann = FALSE)
#compute the cluster membership by "cutting the dendrogram"
memb <- cutree(hcAverage,k=4)
memb
#store data and cluster into a dataframe
myDataClusteredH <- data.frame(myData,memb)
#check the cluster number for Almond_Delight
myDataClusteredH["Almond_Delight", 11] #11th column
#return the centroid of each cluster
aggregate(myDataClusteredH, by=list(memb), mean)
#display the full summary statistics for each cluster
describeBy(myDataClusteredH, myDataClusteredH$memb)
#K-means clustering
#specify the number of clusters
k = 3

#run k-means algorithm
kmOut <- kmeans(myDataScaled,k)

#show cluster membership
kmOut$cluster
#store data and cluster into a dataframe
myDataClusteredK <- data.frame(myData,kmOut$cluster)
#check the cluster number for Almond_Delight
myDataClusteredK["Almond_Delight", 11] #11th column
#show centroids
kmOut$centers
#display the full summary statistics for each cluster
describeBy(myDataClusteredK, myDataClusteredK$kmOut.cluster)
