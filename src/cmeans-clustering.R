library(mltools)
library(cluster)
library(dplyr)
library(proxy)
library(clv)
library(clusterCrit)
library(fclust)
library(fpc)
library(ppclust)
library(e1071)

source("./dataPreparation.R")

#### select random sample from the data
SAMPLE_RATIO <- 0.1
clusteringInput <- slice_sample(playersAttributesFinal, prop=SAMPLE_RATIO)
summary(clusteringInput)

minK = 2
maxK = 40

#### fanny version -> fuzzy fanny with euclidean distance

print("Euclidean")
euclideanDistanceMatrixFull <- dist(playersAttributesFinal, method="Euclidean")
result.euclidean.ppclusts <- list()
result.euclidean.centroids <- list()
result.euclidean.memberships <- list()
result.euclidean.metrics <- list()

for(i in minK:maxK){
  # find initial clusters for sampled data.frame
  result <- fcm(playersAttributesFinal, i, dmetric="euclidean")
  result.euclidean.ppclusts[[i-minK+1]] <- result
  result.euclidean.memberships <- result$u
  result.euclidean.centroids <- result$v
  
  print(paste("Found ", i, "clusters"))

  # find metric values 
  result.euclidean.metrics[i-minK+1] <- SIL.F(euclideanDistanceMatrixFull, result.euclidean.memberships[[i-minK+1]], alpha=1, distance=TRUE)
  print(paste("Evaluated metrics for", i, "clusters"))
}
rm(euclideanDistanceMatrixFull)


#### fanny version -> fuzzy fanny with minkowski distance


print("Minkowski")
minkowskiDistanceMatrixFull <- dist(playersAttributesFinal, method="Minkowski", p=3)
result.minkowski.ppclusts <- list()
result.minkowski.centroids <- list()
result.minkowski.memberships <- list()
result.minkowski.metrics <- list()

for(i in minK:maxK){
  # find initial clusters for sampled data.frame
  result <- fcm(playersAttributesFinal, i, dmetric="minkowski", pw=3)
  result.minkowski.ppclusts[[i-minK+1]] <- result
  result.minkowski.memberships <- result$u
  result.minkowski.centroids <- result$v
  
  print(paste("Found ", i, "clusters"))
  
  # find metric values 
  result.minkowski.metrics[i-minK+1] <- SIL.F(minkowskiDistanceMatrixFull, result.minkowski.memberships[[i-minK+1]], alpha=1, distance=TRUE)
  print(paste("Evaluated metrics for", i, "clusters"))
}
rm(minkowskiDistanceMatrixFull)


#### fanny version -> fuzzy fanny with manhattan distance


print("Manhattan")
manhattanDistanceMatrixFull <- dist(playersAttributesFinal, method="Manhattan")
result.manhattan.ppclusts <- list()
result.manhattan.centroids <- list()
result.manhattan.memberships <- list()
result.manhattan.metrics <- list()

for(i in minK:maxK){
  # find initial clusters for sampled data.frame
  result <- fcm(playersAttributesFinal, i, dmetric="manhattan")
  result.manhattan.ppclusts[[i-minK+1]] <- result
  result.manhattan.memberships <- result$u
  result.manhattan.centroids <- result$v
  
  print(paste("Found ", i, "clusters"))
  
  # find metric values 
  result.manhattan.metrics[i-minK+1] <- SIL.F(manhattanDistanceMatrixFull, result.manhattan.memberships[[i-minK+1]], alpha=1, distance=TRUE)
  print(paste("Evaluated metrics for", i, "clusters"))
}
rm(manhattanDistanceMatrixFull)



#### fanny version -> fuzzy fanny with manhattan distance


print("Correlation")
correlationDistanceMatrixFull <- dist(playersAttributesFinal, method="correlation")
result.correlation.ppclusts <- list()
result.correlation.centroids <- list()
result.correlation.memberships <- list()
result.correlation.metrics <- list()

for(i in minK:maxK){
  # find initial clusters for sampled data.frame
  result <- fcm(playersAttributesFinal, i, dmetric="correlation")
  result.correlation.ppclusts[[i-minK+1]] <- result
  result.correlation.memberships <- result$u
  result.correlation.centroids <- result$v
  
  print(paste("Found ", i, "clusters"))
  
  # find metric values 
  result.correlation.metrics[i-minK+1] <- SIL.F(correlationDistanceMatrixFull, result.correlation.memberships[[i-minK+1]], alpha=1, distance=TRUE)
  print(paste("Evaluated metrics for", i, "clusters"))
}
rm(correlationDistanceMatrixFull)

#### store results in Rdata file for further analysis

save(result.correlation.ppclusts, 
     result.correlation.centroids, 
     result.correlation.memberships,
     result.correlation.metrics,
     
     result.manhattan.ppclusts, 
     result.manhattan.centroids, 
     result.manhattan.memberships, 
     result.manhattan.metrics,
     
     result.minkowski.ppclusts, 
     result.minkowski.memberships, 
     result.minkowski.centroids,
     result.minkowski.metrics,
     
     result.euclidean.ppclusts, 
     result.euclidean.memberships, 
     result.euclidean.centroids, 
     result.euclidean.metrics,
     
     file="./results/cmeans-results-full.RData", version=2
)
