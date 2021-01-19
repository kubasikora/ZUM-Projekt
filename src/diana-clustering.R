library(mltools)
library(cluster)
library(dplyr)
library(proxy)
library(clv)
library(clusterCrit)
library(fpc)

source("./dataPreparation.R")
playersAttributesFinal <- subset(playersAttributesFinal, select=c("GK.Skills", "Tackling", "Shooting", "Short.Ball.Skills"))

#### select random sample from the data
SAMPLE_RATIO <- 0.1
clusteringInput <- slice_sample(playersAttributesFinal, prop=SAMPLE_RATIO)
summary(clusteringInput)

#### range of numbers of clusters to create during tests
minK = 2
maxK = 40

##########################################################################################

#### distance matrices for different methods
euclideanDistanceMatrix <- dist(clusteringInput, method="Euclidean")
euclideanDistanceMatrixFull <- dist(playersAttributesFinal, method="Euclidean")

# results
result.euclidean.centroids <- list()
result.euclidean.clustering <- list()
result.euclidean.metrics <- list()
result.euclidean.compare.outfield <- list()
result.euclidean.compare.general <- list()
result.euclidean.compare.specific <- list()

print("===========================================================================")
print("Euclidean")
print("===========================================================================")


result <- diana(euclideanDistanceMatrix, diss=TRUE)
for (k in minK:maxK) {
  # find initial clusters for sampled data.frame
  clustering <- cutree(result, k=k)
  print(paste("Found ", k, "clusters"))
  
  # find centroids of the clusters
  idx <- (clustering == 1)
  centroids <- colMeans(clusteringInput[idx,])
  for (i in 2:k) {
    idx <- (clustering == i)
    centroids <- rbind(centroids, colMeans(clusteringInput[idx,]))
  }
  print(paste("Found diana centroids"))
  
  distances <- dist(playersAttributesFinal, centroids, method="Euclidean")
  result.euclidean.centroids[[k-minK+1]] <- centroids
  
  # find clusters for all examples
  result.euclidean.clustering[[k-minK+1]] <- apply(distances, 1, which.min)
  print(paste("Found clusters for all examples"))
  
  # find metric values 
  result.euclidean.metrics[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[k-minK+1]])
  result.euclidean.compare.outfield[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
  result.euclidean.compare.general[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
  result.euclidean.compare.specific[[k-minK+1]] <- cluster.stats(euclideanDistanceMatrixFull, result.euclidean.clustering[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
  
  print(paste("Evaluated metrics for", k, "clusters"))
  print("===========================================================================")
}


rm(euclideanDistanceMatrix)
rm(euclideanDistanceMatrixFull)

##########################################################################################


manhattanDistanceMatrix <- dist(clusteringInput, method="Manhattan")
manhattanDistanceMatrixFull <- dist(playersAttributesFinal, method="Manhattan")

result.manhattan.centroids <- list()
result.manhattan.clustering <- list()
result.manhattan.metrics <- list()
result.manhattan.compare.outfield <- list()
result.manhattan.compare.general <- list()
result.manhattan.compare.specific <- list()

print("===========================================================================")
print("Manhattan")
print("===========================================================================")


result <- diana(manhattanDistanceMatrix, diss=TRUE)
for (k in minK:maxK) {
  # find initial clusters for sampled data.frame
  clustering <- cutree(result, k=k)
  print(paste("Found ", k, "clusters"))
  
  # find centroids of the clusters
  idx <- (clustering == 1)
  centroids <- colMeans(clusteringInput[idx,])
  for (i in 2:k) {
    idx <- (clustering == i)
    centroids <- rbind(centroids, colMeans(clusteringInput[idx,]))
  }
  print(paste("Found diana centroids"))
  
  distances <- dist(playersAttributesFinal, centroids, method="Manhattan")
  result.manhattan.centroids[[k-minK+1]] <- centroids
  
  # find clusters for all examples
  result.manhattan.clustering[[k-minK+1]] <- apply(distances, 1, which.min)
  print(paste("Found clusters for all examples"))
  
  # find metric values 
  result.manhattan.metrics[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[k-minK+1]])
  result.manhattan.compare.outfield[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
  result.manhattan.compare.general[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
  result.manhattan.compare.specific[[k-minK+1]] <- cluster.stats(manhattanDistanceMatrixFull, result.manhattan.clustering[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
  
  print(paste("Evaluated metrics for", k, "clusters"))
  print("===========================================================================")
}

rm(manhattanDistanceMatrix)
rm(manhattanDistanceMatrixFull)

##########################################################################################

minkowskiDistanceMatrix <- dist(clusteringInput, method="Minkowski", p=3)
minkowskiDistanceMatrixFull <- dist(playersAttributesFinal, method="Minkowski", p=3)

# results
result.minkowski.centroids <- list()
result.minkowski.clustering <- list()
result.minkowski.metrics <- list()
result.minkowski.compare.outfield <- list()
result.minkowski.compare.general <- list()
result.minkowski.compare.specific <- list()

print("===========================================================================")
print("Minkowski")
print("===========================================================================")


result <- diana(minkowskiDistanceMatrix, diss=TRUE)
for (k in minK:maxK) {
  # find initial clusters for sampled data.frame
  clustering <- cutree(result, k=k)
  print(paste("Found ", k, "clusters"))
  
  # find centroids of the clusters
  idx <- (clustering == 1)
  centroids <- colMeans(clusteringInput[idx,])
  for (i in 2:k) {
    idx <- (clustering == i)
    centroids <- rbind(centroids, colMeans(clusteringInput[idx,]))
  }
  print(paste("Found diana centroids"))
  
  distances <- dist(playersAttributesFinal, centroids, method="Minkowski", p=3)
  result.minkowski.centroids[[k-minK+1]] <- centroids
  
  # find clusters for all examples
  result.minkowski.clustering[[k-minK+1]] <- apply(distances, 1, which.min)
  print(paste("Found clusters for all examples"))
  
  # find metric values 
  result.minkowski.metrics[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[k-minK+1]])
  result.minkowski.compare.outfield[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
  result.minkowski.compare.general[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
  result.minkowski.compare.specific[[k-minK+1]] <- cluster.stats(minkowskiDistanceMatrixFull, result.minkowski.clustering[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
  
  print(paste("Evaluated metrics for", k, "clusters"))
  print("===========================================================================")
}


rm(minkowskiDistanceMatrix)
rm(minkowskiDistanceMatrixFull)

##########################################################################################

correlationDistanceMatrix <- dist(clusteringInput, method="correlation")
correlationDistanceMatrixFull <- dist(playersAttributesFinal, method="correlation")

# results
result.correlation.centroids <- list()
result.correlation.clustering <- list()
result.correlation.metrics <- list()
result.correlation.compare.outfield <- list()
result.correlation.compare.general <- list()
result.correlation.compare.specific <- list()

print("===========================================================================")
print("Correlation")
print("===========================================================================")


result <- diana(correlationDistanceMatrix, diss=TRUE)
for (k in minK:maxK) {
  # find initial clusters for sampled data.frame
  clustering <- cutree(result, k=k)
  print(paste("Found ", k, "clusters"))
  
  # find centroids of the clusters
  idx <- (clustering == 1)
  centroids <- colMeans(clusteringInput[idx,])
  for (i in 2:k) {
    idx <- (clustering == i)
    centroids <- rbind(centroids, colMeans(clusteringInput[idx,]))
  }
  print(paste("Found diana centroids"))
  
  distances <- dist(playersAttributesFinal, centroids, method="correlation")
  result.correlation.centroids[[k-minK+1]] <- centroids
  
  # find clusters for all examples
  result.correlation.clustering[[k-minK+1]] <- apply(distances, 1, which.min)
  print(paste("Found clusters for all examples"))
  
  # find metric values 
  result.correlation.metrics[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[k-minK+1]])
  result.correlation.compare.outfield[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[k-minK+1]], alt.clustering=outfieldTrueClustering, compareonly=TRUE)
  result.correlation.compare.general[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[k-minK+1]], alt.clustering=generalTrueClustering, compareonly=TRUE)
  result.correlation.compare.specific[[k-minK+1]] <- cluster.stats(correlationDistanceMatrixFull, result.correlation.clustering[[k-minK+1]], alt.clustering=specificTrueClustering, compareonly=TRUE)
  
  print(paste("Evaluated metrics for", k, "clusters"))
  print("===========================================================================")
}


rm(correlationDistanceMatrix)
rm(correlationDistanceMatrixFull)

save(result.euclidean.centroids,
     result.euclidean.clustering,
     result.euclidean.metrics,
     result.euclidean.compare.outfield,
     result.euclidean.compare.general,
     result.euclidean.compare.specific,
    
     result.manhattan.centroids,
     result.manhattan.clustering,
     result.manhattan.metrics,
     result.manhattan.compare.outfield,
     result.manhattan.compare.general,
     result.manhattan.compare.specific,
    
     result.minkowski.centroids,
     result.minkowski.clustering,
     result.minkowski.metrics,
     result.minkowski.compare.outfield,
     result.minkowski.compare.general,
     result.minkowski.compare.specific,
    
     result.correlation.centroids,
     result.correlation.clustering,
     result.correlation.metrics,
     result.correlation.compare.outfield,
     result.correlation.compare.general,
     result.correlation.compare.specific,
    
     clusteringInput, 
     file="../results/diana-results-shortened.RData"
)